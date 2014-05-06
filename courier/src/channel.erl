-module(channel).
-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-export([start/1, send_message/2, join_user/2, leave/1, get_channel/2]).

-record(channel, {name, users, history}).
-record(hitem, {username, message}).

-define(HISTORY_MAX_SIZE, 10).


%% gen_server interface

init([Name]) ->
	{ok, #channel{
		name    = Name,
		users   = orddict:new(),
		history = []
	}}.

handle_cast({message, Message, Pid}, S) when is_pid(Pid) ->
	case orddict:find(Pid, S#channel.users) of

		{ok, [Username]} ->

			Hitem = #hitem{username = Username, message = Message},
			Messages = case length(S#channel.history) of
				X when X >= ?HISTORY_MAX_SIZE -> 
					[Hitem | lists:sublist(S#channel.history, ?HISTORY_MAX_SIZE - 1) ];
				_ ->
					[Hitem | S#channel.history]
			end,

			% Broadcast new message
			orddict:fold(fun(CPid, _, _) ->
				CPid ! {message, S#channel.name, Username, Message}
			end, ok, S#channel.users),

			{noreply, S#channel{ history = Messages }};

		_ ->
			exit(Pid, kill),
			{noreply, S}
	end;

handle_cast({join, Username, Pid}, S) when is_pid(Pid) ->
	
	case orddict:find(Pid, S#channel.users) of
		{ok, _} ->
			{noreply, S};
		error -> % Client is added only if it doesn't belong to the channel already

			% Broadcast new member
			orddict:fold(fun (CPid, _, _) ->
				CPid ! {joined, S#channel.name, Username}
			end, ok, S#channel.users),

			% Add to member list
			Users = orddict:append(Pid, Username, S#channel.users),

			% Broadcast history to new member
			Pid ! {
				history, S#channel.name, [ Hi#hitem.message || Hi <- S#channel.history ]
				% users  , orddict:fold(fun(_, [Username], Acc) -> [ Username | Acc ] end, [], Users)
			},
			{noreply, S#channel{ users = Users }}
	end;

handle_cast({leave, Pid}, S) ->

	case orddict:find(Pid, S#channel.users) of

		{ok, [Username]} ->

			Users = orddict:erase(Pid, S#channel.users),
			
			case length(Users) of 
				X when X > 0 ->

					% Broadcast user leaving
					orddict:fold(fun(CPid, _, _) ->
						CPid ! {left, S#channel.name, Username}
					end, ok, Users),

					{noreply, S#channel{ users = Users }};
				_ ->
					% {stop, normal, S}
					{noreply, S}
			end;
		error ->
			{noreply, S}
	end.

handle_info(Msg, _S) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, _S}.

handle_call(Msg, _From, S) ->
	io:format("Unexpected call: ~p~n", [Msg]),
	{noreply, S}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, _S) ->
	io:format("Terminating chat server.~n");
terminate(Reason, _S) ->
	io:format("Terminating chat server: ~p~n", [Reason]).


%% public interface

start(Name) ->
	{ok, Pid} = gen_server:start(?MODULE, [Name], []),
	Pid.

send_message(Pid, Message) ->
	gen_server:cast(Pid, {message, Message, self()}).

join_user(Pid, Username) ->
	gen_server:cast(Pid, {join, Username, self()}).

leave(Pid) ->
	gen_server:cast(Pid, {leave, self()}).

get_channel(Name, _Spawn) ->
	global:set_lock({Name, self()}),
	Pid = case global:whereis_name({channel, Name}) of 
		undefined ->
			NewPid = ?MODULE:start(Name),
			global:register_name({channel, Name}, NewPid),
			NewPid;
		OldPid ->
			OldPid
	end,
	global:del_lock({Name, self()}),
	Pid.