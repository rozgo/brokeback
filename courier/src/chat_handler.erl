-module(chat_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(sock, {rooms}).

init({tcp, http}, _Req, _Opts) ->
    io:format("~p: ~p~n", [self(), "init"]),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    io:format("~p: ~p~n", [self(), "websocket_init"]),
    erlang:start_timer(10000, self(), ok),
    {ok, Req, #sock{ rooms = ordsets:new() }}.

websocket_handle({text, Msg}, Req, State) ->
    io:format("~p: ~p ~s~n", [self(), "websocket_handle", Msg]),
    ParsedCmd = mochijson2:decode(Msg),
    NewState = handle_command(State, ParsedCmd),
    io:format("NewState = ~p~n", [NewState]),
    {ok, Req, NewState};
websocket_handle(_Data, Req, State) ->
    io:format("~p: ~p ~p~n", [self(), "websocket_handle", _Data]),
    {ok, Req, State}.

websocket_info({message, Room, Username, MessageText}, Req, State) ->
    Msg = mochijson2:encode({struct, 
        [
            {cmd, message},
            {room, Room},
            {user, Username},
            {text, MessageText}
        ]
    }),
    io:format("Sent: ~s~n", [Msg]),
    {reply, {text, Msg}, Req, State};
websocket_info({joined, Room, Username}, Req, State) ->
    Msg = mochijson2:encode({struct, 
        [
            {cmd, joined},
            {room, Room},
            {user, Username}
        ]
    }),
    io:format("Sent: ~s~n", [lists:flatten(Msg)]),
    {reply, {text, Msg}, Req, State};  
websocket_info({left, Room, Username}, Req, State) ->
    Msg = mochijson2:encode({struct, 
        [
            {cmd, left},
            {room, Room},
            {user, Username}
        ]
    }),
    io:format("Sent: ~s~n", [Msg]),
    {reply, {text, Msg}, Req, State};  
websocket_info({history, Room, History}, Req, State) ->
    Msg = mochijson2:encode({struct, 
        [
            {cmd, history},
            {room, Room},
            {hist, [ {struct, [{user, User}, {text, Text}]} || {User, Text} <- History ]}
        ]
    }),
    io:format("Sent: ~s~n", [Msg]),
    {reply, {text, Msg}, Req, State};  
websocket_info({timeout, _Ref, _Msg}, Req, State) ->
    erlang:start_timer(10000, self(), ok),
    {reply, ping, Req, State};
websocket_info(_Info, Req, State) ->
    io:format("websocket_info: ~p~n", [_Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, S) ->
    io:format("~p: ~p ~p~n", [self(), "websocket_terminate", _Reason]),
    ordsets:fold(fun (Room, _) ->
        channel:leave(Room)
    end, ok, S#sock.rooms),
    ok.

handle_command(S, {struct, CmdObj}) ->
    io:format("~p: ~p~n", ["handle_command", CmdObj]),
    case proplists:lookup(<<"cmd">>, CmdObj) of 
        {_, <<"join">>} ->
            handle(join, S, CmdObj);
        {_, <<"msg">>} ->
            handle(msg, S, CmdObj);
        {_, <<"leave">>} ->
            handle(leave, S, CmdObj)
    end.

get_room_from_cmd(CmdObj, Create) ->
    {_, RoomName} = proplists:lookup(<<"chan">>, CmdObj),
    channel:get_channel(RoomName, Create).

handle(join, S, CmdObj) ->
    Room = get_room_from_cmd(CmdObj, true),
    {_, User} = proplists:lookup(<<"user">>, CmdObj),
    channel:join_user(Room, User),
    S#sock{ rooms = ordsets:add_element(Room, S#sock.rooms) };

handle(msg, S, CmdObj) ->
    Room = get_room_from_cmd(CmdObj, false),
    {_, Msg} = proplists:lookup(<<"msg">>, CmdObj),
    channel:send_message(Room, Msg),
    S;

handle(leave, S, CmdObj) ->
    Room = get_room_from_cmd(CmdObj, false),
    channel:leave(Room),
    S#sock{ rooms = ordsets:del_element(Room, S#sock.rooms) }.