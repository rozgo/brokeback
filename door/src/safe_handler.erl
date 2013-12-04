-module(safe_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
	% io:format("req:~p state:~p~n",[Req,State]),
	io:format("state: ~p~n",[State]),
    {ok, Req2} = cowboy_req:reply(200, [
            {<<"content-type">>, <<"text/plain">>}
    ], <<"Hello world 4!">>, Req),
    {ok, Req2, state_incr(State)}.

terminate(_Reason, _Req, _State) ->
    ok.

state_incr(undefined) -> 0;
state_incr(State) -> State + 1.
