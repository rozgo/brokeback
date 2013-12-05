-module(safe_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    %io:format("req:~p state:~p~n",[Req,State]),
    %io:format("state: ~p~n",[State]),

    {B,Req2} = cowboy_req:bindings(Req),
    io:format("bindings: ~p~n",[B]),

    [{arg,Arg},{cmd,Cmd}] = B,

    io:format("cmd: ~p~n",[Cmd]),
    io:format("arg: ~p~n",[Arg]),

    HttpC = command({cmd,Cmd},{arg,Arg}),

    {ok, Body, Req3} = cowboy_req:body(Req2),
    io:format("body: ~p~n",[Body]),

    {ok, Req4} = cowboy_req:reply(200,
        [
            {<<"Access-Control-Allow-Origin">>, <<"*">>},
            {<<"content-type">>, <<"application/json">>}
        ],
        HttpC, Req3),

    NewState = state_incr(State),
    NewState2 = state_incr(NewState),

    io:format("state: ~p ~p~n",[NewState,NewState2]),

    {ok, Req4, NewState}.

terminate(_Reason, _Req, _State) ->
    ok.

state_incr(undefined) -> 0;
state_incr(State) -> State + 1.

command({cmd,<<"classes">>},{arg,<<"GameServerConfig">>}) ->
    % {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = 
    {ok, {{_, 200, _}, _, Body}} = 
        httpc:request(get, {"http://s3-us-west-1.amazonaws.com/beyondgames-bow/GameServerConfig", []}, [], []),
    Body.
