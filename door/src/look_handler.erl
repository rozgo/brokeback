-module(look_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    % io:format("BODY: ~s~n", [Body]),
    Query = mochijson2:decode(Body),
    io:format("~p~n", [Query]),
    {Code, Response} = command(Query),
    {ok, Req3} = cowboy_req:reply(Code,[
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"content-type">>, <<"application/json">>}],
    mochijson2:encode(Response), Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

command({struct,[{<<"method">>,<<"put">>},{<<"lookup">>,Name},{<<"key">>,Key},{<<"score">>,Score}]}) ->
    look_pid(Name) ! {put, {key, Key}, {score, Score}},
    {200, []};
command({struct,[{<<"method">>,<<"get">>},{<<"lookup">>,Name},{<<"low">>,Low},{<<"high">>,High}]}) ->
    look_pid(Name) ! {get, {who, self()}, {low, Low}, {high, High}},
    receive
        {matches, Matches} -> {200, Matches}
    after
        5000 -> {408, []}
    end.

look_pid(Name) ->
    case global:whereis_name({look, Name}) of
        undefined ->
            NewPid = spawn(look, start, [Name]),
            global:register_name({look, Name}, NewPid),
            NewPid;
        Pid -> Pid
    end.
