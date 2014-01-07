-module(safe_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    io:format("BODY: ~s~n", [Body]),
    Query = mochijson2:decode(Body),
    {Code, Response} = command(Query),
    {ok, Req3} = cowboy_req:reply(Code,[
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"content-type">>, <<"application/json">>}],
    mochijson2:encode(Response), Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

command({struct,[{<<"method">>,<<"get">>},{<<"path">>,Path},{<<"objects">>,Objs}]}) ->
    safe:get_objs(Path, Objs, []);
command({struct,[{<<"method">>,<<"put">>},{<<"path">>,Path},{<<"objects">>,Objs}]}) ->
    safe:put_objs(Path, Objs, []);
command({struct,[{<<"method">>,<<"del">>},{<<"path">>,Path},{<<"objects">>,Objs}]}) ->
    safe:del_objs(Path, Objs).

