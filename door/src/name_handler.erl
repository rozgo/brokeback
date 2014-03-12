-module(name_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Query = mochijson2:decode(Body),
    {Code, Result} = command(Query),
    {ok, Req3} = cowboy_req:reply(Code,[
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"content-type">>, <<"application/json">>}],
    Result, Req2),

    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

command({struct,[{<<"method">>,<<"reserve">>},{<<"namespace">>,Namespace},{<<"name">>,Name}]}) ->
    {Key, Json} = prepare(Namespace, Name),
    {ok, Code, _} = aws:s3_get(Key),
    case Code of
        404 -> {reserve(Key,Json), Json};
        _ -> {410, Json} % HTTP_GONE
    end;
command({struct,[{<<"method">>,<<"generate">>},{<<"namespace">>,Namespace}]}) ->
    Name = util:str("Player.~s",[util:gen_id()]),
    {Key, Json} = prepare(Namespace, Name),
    {reserve(Key,Json), Json}.

prepare(Namespace, Name) ->
    Json = util:str("\"~s\"",[Name]),
    Hash = util:hash(Name),
    Key = util:str("Names/~s/~s", [Namespace,Hash]),
    {Key, Json}.

reserve(Key, Json) ->
    {ok, Code} = aws:s3_put(Key, Json),
    Code.
