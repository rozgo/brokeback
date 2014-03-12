-module(time_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {MegaSecs, Secs, _} = os:timestamp(),
    Timestamp = MegaSecs * 1000000 + Secs,
    {ok, Req2} = cowboy_req:reply(200, [
            {<<"content-type">>, <<"application/json">>}
        ], mochijson2:encode({struct,[{timestamp,Timestamp}]}), Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
