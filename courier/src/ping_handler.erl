-module(ping_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
	{Path, Req2} = cowboy_req:path(Req),
    {ok, Req3} = cowboy_req:reply(200, [
            {<<"content-type">>, <<"text/plain">>}
        ], <<Path/binary>>, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
