-module(door_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/",  door_handler, []},
            {"/1/[:cmd/:arg]", safe_handler, []},
            {"/c", chat_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    door_sup:start_link().

stop(_State) ->
    ok.