-module(door_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/",  door_handler, []},
            {"/s", safe_handler, []},
            {"/c", chat_handler, []},
            {"/n", name_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 9000}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    door_sup:start_link().

stop(_State) ->
    ok.
