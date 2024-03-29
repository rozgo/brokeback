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
            {"/n", name_handler, []},
            {"/l", look_handler, []},
            {"/t", time_handler, []},
            {"/i", info_handler, []},
            {"/p", push_handler, []},
            {"/e", echo_handler, []},
            {"/r", receipt_handler, []},
            {"/w", world_handler, []}
        ]}
    ]),
    PrivDir = code:priv_dir(door),
    io:format("PrivDir: ~s~n",[PrivDir]),
    {ok, _} = cowboy:start_http(https, 100,
        [
            {port, 9000}
            % {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
            % {certfile, PrivDir ++ "/ssl/server.crt"},
            % {keyfile, PrivDir ++ "/ssl/server.key"}
        ],
        [
            {env, [{dispatch, Dispatch}]}
        ]
    ),
    Result = door_sup:start_link(),

    Result.

stop(_State) ->
    ok.
