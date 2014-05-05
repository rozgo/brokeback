-module(courier_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
        {'_', [
            {"/", ping_handler, []},
            {"/t", cowboy_static, {
                priv_file, courier, "static/index.html"
            }},
            {"/c", chat_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(https, 100,
        [
            {port, 10000}
        ],
        [
            {env, [{dispatch, Dispatch}]}
        ]
    ),

    Result = courier_sup:start_link(),
    Result.

stop(_State) ->
    ok.
