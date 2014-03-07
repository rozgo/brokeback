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
            {"/r", receipt_handler, []}
        ]}
    ]),
    PrivDir = code:priv_dir(door),
    io:format("PrivDir: ~s~n",[PrivDir]),
    {ok, _} = cowboy:start_http(https, 100, [
        {port, 9000}
        % {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
        % {certfile, PrivDir ++ "/ssl/server.crt"},
        % {keyfile, PrivDir ++ "/ssl/server.key"}
        ], [{env, [{dispatch, Dispatch}]}
    ]),
    Result = door_sup:start_link(),

    % statistics collection for New Relic
    % statman_server:add_subscriber(statman_aggregator),
    % case application:get_env(newrelic, license_key) of
    %     undefined ->
    %         ok;
    %     _ ->
    %         newrelic_poller:start_link(fun newrelic_statman:poll/0)
    % end,

    Result.

stop(_State) ->
    ok.
