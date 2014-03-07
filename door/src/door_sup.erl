-module(door_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-spec start_link() -> {ok, pid()}.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [
    % statistics collection for New Relic
    % {statman_server, {statman_server, start_link, [1000]},
    %  permanent, 5000, worker, []},
    % {statman_aggregator, {statman_aggregator, start_link, []},
    %  permanent, 5000, worker, []}
  ],
  {ok, {{one_for_one, 10, 10}, Procs}}.
