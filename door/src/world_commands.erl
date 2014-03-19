-module(world_commands).

-export([submit/2, world/0, parse_tile/1]).

-define(PARAM_MAX_TILE_LISTENERS, 20).

-define(ACTION_GET_BASE,   100000).
-define(ACTION_FETCH_TILE, 200000).
-define(ACTION_TELEPORT,   300000).
-define(ACTION_BATTLE_CMD, 400000).
-define(ACTION_SPAWN_RES,  500000).

-define(RESPONSE_TYPE_TILE, 2115261812).
-define(RESPONSE_TYPE_BATTLE_CMD, -1248618669).

-define(TILE_TYPE_BASE,     2063089).
-define(TILE_TYPE_LAND,     2063089).
-define(TILE_TYPE_RESOURCE, -276420562).

submit(Pid, << Cmd:32/integer-little, Rest/binary >>) ->
    world_proc() ! {Pid, Cmd, Rest};
submit(Pid, _Other) ->
    io:format("Pid ~p invalid binary sequence ~p~n", [Pid, _Other]).

world() ->
    {ok, Hostname} = application:get_env(door, redis_hostname),
    {ok, Port}     = application:get_env(door, redis_port),
    io:format("Redis Connection ~s:~p~n", [Hostname, Port]),
    {ok, Redis}    = eredis:start_link(Hostname, Port),
    world(Redis, ets:new(tile_listeners, [set])).

world(C, TileList) ->
    receive 
        {Pid, ?ACTION_GET_BASE, Rest} ->
            % io:format("Cmd: ~p~n", [?ACTION_GET_BASE]),
            Pid ! {push, get_user_base(C, Rest)};
        {Pid, ?ACTION_FETCH_TILE, Rest} -> 
            % io:format("Cmd: ~p~n", [?ACTION_FETCH_TILE]),
            case fetch_tile(C, Rest) of
                {Coord, not_found} ->
                    add_tile_listener(C, TileList, Coord, Pid);
                {Coord, Tile} -> 
                    add_tile_listener(C, TileList, Coord, Pid),
                    Pid ! {push, Tile};
                _ -> io:format("fetch_tile => ?", [])
            end;
        {_Pid, ?ACTION_TELEPORT, Rest} ->
            io:format("Cmd: ~p~n", [?ACTION_TELEPORT]),
            {BaseCoord, Base, LandCoord, Land} = teleport_base(C, Rest),
            BaseKey = format_coord(BaseCoord),
            LandKey = format_coord(LandCoord),
            notify_listeners(TileList, BaseKey, Base),
            notify_listeners(TileList, LandKey, Land);
        {_Pid, ?ACTION_BATTLE_CMD, Rest} ->
            io:format("Cmd[~p]: ~p~n", [_Pid, ?ACTION_BATTLE_CMD]),
            {Coord, Cmd} = exec_battle_command(C, Rest),
            broadcast_to_listeners(TileList, Coord, Cmd);
        {_Pid, ?ACTION_SPAWN_RES, Rest} ->
            io:format("Cmd[~p]: ~p~n", [_Pid, ?ACTION_SPAWN_RES]),
            {Coord, Res} = spawn_resource(C, Rest),
            broadcast_to_listeners(TileList, Coord, Res);
        {_Pid, Cmd, _} -> 
            io:format("Mensaje desconocido: ~p~n", [Cmd]);
        _Message ->
            io:format("Mensaje invalido: ~p~n", [_Message])
    end,
    world(C, TileList).

add_tile_listener(C, Listeners, {X, Y}, Pid) ->
    TileKey = format_coord(X, Y),
    CmdKey = format_battle(X, Y),
    add_tile_listener(C, Listeners, TileKey, CmdKey, Pid).

add_tile_listener(C, Listeners, TileKey, CmdKey, Pid) ->
    AppendListener = fun(Pids) ->
        case lists:member(Pid, Pids) of
            false ->
                io:format("~p listening to ~p~n", [[Pid | Pids], TileKey]),
                broadcast_cmd_history(C, CmdKey, Pid), 
                [Pid | Pids];
            true  -> Pids
        end
    end,
    case ets:lookup(Listeners, TileKey) of
        [] ->
            ets:insert(Listeners, {TileKey, AppendListener([])});
        [{_, Pids}] when length(Pids) < 10 ->
            ets:insert(Listeners, {TileKey, AppendListener(Pids)});
        [{_, Pids}] ->
            ets:insert(Listeners, {TileKey, AppendListener(lists:sublist(Pids, ?PARAM_MAX_TILE_LISTENERS))})
    end.

broadcast(Pids, Blob) when is_list(Pids), is_binary(Blob) ->
    lists:foldl(fun(Pid, _) ->
        io:format("Notifying ~p~n", [Pid]),
        Pid ! {push, Blob}
    end, ok, Pids);
broadcast(Pid, Blobs) when is_pid(Pid), is_list(Blobs) ->
    lists:foldl(fun(Blob, _) ->
        io:format("Notifying ~p~n", [Pid]),
        Pid ! {push, Blob}
    end, ok, Blobs).


notify_listeners(Listeners, TileKey, Tile) ->
    case ets:lookup(Listeners, TileKey) of
        [] -> ok;
        [{_, Pids}] ->
            broadcast(Pids, Tile)
    end.

broadcast_cmd_history(C, CmdKey, Pid) ->
    case eredis:q(C, ["LRANGE", CmdKey, 0, -1]) of 
        {ok, BattleCommands} ->
            broadcast(Pid, BattleCommands);
        {_Other} -> io:format("Redis Error: ~p~n", [_Other])
    end.

broadcast_to_listeners(Listeners, {X, Y}, Cmd) ->
    TileKey = format_coord(X, Y),
    case ets:lookup(Listeners, TileKey) of
        [] ->
            ok;
        [{_, Pids}] ->
            broadcast(Pids, Cmd)
    end.

get_user_base(C, Username) ->
    Key = format_user(Username),
    io:format("Key lookup: ~s~n", [Key]),
    {ok, Result} = eredis:q(C, ["GET", Key]),
    ResBase = case Result of 
        undefined ->
            Base = generate_base(C, Username),
            eredis:q(C, ["SET", Key, Base]),
            Base;
        Base ->
            Base
    end,
    io:format("Base ~p~n", [ResBase]),
    ResBase.

fetch_tile(C, << X:64/float-little, Y:64/float-little >>) ->
    Key = format_coord(X, Y),
    case eredis:q(C, ["GET", Key]) of
        {ok, undefined} -> {{X, Y}, not_found};
        {ok, Tile} ->
            {{X, Y}, Tile}
    end.

teleport_base(C, <<Username:10/binary, X:64/float-little, Y:64/float-little>>) ->
    io:format("teleport_base(~p, ~p, ~p)~n", [binary_to_list(Username), X, Y]),
    {_, ?TILE_TYPE_BASE, OldX, OldY, _, BaseId} = explode_tile(get_user_base(C, Username)),
    NewBase = build_tile(?TILE_TYPE_BASE, X, Y, Username, BaseId),
    NewLand = build_tile(?TILE_TYPE_LAND, OldX, OldY),
    UserKey = format_user(Username),
    eredis:q(C, ["SET", UserKey, NewBase]),

    LandCoord = format_coord(OldX, OldY),
    eredis:q(C, ["SET", LandCoord, NewLand]),

    BaseCoord = format_coord(X, Y),
    eredis:q(C, ["SET", BaseCoord, NewBase]),
    {{X, Y}, NewBase, {OldX, OldY}, NewLand}.

exec_battle_command(C, <<BattleId:10/binary, X:64/float-little, Y:64/float-little, CmdStr/binary>>) ->
    CmdKey  = format_battle(X, Y),
    Cmd     = build_battle_cmd(BattleId, X, Y, CmdStr),
    case eredis:q(C, ["LINDEX", CmdKey, 0]) of
        {ok, undefined} ->
            ok;
        {ok, LastCmd} ->
            io:format("LastCmd: ~p~n", [LastCmd]),
            {_, LastBattleId, _, _, _} = explode_battle_cmd(LastCmd),
            if
                LastBattleId /= BattleId ->
                    eredis:q(C, ["DEL", CmdKey]);
                true -> 
                    ok
            end
    end,
    eredis:q(C, ["RPUSH", CmdKey, Cmd]),
    eredis:q(C, ["EXPIRE", CmdKey, 60]),
    {{X, Y}, Cmd}.

spawn_resource(C, <<X:64/float-little, Y:64/float-little, _:10/binary, ResType:32/little, Blob/binary>>) ->
    Tile = build_tile(?TILE_TYPE_RESOURCE, X, Y, ResType, Blob),
    ResKey = format_coord(X, Y),
    io:format("Resource@~s~n", [ResKey]),
    eredis:q(C, ["SET", ResKey, Tile]),
    {{X, Y}, Tile}.

% fetch_tiles(C, TileList) when is_binary(TileList) ->
%     Commands = [ ["GET", format_coord(X, Y) ] || <<X:64/float, Y:64/float>> <= TileList ],
%     Res = eredis:qp(C, Commands),
%     [ Value || {ok, Value} <- Res, Value /= undefined ].

put_tile(C, X, Y, TileInfo) ->
    Key = format_coord(X, Y),
    io:format("put_tile(~p) <- ~p~n", [Key, TileInfo]),
    case eredis:q(C, ["SET", Key, TileInfo]) of
        {ok, _} -> ok;
        {error, Reason} -> 
            io:format("Didn't put ~s because ~p~n", [Key, Reason]);
        _Other ->
            io:format("Wat? ~p~n", [_Other])
    end.

%% "Private" functions

build_tile(?TILE_TYPE_BASE, X, Y, Username, BaseId) ->
    PaddedUsername = list_to_binary(string:left(binary_to_list(Username), 10, 32)),
    <<?RESPONSE_TYPE_TILE:32/little-signed, X:64/float-little, Y:64/float-little, PaddedUsername/binary, ?TILE_TYPE_BASE:32/little, BaseId:32/little>>;
build_tile(?TILE_TYPE_RESOURCE, X, Y, ResType, Blob) ->
    EmptyOccupant = << <<0>> || _ <- lists:seq(1, 10) >>,
    <<?RESPONSE_TYPE_TILE:32/little-signed, X:64/float-little, Y:64/float-little, EmptyOccupant/binary, ResType:32/little, Blob/binary>>.

build_tile(?TILE_TYPE_LAND, X, Y) ->
    <<?RESPONSE_TYPE_TILE:32/little-signed, X:64/float-little, Y:64/float-little>>.

build_battle_cmd(BattleId, X, Y, Command) ->
    <<?RESPONSE_TYPE_BATTLE_CMD:32/little-signed, BattleId:10/binary, X:64/float-little, Y:64/float-little, Command/binary >>.

explode_tile(<<?RESPONSE_TYPE_TILE:32/little-signed, X:64/float-little, Y:64/float-little,  ?TILE_TYPE_LAND:32/little>>) ->
    {?RESPONSE_TYPE_TILE, ?TILE_TYPE_LAND, X, Y};
explode_tile(<<?RESPONSE_TYPE_TILE:32/little-signed, X:64/float-little, Y:64/float-little, Username:10/binary, ?TILE_TYPE_BASE:32/little, BaseId:32/little>>) ->
    {?RESPONSE_TYPE_TILE, ?TILE_TYPE_BASE, X, Y, binary_to_list(Username), BaseId};
explode_tile(_Other) ->
    io:format("ERROR - Can't decode tile: ~p~n", [_Other]).

explode_battle_cmd(<<?RESPONSE_TYPE_BATTLE_CMD:32/little-signed, BattleId:10/binary, X:64/float-little, Y:64/float-little, Command/binary>>) ->
    {?RESPONSE_TYPE_BATTLE_CMD, BattleId, X, Y, Command}.

generate_base(C, Username) ->
    random:seed(now()),
    X = float(round(random:uniform() * 100.0)),
    Y = float(round(random:uniform() * 100.0)),
    Tile = build_tile(?TILE_TYPE_BASE, X, Y, Username, 0),
    put_tile(C, X, Y, Tile),
    Tile.

parse_tile(Tile) ->
    <<Res:32/little, X:64/float-little, Y:64/float-little, Username:10/binary, TileType:32/little, Base:32/little>> = Tile,
    {Res, X, Y, Username, TileType, Base}.


format_user(Username) ->
    io_lib:format("U[~s]", [ binary_to_list(Username) ]).

format_coord({X, Y}) when is_float(X), is_float(Y) -> 
    format_coord(X, Y).

format_coord(X, Y) when is_float(X), is_float(Y) -> 
    lists:flatten(io_lib:format("C[~p,~p]", [round(X), round(Y)])).

format_battle(X, Y) when is_float(X), is_float(Y) -> 
    lists:flatten(io_lib:format("B[~p,~p]", [round(X), round(Y)])).

world_proc() ->
    case global:whereis_name(world) of
        undefined ->
            Pid = spawn(?MODULE, world, []),
            global:register_name(world, Pid),
            Pid;
        Pid -> Pid
    end.