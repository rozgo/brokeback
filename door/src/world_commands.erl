-module(world_commands).

-export([submit/2, world/0]).

-define(PARAM_MAX_TILE_LISTENERS, 20).

-define(ACTION_GET_BASE,     100000).
-define(ACTION_FETCH_TILE,   200000).
-define(ACTION_TELEPORT,     300000).
-define(ACTION_BATTLE_CMD,   400000).
-define(ACTION_SPAWN_RES,    500000).
-define(ACTION_MOBILIZE,     600000).
-define(ACTION_CREATE_CBASE, 700000).

-define(RESPONSE_TYPE_TILE,        2115261812).
-define(RESPONSE_TYPE_BATTLE_CMD, -1248618669).
-define(RESPONSE_TYPE_MOBILIZE,   -1896823039).

-define(TILE_TYPE_BASE,        2063089).
-define(TILE_TYPE_CBASE,     -75274960).
-define(TILE_TYPE_LAND,              0).
-define(TILE_TYPE_RESOURCE, -276420562).

submit(Pid, << Cmd:32/integer-little, Rest/binary >>) ->
    world_proc() ! {Pid, Cmd, Rest}.

world() ->
    {ok, Hostname} = application:get_env(door, redis_hostname),
    {ok, Port}     = application:get_env(door, redis_port),
    io:format("Redis Connection ~s:~p~n", [Hostname, Port]),
    {ok, Redis}    = eredis:start_link(Hostname, Port),
    world(Redis, ets:new(tile_listeners, [set])).

world(C, TileList) ->
    receive 
        {Pid, ?ACTION_GET_BASE, Rest} ->
            Pid ! {push, get_user_base(C, Rest)};
        {Pid, ?ACTION_FETCH_TILE, Rest} -> 
            io:format("Cmd: ~p~n", [?ACTION_FETCH_TILE]),
            case fetch_tile(C, Rest) of
                {Coord, not_found} ->
                    add_tile_listener(C, TileList, Coord, Pid);
                {Coord, Tile} -> 
                    add_tile_listener(C, TileList, Coord, Pid),
                    io:format("Broadcast tile ~p~n", [Coord]),
                    Pid ! {push, Tile};
                _ -> io:format("fetch_tile => ?", [])
            end;
        {_Pid, ?ACTION_TELEPORT, Rest} ->
            io:format("Cmd: ~p~n", [?ACTION_TELEPORT]),
            {BaseCoord, Base, LandCoord, Land} = teleport_base(C, Rest),
            BaseKey = make_key_for(coord, BaseCoord),
            LandKey = make_key_for(coord, LandCoord),
            notify_listeners(TileList, BaseKey, Base),
            notify_listeners(TileList, LandKey, Land);
        {_Pid, ?ACTION_BATTLE_CMD, Rest} ->
            io:format("Cmd: ~p~n", [?ACTION_BATTLE_CMD]),
            {Coord, Cmd} = exec_battle_command(C, Rest),
            broadcast_to_listeners(TileList, Coord, Cmd);
        {_Pid, ?ACTION_SPAWN_RES, Rest} ->
            io:format("Cmd: ~p~n", [?ACTION_SPAWN_RES]),
            {Coord, Res} = spawn_resource(C, Rest),
            broadcast_to_listeners(TileList, Coord, Res);
        {_Pid, ?ACTION_MOBILIZE, Rest} ->
            io:format("Cmd: ~p~n", [?ACTION_MOBILIZE]),
            {From, To, Mob} = exec_mobilization(C, Rest),
            broadcast_to_listeners(TileList, From, Mob),
            broadcast_to_listeners(TileList, To  , Mob);
        {_Pid, ?ACTION_CREATE_CBASE, Rest} ->
            io:format("Cmd: ~p~n", [?ACTION_CREATE_CBASE]),
            {Coord, Base} = create_campaign_base(C, Rest),
            broadcast_to_listeners(TileList, Coord, Base);
        {_Pid, Cmd, _} -> 
            io:format("Mensaje desconocido: ~p~n", [Cmd]);
        _Message ->
            io:format("Mensaje invalido: ~p~n", [_Message])
    end,
    world(C, TileList).

add_tile_listener(C, Listeners, {X, Z}, Pid) ->
    TileKey = make_key_for(coord, X, Z),
    AppendListener = fun(Pids) ->
        case lists:member(Pid, Pids) of
            false ->
                io:format("~p listening to ~s~n", [[Pid | Pids], TileKey]),
                broadcast_events_history(C, {X, Z}, Pid), 
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
        Pid ! {push, Blob}
    end, ok, Pids);
broadcast(Pid, Blobs) when is_pid(Pid), is_list(Blobs) ->
    lists:foldl(fun(Blob, _) ->
        Pid ! {push, Blob}
    end, ok, Blobs).

notify_listeners(Listeners, TileKey, Tile) ->
    case ets:lookup(Listeners, TileKey) of
        [] -> ok;
        [{_, Pids}] ->
            io:format("Broadcast ~s to ~p~n", [TileKey, Pids]),
            broadcast(Pids, Tile)
    end.

broadcast_events_history(C, {X, Z}, Pid) ->
    broadcast_event_history(C, Pid, make_key_for(battle, X, Z)),
    broadcast_event_history(C, Pid, make_key_for(mobilization, X, Z)).

broadcast_event_history(C, Pid, EvtKey) ->
    case eredis:q(C, ["LRANGE", EvtKey, 0, -1]) of 
        {ok, Events} ->
            broadcast(Pid, Events);
        {_Other} -> io:format("Redis Error: ~p~n", [_Other])
    end.

broadcast_to_listeners(Listeners, {X, Z}, Cmd) ->
    TileKey = make_key_for(coord, X, Z),
    case ets:lookup(Listeners, TileKey) of
        [] ->
            ok;
        [{_, Pids}] ->
            broadcast(Pids, Cmd)
    end.

get_user_base(C, Username) ->
    Key = make_key_for(user, Username),
    {ok, Result} = eredis:q(C, ["GET", Key]),
    ResBase = case Result of 
        undefined ->
            Base = generate_base(C, Username),
            eredis:q(C, ["SET", Key, Base]),
            Base;
        Base ->
            Base
    end,
    ResBase.

fetch_tile(C, << X:64/float-little, Z:64/float-little >>) ->
    Key = make_key_for(coord, X, Z),
    case eredis:q(C, ["GET", Key]) of
        {ok, undefined} -> {{X, Z}, not_found};
        {ok, Tile} ->
            {{X, Z}, Tile}
    end.

teleport_base(C, <<Username:10/binary, X:64/float-little, Z:64/float-little>>) ->
    io:format("teleport_base(~p, ~p, ~p)~n", [binary_to_list(Username), X, Z]),
    {_, ?TILE_TYPE_BASE, OldX, OldY, _, BaseId} = explode_tile(get_user_base(C, Username)),
    NewBase = build_tile(?TILE_TYPE_BASE, X, Z, Username, BaseId),
    NewLand = build_tile(?TILE_TYPE_LAND, OldX, OldY),
    UserKey = make_key_for(user, Username),
    eredis:q(C, ["SET", UserKey, NewBase]),

    LandCoord = make_key_for(coord, OldX, OldY),
    eredis:q(C, ["SET", LandCoord, NewLand]),

    BaseCoord = make_key_for(coord, X, Z),
    eredis:q(C, ["SET", BaseCoord, NewBase]),
    {{X, Z}, NewBase, {OldX, OldY}, NewLand}.

exec_battle_command(C, <<BattleId:10/binary, X:64/float-little, Z:64/float-little, CmdStr/binary>>) ->
    CmdKey  = make_key_for(battle, X, Z),
    Cmd     = build_battle_cmd(BattleId, X, Z, CmdStr),
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
    push_expiring_event(C, CmdKey, Cmd, 60),
    {{X, Z}, Cmd}.

spawn_resource(C, <<X:64/float-little, Z:64/float-little, _:10/binary, ResType:32/little, Blob/binary>>) ->
    Tile = build_tile(?TILE_TYPE_RESOURCE, X, Z, ResType, Blob),
    ResKey = make_key_for(coord, X, Z),
    io:format("Resource@~s~n", [ResKey]),
    eredis:q(C, ["SET", ResKey, Tile]),
    {{X, Z}, Tile}.

exec_mobilization(C, <<FromX:64/float-little, FromY:64/float-little, ToX:64/float-little, ToY:64/float-little, 
    PlayerId:10/binary, Start:32/little, Duration:32/little, MobType:32/little>>) ->
    Mob = build_mobilization(FromX, FromY, ToX, ToY, PlayerId, Start, Duration, MobType),
    FromKey = make_key_for(mobilization, FromX, FromY),
    ToKey = make_key_for(mobilization, ToX, ToY),

    push_expiring_event(C, FromKey, Mob, Duration),
    push_expiring_event(C, ToKey,   Mob, Duration),
    {{FromX, FromY}, {ToX, ToY}, Mob}.

create_campaign_base(C, <<Username:10/binary, X:64/float-little, Z:64/float-little>>) ->
    Tile = build_tile(?TILE_TYPE_CBASE, X, Z, Username, 0),
    put_tile(C, X, Z, Tile),
    {{X, Z}, Tile}.

% fetch_tiles(C, TileList) when is_binary(TileList) ->
%     Commands = [ ["GET", format_coord(X, Z) ] || <<X:64/float, Z:64/float>> <= TileList ],
%     Res = eredis:qp(C, Commands),
%     [ Value || {ok, Value} <- Res, Value /= undefined ].

put_tile(C, X, Z, TileInfo) ->
    Key = make_key_for(coord, X, Z),
    io:format("put_tile(~p) <- ~p~n", [Key, TileInfo]),
    case eredis:q(C, ["SET", Key, TileInfo]) of
        {ok, _} -> ok;
        {error, Reason} -> 
            io:format("Didn't put ~s because ~p~n", [Key, Reason]);
        _Other ->
            io:format("Wat? ~p~n", [_Other])
    end.

%% "Private" functions

push_expiring_event(C, EvtKey, Evt, Duration) ->
    eredis:q(C, ["RPUSH", EvtKey, Evt]),
    eredis:q(C, ["EXPIRE", EvtKey, Duration]).

build_base_tile(Type, X, Z, PlayerId, BaseId) ->
    PaddedPlayerId = pad_player_id(PlayerId),
    Coord = binary_coord(X, Z),
    <<?RESPONSE_TYPE_TILE:32/little-signed, Coord/binary, PaddedPlayerId/binary, Type:32/little-signed, BaseId:32/little>>.    

build_tile(?TILE_TYPE_BASE, X, Z, PlayerId, BaseId) ->
    build_base_tile(?TILE_TYPE_BASE, X, Z, PlayerId, BaseId);
build_tile(?TILE_TYPE_CBASE, X, Z, PlayerId, BaseId) ->
    build_base_tile(?TILE_TYPE_CBASE, X, Z, PlayerId, BaseId);
build_tile(?TILE_TYPE_RESOURCE, X, Z, ResType, Blob) ->
    EmptyOccupant = list_to_binary([ "0" || _ <- lists:seq(1, 10) ]),
    Coord = binary_coord(X, Z),
    <<?RESPONSE_TYPE_TILE:32/little-signed, Coord/binary, EmptyOccupant/binary, ResType:32/little, Blob/binary>>.

build_tile(?TILE_TYPE_LAND, X, Z) ->
    Coord = binary_coord(X, Z),
    <<?RESPONSE_TYPE_TILE:32/little-signed, Coord/binary>>.

build_battle_cmd(BattleId, X, Z, Command) ->
    Coord = binary_coord(X, Z),
    <<?RESPONSE_TYPE_BATTLE_CMD:32/little-signed, BattleId:10/binary, Coord/binary, Command/binary >>.

build_mobilization(FromX, FromY, ToX, ToY, PlayerId, Start, Duration, MobType) ->
    io:format("FromX = ~p, FromY = ~p, ToX = ~p, ToY = ~p, PlayerId = ~p, Start = ~p, Duration = ~p, MobType = ~p~n", [FromX, FromY, ToX, ToY, PlayerId, Start, Duration, MobType]),
    From = binary_coord(FromX, FromY),
    To   = binary_coord(ToX, ToY),
    PaddedPlayerId = pad_player_id(PlayerId),
    <<?RESPONSE_TYPE_MOBILIZE:32/little-signed, From/binary, To/binary, PaddedPlayerId/binary, Start:32/little, Duration:32/little, MobType:32/little>>.

binary_coord(X, Z) when is_float(X), is_float(Z) ->
    << X:64/float-little, Z:64/float-little>>.

pad_player_id(PlayerId) ->
    list_to_binary(string:left(binary_to_list(PlayerId), 10, 32)).

explode_tile(<<?RESPONSE_TYPE_TILE:32/little-signed, X:64/float-little, Z:64/float-little,  ?TILE_TYPE_LAND:32/little>>) ->
    {?RESPONSE_TYPE_TILE, ?TILE_TYPE_LAND, X, Z};
explode_tile(<<?RESPONSE_TYPE_TILE:32/little-signed, X:64/float-little, Z:64/float-little, Username:10/binary, ?TILE_TYPE_BASE:32/little, BaseId:32/little>>) ->
    {?RESPONSE_TYPE_TILE, ?TILE_TYPE_BASE, X, Z, binary_to_list(Username), BaseId};
explode_tile(_Other) ->
    io:format("ERROR - Can't decode tile: ~p~n", [_Other]).

explode_battle_cmd(<<?RESPONSE_TYPE_BATTLE_CMD:32/little-signed, BattleId:10/binary, X:64/float-little, Z:64/float-little, Command/binary>>) ->
    {?RESPONSE_TYPE_BATTLE_CMD, BattleId, X, Z, Command}.

get_empty_tile(C) ->
    <<Ra:32, Rb:32, Rc:32>> = crypto:rand_bytes(12),
    random:seed({Ra, Rb, Rc}),
    X = float(round(random:uniform() * 20.0) - 10),
    Z = float(round(random:uniform() * 20.0) - 10),
    Key = make_key_for(coord, X, Z),
    case eredis:q(C, ["GET", Key]) of
        {ok, undefined} -> {X, Z};
        {ok, _} ->
            get_empty_tile(C)
    end.

generate_base(C, Username) ->
    {X, Z} = get_empty_tile(C),
    Tile = build_tile(?TILE_TYPE_BASE, X, Z, Username, 0),
    put_tile(C, X, Z, Tile),
    Tile.

make_key_for(coord, X, Z) when is_float(X), is_float(Z) -> 
    io_lib:format("C[~p,~p]", [round(X), round(Z)]);
make_key_for(battle, X, Z) when is_float(X), is_float(Z) -> 
    io_lib:format("B[~p,~p]", [round(X), round(Z)]);
make_key_for(mobilization, X, Z) when is_float(X), is_float(Z) -> 
    io_lib:format("M[~p,~p]", [round(X), round(Z)]).

make_key_for(coord, {X, Z}) when is_float(X), is_float(Z) -> 
    make_key_for(coord, X, Z);
make_key_for(user, Username) ->
    io_lib:format("U[~s]", [ binary_to_list(Username) ]).

world_proc() ->
    case global:whereis_name(world) of
        undefined ->
            Pid = spawn(?MODULE, world, []),
            global:register_name(world, Pid),
            Pid;
        Pid -> Pid
    end.