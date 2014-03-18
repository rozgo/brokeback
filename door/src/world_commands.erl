-module(world_commands).

-export([submit/2, world/0]).

-define(ACTION_GET_BASE, 100000).
-define(ACTION_FETCH_TILE, 200000).
-define(ACTION_FETCH_FILE, 3).
-define(ACTION_PUT_TILE, 4).

-define(RESPONSE_TYPE_TILE, 2115261812).
-define(TILE_TYPE_BASE, 2063089).

submit(Pid, << Cmd:32/integer-little, Rest/binary >>) ->
    world_proc() ! {Pid, Cmd, Rest}.

world() ->
    {ok, Hostname} = application:get_env(door, redis_hostname),
    {ok, Port}     = application:get_env(door, redis_port),
    {ok, Redis}    = eredis:start_link(Hostname, Port),
    world(Redis).

world(C) ->
    receive 
        {Pid, ?ACTION_GET_BASE, Rest} ->
            Pid ! {push, get_user_base(C, Rest)};
        {Pid, ?ACTION_FETCH_TILE, Rest} -> 
            case fetch_tile(C, Rest) of
                undefined ->
                    ok;
                Tile ->
                    Pid ! {push, Tile}
            end;
        {Pid, ?ACTION_FETCH_FILE, Rest} ->
            Pid ! fetch_file(C, Rest);
        {Pid, ?ACTION_PUT_TILE, Rest} ->
            Pid ! put_tile(C, Rest);
        {_Pid, Cmd, _} -> 
            io:format("Mensaje desconocido: ~p~n", [Cmd]);
        _Message ->
            io:format("Mensaje invalido: ~p~n", [_Message])
    end,
    world(C).

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
    fetch_tile(C, X, Y).

fetch_tile(C, X, Y) when is_float(X), is_float(Y) ->
    Key = format_coord(X, Y),
    {ok, Result} = eredis:q(C, ["GET", Key]),
    io:format("fetch_tile(~p) -> ~p~n", [Key, Result]),
    Result.

fetch_file(C, TileList) when is_binary(TileList) ->
    Commands = [ ["GET", format_coord(X, Y) ] || <<X:64/float, Y:64/float>> <= TileList ],
    Res = eredis:qp(C, Commands),
    [ Value || {ok, Value} <- Res, Value /= undefined ].

put_tile(C, <<X:64/float, Y:64/float, Rest/binary>>) ->
    put_tile(C, X, Y, Rest).

put_tile(C, X, Y, TileInfo) ->
    Key = format_coord(X, Y),
    io:format("put_tile(~p) <- ~p~n", [Key, TileInfo]),
    {ok, _} = eredis:q(C, ["SET", Key, TileInfo]),
    ok.


%% Private functions

generate_base(C, Username) ->
    random:seed(now()),
    X = float(round(random:uniform() * 100.0)),
    Y = float(round(random:uniform() * 100.0)),
    PaddedUsername = list_to_binary(string:left(binary_to_list(Username), 10, 32)),
    Tile = <<?RESPONSE_TYPE_TILE:32/little, X:64/float-little, Y:64/float-little, PaddedUsername/binary, ?TILE_TYPE_BASE:32/little, 0:32/little>>,
    % <<?Res:32/little, X:64/float-little, Y:64/float-little, Username:10/binary, TileType:32/little, Base:32/little>>,
    put_tile(C, X, Y, Tile),
    Tile.

format_user(Username) ->
    io_lib:format("U[~s]", [ binary_to_list(Username) ]).

format_coord(X, Y) when is_float(X), is_float(Y) -> 
    lists:flatten(io_lib:format("C[~p,~p]", [round(X), round(Y)])).

world_proc() ->
    case global:whereis_name(world) of
        undefined ->
            Pid = spawn(?MODULE, world, []),
            global:register_name(world, Pid),
            Pid;
        Pid -> Pid
    end.