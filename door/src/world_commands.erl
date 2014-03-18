-module(world_commands).

-export([submit/2, world/0]).

-define(ACTION_GET_BASE, 100000).
-define(ACTION_FIND_TILE, 2).
-define(ACTION_FIND_TILES, 3).
-define(ACTION_PUT_TILE, 4).

-define(TYPE_TILE, 2115261812).
-define(TILE_TYPE_BASE, 2063089).

submit(Pid, << Cmd:32/integer-little, Rest/binary >>) ->
    world_proc() ! {Pid, Cmd, Rest}.

world() ->
    {ok, Redis} = eredis:start_link(
        "red1.toqrlt.0001.usw1.cache.amazonaws.com"
        % application:get_env(redis, port)
    ),
    world(Redis).

world(C) ->
    receive 
        {Pid, ?ACTION_GET_BASE, Rest} ->
            Pid ! {push, get_user_base(C, Rest)};
        {Pid, ?ACTION_FIND_TILE, Rest} -> 
            Pid ! find_tile(C, Rest);
        {Pid, ?ACTION_FIND_TILES, Rest} ->
            Pid ! find_tiles(C, Rest);
        {Pid, ?ACTION_PUT_TILE, Rest} ->
            Pid ! put_tile(C, Rest);
        {_Pid, Cmd, _} -> 
            io:format("Mensaje desconocido: ~p~n", [Cmd]);
        _Message ->
            io:format("Mensaje invalido: ~p~n", [_Message])
    end,
    world(C).

get_user_base(C, <<UserId:32/integer-little>>) ->
    Key = format_user(UserId),
    io:format("Key lookup: ~s~n", [Key]),
    {ok, Result} = eredis:q(C, ["GET", Key]),
    ResBase = case Result of 
        undefined ->
            Base = generate_base(UserId),
            eredis:q(C, ["SET", Key, Base]),
            Base;
        Base ->
            Base
    end,
    io:format("Base ~p~n", [ResBase]),
    ResBase.

generate_base(UserId) ->
    random:seed(now()),
    X = float(round(random:uniform() * 100.0)),
    Y = float(round(random:uniform() * 100.0)),
    <<?TYPE_TILE:32/integer-little, X:64/float-little, Y:64/float-little, UserId:32/integer-little, ?TILE_TYPE_BASE:32/integer-little, 0:32/integer-little>>.
    
format_user(UserId) ->
    io_lib:format("U[~p]", [UserId]).

find_tile(C, << X:64/float, Y:64/float >>) ->
    find_tile(C, X, Y).

find_tile(C, X, Y) when is_float(X), is_float(Y) ->
    Key = format_coord(X, Y),
    {ok, Result} = eredis:q(C, ["GET", Key]),
    Result.

find_tiles(C, TileList) when is_binary(TileList) ->
    Commands = [ ["GET", format_coord(X, Y) ] || <<X:64/float, Y:64/float>> <= TileList ],
    Res = eredis:qp(C, Commands),
    [ Value || {ok, Value} <- Res, Value /= undefined ].
% find_tiles(C, TileList) ->
%     Commands = [ ["GET", format_coord(X, Y)] || {X, Y} <- TileList ],
%     Res = eredis:qp(Commands),
%     [ Value || {ok, Value} <- Res, Value /= undefined ].

put_tile(C, <<X:64/float, Y:64/float, Rest/binary>>) ->
    put_tile(C, X, Y, Rest).

put_tile(C, X, Y, TileInfo) ->
    Key = format_coord(X, Y),
    {ok, _} = eredis:q(C, ["SET", Key, TileInfo]),
    ok.


%% Private functions

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