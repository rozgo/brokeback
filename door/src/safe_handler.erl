-module(safe_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    io:format("BODY: ~s~n", [Body]),
    Query = mochijson2:decode(Body),
    {Code, Response} = command(Query),
    {ok, Req3} = cowboy_req:reply(Code,[
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"content-type">>, <<"application/json">>}],
    mochijson2:encode(Response), Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

command({struct,[{<<"method">>,<<"get">>},{<<"path">>,Path},{<<"objects">>,Objs}]}) ->
    get_objs(Path, Objs, []);
command({struct,[{<<"method">>,<<"put">>},{<<"path">>,Path},{<<"objects">>,Objs}]}) ->
    put_objs(Path, Objs, []);
command({struct,[{<<"method">>,<<"del">>},{<<"path">>,Path},{<<"objects">>,Objs}]}) ->
    del_objs(Path, Objs).

get_objs(_, [], Classes) -> {200, Classes};
get_objs(Path, [Obj|Objs], Classes) ->
    Key = util:str("~s/~s", [Path,Obj]),
    {ok, Code, Data} = aws:s3_get(Key),
    case Code of
        404 -> {404, []};
        200 ->
            Class = {Obj,mochijson2:decode(Data)},
            get_objs(Path, Objs, [Class|Classes])
    end.

put_objs(_, [], NewIds) -> {200, NewIds};
put_objs(Path, [Obj|Objs], NewIds) ->
    {struct,[{<<"class">>,Class},{<<"instances">>,PreSrcInsts}]} = Obj,
    {SrcInsts, NewIds2} = make_insts(PreSrcInsts, [], NewIds),
    Key = util:str("~s/~s", [Path,Class]),
    {ok, Code, Data} = aws:s3_get(Key),
    case Code of
        200 ->
            Response = {Class,mochijson2:decode(Data)},
            {_,DstInsts} = Response,
            MergedInsts = set_insts(SrcInsts, DstInsts, []),
            Json = util:str("~s", [mochijson2:encode(MergedInsts)]),
            {ok, 200} = aws:s3_put(Key, Json),
            put_objs(Path, Objs, NewIds2);
        404 ->
            Json = util:str("~s", [mochijson2:encode(SrcInsts)]),
            {ok, 200} = aws:s3_put(Key, Json),
            put_objs(Path, Objs, NewIds2)
    end.

del_objs(_, []) -> {200, []};
del_objs(Path, [Obj|Objs]) ->
    {struct,[{<<"class">>,Class},{<<"instances">>,SrcInsts}]} = Obj,
    Key = util:str("~s/~s", [Path,Class]),
    {ok, Code, Data} = aws:s3_get(Key),
    case Code of
        200 ->
            Insts = mochijson2:decode(Data),
            MergedInsts = del_insts(Insts, SrcInsts, []),
            Json = util:str("~s", [mochijson2:encode(MergedInsts)]),
            {ok, 200} = aws:s3_put(Key, Json),
            del_objs(Path, Objs);
        404 -> del_objs(Path, Objs)
    end.

del_insts([], _, DstInsts) -> DstInsts;
del_insts([Inst|Insts], SrcInsts, DstInsts) ->
    DelInst = find_inst(Inst, SrcInsts),
    case DelInst of
        not_found -> del_insts(Insts, SrcInsts, [Inst|DstInsts]);
        _ -> del_insts(Insts, SrcInsts, DstInsts)
    end.

make_insts([], IdInsts, NewIds) -> {IdInsts, NewIds};
make_insts([Inst|Insts], IdInsts, NewIds) ->
    ObjId = object_id(Inst),
    case ObjId of
        not_found ->
            NewObjId = {<<"objectId">>, util:gen_id()},
            {struct,Props} = Inst,
            NewProps = [NewObjId|Props],
            make_insts(Insts, [{struct,NewProps}|IdInsts], [{struct,[NewObjId]}|NewIds]);
        _ -> make_insts(Insts, [Inst|IdInsts], NewIds)
    end.

set_insts([], [], MergedInsts) -> MergedInsts;
set_insts([SrcInst|SrcInsts], [], MergedInsts) ->
    Inst = find_inst(SrcInst, MergedInsts),
    case Inst of
        not_found -> set_insts(SrcInsts, [], [SrcInst|MergedInsts]);
        _ -> set_insts(SrcInsts, [], MergedInsts)
    end;
set_insts(SrcInsts, [DstInst|DstInsts], MergedInsts) ->
    SrcInst = find_inst(DstInst, SrcInsts),
    MergedInst = merge_inst(SrcInst, DstInst),
    set_insts(SrcInsts, DstInsts, [MergedInst|MergedInsts]).

merge_inst(not_found, DstInst) -> DstInst;
merge_inst(SrcInst, DstInst) ->
    {struct,Src} = SrcInst,
    {struct,Dst} = DstInst,
    MergedProps = merge_props(Src, Dst, []),
    {struct,MergedProps}.

merge_props([], [], MergedProps) -> MergedProps;
merge_props([{Key,Value}|Props], [], MergedProps) ->
    case find_prop_value(MergedProps, Key) of
        not_found ->
            merge_props(Props, [], [{Key,Value}|MergedProps]);
        _ -> merge_props(Props, [], MergedProps)
    end;
merge_props(Src, [{Key,Value}|Props], MergedProps) ->
    Prop = {Key,find_prop_value(Src, Key, Value)},
    merge_props(Src, Props, [Prop|MergedProps]).

find_inst(Inst, Insts) ->
    ObjId = object_id(Inst),
    find_inst_by_id(ObjId, Insts).

object_id(Inst) ->
    {struct,Props} = Inst,
    find_prop_value(Props, <<"objectId">>).

find_inst_by_id(_, []) -> not_found;
find_inst_by_id(ObjId, [Inst|Insts]) ->
    OtherId = object_id(Inst),
    case ObjId of
        OtherId -> Inst;
        _ -> find_inst_by_id(ObjId, Insts)
    end.

find_prop_value([], _) -> not_found;
find_prop_value([{Key,Value}|Props], Name) ->
    case Key of
        Name -> Value;
        _ -> find_prop_value(Props, Name)
    end.
    
find_prop_value([], _, Default) -> Default;
find_prop_value([{Key,Value}|Props], Name, Default) ->
    case Key of
        Name -> Value;
        _ -> find_prop_value(Props, Name, Default)
    end.

