-module(safe).

-export([get_objs/3]).
-export([put_objs/3]).
-export([del_objs/2]).

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
            save_obj(Key, set_insts(SrcInsts, mochijson2:decode(Data), [])),
            put_objs(Path, Objs, NewIds2);
        404 ->
            save_obj(Key, SrcInsts),
            put_objs(Path, Objs, NewIds2)
    end.

del_objs(_, []) -> {200, []};
del_objs(Path, [Obj|Objs]) ->
    {struct,[{<<"class">>,Class},{<<"instances">>,SrcInsts}]} = Obj,
    Key = util:str("~s/~s", [Path,Class]),
    {ok, Code, Data} = aws:s3_get(Key),
    case Code of
        200 ->
            save_obj(Key, del_insts(mochijson2:decode(Data), SrcInsts, [])),
            del_objs(Path, Objs);
        404 -> del_objs(Path, Objs)
    end.

del_insts([], _, DstInsts) -> DstInsts;
del_insts([Inst|Insts], SrcInsts, DstInsts) ->
    case find_inst(Inst, SrcInsts) of
        not_found -> del_insts(Insts, SrcInsts, [Inst|DstInsts]);
        _ -> del_insts(Insts, SrcInsts, DstInsts)
    end.

save_obj(Key, Insts) ->
    Json = util:str("~s", [mochijson2:encode(Insts)]),
    {ok, 200} = aws:s3_put(Key, Json).

make_insts([], IdInsts, NewIds) -> {IdInsts, NewIds};
make_insts([Inst|Insts], IdInsts, NewIds) ->
    case object_id(Inst) of
        not_found ->
            ObjId = {<<"objectId">>, util:gen_id()},
            {struct,Props} = Inst,
            NewProps = [ObjId|Props],
            make_insts(Insts, [{struct,NewProps}|IdInsts], [{struct,[ObjId]}|NewIds]);
        _ -> make_insts(Insts, [Inst|IdInsts], NewIds)
    end.

set_insts([], [], MergedInsts) -> MergedInsts;
set_insts([SrcInst|SrcInsts], [], MergedInsts) ->
    case find_inst(SrcInst, MergedInsts) of
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
        not_found -> merge_props(Props, [], [{Key,Value}|MergedProps]);
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
    case object_id(Inst) of
        ObjId -> Inst;
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
