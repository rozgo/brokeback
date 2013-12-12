-module(safe_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

s3path() -> "http://s3-us-west-1.amazonaws.com/beyondgames-bow".
s3AllUsers() -> "uri=http://acs.amazonaws.com/groups/global/AllUsers".

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    save_s3(),
    Query = mochijson2:decode(Body),
    io:format("query: ~p~n",[Query]),
    HttpC = command(Query),
    Response = mochijson2:encode(HttpC),
    % io:format("date: ~p~n",[aws:iso_8601_basic_time()]),
    {ok, Req3} = cowboy_req:reply(200,[
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"content-type">>, <<"application/json">>}],
    Response, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

save_s3() ->
    AWS_Key = "Users/Profiles/haha",
    AWS_Datetime = aws:datetime(),
    AWS_canon_headers = lists:flatten(io_lib:format("x-amz-date:~s~nx-amz-grant-read:~s", [AWS_Datetime, s3AllUsers()])),
    AWS_canon_resource = lists:flatten(io_lib:format("/~s/~s", [aws:bucket(), AWS_Key])),
    AWS_msg_sign = lists:flatten(io_lib:format("PUT\n\ntext/plain\n\n~s\n~s", [AWS_canon_headers, AWS_canon_resource])),
    AWS_signature_hmac = crypto:hmac(sha, aws:secretkey(), AWS_msg_sign),
    AWS_signature = base64:encode(AWS_signature_hmac),
    AWS_authorization = lists:flatten(io_lib:format("AWS ~s:~s", [aws:accesskey(), AWS_signature])),
    AWS_path = lists:flatten(io_lib:format("~s/~s/~s", [aws:host(), aws:bucket(), AWS_Key])),
    {_,{{_,_,_},_,Json}} = httpc:request(put, {AWS_path, [
        {"Authorization", AWS_authorization},
        {"X-Amz-Date", AWS_Datetime},
        {"X-Amz-Grant-Read", s3AllUsers()},
        {"Content-Type", "text/plain"}
    ],"text/plain", "this is a test"}, [{ssl,[{verify,0}]}], []),
    io:format("save_json: ~p~n", [Json]),
    io:format("save_s3: ~p~n~p~n", [AWS_path, AWS_signature]),
    ok.

command({struct,[{<<"method">>,<<"get">>},{<<"path">>,Path},{<<"objects">>,Objects}]}) ->
    FullPath = lists:flatten(io_lib:format("~s/~s", [s3path(),Path])),
    get_objects(FullPath, Objects, []);
command({struct,[{<<"method">>,<<"put">>},{<<"path">>,Path},{<<"objects">>,Objects}]}) ->
    FullPath = lists:flatten(io_lib:format("~s/~s", [s3path(),Path])),
    put_objects(FullPath, Objects).

get_objects(_, [], Responses) ->
    Responses;
get_objects(Path, [Object|Objects], Responses) ->
    ObjectPath = lists:flatten(io_lib:format("~s/~s", [Path,Object])),
    % io:format("get object: ~p~n",[ObjectPath]),
    {ok, {{_, 200, _}, _, Json}} = httpc:request(get, {ObjectPath, []}, [], []),
    Response = {Object,mochijson2:decode(Json)},
    get_objects(Path, Objects, [Response|Responses]).

put_objects(_, []) ->
    [1,2,3];
put_objects(Path, [Object|Objects]) ->
    {struct,[{<<"class">>,Class},{<<"instances">>,SrcInstances}]} = Object,
    ObjectPath = lists:flatten(io_lib:format("~s/~s", [Path,Class])),
    % io:format("put object: ~p~n",[ObjectPath]),
    {ok,{{_,200,_},_,Json}} = httpc:request(get, {ObjectPath, []}, [], []),
    Response = {Class,mochijson2:decode(Json)},
    % io:format("response object: ~p~n",[Response]),
    {_,DstInstances} = Response,
    MergedInstances = set_instances(SrcInstances, DstInstances, []),
    MergedInstances,
    % io:format("MergedInstances: ~p~n",[MergedInstances]),
    put_objects(Path, Objects).

set_instances([], [], MergedInstances) ->
    MergedInstances;
set_instances([SrcInstance|SrcInstances], [], MergedInstances) ->
    {struct,Properties} = SrcInstance,
    ObjectId = find_property_value(Properties, <<"objectId">>),
    Instance = find_instance_by_id(ObjectId, MergedInstances),
    case Instance of
        not_found ->
            io:format("new instance: ~p~n",[SrcInstance]),
            set_instances(SrcInstances, [], [SrcInstance|MergedInstances]);
        _ ->
            set_instances(SrcInstances, [], MergedInstances)
    end;
set_instances(SrcInstances, [DstInstance|DstInstances], MergedInstances) ->
    SrcInstance = find_instance(DstInstance, SrcInstances),
    MergedInstance = merge_instances(SrcInstance, DstInstance),
    set_instances(SrcInstances, DstInstances, [MergedInstance|MergedInstances]).

merge_instances(not_found, DstInstance) ->
    DstInstance;
merge_instances(SrcInstance, DstInstance) ->
    {struct,Src} = SrcInstance,
    {struct,Dst} = DstInstance,
    MergedProperties = merge_properties(Src, Dst, []),
    {struct,MergedProperties}.

merge_properties([], [], MergedProperties) ->
    MergedProperties;
% new properties
merge_properties([{Key,Value}|Properties], [], MergedProperties) ->
    Found = find_property_value(MergedProperties, Key),
    case Found of
        not_found ->
            io:format("new property: ~p ~p~n",[Key, Value]),
            merge_properties(Properties, [], [{Key,Value}|MergedProperties]);
        _ -> merge_properties(Properties, [], MergedProperties)
    end;
% existing properties
merge_properties(Src, [{Key,Value}|Properties], MergedProperties) ->
    Property = {Key,find_property_value(Src, Key, Value)},
    % io:format("merge_properties: ~p ~p~n",[Key, Value]),
    merge_properties(Src, Properties, [Property|MergedProperties]).

find_instance(Instance, Instances) ->
    {struct,Properties} = Instance,
    ObjectId = find_property_value(Properties, <<"objectId">>),
    find_instance_by_id(ObjectId, Instances).

find_instance_by_id(_, []) ->
    not_found;
find_instance_by_id(ObjectId, [Instance|Instances]) ->
    {struct,Properties} = Instance,
    OtherId = find_property_value(Properties, <<"objectId">>),
    % io:format("find_instance_by_id: ~p ~p~n",[ObjectId, OtherId]),
    case ObjectId of
        OtherId -> Instance;
        _ -> find_instance_by_id(ObjectId, Instances)
    end.

find_property_value([], _) ->
    not_found;
find_property_value([{Key,Value}|Properties], Name) ->
    case Key of
        Name -> Value;
        _ -> find_property_value(Properties, Name)
    end.
    
find_property_value([], _, Default) ->
    Default;
find_property_value([{Key,Value}|Properties], Name, Default) ->
    case Key of
        Name -> Value;
        _ -> find_property_value(Properties, Name, Default)
    end.

