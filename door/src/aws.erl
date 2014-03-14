-module(aws).

-export([datetime/0]).
-export([bucket/0]).
-export([host/0]).
-export([accesskey/0]).
-export([secretkey/0]).
% -export([s3_path/0]).
-export([s3_put/2]).
-export([s3_get/1]).


iso_8601_basic_time() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
    lists:flatten(io_lib:format(
                    "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

datetime() -> iso_8601_basic_time().

bucket() -> 
    {ok, Bucket} = application:get_env(door, s3_bucket),
    Bucket.

host() -> 
    {ok, Host} = application:get_env(door, s3_host),
    Host.

accesskey() -> 
    {ok, AccessKey} = application:get_env(door, s3_accesskey),
    AccessKey.

secretkey() ->
    {ok, SecretKey} = application:get_env(door, s3_secretkey),
    SecretKey.

s3_path() -> 
    {ok, Path} = application:get_env(door, s3_path),
    Path.

s3_all_users() -> 
    {ok, Url} = application:get_env(door, s3_all_users),
    Url.

s3_get(Key) ->
    Path = util:str("~s/~s", [s3_path(),Key]),
    % {Status,{{_,Code,_},_,Data}} = httpc:request(get, {Path, []}, [], []),
    {Status, {{Code, _}, _, Data}} = lhttpc:request(Path, get, [], infinity),
    {Status, Code, Data}.

s3_put(Key,Data) ->
    AWS_Key = Key,
    AWS_Datetime = aws:datetime(),
    AWS_canon_headers = util:str("x-amz-date:~s~nx-amz-grant-read:~s", [AWS_Datetime, s3_all_users()]),
    AWS_canon_resource = util:str("/~s/~s", [aws:bucket(), AWS_Key]),
    AWS_msg_sign = util:str("PUT\n\napplication/json\n\n~s\n~s", [AWS_canon_headers, AWS_canon_resource]),
    AWS_signature_hmac = crypto:hmac(sha, aws:secretkey(), AWS_msg_sign),
    AWS_signature = base64:encode(AWS_signature_hmac),
    AWS_authorization = util:str("AWS ~s:~s", [aws:accesskey(), AWS_signature]),
    AWS_path = util:str("~s/~s/~s", [aws:host(), aws:bucket(), AWS_Key]),
    {Status, {{Code, _}, _, _}} = lhttpc:request(AWS_path, put, [
        {"Authorization", AWS_authorization},
        {"X-Amz-Date", AWS_Datetime},
        {"X-Amz-Grant-Read", s3_all_users()},
        {"Content-Type", "application/json"}
    ], Data, infinity),
    % {Status,{{_,Code,_},_,_}} = httpc:request(put, {AWS_path, [
    %     {"Authorization", AWS_authorization},
    %     {"X-Amz-Date", AWS_Datetime},
    %     {"X-Amz-Grant-Read", s3_all_users()},
    %     {"Content-Type", "application/json"}
    % ],"application/json", Data}, [{ssl,[{verify,0}]}], []),
    {Status, Code}.

