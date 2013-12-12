-module(aws).

-export([datetime/0]).
-export([bucket/0]).
-export([host/0]).
-export([accesskey/0]).
-export([secretkey/0]).

iso_8601_basic_time() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
    lists:flatten(io_lib:format(
                    "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

datetime() -> iso_8601_basic_time().

bucket() -> "beyondgames-bow".

host() -> "https://s3-us-west-1.amazonaws.com".

accesskey() -> "AKIAJVOLEECLCMRC75QA".

secretkey() -> "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".

