-module(util).

-export([str/2]).
-export([gen_id/0]).
-export([hash/1]).

str(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

gen_id() ->
    Str = util:str("~w/~w", [self(),now()]),
    Sha = crypto:hash(sha, Str),
    B64 = binary:bin_to_list(base64:encode(Sha)),
    binary:list_to_bin(take_chars(B64, 10, [])).

take_chars(_, 0, Take) ->
    Take;
take_chars([Chr|Str], Count, Take) when Chr >= 65, Chr =< 90; Chr >= 97, Chr =< 122; Chr >=48, Chr =< 57 ->
    take_chars(Str, Count-1, [Chr|Take]);
take_chars([_|Str], Count, Take) ->
    take_chars(Str, Count-1, Take).

hash(Data) ->
    Sha = crypto:hash(sha, Data),
    B64 = binary:bin_to_list(base64:encode(Sha)),
    binary:list_to_bin(take_chars(B64, 10, [])).
