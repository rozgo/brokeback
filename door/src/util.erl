-module(util).

-export([str/2]).
-export([gen_id/0]).
-export([hash/1]).
-export([bin_to_hexstr/1]).
-export([hexstr_to_bin/1]).


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

bin_to_hexstr(Bin) ->
   lists:flatten([io_lib:format("~2.16.0B", [X]) ||
                  X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
   hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
   list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
   {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
   hexstr_to_bin(T, [V | Acc]).