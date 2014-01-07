-module(look).

-export([start/1]).

start(Name) ->
    handle(Name, []).

handle(Name, Scores) ->
    receive
        {put, {key, Key}, {score, Score}} ->
            io:format("PUT key: ~p score: ~p~n",[Key, Score]),
            NewScores = proplists:delete(Key, Scores),
            handle(Name, [{Key,Score}|NewScores]);
        {get, {who, Who}, {low, Low}, {high, High}} ->
            Matches = range(Scores, Low, High, []),
            Who ! {matches, Matches},
            handle(Name, Scores);
        _ ->
            handle(Name, Scores)
    end.

range([], _, _, Matches) -> Matches;
range([Score|Scores], Low, High, Matches) ->
    {Key, Val} = Score,
    if
        (Val >= Low) and (Val =< High) ->
            range(Scores, Low, High, [Key|Matches]);
        true ->
            range(Scores, Low, High, Matches)
    end.
