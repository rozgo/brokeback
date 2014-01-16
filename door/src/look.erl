-module(look).

-export([start/1]).

start(Name) ->
    handle(Name, [], empty, empty).

handle(Name, Scores, Lowest, Highest) ->
    receive
        {put, {who, Who}, {key, Key}, {score, Score}} ->
            io:format("PUT key: ~p score: ~p~n",[Key, Score]),
            NewScores = proplists:delete(Key, Scores),
            {NewLow, NewHigh} = lowhigh(Lowest, Highest, Score),
            io:format("Lowest: ~p Highest: ~p~n",[NewLow, NewHigh]),
            LowHigh = {struct,[{<<"low">>,NewLow},{<<"high">>,NewHigh}]},
            Who ! {lowhigh, LowHigh},
            handle(Name, lists:sublist([{Key,Score}|NewScores],10000), NewLow, NewHigh);
        {get, {who, Who}, {low, Low}, {high, High}, {limit, Limit}} ->
            Matches = range(Scores, Low, High, [], Limit),
            Who ! {matches, Matches},
            handle(Name, Scores, Lowest, Highest);
        {get, {who, Who}} ->
            LowHigh = {struct,[{<<"low">>,Lowest},{<<"high">>,Highest}]},
            Who ! {lowhigh, LowHigh},
            handle(Name, Scores, Lowest, Highest);
        _ ->
            handle(Name, Scores, Lowest, Highest)
    end.

range(_, _, _, Matches, 0) -> Matches;
range([], _, _, Matches, _) -> Matches;
range([Score|Scores], Low, High, Matches, Count) ->
    {_, Val} = Score,
    if
        (Val >= Low) and (Val =< High) ->
            range(Scores, Low, High, [{struct, [Score]}|Matches], Count-1);
        true ->
            range(Scores, Low, High, Matches, Count)
    end.

lowhigh(Low, High, Score) ->
    NewLow = case Low of
        empty -> Score;
        Low when Score =< Low -> Score;
        Low -> Low
    end,
    NewHigh = case High of
        empty -> Score;
        High when Score >= High -> Score;
        High -> High
    end,
    {NewLow, NewHigh}.
