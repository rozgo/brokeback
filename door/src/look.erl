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
            handle(Name, [{Key,Score}|NewScores], NewLow, NewHigh);
        {get, {who, Who}, {low, Low}, {high, High}} ->
            Matches = range(Scores, Low, High, []),
            Who ! {matches, Matches},
            handle(Name, Scores, Lowest, Highest);
        {get, {who, Who}} ->
            LowHigh = {struct,[{<<"low">>,Lowest},{<<"high">>,Highest}]},
            Who ! {lowhigh, LowHigh},
            handle(Name, Scores, Lowest, Highest);
        _ ->
            handle(Name, Scores, Lowest, Highest)
    end.

range([], _, _, Matches) -> Matches;
range([Score|Scores], Low, High, Matches) ->
    {_, Val} = Score,
    if
        (Val >= Low) and (Val =< High) ->
            range(Scores, Low, High, [{struct, [Score]}|Matches]);
        true ->
            range(Scores, Low, High, Matches)
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
