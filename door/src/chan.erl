-module(chan).

-export([start/1]).

start(Chan) ->
    Whos = ets:new(whos, []),
    Msgs = ets:new(msgs, [ordered_set]),
    handle(Chan, Whos, Msgs).

handle(Chan, Whos, Msgs) ->
    receive
        {pub, {who, _}, {msg, Msg}} ->
            io:format("pub: ~s~n",[Msg]),
            ets:insert(Msgs, {now(), Msg}),
            broadcast(Whos, Msg, ets:first(Whos)),
            handle(Chan, Whos, Msgs);
        {join, {who, Who}, {pid, Where}} ->
            boot_prev(Whos,{who, Who},{chan, Chan},{pid, Where}),
            io:format("join: ~s @ ~s ~p~n",[Who, Chan, Where]),
            ets:insert(Whos, {Who, Where}),
            History = history(Msgs, ets:last(Msgs), 0, []),
            push(Where, History),
            handle(Chan, Whos, Msgs);
        {leave, {who, _}} ->
            io:format("leave~n",[]),
            handle(Chan, Whos, Msgs)
    end.

broadcast(_, _, '$end_of_table') ->
    done;
broadcast(Whos, Msg, Key) ->
    [{_,Pid}] = ets:lookup(Whos, Key),
    Pid ! {push, Msg},
    broadcast(Whos, Msg, ets:next(Whos, Key)).

history(_, '$end_of_table', _, History) ->
    History;
history(Msgs, Key, Count, History) when Count > 10 ->
    ets:delete(Msgs, Key),
    history(Msgs, ets:prev(Msgs, Key), Count+1, History);
history(Msgs, Key, Count, History) ->
    [{_,Msg}] = ets:lookup(Msgs, Key),
    history(Msgs, ets:prev(Msgs, Key), Count+1, [Msg|History]).

push(_, []) ->
    done;
push(Where, [Msg|Msgs]) ->
    Where ! {push, Msg},
    push(Where, Msgs).

boot_prev(Whos, {who, Who}, {chan, Chan}, {pid, Where}) ->
    [{_,Pid}] = ets:lookup(Whos, Who),
    case Pid of
        undefined -> ok;
        Where -> ok;
        _ ->
            Reason = lists:flatten(io_lib:format("boot: ~s @ ~s ~p", [Who, Chan, Pid])),
            exit(Pid,Reason)
    end.

