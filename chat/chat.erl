-module(chat).

-export([start/0]).
-export([handle_init/0]).

start() ->
    Handle = spawn(chat, handle_init, []),
    register(chat, Handle).

handle_init() ->
    ets:new(chans, [named_table]),
    handle().

handle() ->
    receive
        {{pid, Where}, {text, Msg}} ->
            Parsed = mochijson2:decode(Msg),
            command({pid, Where}, {msg, Msg}, Parsed),
            handle()
    end.

command({pid, _}, {msg, Msg},
    {struct,[{<<"cmd">>,<<"pub">>},{<<"chan">>,Chan},{<<"who">>,Who},_]}) ->
    chan_pid(Chan) ! {pub, {who, Who}, {msg, Msg}};
command({pid, Where}, {msg, _},
    {struct,[{<<"cmd">>,<<"join">>},{<<"chan">>,Chan},{<<"who">>,Who}]}) ->
    chan_pid(Chan) ! {join, {who, Who}, {pid, Where}};
command({pid, _}, {msg, _},
    {struct,[{<<"cmd">>,<<"leave">>},{<<"chan">>,Chan},{<<"who">>,Who}]}) ->
    chan_pid(Chan) ! {leave, {who, Who}}.

chan_pid(Chan) ->
    case ets:lookup(chans, Chan) of
        [] ->
            NewPid = spawn(chan, start, [Chan]),
            ets:insert(chans, {Chan, NewPid}),
            NewPid;
        [{_,Pid}] ->
            Pid
    end.
