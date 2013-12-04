-module(chat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Handle = spawn(chat, handle_init, []),
    register(chat, Handle).
    chat_sup:start_link().

stop(_State) ->
    ok.

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