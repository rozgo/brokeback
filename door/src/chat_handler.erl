-module(chat_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    io:format("websocket_init: ~s~n",[_TransportName]),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    Parsed = mochijson2:decode(Msg),
    command({msg, Msg}, Parsed),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({push, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    io:format("websocket_terminate~n",[]),
    ok.
    
command({msg, Msg},
    {struct,[{<<"cmd">>,<<"pub">>},{<<"chan">>,Chan},{<<"who">>,Who},_]}) ->
    chan_pid(Chan) ! {pub, {who, Who}, {msg, Msg}};
command({msg, _},
    {struct,[{<<"cmd">>,<<"join">>},{<<"chan">>,Chan},{<<"who">>,Who}]}) ->
    chan_pid(Chan) ! {join, {who, Who}, {pid, self()}};
command({msg, _},
    {struct,[{<<"cmd">>,<<"leave">>},{<<"chan">>,Chan},{<<"who">>,Who}]}) ->
    chan_pid(Chan) ! {leave, {who, Who}}.

chan_pid(Chan) ->
    case global:whereis_name({chan,Chan}) of
        undefined ->
            io:format("spawn channel: ~s~n",[Chan]),
            NewPid = spawn(chan, start, [Chan]),
            global:register_name({chan,Chan}, NewPid),
            NewPid;
        Pid -> Pid
    end.
