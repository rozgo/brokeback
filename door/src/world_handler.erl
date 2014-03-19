-module(world_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({binary, Blob}, Req, State) ->
    world_commands:submit(self(), Blob),
    {ok, Req, State};
websocket_handle(Blob, Req, State) ->
    io:format("ERROR[websocket_handle] - Received: ~p~n", [Blob]),
    {ok, Req, State}.

websocket_info({push, noop}, Req, State) ->
    {ok, Req, State};
websocket_info({push, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State};
websocket_info(_Info, Req, State) ->
    io:format("websocket_info: ~p~n", [_Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.