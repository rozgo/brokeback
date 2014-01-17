-module(push_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    % io:format("BODY: ~s~n", [Body]),
    Query = mochijson2:decode(Body),
    io:format("Query: ~p~n", [Query]),
    Packet = createpush(Query),
    io:format("Created push.~n"),
    PushResult = sendpush(Packet),

    {Code, Response} = {200, [PushResult]},
    {ok, Req3} = cowboy_req:reply(Code,[
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"content-type">>, <<"application/json">>}],
        mochijson2:encode(Response), Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

createpush({struct,[{<<"method">>,<<"put">>},{<<"token">>,Token},{<<"msg">>,Msg}]}) ->
  io:format("boom: ~p~n", [Token]),
  Payload = mochijson2:encode({struct, [{"aps", {struct, [{"alert", Msg}]}}]}),
  BPayload = erlang:list_to_binary(Payload),
  PayloadLen = erlang:byte_size(BPayload),
  BToken = Token,%util:hexstr_to_bin(Token),
  BTokenLength = erlang:byte_size(BToken),
  SomeID= 1,
  {MSeconds,Seconds,_} = erlang:now(),
  Expiry = MSeconds * 1000000 + Seconds + 3600*1,
  Packet = <<1:8, SomeID:32/big, Expiry:32/big, BTokenLength:16/big, BToken/binary, PayloadLen:16/big, BPayload/binary>>,
  Packet.

sendpush(Packet) ->
    push_pid() ! {put, {packet, Packet}, {who, self()}},
    receive 
        {pushresult, PushResult} -> PushResult
    after
        10000 -> {200, [{"result",<<"unknown">>}]}
    end.


push_pid() ->
    case global:whereis_name({push, "push"}) of
        undefined ->
            NewPid = spawn(push, start, []),
            global:register_name({push, "push"}, NewPid),
            NewPid;
        Pid -> Pid
    end.
