-module(push).
-export([sendpush/1]).

sendpush({struct,[{<<"method">>,<<"put">>},{<<"token">>,Token},{<<"msg">>,Msg}]}) -> ->
  io:format("boom"),
  PrivDir = code:priv_dir(door),
  %io:format("Priv Dir: ~s", [PrivDir]),
  Address = "gateway.sandbox.push.apple.com",
  Port = 2195,
  Cert = PrivDir ++ "/push/apns-dev-cert.pem",
  Key = PrivDir ++ "/push/apns-dev-key.noenc.pem",  

  %Options = [{cacertfile, CaCert}, {certfile, Cert}, {keyfile, Key}, {mode, binary}],
  Options = [{certfile, Cert}, {keyfile, Key}, {mode, binary}],
  Timeout = 1000,
  {ok, Socket} = ssl:connect(Address, Port, Options, Timeout),

  Payload = mochijson2:encode({struct, [{"aps", {struct, [{"alert", Msg}]}}]}),
  BPayload = erlang:list_to_binary(Payload),
  PayloadLen = erlang:byte_size(BPayload),

  %Token = "9aefcf3355f33ac459e8baa65cc6192017cf7674483f7ce4b63ba8edcb5564f1",
  %Token = "3f01d3c889e3df2c2ca1b9d103f9a2ef277011b377cd827b521097f5820eba0b",
  BToken = util:hexstr_to_bin(Token),
  BTokenLength = erlang:byte_size(BToken),

  SomeID= 1,
  {MSeconds,Seconds,_} = erlang:now(),
  Expiry = MSeconds * 1000000 + Seconds + 3600*1,

  Packet = <<1:8, SomeID:32/big, Expiry:32/big, BTokenLength:16/big, BToken/binary, PayloadLen:16/big, BPayload/binary>>,

  ssl:send(Socket, Packet),
  Value = receive
          {ssl,{sslsocket,new_ssl,_}, Data} ->
              io:format("Client received: ~p~n",[Data])
          after 2000 ->
              0
          end,
  ssl:close(Socket),
  Value -> {Value, "push sent"}.