-module(push).
-export([start/0]).

start() ->
  io:format("push socket creation...~n"),
  PrivDir = code:priv_dir(door),
  Address = "gateway.sandbox.push.apple.com",
  Port = 2195,
  Cert = PrivDir ++ "/push/apns-dev-cert.pem",
  Key = PrivDir ++ "/push/apns-dev-key.noenc.pem",  

  Options = [{certfile, Cert}, {keyfile, Key}, {mode, binary}],
  Timeout = 10000,
  io:format("socket connecting...~n"),
  {ok, Socket} = ssl:connect(Address, Port, Options, Timeout),
  io:format("connected~n"),
  handle(Socket).

handle(Socket) ->
  receive
    {put, {packet, Packet}, {who, Who}} ->
      io:format("sending push...~n"),
      ssl:send(Socket, Packet),
      io:format("sent.~n"),
      Value = receive
              {ssl,{sslsocket,new_ssl,_}, Data} ->
                  io:format("Client received: ~p~n",[Data])
              after 2000 ->
                  0
              end,

      ResultMsg = if Value =:= 0 -> <<"success">>;
                      Value =/= 0 -> <<"error">>
                   end,

      PushResult = {200, [{"code",Value}, {"result",ResultMsg}]},

      Who ! {pushresult, PushResult},
      handle(Socket);
    _ ->
      handle(Socket)

  end.

