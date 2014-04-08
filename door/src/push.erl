-module(push).
-export([start/0]).

start() ->
  io:format("push socket creation...~n"),
  PrivDir = code:priv_dir(door),
  Address = "gateway.sandbox.push.apple.com",
  Port = 2195,

  {ok, CertFile} = application:get_env(door, push_cert),
  {ok, KeyFile}  = application:get_env(door, push_key),
  Cert = PrivDir ++ "/push/" ++ CertFile,
  Key = PrivDir ++ "/push/" ++ KeyFile,  

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
        {ssl,{sslsocket,_,_}, Data} -> Data
        after 20000 -> timeout
      end,

      io:format("Client received: ~p~n",[Value]),

      ResultMsg = if 
        Value =:= timeout -> <<"success">>;
        Value =/= timeout -> <<"error">>
      end,

      PushResult = {200, [{"code",Value}, {"result",ResultMsg}]},

      Who ! {pushresult, PushResult},
      handle(Socket);
    _ ->
      handle(Socket)

  end.