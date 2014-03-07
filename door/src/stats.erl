-module(stats).
-export([
  handle_request/1,
  handle_response/4
]).


handle_request(Req) ->
  { _, Req1 } = cowboy_req:path(Req),
  Req1.

handle_response(404, _, <<>>, Req) ->
  {Path, Req2} = cowboy_req:path(Req),
  statman_counter:incr({Path, {error, {404, <<"Not found">>}}}),
  Req2;

handle_response(Code, _, <<>>, Req) when is_integer(Code), Code >= 400 ->
    {Path, Req2} = cowboy_req:path(Req),
    statman_counter:incr({Path, {error, {Code, <<"ERROR">>}}}),
    Req2;

handle_response(_Code, _Headers, _Body, Req) ->
    Req.