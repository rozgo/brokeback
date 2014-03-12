-module(receipt_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

apple_receipt_url() -> "https://buy.itunes.apple.com/verifyReceipt".
apple_sandbox_receipt_url() -> "https://sandbox.itunes.apple.com/verifyReceipt".

receipt_status_codes() -> [
  { 21000, 400, <<"The App Store could not read the JSON object you provided.">> },
  { 21002, 400, <<"The data in the receipt-data property was malformed.">> },
  { 21003, 500, <<"The receipt could not be authenticated.">> },
  { 21004, 500, <<"The shared secret you provided does not match the shared secret on file for your account.">> },
  { 21005, 503, <<"The receipt server is not currently available.">> },
  { 21006, 500, <<"This receipt is valid but the subscription has expired.">> },
  { 21007, 400, <<"This receipt is a sandbox receipt, but it was sent to the production service for verification.">> },
  { 21008, 400, <<"This receipt is a production receipt, but it was sent to the sandbox service for verification.">> }
].

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    VerifyRes = send_receipt_request(mochijson2:decode(Body)),
    
    {Code, Result} = process_receipt_response(VerifyRes),

    {ok, Req3} = cowboy_req:reply(Code, [
      { <<"Access-Control-Allow-Origin">>, <<"*">> },
      { <<"content-type">>, <<"application/json">> },
      { <<"X-Status-Code">>, binary:list_to_bin(io_lib:format("~p", [Code])) }
    ], Result, Req2),

    {ok, Req3, State}.

create_receipt_request(Url, Json) ->
    httpc:request(post, {
      Url, [
        {"Content-Type", "application/json"}
      ],
      "application/json",
      Json}, [], []).

get_receipt_status(Req) ->
    {ok, ReqResult} = Req,
    {_, _, Body} = ReqResult,

    {struct, ReceiptResp} = mochijson2:decode(Body),
    {_, Status} = lists:keyfind(<<"status">>, 1, ReceiptResp),
    Status.

send_receipt_request({struct, [{<<"receipt-data">>, Receipt}]}) ->
    Json = binary:list_to_bin(mochijson2:encode({struct, [{<<"receipt-data">>, Receipt}]})),
    io:format("Sending request to ~s~n", [apple_receipt_url()]),
    Req = create_receipt_request(apple_receipt_url(), Json),
    Status = get_receipt_status(Req),

    case Status of
      21007 ->
        io:format("Sandbox receipt receieved, redirecting to ~s~n", [apple_sandbox_receipt_url()]),

        RedirReq = create_receipt_request(apple_sandbox_receipt_url(), Json),
        io:format("Apple receipt verification responded with: ~p~n", [get_receipt_status(RedirReq)]),

        RedirReq;
      _ ->
        io:format("Apple receipt verification responded with: ~p~n", [Status]),
        Req
    end;
send_receipt_request(_) ->
  {client_error, "invalid receipt-data"}.

% should we return an HTTP error code with invalid receipt verifications?
process_receipt_response({ok, Result}) ->
    {_, _, Body} = Result,
    Res = mochijson2:decode(Body),
    {_, ResKeys} = Res,
    {_, Code} = lists:keyfind(<<"status">>, 1, ResKeys),

    ValidReceipt = { <<"valid_receipt">>, Code == 0 },
    StatusDesc = lists:keyfind(Code, 1, receipt_status_codes()),
    {ResponseCode, Response} = case Code of
      0 ->
        {200, [ValidReceipt]};
      _ when is_tuple(StatusDesc) ->
        {_, HttpCode, Desc} = StatusDesc,
        io:format("Error response from Apple: ~s~n", [Desc]),
        {HttpCode, [ValidReceipt]}
    end,

    {ResponseCode, mochijson2:encode({struct, Response})};
process_receipt_response({error, Reason}) ->
    io:format("HTTP client error during request to Apple: ~s~n", [Reason]),
    {500, mochijson2:encode({struct, [
      { <<"valid_receipt">>, false }
    ]})};
process_receipt_response({client_error, _}) ->
    {400, mochijson2:encode({struct, [
      { <<"valid_receipt">>, false }
    ]})}.

terminate(_Reason, _Req, _State) ->
    ok.