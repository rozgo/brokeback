-module(receipt_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

apple_receipt_url() -> "https://buy.itunes.apple.com/verifyReceipt".
apple_sandbox_receipt_url() -> "https://sandbox.itunes.apple.com/verifyReceipt".

receipt_status_codes() -> [
  { 21000, <<"The App Store could not read the JSON object you provided.">> },
  { 21002, <<"The data in the receipt-data property was malformed.">> },
  { 21003, <<"The receipt could not be authenticated.">> },
  { 21004, <<"The shared secret you provided does not match the shared secret on file for your account.">> },
  { 21005, <<"The receipt server is not currently available.">> },
  { 21006, <<"This receipt is valid but the subscription has expired.">> },
  { 21007, <<"This receipt is a sandbox receipt, but it was sent to the production service for verification.">> },
  { 21008, <<"This receipt is a production receipt, but it was sent to the sandbox service for verification.">> }
].

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    io:format("Received validation request: ~p~n", [Body]),
    VerifyRes = send_receipt_request(mochijson2:decode(Body)),
    
    {Code, Result} = process_receipt_response(VerifyRes),

    {ok, Req3} = cowboy_req:reply(Code, [
      { <<"Access-Control-Allow-Origin">>, <<"*">> },
      { <<"content-type">>, <<"application/json">> }
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
    io:format("Sending request to ~s: ~p~n", [apple_receipt_url(), Json]),
    Req = create_receipt_request(apple_receipt_url(), Json),
    Status = get_receipt_status(Req),

    io:format("Got response: ~p~n", [Status]),
    case Status of
      21007 ->
        io:format("Sandbox receipt receieved, redirecting to ~s~n", [apple_sandbox_receipt_url()]),

        RedirReq = create_receipt_request(apple_sandbox_receipt_url(), Json),
        io:format("Got response: ~p~n", [get_receipt_status(RedirReq)]),

        RedirReq;
      _ ->
        Req
    end;
send_receipt_request(_) ->
  {error, "invalid receipt-data"}.

% should we return an HTTP error code with invalid receipt verifications?
process_receipt_response({ok, Result}) ->
    {_, _, Body} = Result,
    Res = mochijson2:decode(Body),
    {_, ResKeys} = Res,
    {_, Code} = lists:keyfind(<<"status">>, 1, ResKeys),

    ValidReceipt = { <<"valid_receipt">>, Code == 0 },
    StatusDesc = lists:keyfind(Code, 1, receipt_status_codes()),
    Response = case Code of
      0 ->
        [ValidReceipt];
      _ when is_tuple(StatusDesc) ->
        {_, Desc} = StatusDesc,
        [ValidReceipt, { <<"error">>, Desc }]
    end,

    {200, mochijson2:encode({struct, Response})};
process_receipt_response({error, Reason}) ->
    {200, mochijson2:encode({struct, [
      { <<"valid_receipt">>, false },
      { <<"error">>, Reason }
    ]})}.

terminate(_Reason, _Req, _State) ->
    ok.