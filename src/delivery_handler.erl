-module(delivery_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Req2 = process_event(Method, HasBody, Req),
    {ok, Req2, Opts}.

process_event(<<"POST">>, true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    ClientId = proplists:get_value(<<"client_id">>, PostVals),
    Msg = proplists:get_value(<<"msg">>, PostVals),
    dispatch_event(ClientId, Msg, Req2);
process_event(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);
process_event(_, _, Req) ->
    cowboy_req:reply(405, Req).
    
dispatch_event(undefined, _Msg, Req) ->
    cowboy_req:reply(400, [], <<"Missing client id parameter.">>, Req);
dispatch_event(ClientId, Msg, Req) ->
    PidStr = redis_worker:get(["source_", ClientId]),
    list_to_pid(binary_to_list(PidStr)) ! {message, Msg},
    cowboy_req:reply(200, [
			   {<<"content-type">>, <<"text/plain; charset=utf-8">>}
			  ], <<"ok">>, Req).
