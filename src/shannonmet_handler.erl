-module(shannonmet_handler).
-include("shannonmet_internal.hrl").

-export([init/2, info/3, terminate/3,
         websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-record(http_state, {action, config, sid, heartbeat_tref, messages, pid}).

init(Req, [Config]) ->
    PathInfo = cowboy_req:path_info(Req),
    Method = cowboy_req:method(Req),
    case PathInfo of
        [] ->
            %% {ok, Req, #http_state{action = create_session, config = Config}};
	    create_session(Req, #http_state{action = create_session, config = Config});
        [<<"websocket">>, _Sid] ->
	    self() ! post_init,
	    {cowboy_websocket, Req, {Config}};
	[<<"xhr-polling">>, Sid] ->
	    case {shannonmet_session:find(Sid), Method} of
                {{ok, Pid}, <<"GET">>} ->
                    case shannonmet_session:pull_no_wait(Pid, self()) of
                        session_in_use ->
			    Req1 = cowboy_req:reply(404, [], <<>>, Req), 
                            {ok, Req1, #http_state{action = session_in_use, config = Config, sid = Sid}};
                        [] ->
                            TRef = erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
                            {cowboy_loop, Req, #http_state{action = heartbeat, config = Config, sid = Sid, heartbeat_tref = TRef, pid = Pid}};
                        Messages ->
			    Req1 = reply_messages(Req, Messages, Config, false),
                            {ok, Req1, #http_state{action = data, messages = Messages, config = Config, sid = Sid, pid = Pid}}
                    end;
                {{ok, Pid}, <<"POST">>} ->
                    Protocol = Config#config.protocol,
                    case cowboy_req:body(Req) of
                        {ok, Body, Req1} ->
                            Messages = Protocol:decode(Body),
			    %% shannonmet_session:recv(Pid, Messages),
			    shannonmet_session:route(Messages),
			    Req2 = cowboy_req:reply(200, text_headers(), <<>>, Req1), 
                            {ok, Req2, #http_state{action = ok, config = Config, sid = Sid}};
                        {error, _} ->
                            {shutdown, Req, #http_state{action = error, config = Config, sid = Sid}}
                    end;
                {{error, not_found}, _} ->
                    {ok, Req, #http_state{action = not_found, sid = Sid, config = Config}};
                _ ->
                    {ok, Req, #http_state{action = error, sid = Sid, config = Config}}
            end;
        Other ->
	    cowboy_req:reply(404, [], <<>>, Req), 
            {ok, Req, #http_state{config = Config}}
    end.

%% Http handlers
create_session(Req, HttpState = #http_state{action = create_session, config = #config{heartbeat_timeout = HeartbeatTimeout,
                                                                              ttl = TTL,
                                                                              opts = Opts}}) ->
    {Id, _} = uuid:get_v1(uuid:new(self(), erlang)),
    Sid0 = re:replace(uuid:uuid_to_string(Id), "-", "", [global, {return, list}]),
    Sid = list_to_binary(Sid0),
    HeartbeatTimeoutBin = list_to_binary(integer_to_list(HeartbeatTimeout div 1000)),
    TtlBin = list_to_binary(integer_to_list(TTL div 1000)),
    _Pid = shannonmet_session:open_session(Sid, TTL, Opts),
    Result = <<":", HeartbeatTimeoutBin/binary, ":", TtlBin/binary, ":websocket,xhr-polling">>,
    Headers = text_headers(),
    Body = <<Sid/binary, Result/binary>>,
    Req1 = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req1, HttpState}.
%% handle(Req, HttpState = #http_state{action = data, messages = Messages, config = Config}) ->
%%     {ok, Req1} = reply_messages(Req, Messages, Config, false),
%%     {ok, Req1, HttpState};
%% handle(Req, HttpState = #http_state{action = session_in_use}) ->
%%     {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
%%     {ok, Req1, HttpState};
%% handle(Req, HttpState) ->
%%     {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
%%     {ok, Req1, HttpState}.

info({timeout, TRef, {?MODULE, Pid}}, Req, HttpState = #http_state{action = heartbeat, heartbeat_tref = TRef}) ->
    safe_poll(Req, HttpState#http_state{heartbeat_tref = undefined}, Pid, false);
info({message_recv, Pid}, Req, HttpState = #http_state{action = heartbeat}) ->
    safe_poll(Req, HttpState, Pid, true);
info(_Info, Req, HttpState) ->
    {stop, Req, HttpState}.

terminate(Reason, _Req, _HttpState = #http_state{action = create_session}) ->
    ok;
terminate(Reason, _Req, _HttpState = #http_state{action = session_in_use}) ->
    ok;
terminate(Reason, Req, _HttpState = #http_state{action = not_found}) ->
    Req1 = cowboy_req:reply(404, [], <<>>, Req),
    ok;
terminate(Reason, Req, _HttpState = #http_state{action = error}) ->
    Req1 = cowboy_req:reply(404, [], <<>>, Req),
    ok;
terminate(Reason, _Req, _HttpState = #http_state{heartbeat_tref = HeartbeatTRef}) ->
    case HeartbeatTRef of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(HeartbeatTRef)
    end;
terminate(Reason, _Req, _HttpState) ->
    ok.

text_headers() ->
    [{<<"content-Type">>, <<"text/plain; charset=utf-8">>},
     {<<"Cache-Control">>, <<"no-cache">>},
     {<<"Expires">>, <<"Sat, 25 Dec 1999 00:00:00 GMT">>},
     {<<"Pragma">>, <<"no-cache">>},
     {<<"Access-Control-Allow-Credentials">>, <<"true">>},
     {<<"Access-Control-Allow-Origin">>, <<"null">>}].

%% Websocket handlers
websocket_handle({text, Data}, Req, {Config = #config{protocol = Protocol}, Pid}) ->
    Messages = Protocol:decode(Data),
    shannonmet_session:recv(Pid, Messages),
    {ok, Req, {Config, Pid}};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(post_init, Req, {Config}) ->
    PathInfo = cowboy_req:path_info(Req),
    [<<"websocket">>, Sid] = PathInfo,
    case shannonmet_session:find(Sid) of
        {ok, Pid} ->
            erlang:monitor(process, Pid),
            self() ! go,
            erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
            {ok, Req, {Config, Pid}};
        {error, not_found} ->
            {shutdown, {Config, undefined}}
    end;
websocket_info(go, Req, {Config, Pid}) ->
    case shannonmet_session:pull(Pid, self()) of
        session_in_use ->
            {ok, Req, {Config, Pid}};
        Messages ->
            reply_ws_messages(Req, Messages, {Config, Pid})
    end;
websocket_info({message_recv, Pid}, Req, {Config, Pid}) ->
    Messages = shannonmet_session:poll(Pid),
    self() ! go,
    reply_ws_messages(Req, Messages, {Config, Pid});
websocket_info({timeout, _TRef, {?MODULE, Pid}}, Req, {Config = #config{protocol = Protocol}, Pid}) ->
    shannonmet_session:refresh(Pid),
    erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
    Packet = Protocol:encode(heartbeat),
    {reply, {text, Packet}, Req, {Config, Pid}};
websocket_info({'DOWN', _Ref, process, Pid, _Reason}, Req, State = {_Config, Pid}) ->
    {shutdown, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State = {_Config, Pid}) ->
    shannonmet_session:disconnect(Pid),
    ok.

safe_poll(Req, HttpState = #http_state{config = Config = #config{protocol = Protocol}}, Pid, WaitIfEmpty) ->
    try
        Messages = shannonmet_session:poll(Pid),
        case {WaitIfEmpty, Messages} of
            {true, []} ->
                {ok, Req, HttpState, hibernate};
            _ ->
                Req1 = reply_messages(Req, Messages, Config, true),
		{stop, Req1, HttpState}
        end
    catch
        exit:{noproc, _} ->
            RD = cowboy_req:reply(200, text_headers(), Protocol:encode(disconnect), Req),
            {ok, RD, HttpState#http_state{action = disconnect}}
    end.

reply_ws_messages(Req, Messages, State = {_Config = #config{protocol = Protocol}, _Pid}) ->
    case Protocol:encode(Messages) of
        <<>> ->
            {ok, Req, State};
        Packet ->
            {reply, {text, Packet}, Req, State}
    end.

reply_messages(Req, Messages, _Config = #config{protocol = Protocol}, SendNop) ->
    Packet = case {SendNop, Messages} of
                 {true, []} ->
		     Protocol:encode([nop]);
                 _ ->
                     Protocol:encode(Messages)
             end,
    cowboy_req:reply(200, text_headers(), Packet, Req).
