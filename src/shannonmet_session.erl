-module(shannonmet_session).
-author('iswap2009@gmail.com').
-behaviour(gen_server).

-include("shannonmet_internal.hrl").

%% API
-export([start_link/3, open_session/3, route/1, find/1, pull/2, pull_no_wait/2, poll/1, recv/2,
         send_obj/2, refresh/1, disconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id,
                messages,
                ttl,
                session_timeout_tref,
                connect,
                registered,
                opts,
                session_state}).

%%%===================================================================
%%% API
%%%===================================================================
%% configure(Opts) ->
%%     #config{heartbeat = proplists:get_value(heartbeat, Opts, 5000),
%%             heartbeat_timeout = proplists:get_value(heartbeat_timeout, Opts, 30000),
%%             ttl = proplists:get_value(ttl, Opts, 30000),
%%             %% callback = proplists:get_value(callback, Opts),
%%             protocol = proplists:get_value(protocol, Opts, shannonmet_data_protocol),
%%             opts = proplists:get_value(opts, Opts, undefined)
%%            }.

start_link(Sid, TTL, Opts) ->
    gen_server:start_link(?MODULE, [Sid, TTL, Opts], []).

open_session(Sid, TTL, Opts) ->
    {ok, Pid} = shannonmet_session_sup:start_child(Sid, TTL, Opts),
    Key = ["sid_", binary_to_list(Sid)], 
    PidStr = pid_to_list(Pid),
    redis_worker:set(Key, PidStr),
    redis_worker:expire(Key, TTL),
    redis_worker:sadd(<<"all_session">>, Sid),
    gen_server:cast(Pid, {send, Sid, {connect, <<>>}}), 
    Pid.

route(Message) ->
    case redis_worker:smembers(<<"all_session">>) of
	Sessions when is_list(Sessions) ->
	    [send_msg(S, Message) || S <- Sessions];
	_ ->
	    ok
    end.

route_msg_to_all(_Conn, _Sid, Message) ->
    case redis_worker:smembers(<<"all_session">>) of
	Sessions when is_list(Sessions) ->
	    [send_msg(S, Message) || S <- Sessions];
	_ ->
	    ok
    end.

send_msg(Sid, Message) ->
    Key = ["sid_", binary_to_list(Sid)], 
    case redis_worker:get(Key) of
	PidStr when is_binary(PidStr) ->
	    Pid = list_to_pid(binary_to_list(PidStr)),
	    gen_server:cast(Pid, {send, Sid, Message});
	_ ->
	    ok
    end.

recv(SessionPid, Message) ->
    gen_server:call(SessionPid, {recv, Message}, infinity).

pull(Session, Conn) ->
    gen_server:call(Session, {pull, Conn, true}, infinity).

pull_no_wait(Pid, Caller) ->
    gen_server:call(Pid, {pull, Caller, false}, infinity).

poll(Session) ->
    gen_server:call(Session, {poll}, infinity).

send_obj(Pid, Obj) ->
    gen_server:cast(Pid, {send, {json, <<>>, <<>>, Obj}}).

refresh(Pid) ->
    gen_server:cast(Pid, {refresh}).

disconnect(Pid) ->
    gen_server:cast(Pid, {disconnect}).

find(Sid) ->
    Key = ["sid_", binary_to_list(Sid)], 
    case redis_worker:get(Key) of
	PidStr when is_binary(PidStr) ->
	    Pid = list_to_pid(binary_to_list(PidStr)),
	    {ok, Pid};
	none ->
            {error, not_found};
        _ ->
            {error, not_found}
    end.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([Sid, TTL, Opts]) ->
    TRef = erlang:send_after(TTL, self(), session_timeout),
    {ok, #state{id = Sid,
                messages = [],
                registered = false,
                opts = Opts,
                session_timeout_tref = TRef,
                ttl = TTL}}.
%%--------------------------------------------------------------------
handle_call({pull, Pid, Wait}, _From,  State = #state{messages = Messages, connect = undefined}) ->
    State1 = refresh_session_timeout(State),
    case Messages of
        [] ->
            {reply, [], State1#state{connect = Pid}};
        _ ->
            NewConn = case Wait of
                            true ->
                                Pid;
                            false ->
                                undefined
                        end,
            {reply, lists:reverse(Messages), State1#state{messages = [], connect = NewConn}}
    end;
handle_call({pull, _Pid, _}, _From,  State) ->
    {reply, session_in_use, State};
handle_call({poll}, _From, State = #state{messages = Messages}) ->
    State1 = refresh_session_timeout(State),
    {reply, lists:reverse(Messages), State1#state{messages = [], connect = undefined}};
handle_call({recv, Messages}, _From, State) ->
    State1 = refresh_session_timeout(State),
    process_messages(Messages, State1);
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%%-------------------------------------------------------------------
handle_cast({send, Sid, Message}, State = #state{id = Sid, messages = Messages, connect = Conn}) ->
    case Conn of
	undefined ->
	    ok;
	Pid when is_pid(Pid) ->
	    Pid ! {message_recv, self()};
	_ ->
	    ok
    end,
    {noreply, State#state{messages = [Message|Messages]}};
handle_cast(_Msg, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
handle_info(session_timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
terminate(_Reason, _State = #state{id = Sid, registered = Registered, connect = Conn, session_state = SessionState}) ->
    Key = ["sid_", binary_to_list(Sid)], 
    redis_worker:del(Key),
    redis_worker:srem(Key, Sid),
    case Registered of
        true ->
            %% Callback:close(self(), SessionId, SessionState),
            ok;
        _ ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
refresh_session_timeout(State = #state{id = Sid, ttl = TTL, session_timeout_tref = TRef}) ->
    erlang:cancel_timer(TRef),
    NewTRef = erlang:send_after(TTL, self(), session_timeout),
    Key = ["sid_", binary_to_list(Sid)], 
    redis_worker:expire(Key, TTL),
    State#state{session_timeout_tref = NewTRef}.

process_messages([], _State) ->
    {reply, ok, _State};
process_messages([Message|Rest], State = #state{id = Sid, connect = Conn}) ->
    case Message of
        {disconnect, _EndPoint} ->
            {stop, normal, ok, State};
        {connect, _EndPoint} ->
            process_messages(Rest, State);
        disconnect ->
            {stop, normal, ok, State};
        heartbeat ->
            process_messages(Rest, State);
        {message, <<>>, EndPoint, Obj} ->
	    route_msg_to_all(Conn, Sid, {message, <<>>, <<>>, Obj}),
	    process_messages(Rest, State);
        {json, <<>>, EndPoint, Obj} ->
	    route_msg_to_all(Conn, Sid, {json, <<>>, <<>>, Obj}),
	    process_messages(Rest, State);
        _ ->
            %% Skip message
            process_messages(Rest, State)
    end.
