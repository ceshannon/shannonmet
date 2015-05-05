-module(redis_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1, set/2, get/1, del/1, sunion/1, sadd/2, srem/2,smembers/1, expire/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).



-record(state, {conn}).

get(Key) ->
    Pid = get_random_pid(),
    case eredis:q(Pid, ["GET", Key]) of
	{ok, Obj} when Obj /= undefined ->
	    Obj;
	_Any ->
	    none
    end.

set(Key, Value) ->
    Pid = get_random_pid(),
    eredis:q(Pid, ["SET", Key, Value]).

del(Key) ->
    Pid = get_random_pid(),
    eredis:q(Pid, ["DEL", Key]).

expire(Key, T) ->
    Pid = get_random_pid(),
    eredis:q(Pid, ["EXPIRE", Key, T]).

sunion(Keys) ->
    Pid = get_random_pid(),
    case eredis:q(Pid, ["SUNION"] ++ Keys) of
	{ok, Obj} when Obj /= undefined ->
	    Obj;
	_Any ->
	    none
    end.

sadd(Key, Value) ->
    Pid = get_random_pid(),
    eredis:q(Pid, ["SADD", Key, Value]).

srem(Key, Value) ->
    Pid = get_random_pid(),
    eredis:q(Pid, ["SREM", Key, Value]).

smembers(Key) ->
    Pid = get_random_pid(),
    case eredis:q(Pid, ["SMEMBERS", Key]) of
	{ok, T} when is_list(T)->
	    T;
	_ ->
	    none
    end.

get_random_pid() ->
    poolboy:transaction(redis_pool, fun(Worker) ->
					    gen_server:call(Worker, {get_pid})
				    end).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([Server, Port, Pwd]) ->
    process_flag(trap_exit, true),
    case eredis:start_link(Server, Port, 0, Pwd) of
	{ok, Pid} ->
	    {ok, #state{conn = Pid}};
	Err ->
	    {stop, Err}
    end.

handle_call({get_pid}, _From, #state{conn=Conn}=State) ->
    {reply, Conn, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = eredis:stop(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
