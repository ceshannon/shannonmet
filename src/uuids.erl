-module(uuids).

-behaviour(gen_server).
 
-export([start/0, stop/0]).
-export([get/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {uuid_state}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

get() ->
    gen_server:call(?MODULE, create).

init([]) ->
    self() ! post_init,
    {ok, #state{uuid_state = undefined}}.

terminate(_Reason, _State) ->
    ok.

handle_call(create, _From, State) ->
    UuidState = case State#state.uuid_state of
		    undefined ->
			NewState = uuid:new(self()),
			NewState;
		    State0 ->
			State0
		end,
    {Id, State1} = uuid:get_v1(UuidState),
    Result = re:replace(uuid:uuid_to_string(Id), "-", "", [global, {return, list}]),
    {reply, list_to_binary(Result), State1};
handle_call(_, _From, State) ->
    {reply, undefined, State}. 

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(post_init, State) ->
    NewState = uuid:new(self()),
    {noreply, State#state{uuid_state = NewState}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


