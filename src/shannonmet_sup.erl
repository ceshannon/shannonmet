-module(shannonmet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, PoolConfig} = application:get_env(shannonmet, redis_pool),
    Hostname = proplists:get_value(hostname, PoolConfig),
    PoolSize = proplists:get_value(size, PoolConfig),
    Port = proplists:get_value(port, PoolConfig),
    Password = proplists:get_value(password, PoolConfig),
    PoolArgs = [
                {name, {local, redis_pool}},
                {worker_module, redis_worker},
		{size, PoolSize},
                {max_overflow, 10}
               ],
    WorkerArgs = [Hostname, Port, Password],
    RedisSpec = poolboy:child_spec(redis_pool, PoolArgs, WorkerArgs),
    UuidsSpec = {uuids, {uuids, start, []}, permanent, 5000, worker, [uuids]},
    SessionSpec = {shannonmet_session_sup, {shannonmet_session_sup, start_link, []},permanent, 5000, supervisor, [shannonmet_session_sup]},
    {ok, { {one_for_one, 5, 10}, [RedisSpec, UuidsSpec, SessionSpec]} }.

