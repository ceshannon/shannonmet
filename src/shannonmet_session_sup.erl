-module(shannonmet_session_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

%% --------------------------------------------------------------------------

-spec start_link() -> ignore | {'ok', pid()} | {'error', any()}.
start_link() ->
     supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{undefined, {shannonmet_session, start_link, []},
            temporary, 5000, worker, [shannonmet_session]}]}}.

start_child(SessionId, TTL, Opts) ->
   supervisor:start_child(?MODULE, [SessionId, TTL, Opts]).
