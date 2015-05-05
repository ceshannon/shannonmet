-module(shannonmet_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    PrivDir = code:priv_dir(shannonmet),
    io:format("priv dir is : ~p ~n", [PrivDir]),
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/socket.io/1/[...]", shannonmet_handler, [shannonmet_config:configure([{heartbeat, 5000},
														      {heartbeat_timeout, 30000},
														      {ttl, 30000},
														      {protocol, shannonmet_data_protocol}
														     ])]},
					     {"/[...]", cowboy_static, {dir, PrivDir, [{mimetypes, cow_mimetypes, all}]}}
					    ]}
				     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
							    {env, [{dispatch, Dispatch}]}
							   ]),
    shannonmet_sup:start_link().

stop(_State) ->
    ok.
