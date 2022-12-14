-module(server_monitor).
-behaviour(gen_statem).
-define(NAME, server_monitor).

-export([start_link/1,stop/0]). % Server

-export([init/1,callback_mode/0,terminate/3]). % Behaviour

-export([wait_connect/3, connected/3]). % States

%% Start and stop
start_link({Host, Port, Monitor}) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, {Host, Port, Monitor}, []).
stop() ->
    gen_statem:stop(?NAME).

callback_mode() -> [state_functions, state_enter].


%% API

%% Init and terminate

init({Host, Port, Monitor}) ->
    Data = #{host => Host, port => Port, monitor=>Monitor},
    Actions = [{next_event, internal, connect}],
    {ok, wait_connect, Data, Actions}.

terminate(_Reason, _State, _Data) ->
    ok.

%% States
wait_connect(enter, connected, Data) ->
    io:format("connection closed, auto reconnect ~n", []),
    Actions = [{{timeout, reconnect}, 10000, reconnect}],
    {keep_state, maps:remove(socket, Data), Actions};
wait_connect(enter, wait_connect, _Data) ->
    keep_state_and_data;
wait_connect({timeout, reconnect}, _EventContent, Data) ->
    Actions = [{next_event, internal, reconnect}],
    {keep_state, Data, Actions};
wait_connect(internal, connect, Data) ->
    #{host := Host, port := Port, monitor := _Monitor} = Data,
    case gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, 0}]) of
	{ok, Socket} ->
	    {next_state, connected, Data#{socket => Socket}};
	{error, Error} ->
	    io:format("Connection failed: ~p ~n", [Error]),
	    Actions = [{{timeout, reconnect}, 10000, reconnect}],
	    {keep_state, Data, Actions}
    end;
wait_connect(internal, reconnect, Data) ->
    #{host := Host, port := Port, monitor := Monitor} = Data,
    case gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, 0}]) of
	{ok, Socket} ->
	    case Monitor of
		true ->
		    io:format("connection reestablished after fail~n", []);
		_ -> ok
	    end,
	    {next_state, connected, Data#{socket => Socket}};
	{error, Error} ->
	    io:format("Connection failed: ~p ~n", [Error]),
	    Actions = [{{timeout, reconnect}, 10000, reconnect}],
	    {keep_state, Data, Actions}
    end.

connected(enter,_OldState, Data) ->
    Actions = [{{timeout, nodata}, 60000, nodata}],
    {keep_state, Data, Actions};
connected(info, {tcp, socket, Packet}, _Data) ->
    io:format("Recv packet: ~p ~n", [Packet]),
    keep_state_and_data;
connected(info, {tcp_closed, _Socket}, Data) ->
    {next_state, wait_connect, Data};
connected(internal, nodata, Data) ->
    Actions = [{{timeout, nodata}, 60000, nodata}],
    {keep_state, Data, Actions};
connected({timeout, nodata}, _EventContent, Data) ->
    io:format("One mimute no data~n", []),
    Actions = [{next_event, internal, nodata}],
    {keep_state, Data, Actions}.

% Helpers
