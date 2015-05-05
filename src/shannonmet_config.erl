-module(shannonmet_config).
-author('iswap2009@gmail.com').
%% -behaviour(gen_server).

-include("shannonmet_internal.hrl").

%% API
-export([configure/1]).

%%%===================================================================
%%% API
%%%===================================================================
configure(Opts) ->
    #config{heartbeat = proplists:get_value(heartbeat, Opts, 5000),
            heartbeat_timeout = proplists:get_value(heartbeat_timeout, Opts, 30000),
            ttl = proplists:get_value(ttl, Opts, 30000),
            %% callback = proplists:get_value(callback, Opts),
            protocol = proplists:get_value(protocol, Opts, shannonmet_data_protocol),
            opts = proplists:get_value(opts, Opts, undefined)
           }.
