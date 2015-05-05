-module(shannonmet).

-export([start/0, stop/0]).

start() ->
    ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),                                                                                                                                       
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    application:start(shannonmet).    

stop() ->
    application:stop(shannonmet).
