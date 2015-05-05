%% @doc EventSource emitter.
-module(eventsource_handler).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

-define(TIMEOUT, 30).

init(Req, Opts) ->
    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    SourceId = cowboy_req:binding(sourceid, Req),
    Req2 = cowboy_req:chunked_reply(200, Headers, Req),
    Key = ["source_", SourceId],
    redis_worker:set(Key, pid_to_list(self())),
    redis_worker:expire(Key, ?TIMEOUT),
    {cowboy_loop, Req2, Opts, ?TIMEOUT*1000}.

info(eof, Req, State) ->
    io:format("end of message ~n", []),
    {shutdown, Req, State};
info({message, Msg}, Req, State) ->
    cowboy_req:chunk(["id: ", id(), "\ndata: ", Msg, "\n\n"], Req),
    {ok, Req, State};
info(Msg, Req, State) ->
    io:format("unkown message:~p ~n", [Msg]),
    {ok, Req, State}.

id() ->
    {Mega, Sec, Micro} = erlang:now(),
    Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_list(Id, 16).

terminate(Reason, _Req, State) ->
    io:format("event source terminate: ~p ~n", [Reason]),
    ok.
