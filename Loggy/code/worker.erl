-module(worker).
-export([start/5,stop/1,peers/2]).

% the worker will wait for a while and then send a message to one of its peers. 
% while waiting, it is prepared to receive messages from peers. 
% To keep track of what is happening and in what order things are done we send a log entry to the logger every time we send or receive a message.
% First implementation has no logical time

% the sleep time decide how active the worker will be
% the jitter is an unique vaulue for each worker, the jitter value intruduce a random delay between the sending of a message and the sending of a log entry

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter);
        stop ->
            ok
    end.

        peers(Wrk, Peers) ->
            Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            Log ! {log, Name, Time, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = na,
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).