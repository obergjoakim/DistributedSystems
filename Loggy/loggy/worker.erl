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
% 4 added time to loop
init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter,time:zero());
        stop ->
            ok
    end.



peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.
% added time
loop(Name, Log, Peers, Sleep, Jitter,Time)->
    Wait = random:uniform(Sleep),
    receive
        % take max time and increment
        {msg, ReceivedTime, Msg} ->
            MaxTime = time:inc(Name, time:merge(Time, ReceivedTime)),
            %print ReceivedTime to see if out of order, or the new internal MaxTime
            Log ! {log, Name, MaxTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter,MaxTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        UpdatedTime = time:inc(Name, Time),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, UpdatedTime, Message},
        jitter(Jitter), % the random sleep befor actually logging that we sent a message
        Log ! {log, Name, UpdatedTime, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter,UpdatedTime)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).