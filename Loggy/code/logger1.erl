-module(logger1).
-export([start/1,stop/1]).
% if file had name logger I got error
%=ERROR REPORT==== 21-Sep-2020::23:07:52.297839 ===
%Can't load module 'logger' that resides in sticky dir


% the logger simply accepts events and prints them on the screen(prepared to receive time stamps)
%The logger is given a list of nodes that will send it messages (will use later)

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    loop(Clock,[]).

loop(Clock,Queue) ->
    receive
        {log,From,Time,Msg} ->
            % returns tuple list of all times
            NewClock = time:update(From, Time, Clock),
            % returns FIFO queue with added msg
            NewQueue =  lists:keysort(2, [{From,Time,Msg}|Queue]),
            % every time a message is added to the queue the length of the queue is printed
            io:format("Holdback queue length: ~w~n", [length(NewQueue)]),
            % print msg safe to print
            UpdatedQueue = safeToPrint(NewClock,NewQueue,[]),
            loop(NewClock, UpdatedQueue);

        stop ->
            %print the length of holdback queue when stop
            io:format("Holdback queue length: ~w~n", [length(Queue)]),
            ok
end.

safeToPrint(_Clock,[],Queue) -> Queue;
% iterate through queue, if a node clock in queue has a time lower than incoming clock -> print the one in queu
safeToPrint(Clock,[{Node,QueueTime,Msg}|Tail],Q) ->
            case time:safe(QueueTime, Clock) of
                true -> log(Node, QueueTime,Msg),
                        safeToPrint(Clock, Tail, Q);
                false -> safeToPrint(Clock, Tail, [{Node,QueueTime,Msg}|Q])
    end.

    

log(From,Time,Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
