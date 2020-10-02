-module(gms3).
-export([start/1,start/2]).

-define(timeout, 2500).
-define(arghh, 300).

% 
% start first node, is its own leader
start(Id) ->
    Rnd = random:uniform(1000), % ADDED in 3.2
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd,Self) end)}.

% the first node will have itself as leader, empty list of peers
init(Id, Rnd,Master) -> % added Rnd
    random:seed(Rnd,Rnd,Rnd),
    leader(Id, Master, 0, [], [Master]). % set leader to wait for its first message

% starting a node that should join a group, is initially a slave
start(Id, Grp) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self, Rnd) end)}.

% to be a part of a group we need to send a "join" message to a node in group, 
init(Id, Grp, Master,Rnd) -> % added Rnd
    random:seed(Rnd,Rnd,Rnd),
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader), % ADDED CODE from 3.1
            Master ! {view, Group},
            slave(Id, Master, Leader, N+1, {view, [Leader|Slaves], Group}, Slaves, Group) % slave waiting for first message, last message is view
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.


% from instructions, the leader
% Id: an unique id for debugging, Master: PID for application layer, 
% Slaves: ordered list of the PIDs of all slaves in group, Group: list of all appication layer processes in the group
leader(Id, Master, N, Slaves, Group) -> % ADDED N in 3.3
    receive
        {mcast, Msg} -> % from either its own master or a peer node,  
            bcast(Id, {msg, N, Msg}, Slaves), % {msg,N, Msg} multicasted to all peers
            Master ! Msg, % sent to application level
            leader(Id, Master, N+1, Slaves, Group); % added N
        {join, Wrk, Peer} -> % from either peer or the master, request to join group, Wrk is PID of application layer, Peer PID of its group process
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2), % only appication layer needs to know N
            Master ! {view, Group2}, % msg sent to Worker(not applic layer)
            leader(Id, Master, N+1, Slaves2, Group2); % added N+1
        stop ->
            ok
end.

% N is expected next message, Last is copy of last message
slave(Id, Master, Leader, N , Last ,Slaves, Group) ->
    receive
        %if old message ie lower than N, dont care, what if I > N??
        {msg, I, _} when I < N ->                   
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        NewLastMsg = {msg, N,Msg} -> % multicasted message from leader
            Master ! Msg, % sent to master
            slave(Id, Master, Leader, N+1, NewLastMsg,Slaves, Group);

        %if old message ie lower than N, dont care, what if I > N??
        {view, I,_, _} when I < N ->                   
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        NewLastMsg = {view, N,[Leader|Slaves2], Group2} -> % multicasted view from leader
            Master ! {view, Group2}, % view deliverd to master process
            slave(Id, Master, Leader, N+1, NewLastMsg, Slaves2, Group2);

        {'DOWN', _Ref, process, Leader, _Reason} -> % ADDED CODE from 3.1
            election(Id, Master, N, Last, Slaves, Group);
        {mcast, Msg} -> % request from master to multicast message
            Leader ! {mcast, Msg}, % forwarded to leader
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} -> % request from master to allow new node to join group
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        stop ->
            ok
end.




% ADDED CODE from 3.1
% if a node find itself first in list, then becomes new leader of group
election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, Last, Rest),
            Master ! {view, Group},
            leader(Id, Master, N, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.


% from 3.2
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

% from 3.2
crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.
