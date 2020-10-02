-module(gms1).
-export([start/1, start/2]).


% from instructions, the leader
% Id: an unique id for debugging, Master: PID for application layer, 
% Slaves: ordered list of the PIDs of all slaves in group, Group: list of all appication layer processes in the group
leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} -> % from either its own master or a peer node,  
            bcast(Id, {msg, Msg}, Slaves), % {msg,Msg} multicasted to all peers
            Master ! Msg, % sent to application level
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} -> % from either peer or the master, request to join group, Wrk is PID of application layer, Peer PID of its group process
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            ok
end.

% written by me, sends the message to all its slaves(processes)
bcast(Id, Msg, Slaves) ->
    case Slaves of
        [] -> ok;
        [P1|Rest] -> 
            P1 ! Msg,
            bcast(Id, Msg, Rest)
end.


slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} -> % request from master to multicast message
            Leader ! {mcast, Msg}, % forwarded to leader
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} -> % request from master to allow new node to join group
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} -> % multicasted message from leader
            Master ! Msg, % sent to master
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} -> % multicasted view from leader
            Master ! {view, Group2}, % view deliverd to master process
            slave(Id, Master, Leader, Slaves2, Group2);
        stop ->
            ok
end.

% start first node, is its own leader
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

% the first node will have itself as leader, empty list of peers
init(Id, Master) ->
    leader(Id, Master, [], [Master]).

% starting a node that should join a group, is initially a slave
start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

% to be a part of a group we need to send a "join" message to a node in group, 
init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    end.