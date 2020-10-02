-module(gms2).
-export([start/1,start/2]).

-define(timeout, 2500).
-define(arghh, 200).


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

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {'DOWN', _Ref, process, Leader, _Reason} -> % ADDED CODE from 3.1
            election(Id, Master, Slaves, Group);
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


% ADDED CODE from 3.2
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

% ADDED CODE from 3.2
crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.

% 
% start first node, is its own leader
start(Id) ->
    Rnd = random:uniform(1000), % ADDED CODE Rnd in 3.2
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd,Self) end)}.

% the first node will have itself as leader, empty list of peers
init(Id, Rnd,Master) -> % ADDED CODE Rnd in 3.2
    random:seed(Rnd,Rnd,Rnd),
    leader(Id, Master, [], [Master]).

% starting a node that should join a group, is initially a slave
start(Id, Grp) ->
    Rnd = random:uniform(1000), % ADDED CODE Rnd in 3.2
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self, Rnd) end)}.

% to be a part of a group we need to send a "join" message to a node in group, 
init(Id, Grp, Master,Rnd) -> % ADDED CODE Rnd in 3.2
    random:seed(Rnd,Rnd,Rnd), 
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader), % ADDED CODE from 3.1
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    % ADDED CODE from 3.1
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.

% ADDED CODE from 3.1
% if a node find itself first in list, then becomes new leader of group
election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
    end.