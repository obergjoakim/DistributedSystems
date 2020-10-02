-module(node1).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).


start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).



connect(Id, nil) -> {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
    {Qref, Skey} -> 
        {ok, {Skey, Peer}}
    after ?Timeout ->
    io:format("Time out: no response~n",[])
end.


init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).




node(Id, Predecessor, Successor) ->
    receive
    {key, Qref, Peer} ->
        Peer ! {Qref, Id},
        node(Id, Predecessor, Successor);
    {notify, New} ->
        Pred = notify(New, Id, Predecessor),
        node(Id, Pred, Successor);
    {request, Peer} ->
        request(Peer, Predecessor),
        node(Id, Predecessor, Successor);
    {status, Pred} ->
        Succ = stabilize(Pred, Id, Successor),
        node(Id, Predecessor, Succ);
    stabilize ->
        stabilize(Successor),
        node(Id, Predecessor, Successor);
    probe ->
        create_probe(Id, Successor),
        node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} ->
        remove_probe(T, Nodes),
        node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
        forward_probe(Ref, T, Nodes, Id, Successor),
        node(Id, Predecessor, Successor)
end.

% do we want to save all Spids? to what??
create_probe(Id, _Successor = {_,Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

forward_probe(Ref, T, Nodes, Id, _Successor={_,Spid}) ->
    Spid ! {probe, Ref, [Id|Nodes], T}.

%remove_probe(T, []) -> ok;
remove_probe(T, Nodes) ->
    Probe_time = erlang:system_time(micro_seconds) - T,
    io:format("Total time for probe: ~w micro seconds, number of nodes ~w~n ~w ~n", [Probe_time, lists:flatlength(Nodes),Nodes]).



stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
    nil ->
        Peer ! {status, nil};
    {Pkey, Ppid} ->
        Peer ! {status, {Pkey, Ppid}}
    end.

% Pred is next.prev in java, Successor is next
% return who is in front of me and sometimes notify the one in front who is behind
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
    nil ->
        Spid  ! {notify,{Id, self()}},
        Successor;
    {Id, _} -> Successor;
    {Skey, _} -> 
        Spid  ! {notify,{Id, self()}},
        Successor;
    {Xkey, Xpid} ->
    case key:between(Xkey, Id, Skey) of
    true ->
        stabilize(Pred, Id, {Xkey, Xpid});
    false ->
    Spid  ! {notify,{Id, self()}},
    Successor
    end
end.


schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

notify({Nkey, Npid}, Id, {Id,_}) -> {Nkey, Npid}; 
notify({Nkey, Npid}, Id, Predecessor) ->
   % io:format("Id: ~w got notify msg from: ~w ~n", [Id,Nkey]),
    case Predecessor of
        nil -> {Nkey, Npid};
        {Pkey, _} ->
        case key:between(Nkey, Pkey, Id) of
            true -> {Nkey, Npid};
            false -> Predecessor
        end
    end.