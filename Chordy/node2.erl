-module(node2).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).


start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).



connect(Id, nil) -> {ok, {Id, self()}};
connect(_Id, Peer) ->
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
    node(Id, Predecessor, Successor, []).




node(Id, Predecessor, Successor, Store) ->
    receive
    {key, Qref, Peer} ->
        Peer ! {Qref, Id},
        node(Id, Predecessor, Successor, Store);
    {notify, New} ->
        {Pred, Keep} = notify(New, Id, Predecessor, Store),
        node(Id, Pred, Successor, Keep);
    {request, Peer} ->
        request(Peer, Predecessor),
        node(Id, Predecessor, Successor, Store);
    {status, Pred} ->
        Succ = stabilize(Pred, Id, Successor),
        node(Id, Predecessor, Succ, Store);
    stabilize ->
        stabilize(Successor),
        node(Id, Predecessor, Successor, Store);
    probe ->
        create_probe(Id, Successor),
        node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} ->
        remove_probe(T, Nodes),
        node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
        forward_probe(Ref, T, Nodes, Id, Successor),
        node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->          % added in 2.2
        Added = add(Key, Value, Qref, Client,
        Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->              % added in 2.2
        lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
        Merged = storage:merge(Store, Elements),
        node(Id, Predecessor, Successor, Merged)

end.

% ADDED From 2.3
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of % is the new key between the one behind and me?
        true ->
            Client ! {Qref, ok},
            storage:add(Key,Value,Store);
        false ->
        Spid ! {add, Key, Value, Qref, Client},
        Store
end.

% ADDED From 2.4
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup,Key,Qref,Client}
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


notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid},Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
        true ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid},Keep};
        false ->
            {Predecessor, Store}
    end
end.

handover(Id, Store, Nkey, Npid) ->
    % changed place of Id and Nkey, we want to include Id
    {Keep, Rest} = storage:split(Nkey,Id, Store), 
    Npid ! {handover, Rest},
    Keep.