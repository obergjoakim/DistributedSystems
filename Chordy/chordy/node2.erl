-module(node2).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).


start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).



connect(Id, nil) -> {ok, {Id, self()}};%ADDED CODE the tuple
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
    {Qref, Skey} -> %ADDED CODE
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
    probe ->                                    % iterates through all nodes, takes the total time for one circle and prints total number of nodes
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
        node(Id, Predecessor, Successor, Merged);
    status ->                                        %ADDED CODE(my own idea), used to check one specific node
        io:format("Node (Id: ~w ) have ~w number of keys in storage ~n", [Id,lists:flatlength(Store)]),
        node(Id, Predecessor, Successor, Store)


end.

% ADDED From 2.3
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of % ADDED CODE, is the new key between the one behind and me?
        true ->
            Client ! {Qref, ok},
            storage:add(Key,Value,Store); %ADDED CODE
        false ->%ADDED CODE
        Spid ! {add, Key, Value, Qref, Client},
        Store
end.

% ADDED From 2.4
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of%ADDED CODE
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup,Key,Qref,Client}%ADDED CODE
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

%request(Peer, Predecessor) ->
 %   case Predecessor of
  %  nil ->
   %     Peer ! {status, nil};
   % {Pkey, Ppid} ->
    %    Peer ! {status, {Pkey, Ppid}}
   % end.
   
% ADDED CODE, because we always send the predecessor we can just forward whatever it holds
request(Peer, Predecessor) -> Peer ! {status, Predecessor}.

% Pred is next.prev in java, Successor is next
% return who is in front of me and sometimes notify the one in front who is behind
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
    nil -> %ADDED CODE
        Spid  ! {notify,{Id, self()}},
        Successor;
    {Id, _} -> Successor;%ADDED CODE
    {Skey, _} -> %ADDED CODE
        Spid  ! {notify,{Id, self()}},
        Successor;
    {Xkey, Xpid} ->
    case key:between(Xkey, Id, Skey) of
    true ->%ADDED CODE
        stabilize(Pred, Id, {Xkey, Xpid});
    false ->%ADDED CODE
    Spid  ! {notify,{Id, self()}},
    Successor
    end
end.


schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).


notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->%ADDED CODE
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid},Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
        true ->%ADDED CODE
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid},Keep};
        false ->%ADDED CODE
            {Predecessor, Store}
    end
end.

% only used when adding more nodes, dont affect the lookup time in check, therefor ok to have io:print
handover(Id, Store, Nkey, Npid) ->
    % changed place of Id and Nkey, we want to include Id
    {Keep, Rest} = storage:split(Nkey,Id, Store),
    io:format("~n ---------------------------------------- ~n Node (Id: ~w ) had ~w number of keys in storage ~n Is now updated and holds ~w keys ~n ---------------------------------------- ~n~n", [Id,lists:flatlength(Store), lists:flatlength(Keep)]), 
    Npid ! {handover, Rest},
    Keep.