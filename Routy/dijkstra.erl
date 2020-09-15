-module(dijkstra).
-export([route/2,table/2]).
% This code is written by Joakim Öberg 14/09-2020, from instructions given in text

% returns shortest path to node, or 0 if not found
%entry(Node, Sorted)
entry(_Node, []) -> 0;
entry(Node, [{Node, PathLength, _Gateway}|_Rest]) -> PathLength;
entry(Node, [{_X,_PathLength,_Gateway}|Rest]) -> entry(Node, Rest).

% replace replaces the entry for node in sorted with a new entry having length N and Gateway
% this implementation sorts the list, we could instead try to replace the entry at the right place directly
%replace(Node, N, Gateway, Sorted)
replace(_Node, _PathLength, _Gateway, []) -> []; 
replace(Node, PathLength, Gateway,[{Node,_N,_G}|Rest]) -> lists:keysort(2, [{Node,PathLength,Gateway}|Rest]);
replace(Node,PathLength, Gateway,[{X,N,G}|Rest]) -> lists:keysort(2, [{X,N,G}|replace(Node,PathLength,Gateway,Rest)]).

% update the list sorted given the information that node can be reached in N hops using Gateway
% update(Node, Pathlength, Gateway, Sorted)
update(Node, PathLength, Gateway, Sorted) -> 
    case PathLength < entry(Node, Sorted) of
        true -> replace(Node,PathLength,Gateway,Sorted);
        _ -> Sorted
end.



% the path is N hops from Gateway to Node, therefor the path to each link connected to Node has path N+1 from gateway
% The list Sorted is updated with this new paths, and table updated with node and gateway
%iterate(Sorted,Map,Table)
iterate([],_Map,Table) -> Table;
iterate([{_,inf,_}|_Rest],_Map,Table) -> Table;
iterate([{Node,N,Gateway}|SortedRest],Map,Table) ->
    %list of connections to node
    Connected = map:reachable(Node,Map),
    %add all paths to nodes(connections) from gateway with N+1 hops
    NewSorted =  forEachInList(SortedRest,Connected,Gateway,N+1),
    iterate(NewSorted,Map,[{Node,Gateway}|Table]).


% add path from gateway to dest with N hops
%forEachInList(Sorted,Connections,Gateway,Hops)
forEachInList(Sorted,[],_Gateway,_N) -> Sorted;
forEachInList(Sorted,[Dest|ConnectedRest],Gateway, N) ->
    forEachInList(update(Dest,N,Gateway,Sorted),ConnectedRest,Gateway,N).

% constructs a routing table, given gateways and a map
table(Gateways,Map) ->
    AllNodes = map:all_nodes(Map),
    % first create for each gateway {node, 0, node} then for all in Map {node,inf,unknown}
    %creates a initial sorted list, first only 0, then inf
    Initial = forEach(AllNodes,unknown),
    InitialGateways = forEach(Gateways,gateway),
    % how to do this instead? operation cost N, usort (unique sort) delete duplicates
    InitialSorted = InitialGateways ++ Initial,
    iterate(InitialSorted,Map,[]).

% make a list of tuples, if map(m): {node,inf,unknown}, if gateways(g): {node,0,node} 
forEach([],_) -> [];
forEach([H|T],Case) -> 
case Case of
    unknown -> [{H,inf,unknown}|forEach(T,unknown)];
    gateway -> [{H,0,H}|forEach(T,gateway)]
end.

% search Table if we can send a message to some gateway from node
route(Node,Table) -> 
    case lists:keyfind(Node,1,Table) of
        {Node, Gateway} -> {ok,Gateway};
        _ -> notfound
end.
