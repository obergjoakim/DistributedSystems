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


table(Gateways,Map) ->
    % get all nodes from map
    AllNodes = map:all_nodes(Map),
    % delete duplicates from map and gateways
    UsortedList = lists:usort(AllNodes ++ Gateways),
    %all nodes in usortedlist is given inf and unknown
    InfList = lists:map(fun(Node) -> {Node,inf,unknown} end, UsortedList),
   % for the gateways add {Gateway, 0,Gateway} instead of inf and unknown
    Sorted = isGatewayOrNot(InfList, Gateways),
   % Call iterate to make table
    iterate(Sorted,Map,[]).

% if H is in gateways, add new length=0 and gateway to itself
isGatewayOrNot(InfList,[]) -> InfList;
isGatewayOrNot(InfList,[H|Tail]) -> 
    case lists:keyfind(H, 1, InfList) of
        {H,_,_} -> isGatewayOrNot(update(H,0,H,InfList),Tail);
        false -> isGatewayOrNot(InfList,Tail)
end.



% search Table if we can send a message to some gateway from node
route(Node,Table) -> 
    case lists:keyfind(Node,1,Table) of
        {Node, Gateway} -> {ok,Gateway};
        _ -> notfound
end.
