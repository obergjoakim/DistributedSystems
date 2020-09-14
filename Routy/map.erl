-module(map).
-export([new/0,update/3,reachable/2,all_nodes/1]).

% This program is written by Joakim Öberg 9/11-2020, from instructions given in text
% The map is a implementation of a map. The map is a list of tuples, 
% the map looks like [ {City, ConnectedNodes}| moreTuples]

new() -> [].

% Map looks like [{City,[reachableCitys]}| Rest]
update(Node, Links, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {Node, _OldLinks} -> 
            NewMap = lists:keydelete(Node, 1, Map),
            [{Node,Links}|NewMap];
        
        false -> [{Node,Links}|Map]
end.

% return list of reachable links from node, if any
reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {Node, Links} -> Links;

        false -> []
end.



% prints all nodes in map, duplicates is taken away
% poor complexity?? can we do it faster?
all_nodes(Map) ->
    L = noTuples(Map,[]),
    noDuplicates(lists:sort(L)).

% noTuples extract node and links from tuples, flatten take away lists inside lists [[a],x,[b]] becomes [a,x,b] 
% does this differ in performance from foldl? What is a better way to do it?
noTuples([],L) -> lists:flatten(L);
noTuples([{Node,Links}|Tail],L) -> noTuples(Tail, [Node,Links|L]).


% deletes duplicates from a list
noDuplicates([]) -> [];
noDuplicates([H,H|T]) -> noDuplicates([H|T]);
noDuplicates([H|T]) -> [H|noDuplicates(T)].


