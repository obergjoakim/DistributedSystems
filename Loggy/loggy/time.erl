-module(time).
-export([zero/0,inc/2,merge/2,clock/1,update/3,safe/2]).
%--------------for workers---------------

%return an initial Lamport value
zero() -> 0. % does 0 work for initial? 

%return Time incremented by one
inc(_Name, T) -> T+1.

%merge,ie return the max value
merge(Ti, Tj) -> erlang:max(Ti, Tj).

%return true if Ti is less or equal to Tj
leq(Ti, Tj) -> Ti =< Tj.


% clock(Nodes)
% dont know if right, a guess
clock([]) -> [];
clock([Node|Rest]) -> [{Node,zero()}|clock(Rest)].

% we want to update the clock for the node that we received from
%update(Node,Time,Clock)
update(Node, Time, Clock) -> 
    Updated = lists:keydelete(Node, 1, Clock),
    lists:keysort(2,[{Node,Time}|Updated]). 




% is it safe to log an event that happend at a given time, true or false?
%safe(Time,Clock)
%sorted clock, check if the time is lower than lowest in clock
safe(Time, [{_Node,Clocktime}|Rest]) -> leq(Time,Clocktime).