-module(time).
-export([zero/0,inc/2,merge/2,leq/2]).

%return an initial Lamport value
zero() -> 0. % does 0 work for initial? 

%return Time incremented by one
inc(_Name, T) -> T+1.

%merge,ie return the max value
merge(Ti, Tj) -> erlang:max(Ti, Tj).

%return true if Ti is less or equal to Tj
leq(Ti, Tj) ->
    case Ti =< Tj of
    true -> true;
    false -> false
end.

%----------In 4, make worker use time module.
% --------------------- DO TESTS IN 3 and 4 before playing with these
%----------In 4.1, make logger use time module.
% clock(Nodes)
% dont know if right, a guess
clock([]) -> {};
clock([Node|Rest]) -> [{Node,zero()}|clock(Rest)].

% we want to update the clock for the node that we received from
%update(Node,Time,Clock)
update(Node, Time, {Node,ClockTime}) -> {Node, inc(Node, merge(Time, ClockTime))}.

% is it safe to log an event that happend at a given time, true or false?
%safe(Time,Clock)
safe(Time, {_Node,ClockTime}) -> leq(ClockTime,Time).