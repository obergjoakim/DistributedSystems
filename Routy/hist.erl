-module(hist).
-export([new/1,update/3]).
% This code is created by Joakim Öberg 14/09-2020, from instructions given in text

% new creates a new history to hold old messages from name
new(Name) -> {Name,0}.

% check if we already has recieved the msg number or if this is a new msg
update(Node, N, {Node,Sofar}) ->
    
    case N =< Sofar of
    true -> old;
    false -> {new, N}
end.

