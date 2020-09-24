-module(vect).
-export([zero/0,inc/2,merge/2,safe/2,clock/1,update/3]).

% ADDED CODE
zero() -> []. 

% When we ex get a message we want to update the clock for this name, otherwise it is the first message
inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, NameTime} -> %ADDED
            lists:keyreplace(Name, 1, Time, {Name, NameTime +1}); % last tuple added
            false ->
                [{Name, 1}|Time] % first tuple added
end. 

% if we have the name already in list, take the max value, otherwise add new entry
merge([], Time) -> Time; % added return the time with merged incoming
merge([{Name, Ti}|Rest],Time) -> 
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            [{Name,erlang:max(Ti,Tj)}|merge(Rest,lists:keydelete(Name, 1, Time))]; %first tuple added
        false ->
            [{Name,Ti}|merge(Rest,Time)] %first tuple added
end.

% we want ALL the times in list to be lower than the corresponding in clock
% when empty list, we have checked every entry, hence it is true that all are less/equal
leq([],_) -> true; % added
leq([{Name,Ti}|Rest],Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} -> 
            if 
                Ti =< Tj -> 
                    leq(Rest, lists:keydelete(Name, 1, Time)); % added, keep go through list
                true ->
                    false % added, if someone is bigger than not safe
            end;
        false ->
            false % added, if someone is bigger than not safe
end.

% initial clock is empty, we have not seen any messages
clock(_) -> []. % added

% Take the time for From from Time, check if we already has got any messages from From in clock
% if we already had a time, replace it otherwise add it to clock
update(From, Time, Clock) ->
    {From, FromTime} = lists:keyfind(From, 1, Time), % first tuple added
    case lists:keyfind(From,1,Clock) of
        {From,_} ->
            lists:keyreplace(From, 1, Clock, {From, FromTime}); % last tuple added
        false ->
            [{From, FromTime}|Clock] % first tuple added
        end.
% if each entry in Time is lower than the corresponding in clock then it is safe to print
safe(Time, Clock) ->
    leq(Time, Clock). % leq call added