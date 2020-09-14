-module(interface).
-export([new/0,add/4,remove/2,lookup/2,ref/2,name/2,list/1,broadcast/2]).
% Code created by Joakim Öberg 14/09-2020
% all code is added by me, from instructions given in text

new() -> [].

add(Name, Ref, Pid, Intf) -> [{Name,Ref,Pid}|Intf].

remove(_Name,[]) -> [];
remove(Name,[{Name,_,_}|Rest]) -> Rest;
remove(Name,[{_,_,_}|Rest]) -> remove(Name,Rest).

lookup(Name,Intf) ->
    case lists:keyfind(Name,1,Intf) of
        {Name,_Ref,Pid} -> {ok, Pid};
        _ -> notfound
end.

ref(Name, Intf) ->
    case lists:keyfind(Name,1,Intf) of
        {Name,Ref,_Pid} -> {ok, Ref};
        _ -> notfound
end.

name(Ref,Intf) ->
    case lists:keyfind(Ref,2,Intf) of
        {Name,Ref,_Pid} -> {ok, Name};
        _ -> notfound
end.

list([]) -> [];
list([{Name,_Ref,_Pid}|Rest]) -> [Name|list(Rest)].


broadcast(Message, Intf) ->
    case Intf of
        [{_Name,_Ref,Pid}|Rest] -> 
            Pid ! Message,
            broadcast(Message,Rest)
end.
   