-module(interface).
-export([new/0,add/4,remove/2,lookup/2,ref/2,name/2,list/1,broadcast/2]).
% Code created by Joakim Öberg 14/09-2020
% all code is added by me, from instructions given in text

% ADDED CODE, returns new interface
new() -> [].

%ADDED CODE, Add new interface to Intf, if present return the interface
add(Name, Ref, Pid, Intf) -> 
    case lists:keyfind(Name, 1, Intf) of
        {Name,_,_} -> Intf;
        _ -> [{Name,Ref,Pid}|Intf]
end.

% DONT WORK, doesnt save ignored node, ADDED CODE, remove Name from interfaces
% remove(Name,Intf)
%remove(_Name,[]) -> [];
%remove(Name,[{Name,_,_}|Rest]) -> Rest;
%remove(Name,[H|Rest]) -> remove(Name,Rest).

% ADDED CODE, if name is present remove it from list, otherwise return the list
remove(Name,Intf) ->
    lists:keydelete(Name, 1, Intf).

%ADDED CODE, return {ok, Pid} if Name is present in interfaces
lookup(Name,Intf) ->
    case lists:keyfind(Name,1,Intf) of
        {Name,_Ref,Pid} -> {ok, Pid};
        _ -> notfound
end.
% ADDED CODE, return {ok,Ref} if Name is present in Interface
ref(Name, Intf) ->
    case lists:keyfind(Name,1,Intf) of
        {Name,Ref,_Pid} -> {ok, Ref};
        _ -> notfound
end.
% ADDED CODE, return {ok, Name} if Ref is present in Interface
name(Ref,Intf) ->
    case lists:keyfind(Ref,2,Intf) of
        {Name,Ref,_Pid} -> {ok, Name};
        _ -> notfound
end.
% ADDED CODE, return list with all names present
%list(Intf)
list([]) -> [];
list([{Name,_Ref,_Pid}|Rest]) -> [Name|list(Rest)].

%ADDED CODE, Send Message to each Pid in interfaces, Pid is {Name, 'country@IPAddr'} erlang can send to the pid directly
%broadcast(Message,Intf)
broadcast(_,[]) -> broadcast;
broadcast(Message, [{_Name,_Ref,Pid}|Rest]) ->
    Pid ! Message,
    broadcast(Message,Rest).

   