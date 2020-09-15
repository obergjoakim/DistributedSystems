-module(routy).
-export([start/2,stop/1,status/1]). % ADDED CODE, status/1

% Register a new spawned router to Reg
start(Reg, Name) ->
    register(Reg,spawn(fun()->init(Name) end)).

% ADDED CODE
% check status on Reg and send back here for print out, look at {status, from} row ~ 75
status(Reg) ->
    Reg ! {status, self()},
    receive
        {status, {Name, N, Hist, Intf, Table, Map}} ->
            io:format(" Status recieved from: ~w~n Name: ~w~n N: ~w~n Hist: ~w~n Intf: ~w~n Table: ~w~n Map: ~w~n", [Reg, Name, N, Hist, Intf, Table, Map])
end.

% Stop a router, and unregister the process from node
stop(Node) ->
    Node ! stop,
    unregister(Node).

% Initialize the router and call router(Name,0,Hist,Intf,Table,Map). to listen for messages
init(Name) ->
    Intf = interface:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    %{Name, N} = hist:new(Name), 
    % what is 0 and Msgs? do they come from Hist?
    router(Name,0,Hist,Intf,Table,Map).

% Listen for incomming messages, messages is different looking tuples, pattern match to the right message
router(Name,N,Hist,Intf,Table,Map) ->
    receive

    {add,Node,Pid} -> 
        Ref = erlang:monitor(process,Pid),
        Intf1 = interface:add(Node,Ref,Pid,Intf),
        router(Name,N,Hist,Intf1,Table,Map);
    
    {remove,Node} -> 
        {ok,Ref} = interface:ref(Node, Intf),
        erlang:demonitor(Ref),
        Intf1 = interface:remove(Node, Intf),
        router(Name,N,Hist,Intf1,Table,Map);

    {'DOWN',Ref,process,_,_} ->
        {ok,Down} = interface:name(Ref, Intf),
        io:format("~w: exit recived from ~w~n", [Name, Down]),
        Intf1 = interface:remove(Down, Intf),
        router(Name, N, Hist, Intf1, Table, Map);
    
    %added from 5.2
    {links, Node, R, Links} ->
        case hist:update(Node, R, Hist) of
        {new, Hist1} ->
            interface:broadcast({links, Node, R, Links}, Intf),
            Map1 = map:update(Node, Links, Map),
            router(Name, N, Hist1, Intf, Table, Map1);
        old ->
            router(Name, N, Hist, Intf, Table, Map)
        end;
    %from 5.2
    update ->
        Table1 = dijkstra:table(interface:list(Intf), Map),
        router(Name, N, Hist, Intf, Table1, Map);

    % from 5.2
    broadcast ->
        Message = {links, Name, N, interface:list(Intf)},
        interface:broadcast(Message, Intf),
        router(Name, N+1, Hist, Intf, Table, Map);

    % if we get {status,From} we send info about us to From ( a pretty print out), look at print out ar row ~ 10
    {status,From} ->
        From ! {status, {Name, N, Hist, Intf, Table, Map}},
        router(Name, N, Hist, Intf, Table, Map);

    % if message routed to us, added from 5.4
    {route, Name, _From, Message} ->
        io:format("~w: received message ~w ~n", [Name, Message]),
        router(Name, N, Hist, Intf, Table, Map);

    % if message routed to someone else, added from 5.4
    {route, To, From, Message} ->
        io:format("~w: routing message (~w)~n", [Name, Message]),
        case dijkstra:route(To, Table) of
        {ok, Gw} ->
            case interface:lookup(Gw, Intf) of
                {ok, {Ref, _IPAddr}} ->
                    Ref ! {route, To, From, Message};
                notfound ->
                    ok
            end;
        notfound ->
            ok
        end,
        router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
        self() ! {route, To, Name, Message},
        router(Name, N, Hist, Intf, Table, Map);

    stop ->
        ok
end.