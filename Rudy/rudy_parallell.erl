-module(rudy_parallell).
-export([start/1, start/2, stop/0,request/1]).
% Program is created by Joakim Öberg, based on code given in rudy4.erl

start(Port) ->
    start(Port, 1).

start(Port, N) ->
    register(rudy_parallell, spawn(fun() -> init(Port, N) end)).

stop() ->
    rudy_parallell ! stop.

% When N processes is spawned from handlers, we wait in super to recieve a "stop". Without super the program does not wait for a "stop" to terminate
% and are instead terminated directly after all processes is started
init(Port, N) ->
    case gen_tcp:listen(Port, [list, {active, false}, {reuseaddr, true}]) of
	{ok, Listen} -> 
	    handlers(Listen, N),
	    super();
	{error, Error} -> 
	    io:format("rudy: initialization failed: ~w~n", [Error]),
	    error
    end.

super() ->
    receive
	stop ->
		ok
    end.

handlers(Listen, N) ->
    case N of
	0 ->
	    ok;
	N ->
	    spawn(fun() -> handler(Listen, N) end),
	    handlers(Listen, N-1)
    end.
			       


handler(Listen, I) ->
    %%io:format("rudy: waiting for request~n", []),
    case gen_tcp:accept(Listen) of
 	{ok, Client} ->
	%    io:format("rudy ~w: received request~n", [I]),
            request(Client),
	    handler(Listen, I);
 	{error, Error} ->
	    io:format("rudy: error ~w~n", [Error]),
	    error
     end.

request(Client) ->
    %%io:format("rudy: reading request from ~w~n", [Client]),
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
	{ok, Str} ->
	    %%io:format("rudy: parsing request~n", []),
	    Request = http:parse_request(Str),
	    %%io:format("rudy: sending reply~n", []),
	    Response = reply(Request),
	    gen_tcp:send(Client, Response),
	    %%io:format("rudy: closing socket ~w~n",[Client]),
	    gen_tcp:close(Client);
	{error, Error} ->
   	    io:format("rudy: error: ~w~n", [Error]),
	    ok
    end.

    


reply({{get, URI, _}, _, _}) -> 
    timer:sleep(40),
    http:ok("<html><head><title>Rudy</title></head><body>This is a test.<br/>" ++ URI ++ "</body></html>").

