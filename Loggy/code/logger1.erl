-module(logger1).
-export([start/1,stop/1]).
% if file had name logger I got error
%=ERROR REPORT==== 21-Sep-2020::23:07:52.297839 ===
%Can't load module 'logger' that resides in sticky dir


% the logger simply accepts events and prints them on the screen(prepared to receive time stamps)
%The logger is given a list of nodes that will send it messages (will use later)

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(_) ->
    loop().

loop() ->
    receive
        {log,From,Time,Msg} ->
            log(From,Time,Msg),
            loop();
        stop ->
            ok
end.

log(From,Time,Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
