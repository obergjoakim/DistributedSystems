-module(startShells).
-export([start/3]).
% Created by Joakim Öberg 1/9 -2020

start(_Host, _Port, 0) ->
    ok;

start(Host, Port, N) ->
    spawn(fun() -> shell(Host, Port, N) end),
    start(Host, Port, N-1).

shell(Host, Port, _N) -> 
    %Start = erlang:system_time(micro_seconds),
    test:bench(Host, Port).
   % Finish = erlang:system_time(micro_seconds),
  %  io:format("shell: ~w time: ~w~n", [N,Finish - Start]). 
