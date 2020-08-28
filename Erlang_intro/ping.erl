-module(ping).
-export([start/0]).

start() ->
    {pong, 'foo@192.168.10.226'} ! {"pong", self()},
    wait().

wait() ->
    receive
        X -> io:format("From bar: ~s~n", [X])
    end.


