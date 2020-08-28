-module(pingpong).
-export([ping/0]).

ping() ->
    {pong, 'foo@192.168.10.226'} ! {"ping", self()},
    wait().

wait() ->
    receive
        X -> io:format("Got ping, sending: ~s~n", [X])
    end.

pong() ->
    receive
        {_Msg, Pid} ->
            Pid ! "pong!"
end.