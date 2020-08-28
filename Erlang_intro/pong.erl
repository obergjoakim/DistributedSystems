-module(pong).
-export([res/0]).


res() ->
    receive
        {"ping", Pid} ->
            Pid ! "pong!"
end.




