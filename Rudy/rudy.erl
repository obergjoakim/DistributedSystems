-module(rudy).
-export([start/1, stop/0]).

start(Port) -> 
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").

init(Port) -> 
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),                    
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            error
end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);

        {error, Error} -> 
            error
end.

request(Client) -> 
    Recv = gen_tcp:recv(Client,0),
    case Recv of
        {ok, Str} ->
            case doubleCRLF(Str) of
                {ok} ->
                    Request = http:parse_request(Str),
                    Response = reply(Request),
                    gen_tcp:send(Client, Response);
                {no} ->
                    request(Client)
        end;

        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
end,
gen_tcp:close(Client).

doubleCRLF([]) -> {no};
doubleCRLF([13,10,13,10| _ ]) -> {ok};
doubleCRLF([_|Rest]) -> doubleCRLF(Rest).



reply({{get, URI, _},_,Body}) ->
    timer:sleep(40),
    http:ok(Body).