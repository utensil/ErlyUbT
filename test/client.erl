-module(client).
-export([client/0]).

client() ->
    {ok, Sock} = gen_tcp:connect("localhost", 5678, [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).