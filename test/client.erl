-module(client).
-compile(export_all).
%-export([client/0]).

client() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3561, [binary, {packet, 0}]),
    {ok, Data} = file:read_file("restproject.swf"),
    ok = gen_tcp:send(Sock, Data),
    ok = gen_tcp:close(Sock).