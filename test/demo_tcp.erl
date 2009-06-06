-module(demo_tcp).
-export([start_server/0, server/1, client/1]).

start_server() ->
    spawn(fun() -> server(3561) end).

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0},
                                        {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = do_recv(Sock, []),
    ok = gen_tcp:close(Sock),
    ok = gen_tcp:close(LSock),
    ok = file:write_file("received.swf", Bin),
    Bin.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

client(Port) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    {ok, Data} = file:read_file("restproject.swf"),
    ok = gen_tcp:send(Sock, Data),
    ok = gen_tcp:close(Sock).
