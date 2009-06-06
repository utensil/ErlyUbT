-module(demo_udp).
-export([start_server/0, server/1, client/1, stop_server/0]).

start_server() ->
    spawn(fun() -> server(3681) end).
%% The server
server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("server opened socket:~p~n" ,[Socket]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} = Msg ->
            io:format("server received:~p~n" ,[Msg]),
            case binary_to_term(Bin) of
                close ->
                    gen_udp:close(Socket);
                N ->
                    Fac = fac(N),
                    gen_udp:send(Socket, Host, Port, term_to_binary(Fac)),
                    loop(Socket)
            end
    end.

fac(0) -> 1;
fac(N) -> N * fac(N-1).
    
%% The client
client(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n" ,[Socket]),
    ok = gen_udp:send(Socket, "localhost" , 3681,
    term_to_binary(N)),
    Value = receive
                {udp, Socket, A, B, Bin} = Msg ->
                io:format("~p~n", [[A, B]]),
                io:format("client received:~p~n" ,[Msg]),
                binary_to_term(Bin)
            after 2000 ->
                0
            end,
    gen_udp:close(Socket),
    Value.
    
stop_server() ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, "localhost" , 3681, term_to_binary(close)).
    