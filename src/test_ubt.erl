-module(test_ubt).
-compile(export_all).

run() ->
   spawn(fun() -> server(3561) end),
   client(3561).
   
server(Port) ->
    {ok, Socket} = ubt_socket:listen(Port),
    {ok, New_Socket} = ubt_socket:accept(Socket),
    ubt_socket:close(New_Socket).

client(Port) ->
    ubt_socket:connect("127.0.0.1", Port).

test_packet_out() ->
    ubt_socket:connect("202.116.64.9", 80).




