-module(test_ubt).
-compile(export_all).

run() ->
   spawn(fun() -> server(3561) end),
   client(3561).
   
server(Port) ->
    {ok, Socket} = ubt_socket:listen(Port),
    {ok, Socket} = ubt_socket:accept(Socket),
    ubt_socket:close(Socket).

client(Port) ->
    ubt_socket:connect("127.0.0.1", Port).




