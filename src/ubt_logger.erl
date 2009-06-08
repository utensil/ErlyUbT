-module(ubt_logger).
-compile(export_all).

init() ->
    register(ubt_log_server, spawn(fun() -> server() end)).

server() ->
    receive
       {exit, Reason} ->
           exit(Reason);
       {log, Pid, Log} ->
           io:format("Process ~p: ~p~n", [Pid, Log]),
           server()
    end.

log(Log) when not whereis(ubt_log_server) == undefined ->
    ubt_log_server ! {log, self(), Log},
    ok.

exit() ->
    ubt_log_server ! {exit, ok},
    ok.
