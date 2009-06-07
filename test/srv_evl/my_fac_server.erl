-module(my_fac_server).
-export([loop/0]).

loop() ->
    receive
        {From, {fac, N}} ->
            From ! {self(), fac(N)},
            loop();
        {become, Something} ->
            Something()
    end.

fac(0) -> 1;
fac(N) -> N * fac(N-1).

%15> c(server5).
%{ok,server5}
%16> c(my_fac_server).
%{ok,my_fac_server}
%17> Pid = server5:start().
%<0.71.0>
%18> Pid ! {become, fun my_fac_server:loop/0}.
%{become,#Fun<my_fac_server.loop.0>}
%19> server5:rpc(Pid, {fac, 30}).
%265252859812191058636308480000000
