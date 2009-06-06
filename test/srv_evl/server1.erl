-module(server1).
-export([start/2, rpc/2]).

% look to functions after Mod:, they are the interface of the server.
% init() is suppose to new a data structure to store the state of the server.
% handle()s are callbacks to match requests.

start(Name, Mod) ->
    register(Name,
        spawn(fun() ->
            loop(Name, Mod, Mod:init())
        end)).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, Response} -> Response
    end.

loop(Name, Mod, State) ->
    receive
        {From, Request} ->
            {Response, State1} = Mod:handle(Request, State),
            From ! {Name, Response},
            loop(Name, Mod, State1)
    end.