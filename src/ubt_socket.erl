-module(ubt_socket).
-export([connect/3, listen/2, accept/1, close/1]).
-export([send/2, recv/2]).
%-export([connect/3, connect/4, listen/2, accept/1, accept/2,
%	 shutdown/2, close/1]).
%-export([send/2, recv/2, recv/3, unrecv/2]).
%-export([controlling_process/2]).
%-export([fdopen/2]).

%%
%% Connect a socket
%%
connect(Address, Port, Opts) ->
    connect(Address,Port,Opts,infinity).

connect(Address, Port, Opts, Time) ->
    Timer = inet:start_timer(Time),
    Res = (catch connect1(Address,Port,Opts,Timer)),
    inet:stop_timer(Timer),
    case Res of
	{ok,S} -> {ok,S};
	{error, einval} -> exit(badarg);
	{'EXIT',Reason} -> exit(Reason);
	Error -> Error
    end.

%%
%% Listen on a tcp port
%%
listen(Port, Opts) ->
    Mod = mod(Opts),
    case Mod:getserv(Port) of
	{ok,TP} ->
	    Mod:listen(TP, Opts);
	{error,einval} ->
	    exit(badarg);
	Other -> Other
    end.
    
%%
%% Generic tcp accept
%%
accept(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:accept(S);
	Error ->
	    Error
    end.

%%
%% Close
%%
close(S) ->
    inet:tcp_close(S).

%%
%% Send
%%
send(S, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:send(S, Packet);
	Error ->
	    Error
    end.

%%
%% Receive data from a socket (passive mode)
%%
recv(S, Length) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Length);
	Error ->
	    Error
    end.