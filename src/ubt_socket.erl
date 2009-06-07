-module(ubt_socket).
-export([connect/3]).
%-export([connect/3, listen/2, accept/1, close/1]).
%-export([send/2, recv/2]).
-include("ubt_header.hrl").

%-export([connect/3, connect/4, listen/2, accept/1, accept/2,
%	 shutdown/2, close/1]).
%-export([send/2, recv/2, recv/3, unrecv/2]).
%-export([controlling_process/2]).
%-export([fdopen/2]).

%%
%% Connect a socket
%%
connect(Address, Port, Opts) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    OtherInfo = 0,
    {ok, LocalPort} = inet:port(Socket),
    ConnectHeader = #ubt_header{
        syn = 1,
        src_port = LocalPort,
        dst_port = Port,
        seq_no = 0 %TODO gen_isn()
    },
    io:format("~p~n", [ConnectHeader]),
    ConnectPacket = ubt_packer:pack({ConnectHeader, <<>>}),
    ok = gen_udp:send(Socket, Address , Port, ConnectPacket),
    receive
        {udp, Socket, _, _, Packet} ->
            case ubt_packer:unpack(Packet) of
                { Header, Rest } when Header#ubt_header.syn == 1 ->
                    EstablishedHeader = #ubt_header{
                        syn = 0,
                        ack = 1,
                        src_port = inet:port(Socket),
                        dst_port = Port,
                        seq_no = 1, %TODO gen_isn()
                        ack_no = Header#ubt_header.seq_no + 1
                    },
                    EstablishedPacket = ubt_packer:pack({EstablishedHeader, <<>>}),
                    ok = gen_udp:send(Socket, Address , Port, EstablishedPacket),
                    {ok, Socket, OtherInfo};
                Error ->
                    {error, Socket, OtherInfo}
            end
    end.

%%%
%%% Listen on a tcp port
%%%
%listen(Port, Opts) ->
%    Mod = mod(Opts),
%    case Mod:getserv(Port) of
%	{ok,TP} ->
%	    Mod:listen(TP, Opts);
%	{error,einval} ->
%	    exit(badarg);
%	Other -> Other
%    end.
%
%%%
%%% Generic tcp accept
%%%
%accept(S) ->
%    case inet_db:lookup_socket(S) of
%	{ok, Mod} ->
%	    Mod:accept(S);
%	Error ->
%	    Error
%    end.
%
%%%
%%% Close
%%%
%close(S) ->
%    inet:tcp_close(S).
%
%%%
%%% Send
%%%
%send(S, Packet) when is_port(S) ->
%    case inet_db:lookup_socket(S) of
%	{ok, Mod} ->
%	    Mod:send(S, Packet);
%	Error ->
%	    Error
%    end.
%
%%%
%%% Receive data from a socket (passive mode)
%%%
%recv(S, Length) when is_port(S) ->
%    case inet_db:lookup_socket(S) of
%	{ok, Mod} ->
%	    Mod:recv(S, Length);
%	Error ->
%	    Error
%    end.