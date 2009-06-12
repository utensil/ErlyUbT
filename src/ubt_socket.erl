-module(ubt_socket).
-export([connect/2, listen/1, accept/1, close/1]).
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
connect(Address, Port) ->
    {ok, LSocket} = gen_udp:open(0, [binary]),
    {ok, LocalPort} = inet:port(LSocket),
    % Shake hand step 1
    ConnectHeader = #ubt_header{
        syn = 1,
        src_port = LocalPort,
        dst_port = Port,
        seq_no = 0
    },
    io:format("Step 1:~p~n", [ConnectHeader]),
    ConnectPacket = ubt_packer:pack({ConnectHeader, <<>>}),
    ok = gen_udp:send(LSocket, Address , Port, ConnectPacket),
    receive
        {udp, LSocket,  RAddress, RPort, Packet} ->
            io:format("Step 2 ack: ~p~n", [{udp, LSocket, Packet}]),
            { Header, _Rest } = ubt_packer:unpack(Packet),
            if Header#ubt_header.syn == 1 ->
                    % Shake hand step 3
                    {ok, LocalPort} = inet:port(LSocket),
                    EstablishedHeader = #ubt_header{
                        syn = 0,
                        ack = 1,
                        src_port = LocalPort,
                        dst_port = Port,
                        seq_no = 1,
                        ack_no = Header#ubt_header.seq_no + 1
                    },
                    io:format("Step 3:~p~n", [EstablishedHeader]),
                    EstablishedPacket = ubt_packer:pack({EstablishedHeader, <<>>}),
                    ok = gen_udp:send(LSocket, Address, Port, EstablishedPacket),
                    {ok, #ubt_struct{
                            l_sock = LSocket,
                            r_addr = RAddress,
                            r_port = RPort
                            }}
            end
    end.

%%
%% Listen on a tcp port
%%
listen(Port) ->
    {ok, LSocket} = gen_udp:open(Port, [binary, {reuseaddr, true}]),
    {ok, #ubt_struct{
                l_sock = LSocket
                }}.

%%
%% Generic tcp accept
%%
accept(Socket) ->
    LSocket = Socket#ubt_struct.l_sock,
    receive
        {udp, LSocket, Address, Port, Packet} ->
            io:format("Step 1 ack: ~p~n", [{udp, LSocket, Address, Port, Packet}]),
            { Header, _Rest } = ubt_packer:unpack(Packet),
            % Shake hand step 2
            {ok, LocalPort} = inet:port(LSocket),
            ConfirmConnectHeader = #ubt_header{
                syn = 1,
                ack = 1,
                src_port = LocalPort,
                dst_port = Port,
                seq_no = 0,
                ack_no = Header#ubt_header.seq_no + 1
            },
            io:format("Step 2: ~p~n", [ConfirmConnectHeader]),
            ConfirmConnectPacket = ubt_packer:pack({ConfirmConnectHeader, <<>>}),
            ok = gen_udp:send(LSocket, Address, Port, ConfirmConnectPacket),
            receive
                {udp, LSocket, Address, Port, Packet3} ->
                    io:format("Step 3 ack: ~p~n", [{udp, LSocket, Address, Port, Packet3}]),
                    { Header3, _Rest3 } = ubt_packer:unpack(Packet3),
                    if ((Header3#ubt_header.syn == 0)
                        and (Header3#ubt_header.ack == 1))
                        and (Header3#ubt_header.rst == 0) ->
                        {ok, Socket#ubt_struct{
                                    r_addr = Address,
                                    r_port = Port
                                }}
                    end
             end
    end.

%%
%% Close
%%
close(Socket) ->
    gen_udp:close(Socket#ubt_struct.l_sock).

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