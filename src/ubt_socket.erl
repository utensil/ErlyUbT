-module(ubt_socket).
-export([connect/3, listen/2, accept/1]).
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
    {ok, LocalPort} = inet:port(Socket),
    % Shake hand step 1
    ConnectHeader = #ubt_header{
        syn = 1,
        src_port = LocalPort,
        dst_port = Port,
        seq_no = 0
    },
    ConnectPacket = ubt_packer:pack({ConnectHeader, <<>>}),
    ok = gen_udp:send(Socket, Address , Port, ConnectPacket),
    receive
        {udp, Socket,  _, _, Packet} ->
            { Header, Rest } = ubt_packer:unpack(Packet),
            if Header#ubt_header.syn == 1 ->
                    % TODO: set_things_up()
                    % Shake hand step 3
                    {ok, LocalPort} = inet:port(Socket),
                    EstablishedHeader = #ubt_header{
                        syn = 0,
                        ack = 1,
                        src_port = LocalPort,
                        dst_port = Port,
                        seq_no = 1,
                        ack_no = Header#ubt_header.seq_no + 1
                    },
                    io:format("~p~n", [EstablishedHeader]),
                    EstablishedPacket = ubt_packer:pack({EstablishedHeader, <<>>}),
                    ok = gen_udp:send(Socket, Address, Port, EstablishedPacket),
                    {ok, Socket}
            end
    end.

%%
%% Listen on a tcp port
%%
listen(Port, Opts) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    {ok, Socket}.

%%
%% Generic tcp accept
%%
accept(Socket) ->
    receive
        {udp, Socket, Address, Port, Packet} ->
            io:format("~p~n", [{udp, Socket, Address, Port, Packet}]),
            { Header, Rest } = ubt_packer:unpack(Packet),
            % Shake hand step 2
            % set_things_up()
            {ok, LocalPort} = inet:port(Socket),
            ConfirmConnectHeader = #ubt_header{
                syn = 1,
                ack = 1,
                src_port = LocalPort,
                dst_port = Port,
                seq_no = 0,
                ack_no = Header#ubt_header.seq_no + 1
            },
            io:format("~p~n", [ConfirmConnectHeader]),
            ConfirmConnectPacket = ubt_packer:pack({ConfirmConnectHeader, <<>>}),
            ok = gen_udp:send(Socket, Address, Port, ConfirmConnectPacket),
            receive
                {udp, Socket, Address, Port, Packet} ->
                    { Header, Rest } = ubt_packer:unpack(Packet),
                    if ((Header#ubt_header.syn == 0)
                        and (Header#ubt_header.ack == 1))
                        and (Header#ubt_header.rst == 0) ->
                        {ok, Socket}
                    end
             end
    end.

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