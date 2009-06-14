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
                    Socket = #ubt_struct{
                            l_sock = LSocket,
                            r_addr = RAddress,
                            r_port = RPort
                            },
                            
                    NewSocket = spawn_established_loop(Socket),
                    {ok, NewSocket}
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
                            NewSocket = spawn_established_loop(
                                        Socket#ubt_struct{
                                        r_addr = Address,
                                        r_port = Port
                                    }),
                            {ok, NewSocket}
                    end
             end
    end.

%%
%% Close
%%
close(Socket) ->
    io:format("Begin closing Socket: ~p~n", [Socket]),
    if
        Socket#ubt_struct.already_closing == true ->
            io:format("Already closing...~n"),
            void;
        true ->
            io:format("Closing...~n"),
            Socket#ubt_struct.bg_pid ! { self(), active_close }
    end,
    BgPid = Socket#ubt_struct.bg_pid,
    receive
        {BgPid, ubt_closed} ->
            io:format("really closed"),
            gen_tcp:close(Socket#ubt_struct.l_sock)
    end.

established_loop(Socket) ->
    {LSocket, Address, Port} =
            { Socket#ubt_struct.l_sock,
              Socket#ubt_struct.r_addr,
              Socket#ubt_struct.r_port },
    BgPid = Socket#ubt_struct.bg_pid,
    receive
        test ->
            io:format("Don't test me, I'm fine: ~p~n", [Socket]);
        {udp, LSocket, Address, Port, Packet} ->
            io:format("unprocessed UDP packet: ~p~n",
                        [{udp, LSocket, Address, Port, Packet}]),
            { Header, _Rest } = ubt_packer:unpack(Packet),
            % first we should process Rest(not imped)
            % then
            if
                Header#ubt_header.fin == 1 ->
                    closing(
                        Socket#ubt_struct{rcv_seq = Header#ubt_header.seq_no +1},
                        passive_react);
                true ->
                    established_loop(Socket)                
            end;
        {update_socket, NewSocket} ->
            established_loop(NewSocket);
        {BgPid, active_close} ->
            closing(Socket, active_fin)
    end.
    
closing(Socket, State) ->
    {LSocket, Address, Port} =
    { Socket#ubt_struct.l_sock,
      Socket#ubt_struct.r_addr,
      Socket#ubt_struct.r_port },
    {ok, LocalPort} = inet:port(LSocket),
    case State of
        passive_react ->
            io:format("Fin 1 received. ~n"),
            AckFinHeader = #ubt_header{
                ack = 1,
                src_port = LocalPort,
                dst_port = Port,
                seq_no = Socket#ubt_struct.snd_seq,
                ack_no = Socket#ubt_struct.rcv_seq + 1
            },
            io:format("Fin 1 Ack sent: ~p~n", [AckFinHeader]),
            AckFinPacket = ubt_packer:pack({AckFinHeader, <<>>}),
            gen_udp:send(LSocket, Address, Port, AckFinPacket),
            Fin2Header = #ubt_header{
                fin = 1,
                src_port = LocalPort,
                dst_port = Port,
                seq_no = Socket#ubt_struct.snd_seq + 1,
                ack_no = Socket#ubt_struct.rcv_seq + 2
            },
            io:format("Fin 2 sent: ~p~n", [Fin2Header]),
            Fin2Packet = ubt_packer:pack({Fin2Header, <<>>}),
            gen_udp:send(LSocket, Address, Port, Fin2Packet),
            closing(Socket, passive_wait);
       passive_wait ->
           % wait fin 2 ack
            receive
                {udp, LSocket, Address, Port, Packet} ->
                    { Header, _Rest } = ubt_packer:unpack(Packet),
                    if 
                        Header#ubt_header.ack == 1 ->
                            io:format("Fin 2 Ack received, connection closed passively.~n"),
                            Socket#ubt_struct.fg_pid ! { self(), ubt_closed}
                    end
            end;
       active_fin ->
            Fin1Header = #ubt_header{
                fin = 1,
                src_port = LocalPort,
                dst_port = Port,
                seq_no = Socket#ubt_struct.snd_seq,
                ack_no = Socket#ubt_struct.rcv_seq + 1
            },
            io:format("Fin 1 sent: ~p~n", [Fin1Header]),
            Fin1Packet = ubt_packer:pack({Fin1Header, <<>>}),
            gen_udp:send(LSocket, Address, Port, Fin1Packet),
            closing(Socket#ubt_struct{already_closing = true}, active_wait);
       active_wait ->
            % wait fin 1 ack.
            receive
                {udp, LSocket, Address, Port, Packet} ->
                    { Header, _Rest } = ubt_packer:unpack(Packet),
                    if 
                        Header#ubt_header.ack == 1 ->
                            io:format("Fin 1 Ack received.~n"),
                            Socket#ubt_struct.fg_pid ! { self(), ubt_closed}
                    end
            end,
           % wait fin 2
           receive
                {udp, LSocket, Address, Port, Packet2} ->
                    { Header2, _Rest2 } = ubt_packer:unpack(Packet2),
                    if 
                        Header2#ubt_header.fin == 1 ->
                            io:format("Fin 2 received, connection closed actively~n"),
                            gen_tcp:close(LSocket)
                    end
            end
       end.

    
spawn_established_loop(Socket) ->
    %create a background process looping in the established status
    BgPid = spawn(fun() -> established_loop(Socket) end),
    ok = gen_udp:controlling_process(Socket#ubt_struct.l_sock, BgPid),
    NewSocket = Socket#ubt_struct{ fg_pid = self(), bg_pid = BgPid },
    BgPid ! {update_socket, NewSocket},
    NewSocket.
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