-define(N_32, 32/unsigned-big-integer).
-define(N_16, 16/unsigned-big-integer).
-define(N_8, 8/unsigned-big-integer).
-define(N_6, 6/unsigned-big-integer).
-define(N_4, 4/unsigned-big-integer).
-define(N_1, 1/unsigned-big-integer).

-record(ubt_header,
    {
        src_port = 0,
        dst_port = 0,
        seq_no = 0,
        ack_no = 0,
        syn = 0,
        ack = 0,
        rst = 0,
        fin = 0,
        wndw = 0
    }).

-record(ubt_struct,
    {
        l_sock,
        r_addr = none,
        r_port = none,
        timout = {}, %suppose to be a timout_struct
        %%timestamp is for each MSS
        cong_win = 0,
        %%buff, %array? no, use binary match
        %% buffer size...
        win = 4096,
        % snd_buf  snd_una          snd_nxt
        %  |            |                |           |
        %  |  acked     | sent, unacked  |           |
        %  |            |             win            | buffer_size
        snd_buf,
        snd_una, % snd_win_left
        snd_nxt, % snd_una + win = snd_win_right
        % rcv_buf  rcv_wup          snd_nxt
        %  |             |                |           |
        %  | rcvd&acked  | rcvd, unacked  |           |
        %  |             |             win            | buffer_size
        rcv_buf,
        rcv_wup,  % last window update
        rcv_nxt

    }
).

% a block is created only after it's sent, and it will be
-record(ubt_block,
    {
        % we don't worry about the beginning, if end_seq + 1 < ack_no, this
        % block is dequeued, and timeout are calculated
        end_seq, 
        snd_timestamp
    }).
