-define(N_32, 32/unsigned-big-integer).
-define(N_16, 16/unsigned-big-integer).
-define(N_8, 8/unsigned-big-integer).

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
