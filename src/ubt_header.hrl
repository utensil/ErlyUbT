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
        r_port = none
    }
).
