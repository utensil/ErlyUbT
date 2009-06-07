-module(ubt_packer).
-export([unpack/1, pack/1]).

-define(N_32, 32/unsigned-big-integer).
-define(N_16, 16/unsigned-big-integer).
-define(N_8, 8/unsigned-big-integer).

-record(udp_header,
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

unpack(<<SrcPort : ?N_16,
      DstPort : ?N_16,
      SeqNo : ?N_32,
      AckNo : ?N_32,
      DataOffset :4,
      Reserved :6,
      %cflag begin
      Urg :1,
      Syn :1,
      Ack :1,
      Rst :1,
      Psh :1,
      Fin :1,
      %cflag end
      Wndw :?N_16,
      Checksum :?N_16,
      Urg_ptr :?N_16,
      Rest/binary>>) ->
      Header = #udp_header{
            src_port = SrcPort,
            dst_port = DstPort,
            seq_no = SeqNo,
            ack_no = AckNo,
            syn = Syn,
            ack = Ack,
            rst = Rst,
            fin = Fin,
            wndw = Wndw
        },
      { Header, Rest }.

pack({{ Header, Rest }}) ->
    {SrcPort, DstPort, SeqNo, AckNo, Syn, Ack, Rst, Fin, Wndw}
        = Header,
    { DataOffset, Reserved, Urg, Psh, Checksum, Urg_ptr}
        = { 0, 0, 0, 0, 0, 0},
    <<SrcPort : ?N_16,
      DstPort : ?N_16,
      SeqNo : ?N_32,
      AckNo : ?N_32,
      DataOffset :4,
      Reserved :6,
      %cflag begin
      Urg :1,
      Syn :1,
      Ack :1,
      Rst :1,
      Psh :1,
      Fin :1,
      %cflag end
      Wndw :?N_16,
      Checksum :?N_16,
      Urg_ptr :?N_16,
      Rest/binary>>.

     