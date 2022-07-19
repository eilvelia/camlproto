open! Base

let src = Logs.Src.create "camlproto.transport.tcp_abridged.caml"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  input : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
}

exception Error of string

(* tcp_abridged packet structure:
 * (https://core.telegram.org/mtproto/mtproto-transports#abridged)
 * +-+----...----+
 * |l|  payload  |
 * +-+----...----+
 * OR
 * +-+---+----...----+
 * |h|len|  payload  +
 * +-+---+----...----+
 * h is 127, len is length(payload) / 4.
 * The second structure is used if len > 127.
 * The client must send 0xEF as the first byte.
 *)

let create (address, port) =
  let%lwt addresses = Lwt_unix.getaddrinfo address port [] in
  let addr = (List.hd_exn addresses).ai_addr in
  Log.info (fun m -> m "Connecting to %s:%s" address port);
  let%lwt input, output = Lwt_io.open_connection addr in
  let%lwt () = Lwt_io.write_char output '\239' (* 239 = 0xef *) in
  Lwt.return { input; output }

let send t payload =
  let payload_len = Cstruct.length payload in
  Log.debug (fun m -> m "tcp_abridged send (len %d)" payload_len);

  assert (payload_len land 0b11 = 0 && payload_len <= 0xFF_FF_FF);

  let data_len = payload_len lsr 2 in
  let header_len = if data_len >= 127 then 4 else 1 in
  let len = header_len + payload_len in

  let buf = Cstruct.create_unsafe len in
  if data_len >= 127 then begin
    Cstruct.set_char buf 0 '\127';
    Cstruct.set_char buf 1 (Char.unsafe_of_int (0xff land data_len));
    Cstruct.set_char buf 2 (Char.unsafe_of_int (0xff land (data_len asr 8)));
    Cstruct.set_char buf 3 (Char.unsafe_of_int (0xff land (data_len asr 16)));
  end else begin
    Cstruct.set_char buf 0 (Char.unsafe_of_int data_len)
  end;
  Cstruct.blit payload 0 buf header_len payload_len;

  (* Caml.print_endline "To server:"; Cstruct.hexdump buf; *)

  let buf_bigstring = buf.buffer in
  let%lwt written = Lwt_io.write_from_bigstring t.output buf_bigstring 0 len in
  Log.debug (fun m -> m "tcp_abridged written %d bytes" written);

  Lwt.return_unit

let receive t =
  let%lwt one_byte_header = Lwt_io.read_char t.input in
  let one_byte_header = Char.to_int one_byte_header in
  let%lwt body_len =
    if one_byte_header >= 127 then begin
      let len_bytes = Bytes.create 4 in
      let%lwt () = Lwt_io.read_into_exactly t.input len_bytes 0 3 in
      Bytes.set len_bytes 3 '\000';
      Lwt.return @@ Int32.to_int_trunc
        (* JS' Base does not have get_int32 functions (yet?) *)
        @@ Caml.Bytes.get_int32_le len_bytes 0
    end else
      Lwt.return one_byte_header
  in
  let body_len = body_len * 4 in

  Log.debug (fun m -> m "tcp_abridged received [len %d]" body_len);

  let buf = Cstruct.create_unsafe body_len in
  let buf_bigstring = buf.buffer in
  let%lwt () = Lwt_io.read_into_exactly_bigstring t.input buf_bigstring 0 body_len in

  Lwt.return buf
