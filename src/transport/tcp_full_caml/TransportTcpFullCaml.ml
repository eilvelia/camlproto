open! Base

module Math = Math.Make(PlatformCaml)
module Crypto = Math.Crypto

let src = Logs.Src.create "camlproto.transport.tcp_full.caml"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  input : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
  mutable seq_no_send : int;
  mutable seq_no_receive : int;
}

let invalid_checksum ~exp ~got =
  Printf.sprintf "Invalid checksum: expected %ld, got %ld" exp got

let incorrect_seq_no ~exp ~got =
  Printf.sprintf "Received incorrect seqno: expected %d, got %d" exp got

exception Error of string

(* tcp_full packet structure:
 * +----+----+----...----+----+
 * |len.|seq.|  payload  |crc.|
 * +----+----+----...----+----+
 *)

let create (address, port) =
  let%lwt addresses = Lwt_unix.getaddrinfo address port [] in
  let addr = (List.hd_exn addresses).ai_addr in
  Log.info (fun m -> m "Connecting to %s:%s" address port);
  let%lwt input, output = Lwt_io.open_connection addr in
  Lwt.return { input; output; seq_no_send = 0; seq_no_receive = 0 }

let send t payload =
  let payload_len = Cstruct.length payload in
  Log.debug (fun m ->
    m "tcp_full send (len %d) [seq_no %d]" payload_len t.seq_no_send);

  let len = 12 + payload_len in
  (* TODO: Reuse cstruct or use bytes? *)
  let buf = Cstruct.create_unsafe len in
  Cstruct.LE.set_uint32 buf 0 (Int.to_int32_trunc len);
  Cstruct.LE.set_uint32 buf 4 (Int.to_int32_trunc t.seq_no_send);
  Cstruct.blit payload 0 buf 8 payload_len;
  let checksum = Crypto.crc32 @@ Cstruct.sub buf 0 (len - 4) in
  Cstruct.LE.set_uint32 buf (len - 4) checksum;

  (* Caml.print_endline "To server:"; Cstruct.hexdump buf; *)

  let buf_bigstring = Cstruct.to_bigarray buf in
  let%lwt written = Lwt_io.write_from_bigstring t.output buf_bigstring 0 len in
  Log.debug (fun m -> m "tcp_full written %d bytes" written);

  t.seq_no_send <- t.seq_no_send + 1;

  Lwt.return_unit

let receive t =
  let%lwt len = Lwt_io.LE.read_int t.input in
  let%lwt seq_no = Lwt_io.LE.read_int t.input in

  Log.debug (fun m -> m "tcp_full received [len %d] [seq_no %d]" len seq_no);

  if t.seq_no_receive <> seq_no then
    raise @@ Error (incorrect_seq_no ~exp:t.seq_no_receive ~got:seq_no);

  let buf = Cstruct.create_unsafe (len - 4) (* without checksum *) in
  Cstruct.LE.set_uint32 buf 0 (Int.to_int32_trunc len);
  Cstruct.LE.set_uint32 buf 4 (Int.to_int32_trunc seq_no);
  let body = Cstruct.shift buf 8 in
  let body_bigstring = Cstruct.to_bigarray body in
  let given_body_len = len - 12 in
  let%lwt () =
    Lwt_io.read_into_exactly_bigstring t.input body_bigstring 0 given_body_len
  in

  let calc_checksum = Crypto.crc32 buf in

  let%lwt given_checksum = Lwt_io.LE.read_int32 t.input in

  if Int32.(calc_checksum <> given_checksum) then
    raise @@ Error (invalid_checksum ~exp:calc_checksum ~got:given_checksum);

  t.seq_no_receive <- t.seq_no_receive + 1;

  Lwt.return body
