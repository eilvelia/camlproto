open! Base
open Math

module TransportTcpFull: Types.MTProtoTransport = struct
  type t = {
    input: Lwt_io.input_channel;
    output: Lwt_io.output_channel;
    mutable seq_no: int32;
  }

  exception Error of string

  let create (address, port): t Lwt.t =
    let%lwt addresses = Lwt_unix.getaddrinfo address port [] in
    let addr = (List.hd_exn addresses).ai_addr in
    Log.info (fun m -> m "tcp_full: Connecting to %s:%s" address port);
    let%lwt (input, output) = Lwt_io.open_connection addr in
    Lwt.return { input; output; seq_no = 0l }

  let send t packet =
    Log.debug (fun m -> m "tcp_full send [seq_no %ld]" t.seq_no);
    (* Cstruct.hexdump packet; *)

    let len = 4 * 3 + Cstruct.len packet in
    let buf = Cstruct.create_unsafe len in
    Cstruct.LE.set_uint32 buf 0 (Int.to_int32_exn len);
    Cstruct.LE.set_uint32 buf 4 t.seq_no;
    Cstruct.blit packet 0 buf 8 (Cstruct.len packet);
    let checksum = Crypto.crc32 (Cstruct.sub buf 0 (len - 4)) in
    Cstruct.LE.set_uint32 buf (len - 4) checksum;

    (* Caml.print_endline "To server:";
    Cstruct.hexdump buf; *)

    let%lwt _ =
      Lwt_io.write_from t.output (Cstruct.to_bytes buf) 0 (Cstruct.len buf) in

    Int32.(t.seq_no <- t.seq_no + 1l);

    Lwt.return ()

  let receive t =
    let%lwt len_int32 = Lwt_io.LE.read_int32 t.input in
    Log.debug (fun m -> m "tcp_full received [len %ld]" len_int32);
    let len = Int.of_int32_exn len_int32 in
    let%lwt seq_no = Lwt_io.LE.read_int32 t.input in

    let given_body_len = len - 12 in
    let body_bytes = Bytes.create given_body_len in
    let%lwt () = Lwt_io.read_into_exactly t.input body_bytes 0 given_body_len in
    let body = Cstruct.of_bytes body_bytes in (* TODO: inefficient *)
    let body_len = Cstruct.len body in

    (* if body_len < given_body_len then begin
      Cstruct.hexdump body;
      raise @@ Error (Printf.sprintf
        "Invalid len: actual %d | given %d" body_len given_body_len)
    end; *)

    let checksum_bytes = Cstruct.create_unsafe (8 + body_len) in
    Cstruct.LE.set_uint32 checksum_bytes 0 len_int32;
    Cstruct.LE.set_uint32 checksum_bytes 4 seq_no;
    Cstruct.blit body 0 checksum_bytes 8 body_len;
    let calc_checksum = Crypto.crc32 checksum_bytes in

    let%lwt given_checksum = Lwt_io.LE.read_int32 t.input in

    if Int32.(calc_checksum <> given_checksum) then
      raise @@ Error "checksums are not equal";

    (* if Int32.(seq_no + 1l <> t.seq_no) then
      raise @@ Error (Printf.sprintf
        "Incorrect sequence number: cl %ld | sv %ld" t.seq_no seq_no); *)

    Lwt.return body
end
