open! Base

module Transport: Types.S = struct
  type t = {
    input: Lwt_io.input_channel;
    output: Lwt_io.output_channel;
  }

  exception Error of string

  let create (address, port): t Lwt.t =
    let%lwt addresses = Lwt_unix.getaddrinfo address port [] in
    let addr = (List.hd_exn addresses).ai_addr in
    Log.info (fun m -> m "tcp_abridged: Connecting to %s:%s" address port);
    let%lwt (input, output) = Lwt_io.open_connection addr in
    let%lwt () = Lwt_io.write_char output '\239' in (* 239 = 0xef *)
    Lwt.return { input; output }

  let send t packet =
    Log.debug (fun m -> m "tcp_abridged send (len %d)" (Cstruct.len packet));
    (* Cstruct.hexdump packet; *)

    let data_len = Cstruct.len packet / 4 in
    let header_len = if data_len >= 127 then 4 else 1 in
    let len = header_len + Cstruct.len packet in

    let buf = Cstruct.create_unsafe len in
    if header_len = 4 then begin
      Cstruct.set_char buf 0 '\127';
      Cstruct.set_char buf 1 (Char.unsafe_of_int (0xff land data_len));
      Cstruct.set_char buf 2 (Char.unsafe_of_int (0xff land (data_len asr 8)));
      Cstruct.set_char buf 3 (Char.unsafe_of_int (0xff land (data_len asr 16)));
    end else begin
      Cstruct.set_uint8 buf 0 data_len
    end;
    Cstruct.blit packet 0 buf header_len (Cstruct.len packet);

    (* Caml.print_endline "To server:";
    Cstruct.hexdump buf; *)

    let%lwt _ =
      Lwt_io.write_from t.output (Cstruct.to_bytes buf) 0 len in

    Lwt.return ()

  let receive t =
    let%lwt len = Lwt_io.read_char t.input in
    let len = Char.to_int len in
    let%lwt len =
      if len >= 127 then begin
        let%lwt new_len_str = Lwt_io.read ~count:3 t.input in
        let new_len_cs = Cstruct.create_unsafe 4 in
        Cstruct.blit_from_string new_len_str 0 new_len_cs 0 3;
        Cstruct.set_char new_len_cs 3 '\000';
        Cstruct.LE.get_uint32 new_len_cs 0
          |> Int.of_int32_exn
          |> Lwt.return
      end else Lwt.return len
    in
    let len = len lsl 2 in

    Log.debug (fun m -> m "tcp_abridged received [len %d]" len);

    let%lwt body_str = Lwt_io.read ~count:len t.input in
    let body = Cstruct.of_string body_str in

    (* Logger.dump "From server" body; *)

    Lwt.return body
end
