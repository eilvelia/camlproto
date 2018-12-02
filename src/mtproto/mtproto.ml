open! Base
open Mtproto_transport
(* open Mtproto_misc *)
(* open Types *)
open Misc

module TLG = TLGen.MTProto
module Crypto = Math.Crypto
module Bigint = Math.Bigint

module Types = Types

(* module BaseMTProtoClient = struct end *)
(* module MTProtoV1Client = struct end *)
module MakeMTProtoV2Client (T: MTProtoTransport) = struct
  open TL.Types

  module RsaManager = Crypto.Rsa.RsaManager

  type request = Request
    : 'a Lwt.u * (module TLFunc with type ResultM.t = 'a)
    -> request

  type t = {
    transport: T.t;
    rsa: RsaManager.t;
    mutable time_offset: int64;
    mutable auth_key_tuple: (Cstruct.t * Cstruct.t) option; (** auth_key, key_id *)
    mutable server_salt: int64;
    mutable session_id: Cstruct.t; (** 8 bytes *)
    mutable seq_no: int32;
    mutable last_msg_id: int64;
    request_map: (int64, request) Hashtbl.t; (** (msg_id, request) *)
  }

  type encrypted_message = {
    (* server_salt: int64;
    session_id: int64; *)
    msg_id: int64;
    msg_seq_no: int32;
    data: Cstruct.t;
  }

  (* TODO: use sum types instead of strings *)
  exception MTPError of string

  let create () = (* TODO: RsaManager.t as argument; (ip, port) argument *)
    let%lwt transport = T.create ("149.154.167.51", "443") in
    Lwt.return {
      transport;
      rsa = RsaManager.default; (* TODO: *)
      time_offset = 0L;
      auth_key_tuple = None;
      server_salt = 0L;
      session_id = Crypto.SecureRand.rand_cs 8;
      seq_no = 0l;
      last_msg_id = 0L;
      request_map = Hashtbl.create (module Int64)
    }

  let reset_state t =
    t.session_id <- Crypto.SecureRand.rand_cs 8;
    t.seq_no <- 0l;
    t.last_msg_id <- 0L

  let gen_msg_id t =
    let open Int64 in
    let sec_time_f = Platform.get_current_time () in
    let sec_time = Int64.of_float sec_time_f in
    let ms_time = Int64.of_float (sec_time_f *. 1000.0) in
    let ns_time = ms_time * 1000L in
    let new_msg_id =
      ((sec_time + t.time_offset) lsl 32) lor (ns_time land 0xffff_fffcL) in
    let new_msg_id =
      if t.last_msg_id >= new_msg_id
        then new_msg_id + 4L
        else new_msg_id
    in
    t.last_msg_id <- new_msg_id;
    new_msg_id

  let next_seq_no t content_related =
    let open Int32 in
    if content_related then begin
      let result = t.seq_no * 2l + 1l in
      t.seq_no <- t.seq_no + 1l;
      result
    end else
      t.seq_no * 2l

  let send_unencrypted t data =
    let auth_key_id = 0L in
    let msg_id = gen_msg_id t in
    let data_len = Cstruct.len data in

    (* NOTE: Caml.Printf.printf does not work in js *)
    Caml.print_endline @@ Printf.sprintf
      "send_unencrypted msg_id(%Ld) data_len(%d)" msg_id data_len;

    let buf = Cstruct.create_unsafe (8 + 8 + 4 + data_len) in
    Cstruct.LE.set_uint64 buf 0 auth_key_id;
    Cstruct.LE.set_uint64 buf 8 msg_id;
    Cstruct.LE.set_uint64 buf 16 (Int64.of_int data_len);
    Cstruct.blit data 0 buf 20 (Cstruct.len data);

    T.send t.transport buf

  let receive_unencrypted t =
    (* Caml.print_endline "receive_unencrypted start"; *)
    let%lwt buf = T.receive t.transport in

    (* Caml.print_endline ("From server " ^ (Cstruct.len buf |> Int.to_string));
    Cstruct.hexdump buf; *)

    if Cstruct.len buf < 20 then begin
      Cstruct.hexdump buf;
      raise @@ MTPError "Invalid MTProto unencrypted message"
    end;

    let auth_key_id = Cstruct.LE.get_uint64 buf 0 in
    let msg_id = Cstruct.LE.get_uint64 buf 8 in
    let data_len = Cstruct.LE.get_uint32 buf 16 in
    let data_len_int = Int32.to_int_exn data_len in

    Caml.print_endline @@ Printf.sprintf
      "receive_unencrypted auth_key_id(%Ld) msg_id(%Ld) data_len(%ld)"
      auth_key_id msg_id data_len;

    if Int64.(auth_key_id <> 0L) then
      raise @@ MTPError "Bad auth_key_id";

    if Int64.(msg_id = 0L) then
      raise @@ MTPError "Bad msd_id";

    if Int32.(data_len < 1l) then
      raise @@ MTPError "Bad data length";

    let data = Cstruct.sub buf 20 data_len_int in

    Lwt.return data

  let send_unencrypted_obj t (type a) (module O : TLFunc with type t = a) (o: a) =
    let data = TL.Encoder.encode O.encode_boxed o in
    let data_cs = data |> TL.Encoder.to_cstruct in
    send_unencrypted t data_cs

  let receive_unencrypted_obj t (type a) (module O : TLObject with type t = a): a Lwt.t =
    let%lwt data = receive_unencrypted t in
    let o = O.decode (TL.Decoder.of_cstruct data) in
    Lwt.return o

  (* it fucking works *)
  let invoke_unencrypted_obj
    t
    (type a) (type result)
    (module O : TLFunc with type t = a and type ResultM.t = result)
    (o: a)
    : result Lwt.t
  =
    let%lwt () = send_unencrypted_obj t (module O) o in
    receive_unencrypted_obj t (module O.ResultM)

  let get_auth_key_tuple t = match t.auth_key_tuple with
    | Some x -> x
    | None -> raise @@ MTPError "Empty auth key"

  module EncryptedMessages: sig
    val encrypt_message: t -> encrypted_message -> Cstruct.t
    val decrypt_message: t -> Cstruct.t -> Cstruct.t
  end = struct
    let sha256 = Crypto.SHA256.digest
    let (++) = Cstruct.append
    let join = Cstruct.concat
    let sub = Cstruct.sub

    (* TODO: Checks from
      https://core.telegram.org/mtproto/description#important-checks *)

    let calc_key_iv auth_key msg_key from_client =
      let x = if from_client then 0 else 8 in

      let sha256a = sha256 @@ msg_key ++ (sub auth_key x 36) in
      let sha256b = sha256 @@ (sub auth_key (40 + x) 36) ++ msg_key in

      let aes_key = join [sub sha256a 0 8; sub sha256b 8 16; sub sha256a 24 8] in
      let aes_iv = join [sub sha256b 0 8; sub sha256a 8 16; sub sha256b 24 8] in

      (aes_key, aes_iv)

    let encrypt_message (t: t) (msg: encrypted_message) =
      let (auth_key, key_id) = get_auth_key_tuple t in

      let data_len = Cstruct.len msg.data in
      let len_without_padding = 8 + 8 + 8 + 4 + 4 + data_len in
      let padding_len = ~-(len_without_padding + 12) % 16 + 12 in
      let len_with_padding = len_without_padding + padding_len in
      let len_with_auth_key = len_with_padding + 32 in

      (* Data ("encrypted_data") with sub(auth_key, 88, 32) and padding *)
      let data_with_key = Cstruct.create_unsafe len_with_auth_key in
      (* TODO: Create BinaryWriter module and use it instead of this. *)
      Cstruct.blit auth_key 88 data_with_key 0 32;
      Cstruct.LE.set_uint64 data_with_key 32 t.server_salt;
      Cstruct.blit t.session_id 0 data_with_key 40 8;
      Cstruct.LE.set_uint64 data_with_key 48 msg.msg_id;
      Cstruct.LE.set_uint32 data_with_key 56 msg.msg_seq_no;
      Cstruct.LE.set_uint32 data_with_key 60 (Int.to_int32_exn data_len);
      Cstruct.blit msg.data 0 data_with_key 64 data_len;
      let padding = Crypto.SecureRand.rand_cs padding_len in
      Cstruct.blit padding 0 data_with_key (64 + data_len) padding_len;
      (* Logger.dump "data_with_key" data_with_key; *)

      let data_with_padding = Cstruct.shift data_with_key 32 in

      let msg_key_large = sha256 data_with_key in
      let msg_key = Cstruct.sub msg_key_large 8 16 in
      (* Logger.dump "msg_key" msg_key; *)

      let (aes_key, aes_iv) = calc_key_iv auth_key msg_key true in

      let ige_encrypted = Crypto.IGE.encrypt data_with_padding aes_key aes_iv in

      join [key_id; msg_key; ige_encrypted]

    let decrypt_message t (enc: Cstruct.t) =
      if Cstruct.len enc < 8 then begin
        Cstruct.hexdump enc;
        raise @@ MTPError "Invalid length"
      end;

      let (auth_key, our_key_id) = get_auth_key_tuple t in

      let key_id = Cstruct.sub enc 0 8 in

      if Cstruct.equal key_id our_key_id |> not then
        raise @@ MTPError "Server sent an invalid auth_key_id";

      let msg_key = Cstruct.sub enc 8 16 in

      let (aes_key, aes_iv) = calc_key_iv auth_key msg_key false in

      let dec = Crypto.IGE.decrypt (Cstruct.shift enc 24) aes_key aes_iv in

      let our_msg_key = sha256 @@ (Cstruct.sub auth_key 96 32) ++ dec in
      let our_msg_key = Cstruct.sub our_msg_key 8 16 in

      if Cstruct.equal msg_key our_msg_key |> not then begin
        Cstruct.hexdump our_msg_key;
        Cstruct.hexdump msg_key;
        raise @@ MTPError "Server sent an invalid msg_key";
      end;

      (* TODO: *)
      (* let server_salt_cs = Cstruct.sub dec 0 8 in
      let session_id = Cstruct.sub dec 8 16 in
      let msg_id = Cstruct.LE.get_uint64 dec 16 in
      let seq_no = Cstruct.LE.get_uint32 dec 24 in *)
      let data_len = Cstruct.LE.get_uint32 dec 28 |> Int32.to_int_trunc in

      Cstruct.sub dec 32 data_len
  end
  include EncryptedMessages

  let send_encrypted t (msg: encrypted_message) =
    let data_len = Cstruct.len msg.data in

    Caml.print_endline @@ Printf.sprintf
      "send_encrypted msg_id(%Ld) seq_no(%ld) data_len(%d)"
      msg.msg_id msg.msg_seq_no data_len;

    let encrypted = encrypt_message t msg in
    (* Logger.dump "encrypted" encrypted; *)
    T.send t.transport encrypted

  let receive_encrypted t =
    Caml.print_endline "receive_encrypted start";
    let%lwt buf = T.receive t.transport in
    Lwt.return (decrypt_message t buf)

  let send_encrypted_obj
    t
    ?(msg_id = gen_msg_id t) ?(content_related=false)
    (type a) (module O : TLFunc with type t = a) (o: a)
  =
    let data_encoder = TL.Encoder.encode O.encode_boxed o in
    let data = data_encoder |> TL.Encoder.to_cstruct in
    let msg_seq_no = next_seq_no t content_related in
    let msg = { msg_id; msg_seq_no; data } in
    send_encrypted t msg

  let handle_pong t (pong: TLG.C_pong.t) =
    Caml.print_endline @@ Printf.sprintf
      "Pong [msg_id %Ld] [ping_id %Ld]" pong.msg_id pong.ping_id;
    match Hashtbl.find t.request_map pong.msg_id with
    | Some (Request (resolver, _)) ->
      let pong = TLG.Pong.(C_pong pong) in
      (* TODO: May cause segmentation fault
        if server sent pong.msg_id that tied to non-ping message *)
      Lwt.wakeup_later resolver (Caml.Obj.magic pong) (* XXX *)
    | None -> Caml.print_endline @@ Printf.sprintf
      "Ping with msg_id %Ld not found" pong.msg_id

  let handle_new_session_created t (msg: TLG.C_new_session_created.t) =
    Caml.print_endline @@ Printf.sprintf
      "new_session_created [server_salt 0x%LX]" msg.server_salt;
    t.server_salt <- msg.server_salt

  let handle_rpc_result t (res: C_rpc_result.t) =
    Caml.print_endline @@ Printf.sprintf
      "rpc_result [req_msg_id %Ld]" res.req_msg_id;
    match Hashtbl.find t.request_map res.req_msg_id with
    | Some (Request (resolver, (module M))) ->
      let obj = M.ResultM.decode (TL.Decoder.of_cstruct res.data) in
      Lwt.wakeup_later resolver obj
    | None -> Caml.print_endline "!!! not found"

  let handle_msgs_ack _t _x =
    Caml.print_endline "msgs_ack not supported"

  let rec handle_container t (cont: MTPObject.msg_container) =
    Caml.print_endline @@ Printf.sprintf
      "msg_container [%d messages]" (List.length cont.messages);
    List.iter cont.messages ~f:(fun msg ->
      process_mtp_object t msg.body
    )

  and process_mtp_object t (obj: MTPObject.t): unit =
    match obj with
    | RpcResult x -> handle_rpc_result t x
    | MessageContainer x -> handle_container t x
    | Pong x -> handle_pong t x
    | NewSessionCreated x -> handle_new_session_created t x
    | MsgsAck x -> handle_msgs_ack t x
    | _ -> Caml.print_endline "not_supported"

  let rec recv_loop t =
    let%lwt data = receive_encrypted t in
    Logger.dump "recv_loop" data;
    let obj = MTPObject.decode (TL.Decoder.of_cstruct data) in
    process_mtp_object t obj;
    recv_loop t

  let invoke
    t
    ?(content_related=true)
    (type a) (type result)
    (module O : TLFunc with type t = a and type ResultM.t = result)
    (o: a)
    : result Lwt.t
  =
    let msg_id = gen_msg_id t in
    let%lwt () = send_encrypted_obj t ~msg_id ~content_related (module O) o in
    let (promise, resolver) = Lwt.task () in
    let rq = Request (resolver, (module O)) in
    Hashtbl.set t.request_map ~key:msg_id ~data:rq;
    (* TODO: Currently data is never removed from hash table *)
    promise

  let do_authentication t =
    let%lwt (auth_key, server_salt, time_offset) =
      Authenticator.authenticate (module struct
        type nonrec t = t
        let send_unencrypted_obj = send_unencrypted_obj
        let receive_unencrypted_obj = receive_unencrypted_obj
        let invoke_unencrypted_obj = invoke_unencrypted_obj
      end) t t.rsa
    in
    t.server_salt <- server_salt;
    t.time_offset <- Int.to_int64 time_offset;
    let key_id = Cstruct.sub (Crypto.SHA1.digest auth_key) 12 8 in
    t.auth_key_tuple <- Some (auth_key, key_id);
    Lwt.return ()
end

(* module type MTProtoSender = sig end *)
(* module BaseMTProtoSender = struct end *)
(* module MTProtoV1Sender = struct end *)
(* module MTProtoV2Sender = struct end *)
