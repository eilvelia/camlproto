open! Base
open MTProtoMisc
open MtpTL

module TLM = TLGen.MTProto

include Types

let src = Logs.Src.create "camlproto.mtproto.client"
module Log = (val Logs.src_log src : Logs.LOG)

let hexdump_pp = Cstruct.hexdump_pp

(* Error descriptions are from ZeroBias's telegram-mtproto
  https://bit.ly/2RXAfjO *)
let get_error_description (error_code: int): string =
  match error_code with
  | 16 -> "msg_id too low"
  | 17 -> "msg_id too high"
  | 18 -> "incorrect two lower order msg_id bits"
  | 19 -> "same container id"
  | 20 -> "message too old"
  | 32 -> "msg_seqno too low"
  | 33 -> "msg_seqno too high"
  | 34 -> "odd seq"
  | 35 -> "even seq"
  | 48 -> "incorrect server salt"
  | 64 -> "invalid container"
  | _ -> "unknown"

(* module BaseMTProtoClient = struct end *)
(* module MakeMTProtoV1Client = struct end *)
module MakeMTProtoV2Client (Platform: PlatformTypes.S) (T: TransportTypes.S) = struct
  open TL.Types

  module Math = Math.Make(Platform)
  module Crypto = Math.Crypto
  module Bigint = Math.Bigint
  module RsaManager = Crypto.Rsa.RsaManager

  type rsa_manager = RsaManager.t

  module Res = MakeRes(Platform)
  open Res

  type request = Request
    : 'a Lwt.u * (module TLFunc with type ResultM.t = 'a)
    -> request

  type sent_msg =
    | SimpleMessage of MTPMessage.t
    | ContainerMessage of { msg_ids: int64 list (* ids of inner messages *) }

  (* TODO: Container messages are never removed from t.sent_msg_map *)

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
    sent_msg_map: (int64, sent_msg) Hashtbl.t;
    send_queue: MTPMessage.t AsyncQueue.t;
    mutable pending_ack: int64 list; (** msg_id list *)
  }

  (* TODO: use sum types instead of strings *)
  exception MTPError of string

  exception RpcError of int * string

  let calc_key_id auth_key = Cstruct.sub (Crypto.SHA1.digest auth_key) 12 8
  [@@inline]

  let create
    ?(auth_key: Cstruct.t option)
    ?(rsa = RsaManager.default)
    ?(dc: DcList.dc = ("149.154.167.51", "443"))
    ()
  =
    let open Option.Monad_infix in
    let%lwt transport = T.create dc in
    let auth_key_tuple = auth_key >>| fun k -> (k, calc_key_id k) in
    Lwt.return {
      transport;
      rsa;
      time_offset = 0L;
      auth_key_tuple;
      server_salt = 0L;
      session_id = Crypto.SecureRand.rand_cs 8;
      seq_no = 0l;
      last_msg_id = 0L;
      request_map = Hashtbl.create (module Int64);
      sent_msg_map = Hashtbl.create (module Int64);
      send_queue = AsyncQueue.create ();
      pending_ack = [];
    }

  let set_auth_key t (auth_key: Cstruct.t) =
    let key_id = calc_key_id auth_key in
    t.auth_key_tuple <- Some (auth_key, key_id)
  [@@inline]

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
        then new_msg_id + 4L + (t.last_msg_id - new_msg_id)
        else new_msg_id
    in
    t.last_msg_id <- new_msg_id;
    new_msg_id

  (* Updates the time offset to the correct one given a known valid msg_id *)
  let update_time_offset t correct_msg_id =
    let open Int64 in
    (* let bad = gen_msg_id t in *)
    let old = t.time_offset in
    let now = Int64.of_float (Platform.get_current_time ()) in
    let correct = Int64.(correct_msg_id asr 32) in
    t.time_offset <- correct - now;
    if t.time_offset <> old then t.last_msg_id <- 0L;
    Log.info (fun m -> m "Updated time offset: %Ld" t.time_offset)

  let gen_seq_no t content_related =
    let open Int32 in
    if content_related then
      let result = t.seq_no * 2l + 1l in
      t.seq_no <- t.seq_no + 1l;
      result
    else
      t.seq_no * 2l

  let send_unencrypted t data =
    let auth_key_id = 0L in
    let msg_id = gen_msg_id t in
    let data_len = Cstruct.len data in

    Log.debug (fun m -> m "send_unencrypted msg_id(%Ld) data_len(%d)" msg_id data_len);

    let buf = Cstruct.create_unsafe (8 + 8 + 4 + data_len) in
    Cstruct.LE.set_uint64 buf 0 auth_key_id;
    Cstruct.LE.set_uint64 buf 8 msg_id;
    Cstruct.LE.set_uint64 buf 16 (Int64.of_int data_len);
    Cstruct.blit data 0 buf 20 (Cstruct.len data);

    T.send t.transport buf

  let receive_unencrypted t =
    (* Log.debug (fun m -> m "receive_unencrypted start"); *)
    let%lwt buf = T.receive t.transport in

    (* Log.debug (fun m -> m "From server [%d]:@.%a"
      (Cstruct.len buf) hexdump_pp buf); *)

    if Cstruct.len buf < 20 then begin
      Log.err (fun m -> m "invalid unenc msg length:@.%a" hexdump_pp buf);
      raise @@ MTPError "Invalid MTProto unencrypted message"
    end;

    let auth_key_id = Cstruct.LE.get_uint64 buf 0 in
    let msg_id = Cstruct.LE.get_uint64 buf 8 in
    let data_len = Cstruct.LE.get_uint32 buf 16 in
    let data_len_int = Int32.to_int_exn data_len in

    Log.debug (fun m -> m
      "receive_unencrypted auth_key_id(%Ld) msg_id(%Ld) data_len(%ld)"
      auth_key_id msg_id data_len);

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

  let invoke_unencrypted_obj
    t
    (type a) (type result)
    (module O : TLFunc with type t = a and type ResultM.t = result)
    (o: a)
    : result Lwt.t
  =
    let%lwt () = send_unencrypted_obj t (module O) o in
    receive_unencrypted_obj t (module O.ResultM)

  module Authenticator = Authenticator.Make(Platform)(struct
    type nonrec t = t
    let send_unencrypted_obj = send_unencrypted_obj
    let receive_unencrypted_obj = receive_unencrypted_obj
    let invoke_unencrypted_obj = invoke_unencrypted_obj
  end)

  let get_auth_key_tuple t = match t.auth_key_tuple with
    | Some x -> x
    | None -> raise @@ MTPError "Empty auth key"

  module EncryptedMessages: sig
    val encrypt_message: t -> MTPMessage.t -> Cstruct.t
    val decrypt_message: t -> Cstruct.t -> MTPMessage.t
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

    let encrypt_message (t: t) (msg: MTPMessage.t) =
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
      (* Log.debug (fun m -> m "data_with_key:@.%a" hexdump_pp data_with_key); *)

      let data_with_padding = Cstruct.shift data_with_key 32 in

      let msg_key_large = sha256 data_with_key in
      let msg_key = Cstruct.sub msg_key_large 8 16 in
      (* Log.debug (fun m -> m "msg_key:@.%a" hexdump_pp msg_key); *)

      let (aes_key, aes_iv) = calc_key_iv auth_key msg_key true in

      let ige_encrypted = Crypto.IGE.encrypt data_with_padding aes_key aes_iv in

      join [key_id; msg_key; ige_encrypted]

    let decrypt_message t (enc: Cstruct.t) =
      if Cstruct.len enc < 8 then begin
        Log.err (fun m -> m "invalid length:@.%a" hexdump_pp enc);
        raise @@ MTPError "Invalid length"
      end;

      let (auth_key, our_key_id) = get_auth_key_tuple t in

      let key_id = Cstruct.sub enc 0 8 in

      if Cstruct.equal key_id our_key_id |> not then
        raise @@ MTPError "Server sent an invalid auth_key_id";

      let msg_key = Cstruct.sub enc 8 16 in

      let (aes_key, aes_iv) = calc_key_iv auth_key msg_key false in

      let plain = Crypto.IGE.decrypt (Cstruct.shift enc 24) aes_key aes_iv in

      let our_msg_key = sha256 @@ (Cstruct.sub auth_key 96 32) ++ plain in
      let our_msg_key = Cstruct.sub our_msg_key 8 16 in

      if Cstruct.equal msg_key our_msg_key |> not then begin
        Log.warn (fun m -> m "Server sent an invalid msg_key");
        Log.info (fun m -> m "client msg_key:@.%a" hexdump_pp our_msg_key);
        Log.info (fun m -> m "server msg_key:@.%a" hexdump_pp msg_key);
        raise @@ MTPError "Server sent an invalid msg_key";
      end;

      (* TODO: *)
      (* let server_salt_cs = Cstruct.sub plain 0 8 in *)

      let session_id = Cstruct.sub plain 8 8 in

      if Cstruct.equal session_id t.session_id |> not then
        raise @@ MTPError "Server sent an invalid session_id";

      let msg_id = Cstruct.LE.get_uint64 plain 16 in
      let msg_seq_no = Cstruct.LE.get_uint32 plain 24 in

      let data_len = Cstruct.LE.get_uint32 plain 28 |> Int32.to_int_trunc in
      let data = Cstruct.sub plain 32 data_len in

      let padding = Cstruct.shift plain (32 + data_len) in
      let padding_len = Cstruct.len padding in

      if padding_len < 12 || padding_len > 1024 then
        raise @@ MTPError "Invalid padding length";

      MTPMessage.{ msg_id; msg_seq_no; data }
  end
  include EncryptedMessages

  let receive_encrypted t =
    Log.debug (fun m -> m "receive_encrypted start");
    let%lwt buf = T.receive t.transport in
    Lwt.return (decrypt_message t buf)

  let send_encrypted t (msg: MTPMessage.t) =
    let data_len = Cstruct.len msg.data in

    Log.info (fun m -> m
      "send_encrypted msg_id(%Ld) seq_no(%ld) data_len(%d)"
      msg.msg_id msg.msg_seq_no data_len);

    let encrypted = encrypt_message t msg in
    (* Log.debug (fun m -> m "encrypted:@.%a" hexdump_pp encrypted); *)
    T.send t.transport encrypted

  let send_encrypted_obj
    t
    ?(msg_id = gen_msg_id t) ?(content_related = false)
    (type a) (module O : TLFunc with type t = a) (o: a)
  =
    let data_encoder = TL.Encoder.encode O.encode_boxed o in
    let data = data_encoder |> TL.Encoder.to_cstruct in
    let msg_seq_no = gen_seq_no t content_related in
    let msg = MTPMessage.{ msg_id; msg_seq_no; data } in
    send_encrypted t msg

  let req_not_found msg_id =
    Printf.sprintf "Request with msg_id %Ld not found" msg_id
  [@@inline]

  let msg_not_found msg_id =
    Printf.sprintf "Message with msg_id %Ld not found" msg_id
  [@@inline]

  let tl_encode (type a) (module O : TLObject with type t = a) (o: a): Cstruct.t =
    let encoder = TL.Encoder.encode O.encode_boxed o in
    TL.Encoder.to_cstruct encoder
  [@@inline]

  let send_msg t (msg: MTPMessage.t) =
    Log.info (fun m -> m "send_msg msg_id(%Ld) seq_no(%ld) data_len(%d)"
      msg.msg_id msg.msg_seq_no (Cstruct.len msg.data));
    AsyncQueue.add t.send_queue msg

  let rec resend_packed_msg t packed_msg =
    match packed_msg with
    | SimpleMessage ({ msg_id; _ } as msg) -> begin
      Log.info (fun m -> m
        "Resending message [msg_id %Ld] [seqno %ld]" msg_id msg.msg_seq_no);
      let new_msg_id = gen_msg_id t in
      let new_msg = { msg with msg_id = new_msg_id } in
      send_msg t new_msg;
      match Hashtbl.find_and_remove t.request_map msg_id with
      | Some rq -> Hashtbl.set t.request_map ~key:new_msg_id ~data:rq;
      | None -> Log.info (fun m -> m "Info: resend_packed_msg: %s" (req_not_found msg_id))
    end
    | ContainerMessage { msg_ids } -> List.iter msg_ids ~f:(resend t)

  (* Resend with new msg_id *)
  and resend t msg_id =
    match Hashtbl.find_and_remove t.sent_msg_map msg_id with
    | Some packed_msg -> resend_packed_msg t packed_msg
    | None -> Log.info (fun m -> m "resend: %s" (msg_not_found msg_id))

  (* Removes message from t.request_map and t.sent_msg_map *)
  let remove_req t msg_id =
    Hashtbl.remove t.request_map msg_id;
    Hashtbl.remove t.sent_msg_map msg_id

  let handle_bad_msg_notification t msg_id (obj: TLM.C_bad_msg_notification.t) =
    Log.warn (fun m ->
      let error_desc = get_error_description obj.error_code in
      m "bad_msg_notification [bad_msg_id %Ld] [bad_msg_seqno %d] (%d - %s)"
        obj.bad_msg_id obj.bad_msg_seqno obj.error_code error_desc);
    let fatal = ref false in
    begin match obj.error_code with
    | 16 | 17 -> update_time_offset t msg_id
    | _ -> fatal := true
    end;
    if !fatal
      then remove_req t msg_id (* TODO: raise exception *)
      else resend t obj.bad_msg_id

  let handle_bad_server_salt t (obj: TLM.C_bad_server_salt.t) =
    Log.warn (fun m ->
      let error_desc = get_error_description obj.error_code in
      m "bad_server_salt [bad_msg_id %Ld] [bad_msg_seqno %d] (%d - %s)"
        obj.bad_msg_id obj.bad_msg_seqno obj.error_code error_desc);
    Log.info (fun m -> m "New server salt: 0x%LX" obj.new_server_salt);
    t.server_salt <- obj.new_server_salt;
    resend t obj.bad_msg_id

  let handle_pong t (pong: TLM.C_pong.t) =
    Log.info (fun m -> m
      "Pong [msg_id %Ld] [ping_id %Ld]" pong.msg_id pong.ping_id);
    Hashtbl.remove t.sent_msg_map pong.msg_id;
    match Hashtbl.find_and_remove t.request_map pong.msg_id with
    | Some (Request (resolver, _)) ->
      let pong = TLM.Pong.(C_pong pong) in
      (* TODO: May cause segmentation fault
        if server sent pong.msg_id that tied to non-ping message *)
      Lwt.wakeup_later resolver (Caml.Obj.magic pong) (* XXX *)
    | None -> Log.info (fun m -> m "Ping with msg_id %Ld not found" pong.msg_id)

  let handle_new_session_created t (obj: TLM.C_new_session_created.t) =
    Log.info (fun m -> m "new_session_created [server_salt 0x%LX]" obj.server_salt);
    t.server_salt <- obj.server_salt

  let handle_rpc_result t (res: C_rpc_result.t) =
    Log.info (fun m -> m "rpc_result [req_msg_id %Ld]" res.req_msg_id);
    Hashtbl.remove t.sent_msg_map res.req_msg_id;
    match Hashtbl.find_and_remove t.request_map res.req_msg_id with
    | Some (Request (rs, (module M))) ->
      begin match decode_result M.ResultM.decode res.data with
        | Ok x -> Lwt.wakeup_later rs x
        | Error x ->
          Log.info (fun m -> m
            "rpc_error [error_code %d] (%s)" x.error_code x.error_message);
          Lwt.wakeup_later_exn rs (RpcError (x.error_code, x.error_message))
      end
    | None -> Log.warn (fun m -> m "%s" (req_not_found res.req_msg_id))

  let handle_msgs_ack t (obj: TLM.C_msgs_ack.t) =
    (* TODO: *)
    let (C_vector msg_ids) = obj.msg_ids in
    let str = String.concat ~sep:" " (List.map ~f:Int64.to_string msg_ids) in
    Log.info (fun m -> m "msgs_ack [msg_ids %s]" str);
    List.iter msg_ids ~f:(fun msg_id ->
      Hashtbl.remove t.sent_msg_map msg_id
      (* Hashtbl.remove t.request_map msg_id *)
    )

  let handle_detailed_info t msg_id (obj: TLM.C_msg_detailed_info.t) =
    (* TODO: *)
    Log.info (fun m -> m "msg_detailed_info [msg_id %Ld]" obj.msg_id);
    t.pending_ack <- msg_id :: t.pending_ack

  let handle_new_detailed_info t msg_id (obj: TLM.C_msg_new_detailed_info.t) =
    (* TODO: *)
    Log.info (fun m -> m
      "msg_new_detailed_info [answer_msg_id %Ld]" obj.answer_msg_id);
    t.pending_ack <- msg_id :: t.pending_ack

  let handle_future_salts t (obj: TLM.C_future_salts.t) =
    (* TODO: *)
    Log.info (fun m -> m "future_salts [req_msg_id %Ld]" obj.req_msg_id);
    Hashtbl.remove t.sent_msg_map obj.req_msg_id

  let rec handle_container t (cont: MTPObject.tl_msg_container) =
    Log.info (fun m -> m
      "msg_container [%d messages]" (List.length cont.messages));
    List.iter cont.messages ~f:(fun msg ->
      process_mtp_object t msg.msg_id msg.body
    )

  and process_mtp_object t (msg_id: int64) (obj: MTPObject.t): unit =
    t.pending_ack <- msg_id :: t.pending_ack; (* TODO: *)
    match obj with
    | RpcResult x -> handle_rpc_result t x
    | MessageContainer x -> handle_container t x
    | Pong x -> handle_pong t x
    | BadServerSalt x -> handle_bad_server_salt t x
    | BadMsgNotification x -> handle_bad_msg_notification t msg_id x
    | MsgDetailedInfo x -> handle_detailed_info t msg_id x
    | MsgNewDetailedInfo x -> handle_new_detailed_info t msg_id x
    | NewSessionCreated x -> handle_new_session_created t x
    | MsgsAck x -> handle_msgs_ack t x
    | FutureSalts x -> handle_future_salts t x
    | MsgsStateReq _
    | MsgResendReq _
    | MsgsAllInfo _ ->
      (* TODO: *)
      Log.warn (fun m -> m "TODO MsgsStateReq/MsgResendReq/MsgsAllInfo")

  let rec recv_loop t =
    Log.debug (fun m -> m "recv_loop start");
    let%lwt msg = receive_encrypted t in
    Log.info (fun m -> m "recv_loop msg_id(%Ld) seq_no(%ld) data_len(%d)"
      msg.msg_id msg.msg_seq_no (Cstruct.len msg.data));
    Log.debug (fun m -> m "recv_loop data:@.%a" hexdump_pp msg.data);
    let obj = MTPObject.decode (TL.Decoder.of_cstruct msg.data) in
    process_mtp_object t msg.msg_id obj;
    Log.debug (fun m ->
      let keys hashtable = hashtable
        |> Hashtbl.keys |> List.sexp_of_t Int64.sexp_of_t |> Sexp.to_string in
      m "t.request_map: %s | t.sent_msg_map: %s"
        (keys t.request_map) (keys t.sent_msg_map));
    recv_loop t

  let create_ack t = MTPMessage.{
    msg_id = gen_msg_id t;
    msg_seq_no = gen_seq_no t false;
    data = tl_encode (module TLM.C_msgs_ack) { msg_ids = C_vector t.pending_ack }
  }

  let rec send_loop t =
    begin match t.pending_ack with
    | [] -> ()
    | _ -> AsyncQueue.add t.send_queue (create_ack t); t.pending_ack <- []
    end;
    let%lwt list = AsyncQueue.get_all t.send_queue in
    let msg = match list with
      | [x] ->
        Hashtbl.set t.sent_msg_map ~key:x.msg_id ~data:(SimpleMessage x);
        x
      | xs ->
        let data = MTPContainer.encode (List.map ~f:MTPMessage.encode xs) in
        Log.info (fun m -> m "Sending container with %d message(s)" (List.length xs));
        let msg_ids = List.map xs ~f:(fun x ->
          Hashtbl.set t.sent_msg_map ~key:x.msg_id ~data:(SimpleMessage x);
          x.msg_id
        ) in
        let msg_id = gen_msg_id t in
        let msg_seq_no = gen_seq_no t false in
        Hashtbl.set t.sent_msg_map ~key:msg_id ~data:(ContainerMessage { msg_ids });
        { msg_id; msg_seq_no; data }
    in
    let%lwt () = send_encrypted t msg in
    send_loop t

  let invoke
    t
    ?(content_related = true)
    (type a result)
    (module O : TLFunc with type t = a and type ResultM.t = result)
    (o: a)
    : result Lwt.t
  =
    let msg_id = gen_msg_id t in
    let msg_seq_no = gen_seq_no t content_related in
    let data = tl_encode (module O) o in
    let msg = MTPMessage.{ msg_id; msg_seq_no; data } in
    let (promise, resolver) = Lwt.task () in
    let rq = Request (resolver, (module O)) in
    Hashtbl.set t.request_map ~key:msg_id ~data:rq;
    (* let%lwt () = send_encrypted_obj t ~msg_id ~content_related (module O) o in *)
    send_msg t msg;
    promise

  let do_authentication t =
    let%lwt (auth_key, server_salt, time_offset) =
      Authenticator.authenticate t t.rsa in
    t.server_salt <- server_salt;
    t.time_offset <- Int.to_int64 time_offset;
    set_auth_key t auth_key;
    (* Lwt.return_unit *)
    Lwt.return auth_key

  (* let init t =
    match t.auth_key_tuple with
    | Some _ -> Lwt.return_unit
    | None -> do_authentication t *)
end

(* module type MTProtoSender = sig end *)
(* module BaseMTProtoSender = struct end *)
(* module MTProtoV1Sender = struct end *)
(* module MTProtoV2Sender = struct end *)
