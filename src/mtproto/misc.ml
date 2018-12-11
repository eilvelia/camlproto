open! Base
open TL
(* open TL.Types *)
open TL.Builtin
open TLGen.MTProto
open Math

module C_rpc_result = struct
  type t = {
    req_msg_id: int64;
    data: Cstruct.t;
  }

  let magic = 0xf35c6d01l

  let decode dec =
    let req_msg_id = TLLong.decode dec in
    let data = TL.Decoder.to_cstruct dec in
    { req_msg_id; data }
end

let gzip_packed_magic_le = Cstruct.of_hex "a1 cf 72 30"
let rpc_error_magic_le = Cstruct.of_hex "19 ca 44 21"

module C_gzip_packed = struct
  type t = {
    packed_data: Cstruct.t
  }

  (* let magic = 0x3072cfa1l *)

  let decode dec =
    let packed_data = TLBytes.decode dec in
    { packed_data }

  let decode_boxed dec =
    Decoder.skip_len dec 4;
    decode dec
end

let decode_gzip_packed (decoder: Decoder.t) =
  (C_gzip_packed.decode_boxed decoder).packed_data
    |> Gzip.decompress
    |> Logger.dump_t "gzip decompressed"

(* let decode_obj_or_gzip_packed (decode: Decoder.t -> 'a) (data: Cstruct.t): 'a =
  let newdata = if Cstruct.equal (Cstruct.sub data 0 4) gzip_packed_magic_le
    then decode_gzip_packed (Decoder.of_cstruct data)
    else data
  in
  decode (Decoder.of_cstruct newdata) *)

let rec decode_result
  (decode: Decoder.t -> 'a) (data: Cstruct.t)
  : ('a, C_rpc_error.t) Result.t
=
  let decoder = Decoder.of_cstruct data in
  let magic = Cstruct.sub data 0 4 in
  match magic with
  | x when Cstruct.equal x gzip_packed_magic_le ->
    decode_result decode (decode_gzip_packed decoder)
  | x when Cstruct.equal x rpc_error_magic_le ->
    let (C_rpc_error err) = RpcError.decode decoder in
    Error err
  | _ ->
    Ok (decode decoder)

(* let _ = decode_result C_req_pq.decode (
  Cstruct.of_hex "19 ca 44 21  00 11 22 33  01 60 00 00") *)

module MTPObject = struct
  exception NotFound of int32 (* magic *)

  type t =
    | RpcResult of C_rpc_result.t
    | MessageContainer of msg_container
    (* | GzipPacked *)
    | Pong of C_pong.t
    | BadServerSalt of C_bad_server_salt.t
    | BadMsgNotification of C_bad_msg_notification.t
    | MsgDetailedInfo of C_msg_detailed_info.t
    | MsgNewDetailedInfo of C_msg_new_detailed_info.t
    | NewSessionCreated of C_new_session_created.t
    | MsgsAck of C_msgs_ack.t
    | FutureSalts of C_future_salts.t
    | MsgsStateReq of C_msgs_state_req.t
    | MsgResendReq of C_msg_resend_req.t
    | MsgsAllInfo of C_msgs_all_info.t

  and message = {
    msg_id: TLLong.t;
    seqno: TLInt.t;
    bytes: TLInt.t;
    body: t;
  }

  and msg_container = {
    messages: message list;
  }

  let rec decode_message dec =
    let msg_id = TLLong.decode dec in
    let seqno = TLInt.decode dec in
    let bytes = TLInt.decode dec in
    let body = decode dec in
    { msg_id; seqno; bytes; body }

  and decode_msg_container dec =
    let len = Decoder.read_int32_le dec |> Int32.to_int_exn in
    let list = ref [] in
    for _ = 1 to len do
      let el = decode_message dec in
      list := el :: !list
    done;
    { messages = !list }

  and decode dec =
    let magic = Decoder.read_int32_le dec in
    (* Decoder.skip_len dec (-4); *)
    let open Int32 in
    match magic with
    | x when x = C_rpc_result.magic -> RpcResult (C_rpc_result.decode dec)
    | 0x73f1f8dcl -> MessageContainer (decode_msg_container dec)
    | x when x = C_pong.magic -> Pong (C_pong.decode dec)
    | x when x = C_bad_server_salt.magic -> BadServerSalt (C_bad_server_salt.decode dec)
    | x when x = C_bad_msg_notification.magic -> BadMsgNotification (C_bad_msg_notification.decode dec)
    | x when x = C_msg_detailed_info.magic -> MsgDetailedInfo (C_msg_detailed_info.decode dec)
    | x when x = C_msg_new_detailed_info.magic -> MsgNewDetailedInfo (C_msg_new_detailed_info.decode dec)
    | x when x = C_new_session_created.magic -> NewSessionCreated (C_new_session_created.decode dec)
    | x when x = C_msgs_ack.magic -> MsgsAck (C_msgs_ack.decode dec)
    | x when x = C_future_salts.magic -> FutureSalts (C_future_salts.decode dec)
    | x when x = C_msgs_state_req.magic -> MsgsStateReq (C_msgs_state_req.decode dec)
    | x when x = C_msg_resend_req.magic -> MsgResendReq (C_msg_resend_req.decode dec)
    | x when x = C_msgs_all_info.magic -> MsgsAllInfo (C_msgs_all_info.decode dec)
    | x -> raise @@ NotFound x
end
