open! Base
open TL
(* open TL.Types *)
open TL.Builtin
open TLGen.MTProto

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

module MTPObject = struct
  exception NotFound of int32 (* magic *)

  type t =
    | RpcResult of C_rpc_result.t
    | MessageContainer of msg_container
    (* | GzipPacked *) (* TODO: *)
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
