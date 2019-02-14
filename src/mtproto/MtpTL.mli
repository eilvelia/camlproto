open! Base
open TL.Builtin
open TLGen.MTProto

module MTPMessage: sig
  type t = {
    msg_id: int64;
    msg_seq_no: int32;
    data: Cstruct.t;
  }

  val encode: t -> Cstruct.t
end

module MTPContainer: sig
  val magic: int32

  val encode: Cstruct.t list -> Cstruct.t
end

module C_rpc_result: sig
  type t = {
    req_msg_id: int64;
    data: Cstruct.t;
  }

  val magic: int32

  val decode: TL.Decoder.t -> t
end

module C_gzip_packed: sig
  type t = {
    packed_data: Cstruct.t
  }

  val decode: TL.Decoder.t -> t
  val decode_boxed: TL.Decoder.t -> t
end

val decode_result
  : (TL.Decoder.t -> 'a) -> Cstruct.t -> ('a, C_rpc_error.t) Result.t

module MTPObject: sig
  exception NotFound of int32
  (** [NotFound magic] *)

  type t =
    | RpcResult of C_rpc_result.t
    | MessageContainer of tl_msg_container
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

  and tl_message = {
    msg_id: TLLong.t;
    seqno: TLInt.t;
    bytes: TLInt.t;
    body: t;
  }

  and tl_msg_container = {
    messages: tl_message list;
  }

  val decode: TL.Decoder.t -> t
end
