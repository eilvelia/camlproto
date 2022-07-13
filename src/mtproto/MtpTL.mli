open! Base
open TLRuntime.Builtin
open TLSchema.MTProto

module MTPMessage: sig
  type t = {
    msg_id: int64;
    msg_seq_no: int32;
    data: Cstruct.t;
  }

  val encode: t -> Cstruct.t
end

module MTPContainer: sig
  val magic: unit -> int32

  val encode: Cstruct.t list -> Cstruct.t
end

module TL_rpc_result: sig
  type t = {
    req_msg_id: int64;
    data: Cstruct.t;
  }

  val magic: unit -> int32

  val decode: TLRuntime.Decoder.t -> t
end

module TL_gzip_packed: sig
  type t = {
    packed_data: Cstruct.t
  }

  val decode: TLRuntime.Decoder.t -> t
  val decode_boxed: TLRuntime.Decoder.t -> t
end

module MakeRes (Platform: PlatformTypes.S): sig
  val decode_result
    : (TLRuntime.Decoder.t -> 'a) -> Cstruct.t -> ('a, TL_rpc_error.t) Result.t
end

module MTPObject: sig
  exception NotFound of int32
  (** [NotFound magic] *)

  type t =
    | RpcResult of TL_rpc_result.t
    | MessageContainer of tl_msg_container
    | Pong of TL_pong.t
    | BadServerSalt of TL_bad_server_salt.t
    | BadMsgNotification of TL_bad_msg_notification.t
    | MsgDetailedInfo of TL_msg_detailed_info.t
    | MsgNewDetailedInfo of TL_msg_new_detailed_info.t
    | NewSessionCreated of TL_new_session_created.t
    | MsgsAck of TL_msgs_ack.t
    | FutureSalts of TL_future_salts.t
    | MsgsStateReq of TL_msgs_state_req.t
    | MsgResendReq of TL_msg_resend_req.t
    | MsgsAllInfo of TL_msgs_all_info.t

  and tl_message = {
    msg_id: TL_long.t;
    seqno: TL_int.t;
    bytes: TL_int.t;
    body: t;
  }

  and tl_msg_container = {
    messages: tl_message list;
  }

  val decode: TLRuntime.Decoder.t -> t
end
