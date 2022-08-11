open! Base
open TLRuntime.Types
open TLRuntime.Builtin
open TLSchema.MTProto

(* This file contains hacky definitions of types in mtproto.tl that cannot be
 * parsed automatically *)

module Decoder = TLRuntime.Decoder
(* module Encoder = TLRuntime.Encoder *)

module MTPMessage = struct
  type t = {
    (* msg_server_salt: int64;
    msg_session_id: int64; *)
    msg_id: int64;
    msg_seq_no: int32;
    data: Cstruct.t;
  }

  (* type req_tl_message = {
    msg_id: TLLong.t;
    seqno: int32;
    bytes: TLInt.t;
    body_cs: Cstruct.t;
  } *)

  let encode (msg: t) =
    let bytes = Cstruct.length msg.data in
    let cs = Cstruct.create_unsafe (8 + 4 + 4 + Cstruct.length msg.data) in
    Cstruct.LE.set_uint64 cs 0 msg.msg_id;
    Cstruct.LE.set_uint32 cs 8 msg.msg_seq_no;
    Cstruct.LE.set_uint32 cs 12 (Int32.of_int_trunc bytes);
    Cstruct.blit msg.data 0 cs 16 (Cstruct.length msg.data);
    cs
end

module MTPContainer = struct
 let [@inline] magic () = 0x73f1f8dcl

  let encode (l: Cstruct.t list) =
    let cont_len = 4 + 4 + (Cstruct.lenv l) in
    let cs = Cstruct.create_unsafe cont_len  in
    Cstruct.LE.set_uint32 cs 0 (magic ());
    Cstruct.LE.set_uint32 cs 4 (Int32.of_int_trunc @@ List.length l);
    let i = ref 8 in
    List.iter l ~f:(fun cs' ->
      let len = Cstruct.length cs' in
      Cstruct.blit cs' 0 cs !i len;
      i := !i + len
    );
    cs
end

module TL_rpc_result = struct
  type t = {
    req_msg_id: int64;
    data: Cstruct.t;
  }

  let [@inline] magic () = 0xf35c6d01l

  let decode dec =
    let req_msg_id = TL_long.decode dec in
    let data = Decoder.to_cstruct dec in
    { req_msg_id; data }
end

let gzip_packed_magic_le = Cstruct.of_hex "a1 cf 72 30"
let rpc_error_magic_le = Cstruct.of_hex "19 ca 44 21"

module TL_gzip_packed = struct
  type t = {
    packed_data: Cstruct.t
  }

  (* let magic = 0x3072cfa1l *)

  let decode dec =
    let packed_data = TL_bytes.decode dec in
    { packed_data }

  let decode_boxed dec =
    Decoder.skip_len dec 4;
    decode dec
end

let src = Logs.Src.create "camlproto.mtproto.gzip"
module Log = (val Logs.src_log src : Logs.LOG)

module MakeRes (Platform: PlatformTypes.S) = struct
  module Math = Math.Make(Platform)
  open Math

  let decode_gzip_packed (decoder: Decoder.t) =
    let data = (TL_gzip_packed.decode_boxed decoder).packed_data in
    let decompressed = Gzip.decompress data in
    Log.debug (fun m -> m "gzip decompressed:@.%a" Cstruct.hexdump_pp decompressed);
    decompressed

  (* let decode_obj_or_gzip_packed (decode: Decoder.t -> 'a) (data: Cstruct.t): 'a =
    let newdata = if Cstruct.equal (Cstruct.sub data 0 4) gzip_packed_magic_le
      then decode_gzip_packed (Decoder.of_cstruct data)
      else data
    in
    decode (Decoder.of_cstruct newdata) *)

  let rec decode_result
    (decode: Decoder.t -> 'a) (data: Cstruct.t)
    : ('a, TL_rpc_error.t) Result.t
  =
    let decoder = Decoder.of_cstruct data in
    let magic = Cstruct.sub data 0 4 in
    match magic with
    | x when Cstruct.equal x gzip_packed_magic_le ->
      decode_result decode (decode_gzip_packed decoder)
    | x when Cstruct.equal x rpc_error_magic_le ->
      let (TL_rpc_error err) = TLT_RpcError.decode decoder in
      Error err
    | _ ->
      Ok (decode decoder)
end

module MTPObject = struct
  type t =
    | RpcResult of TL_rpc_result.t
    | MessageContainer of tl_msg_container
    (* | GzipPacked *)
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

  let rec decode_message dec : tl_message =
    let msg_id = TL_long.decode dec in
    let seqno = TL_int.decode dec in
    let bytes = TL_int.decode dec in
    let body = decode dec in
    { msg_id; seqno; bytes; body }

  and decode_msg_container dec =
    let len = Decoder.read_int32_le dec |> Int32.to_int_trunc in
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
    | x when x = TL_rpc_result.magic () -> RpcResult (TL_rpc_result.decode dec)
    | x when x = MTPContainer.magic () -> MessageContainer (decode_msg_container dec)
    | x when x = TL_pong.magic () -> Pong (TL_pong.decode dec)
    | x when x = TL_bad_server_salt.magic () -> BadServerSalt (TL_bad_server_salt.decode dec)
    | x when x = TL_bad_msg_notification.magic () -> BadMsgNotification (TL_bad_msg_notification.decode dec)
    | x when x = TL_msg_detailed_info.magic () -> MsgDetailedInfo (TL_msg_detailed_info.decode dec)
    | x when x = TL_msg_new_detailed_info.magic () -> MsgNewDetailedInfo (TL_msg_new_detailed_info.decode dec)
    | x when x = TL_new_session_created.magic () -> NewSessionCreated (TL_new_session_created.decode dec)
    | x when x = TL_msgs_ack.magic () -> MsgsAck (TL_msgs_ack.decode dec)
    | x when x = TL_future_salts.magic () -> FutureSalts (TL_future_salts.decode dec)
    | x when x = TL_msgs_state_req.magic () -> MsgsStateReq (TL_msgs_state_req.decode dec)
    | x when x = TL_msg_resend_req.magic () -> MsgResendReq (TL_msg_resend_req.decode dec)
    | x when x = TL_msgs_all_info.magic () -> MsgsAllInfo (TL_msgs_all_info.decode dec)
    | x -> raise @@ DeserializationError x
end
