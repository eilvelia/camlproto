open! Base

(* module type MTProtoStorage = sig
  type t
  val create: unit -> t
  val set_auth_key: t -> Cstruct.t -> unit
  val get_auth_key: t -> Cstruct.t option
  val set_server_salt: t -> int64 -> unit
  val get_server_salt: t -> int64 option
end *)

module type MTProtoPlainObjSender = sig
  open TLRuntime.Types
  type t
  val send_unencrypted_obj
    : t -> (module TLFunc with type t = 'a) -> 'a -> unit Lwt.t
  val receive_unencrypted_obj
    : t -> (module TLAnyType with type t = 'a) -> 'a Lwt.t
  val invoke_unencrypted_obj
    : t -> (module TLFunc with type t = 'a and type ResultM.t = 'b) -> 'a -> 'b Lwt.t
end

module type MTProtoPlainSender = sig
  type t
  val send_unencrypted: t -> Cstruct.t -> unit Lwt.t
  val receive_unencrypted: t -> Cstruct.t Lwt.t
  include MTProtoPlainObjSender with type t := t
end

module type MTProtoClient = sig
  open TLRuntime.Types

  type t

  type rsa_manager

  exception MTPError of string

  exception RpcError of int * string
  (** [RpcError (error_code, error_message)] *)

  val create
    :  ?auth_key:Cstruct.t
    -> ?rsa:rsa_manager
    -> ?dc:TransportTypes.server
    -> unit
    -> t Lwt.t

  val reset_state: t -> unit

  include MTProtoPlainSender with type t := t

  val do_authentication: t -> Cstruct.t Lwt.t
  (** Returns [auth_key]. *)

  (* val init: t -> unit Lwt.t *)

  val send_encrypted_obj
    : t
    -> ?msg_id:int64
    -> ?content_related:bool
    -> (module TLFunc with type t = 'a)
    -> 'a
    -> unit Lwt.t

  val recv_loop: t -> unit Lwt.t

  val send_loop: t -> unit Lwt.t

  val loop : t -> unit

  val invoke
    : t
    -> ?content_related:bool
    -> (module TLFunc with type t = 'a and type ResultM.t = 'b)
    -> 'a
    -> 'b Lwt.t
end
