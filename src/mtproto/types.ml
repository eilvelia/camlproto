open! Base

module type MTProtoStorage = sig (* TODO: currently not used *)
  type t
  val create: unit -> t
  val set_auth_key: t -> Cstruct.t -> unit
  val get_auth_key: t -> Cstruct.t
end

module type MTProtoPlainObjSender = sig
  open TL.Types
  type t
  val send_unencrypted_obj
    : t -> (module TLFunc with type t = 'a) -> 'a -> unit Lwt.t
  val receive_unencrypted_obj
    : t -> (module TLObject with type t = 'a) -> 'a Lwt.t
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
  open TL.Types
  type t
  exception MTPError of string
  val create: unit -> t Lwt.t
  include MTProtoPlainSender with type t := t
  val do_authentication: t -> unit Lwt.t
  val send_encrypted: t -> Cstruct.t -> unit Lwt.t
  val receive_encrypted: t -> Cstruct.t Lwt.t
  val send_encrypted_obj
    : t -> (module TLFunc with type t = 'a) -> 'a -> unit Lwt.t
end
