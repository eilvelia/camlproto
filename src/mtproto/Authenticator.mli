open! Base

exception AuthenticationError of string

val authenticate
  : (module Types.MTProtoPlainObjSender with type t = 'a)
  -> 'a
  -> ?reject_unknown_dh_params:bool
  -> Math.Crypto.Rsa.RsaManager.t
  -> (Cstruct.t * int64 * int) Lwt.t
(** Returns [(auth_key, server_salt, time_offset) Lwt.t]. *)
