open! Base
open Types

exception AuthenticationError of string

val authenticate
  : (module MTProtoPlainObjSender with type t = 'a)
  -> 'a
  -> Math.Crypto.Rsa.RsaManager.t
  -> (Cstruct.t * int64 * int) Lwt.t
(** Returns [(auth_key, server_salt, time_offset) Lwt.t]. *)
