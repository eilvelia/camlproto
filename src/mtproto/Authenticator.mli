open! Base
open Types

exception AuthenticationError of string

module Make (Platform: PlatformTypes.S) (Sender: MTProtoPlainObjSender): sig
  val authenticate
    : Sender.t
    -> ?reject_unknown_dh_params:bool
    -> Math.Make(Platform).Crypto.Rsa.RsaManager.t
    -> (Cstruct.t * int64 * int) Lwt.t
  (** Returns [(auth_key, server_salt, time_offset) Lwt.t]. *)
end
