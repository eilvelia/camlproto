open! Base

include (module type of Types)

val get_error_description: int -> string
(** [get_error_description error_code] *)

module MakeMTProtoV2Client (Platform: PlatformTypes.S) (T: MTPTransport.S)
  : MTProtoClient with type rsa_manager = Math.Make(Platform).Crypto.Rsa.RsaManager.t
