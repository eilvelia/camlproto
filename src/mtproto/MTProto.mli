open! Base

include (module type of Types)

val get_error_description: int -> string
(** [get_error_description error_code] *)

module MakeMTProtoV2Client (T: MTPTransport.S): MTProtoClient
