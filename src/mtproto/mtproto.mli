open! Base
open Mtproto_transport

include (module type of Types)

val get_error_description: int -> string
(** [get_error_description error_code] *)

module MakeMTProtoV2Client (T: MTProtoTransport): MTProtoClient
