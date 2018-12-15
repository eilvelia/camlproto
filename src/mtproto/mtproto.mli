open! Base
open Mtproto_transport
open Types

module Types = Types

val get_error_description: int -> string
(** [get_error_description error_code] *)

module MakeMTProtoV2Client (T: MTProtoTransport): MTProtoClient
