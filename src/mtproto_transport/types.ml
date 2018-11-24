open! Base
open Mtproto_misc

module type MTProtoTransport = sig
  type t
  exception Error of string
  val create: DcList.dc -> t Lwt.t
  val send: t -> Cstruct.t -> unit Lwt.t
  val receive: t -> Cstruct.t Lwt.t
end
