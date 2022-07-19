open! Base

(*_ TODO: Use a more detailed type for "server"? *)

type server = string * string
(** [ip, port] *)

module type S = sig
  type t

  exception Error of string

  val create : server -> t Lwt.t

  val send : t -> Cstruct.t -> unit Lwt.t

  val receive : t -> Cstruct.t Lwt.t
end
