open! Base

type 'a t

val create: unit -> 'a t

val add: 'a t -> 'a -> unit

val get: 'a t -> 'a list Lwt.t
