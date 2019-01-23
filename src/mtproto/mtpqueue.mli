open! Base

type 'a t

val create: unit -> 'a t

val add: 'a t -> 'a -> unit

val add_all: 'a t -> 'a list -> unit

val get_all: 'a t -> 'a list Lwt.t
