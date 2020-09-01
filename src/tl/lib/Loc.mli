open! Base

type position = Lexing.position
[@@deriving show, equal, compare, sexp, hash]

val compare_position : position -> position -> int

val get_pos_info : position -> string * int * int

type t = private {
  loc_start: position;
  loc_end: position;
}
[@@deriving equal, compare, sexp, hash]

val make : position * position -> t

val make' : position -> position -> t

val of_lexbuf : Lexing.lexbuf -> t

val empty : t

val show : t -> string

val to_tuple : t -> (int * int) * (int * int)

val pp : Formatter.t -> t -> unit

val (=) : t -> t -> bool

val concat : t list -> t

type 'node annot = t * 'node
[@@deriving show, equal, compare, sexp, hash]

val map_annot : 'a annot -> ('a -> 'b) -> 'b annot

val value_map_annot : 'a annot -> ('a -> 'b) -> 'b
