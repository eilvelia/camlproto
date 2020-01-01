open! Base

val run : ?store:Store.t -> Ast.t -> Err.Check.t list * Store.t
