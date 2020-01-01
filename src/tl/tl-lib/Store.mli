open! Base
open Types

type t

val show : t -> string

val make
  : ?types:tl_type list
  -> ?constructors:tl_constructor list
  -> ?functions:tl_function list
  -> unit
  -> t

val empty : t
val default : t
val default_constr_ref : int

val insert_type : t -> tl_type -> t * int
val insert_constructor : t -> tl_constructor -> t option
val insert_function : t -> tl_function -> t option

val get_type_by_name : t -> Ast.name -> (int * tl_type) option
val get_type_by_ref : t -> int -> tl_type

val get_constructor_by_name : t -> Ast.name -> tl_constructor option
val get_constructor_by_ref : t -> tl_constr_ref -> tl_constructor

val get_constructors_by_type_ref : t -> int -> tl_constructor list
(** Get all type's constructors *)

val get_function_by_name : t -> Ast.name -> tl_function option

module MapM (M: Monad.S): sig
  val constructors : t -> f:(tl_constructor -> tl_constructor M.t) -> t M.t
  val functions : t -> f:(tl_function -> tl_function M.t) -> t M.t
end

val fold_types : t -> init:'a -> f:('a -> int * tl_type -> 'a) -> 'a 
val fold_constructors : t -> init:'a -> f:('a -> tl_constructor -> 'a) -> 'a 
val fold_functions : t -> init:'a -> f:('a -> tl_function -> 'a) -> 'a 
