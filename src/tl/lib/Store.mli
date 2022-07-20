open! Base
open Types

type t

val show : t -> string

val empty : t
(** The empty store without a prelude *)

val default : t
(** The default prelude *)

val make
  :  ?init:t
  -> ?types:tl_type list
  -> ?constructors:tl_constructor list
  -> ?functions:tl_function list
  -> unit
  -> t
(** Make a new store. By default, [?init] is [empty]. *)

val fresh_constr_ref : t -> int
(** Get a constr ref unused in the store, which can then be
    used as a [tl_constructor'.c_ref] in a new constructor *)

val is_constr_defined_in_prelude : tl_constructor -> bool
(** Check if the constructor is identical to one in the prelude ([default]).
    This checks the name and magic of the constructors.
    Useful to prevent errors/warnings about redefining e.g. Vector. *)

val insert_type : t -> tl_type -> t * int
val insert_constructor : t -> tl_constructor -> t option
val insert_function : t -> tl_function -> t option

val get_type_by_name : t -> Ast.name -> (int * tl_type) option
val get_type_by_ref : t -> int -> tl_type

val get_constructor_by_name : t -> Ast.name -> tl_constructor option
val get_constructor_by_ref : t -> tl_constr_ref -> tl_constructor

val get_constructors_by_type_ref : t -> int -> tl_constructor list
(** Get all constructors of a type *)

val get_function_by_name : t -> Ast.name -> tl_function option

module MapM (M : Monad.S) : sig
  val constructors : t -> f:(tl_constructor -> tl_constructor M.t) -> t M.t
  (** Note: [f] must not change [c_ref] and [c_result_type]
      to keep all references working *)

  val functions : t -> f:(tl_function -> tl_function M.t) -> t M.t
end

val fold_types : t -> init:'a -> f:('a -> int * tl_type -> 'a) -> 'a
val fold_constructors : t -> init:'a -> f:('a -> tl_constructor -> 'a) -> 'a
val fold_functions : t -> init:'a -> f:('a -> tl_function -> 'a) -> 'a
