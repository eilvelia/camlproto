open! Base
open Types

type t

exception TLStoreError of string

val empty: unit -> t

val insert_func: tl_combinator -> t -> t

val insert_constr: tl_combinator -> t -> t

val insert_type: tl_type -> t -> t

val get_comb_by_id: string -> t -> tl_combinator option

val get_comb_by_id_exn: string -> t -> tl_combinator

val get_comb_by_magic: int32 -> t -> tl_combinator option
