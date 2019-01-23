open! Base
open Types

(* module ComparableCombinator = struct
  module T = struct
    type t = tl_combinator
    let compare t1 t2 = Int32.compare t1.magic t2.magic
    let sexp_of_t (t: t): Sexp.t = Sexp.Atom t.id
  end
  include T
  include Comparator.Make(T)
end *)

type lookupId = (string, tl_combinator, String.comparator_witness) Map.t
type lookupMagic = (int32, tl_combinator, Int32.comparator_witness) Map.t

(* TL Store *)
type t = {
  types: tl_type list;
  functions: tl_combinator list;
  constructors: tl_combinator list;
  lookupId: lookupId;
  lookupMagic: lookupMagic;
}

exception TLStoreError of string

let empty (): t = {
  types = [];
  functions = [];
  constructors = [];
  lookupId = Map.empty (module String);
  lookupMagic = Map.empty (module Int32);
}

let update_lookups comb t =
  let lookupId = match Map.add t.lookupId ~key:comb.id ~data:comb with
    | `Ok x -> x
    | `Duplicate -> raise @@ TLStoreError (comb.id ^ " is already declared")
  in
  let lookupMagic = match Map.add t.lookupMagic ~key:comb.magic ~data:comb with
    | `Ok x -> x
    | `Duplicate -> raise
      @@ TLStoreError ("Magic duplication: " ^ Int32.to_string comb.magic)
  in
  { t with lookupId; lookupMagic }

let insert_func comb t =
  if phys_equal comb.kind TLConstructor
    then raise @@ TLStoreError "Not a function";
  update_lookups comb { t with functions = comb :: t.functions }

let insert_constr comb t =
  if phys_equal comb.kind TLFunction
    then raise @@ TLStoreError "Not a constructor";
  update_lookups comb { t with constructors = comb :: t.constructors }

let insert_type typ t =
  { t with types = typ :: t.types }

let get_comb_by_id id t = Map.find t.lookupId id

let get_comb_by_id_exn id t = Map.find_exn t.lookupId id

let get_comb_by_magic magic t = Map.find t.lookupMagic magic
