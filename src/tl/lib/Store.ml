open! Base
open Types

module ComparableConstrRef = struct
  type t = tl_constr_ref
  include Comparable.Make(struct
      type nonrec t = t
      let compare = compare_tl_constr_ref
      let sexp_of_t (ConstrRef d) = Int.sexp_of_t d
  end)
end

type t = {
  types_by_ref: tl_type Map.M(Int).t;
  types_by_name: (int * tl_type) Map.M(Ast.ComparableName).t;
  constructors_by_ref: tl_constructor Map.M(ComparableConstrRef).t;
  constructors_by_name: tl_constructor Map.M(Ast.ComparableName).t;
  constructors_by_result: tl_constructor list Map.M(Int).t;
  functions_by_name: tl_function Map.M(Ast.ComparableName).t;
}

let show t =
  let ts = Map.to_alist t.types_by_name
    |> List.map ~f:(fun (_, (_, ty)) -> show_tl_type ty) in
  let cs = Map.to_alist t.constructors_by_name
    |> List.map ~f:(fun (_, c) -> show_tl_constructor c) in
  let fs = Map.to_alist t.functions_by_name
    |> List.map ~f:(fun (_, f) -> show_tl_function f) in
  let b = Buffer.create 1024 in
  let add_list : string list -> unit =
    let f x = Buffer.add_string b x; Buffer.add_char b '\n' in
    List.iter ~f
  in
  Buffer.add_string b "Types:\n";
  add_list ts;
  Buffer.add_string b "Constructors:\n";
  add_list cs;
  Buffer.add_string b "Functions:\n";
  add_list fs;
  Buffer.contents b

let insert_type t ty =
  let ref = Map.length t.types_by_ref in
  let Type (name, _) = ty in
  let t' = { t with
    types_by_ref = Map.add_exn t.types_by_ref ~key:ref ~data:ty;
    types_by_name = Map.add_exn t.types_by_name ~key:name ~data:(ref, ty);
  } in
  (t', ref)

let insert_constructor t (_, c' as c) =
  let f = function Some xs -> c :: xs | None -> [c] in
  match Map.add t.constructors_by_name ~key:(snd c'.c_id) ~data:c with
  | `Ok constructors_by_name ->
    Some { t with
      constructors_by_name;
      constructors_by_result =
        Map.update t.constructors_by_result c'.c_result_type ~f;
    }
  | `Duplicate -> None

let insert_function t (_, f' as f) =
  let key = snd f'.f_id in
  match Map.add t.functions_by_name ~key ~data:f with
  | `Ok v -> Some { t with functions_by_name = v }
  | `Duplicate -> None

let insert_constructor_exn t f =
  match insert_constructor t f with
  | Some x -> x
  | None -> failwith "the constructor already exists"

let insert_function_exn t f =
  match insert_function t f with
  | Some x -> x
  | None -> failwith "the function already exists"

let get_type_by_name t name =
  Map.find t.types_by_name name

let get_type_by_ref t ref =
  Map.find_exn t.types_by_ref ref

let get_constructor_by_name t name =
  Map.find t.constructors_by_name name

let get_constructor_by_ref t ref =
  Map.find_exn t.constructors_by_ref ref

let get_function_by_name t name =
  Map.find t.functions_by_name name

let get_constructors_by_type_ref t tref =
  Option.value ~default:[] @@ Map.find t.constructors_by_result tref

module MapM (M: Monad.S) = struct
  open M
  open Let_syntax

  (* TODO: Unit tests for this *)

  type constr_tuple =
    tl_constructor list
    * tl_constructor Map.M(Ast.ComparableName).t
    * tl_constructor Map.M(ComparableConstrRef).t

  let constructors t ~f =
    let list_fold_fn : constr_tuple M.t -> tl_constructor -> constr_tuple M.t
    = fun acc c ->
      let%bind (cs, mname, mref) = acc in
      let%bind c' = f c in
      let mname' = Map.set mname ~key:(snd (snd c').c_id) ~data:c' in
      let mref' = Map.set mref ~key:(ConstrRef (snd c').c_ref) ~data:c' in
      let cs' = c' :: cs in
      return (cs', mname', mref')
    in
    let map_fold_fn ~key ~data acc =
      let%bind (mres, mname, mref) = acc in
      let init = return ([], mname, mref) in
      let%bind (data', mname', mref') = List.fold data ~init ~f:list_fold_fn in
      let mres' = Map.set mres ~key ~data:data' in
      return (mres', mname', mref')
    in
    let mres_init = Map.empty (module Int) in
    let mname_init = Map.empty (module Ast.ComparableName) in
    let mref_init = Map.empty (module ComparableConstrRef) in
    let init = return (mres_init, mname_init, mref_init) in
    let%map (mres, mname, mref) =
      Map.fold t.constructors_by_result ~init ~f:map_fold_fn in
    { t with
      constructors_by_ref = mref;
      constructors_by_result = mres;
      constructors_by_name = mname;
    }

  let functions t ~f =
    let init = return @@ Map.empty (module Ast.ComparableName) in
    let fold_fn ~key ~data mapm =
      let%bind map = mapm in
      let%bind data' = f data in
      return @@ Map.set map ~key ~data:data'
    in
    let%map fns = Map.fold t.functions_by_name ~init ~f:fold_fn in
    { t with functions_by_name = fns }
end

let[@inline] fold m init f =
  Map.fold m ~init ~f:(fun ~key:_ ~data acc -> f acc data)

let fold_types t ~init ~f = fold t.types_by_name init f
let fold_constructors t ~init ~f = fold t.constructors_by_name init f
let fold_functions t ~init ~f = fold t.functions_by_name init f

let empty = {
  types_by_ref = Map.empty (module Int);
  types_by_name = Map.empty (module Ast.ComparableName);
  constructors_by_ref = Map.empty (module ComparableConstrRef);
  constructors_by_name = Map.empty (module Ast.ComparableName);
  constructors_by_result = Map.empty (module Int);
  functions_by_name = Map.empty (module Ast.ComparableName);
}

let make ?(types = []) ?(constructors = []) ?(functions = []) () =
  let t = empty in
  let t = List.fold types ~init:t ~f:(fun a b -> fst @@ insert_type a b) in
  let t = List.fold constructors ~init:t ~f:insert_constructor_exn in
  let t = List.fold functions ~init:t ~f:insert_function_exn in
  t

let default =
  let l x = Loc.empty, x in
  let trepeat (a, b) = Loc.empty, TypeRepeat (a, b) in
  let texpr (a, b) = Loc.empty, TypeExpr (a, b) in
  let no_mod e = Loc.empty, Modified (None, e) in
  let tref (a, b) = TypeRef (Loc.empty, (a, b)) in
  let nvar v = Loc.empty, NatVar v in
  let tvar v = Loc.empty, TypeVar v in
  let name str = Loc.empty, Ast.Name str in
  make
  ~types:[
    Type (Name "Int", []);
    Type (Name "Long", []);
    Type (Name "Double", []);
    Type (Name "String", []);
    Type (Name "Bytes", []);
    Type (Name "Vector", [ParamType "t"]);
    Type (Name "Int128", []);
    Type (Name "Int256", []);
  ]
  ~constructors:[
    l { c_id = name "int"; c_magic = 0xa8509bdal; c_args = []; c_ref = 0;
      c_result_type = 0; c_builtin = true; };
    l { c_id = name "long"; c_magic = 0x22076cbal; c_args = []; c_ref = 1;
      c_result_type = 1; c_builtin = true; };
    l { c_id = name "double"; c_magic = 0x2210c154l; c_args = []; c_ref = 2;
      c_result_type = 2; c_builtin = true; };
    l { c_id = name "string"; c_magic = 0xb5286e24l; c_args = []; c_ref = 3;
      c_result_type = 3; c_builtin = true; };
    l { c_id = name "bytes"; c_magic = 0xe937bb82l; c_args = []; c_ref = 4;
      c_result_type = 4; c_builtin = false; };
    l { c_id = name "vector"; c_magic = 0x1cb5c415l; c_args = [
      l { arg_id = Some (l "t"); arg_ref = 0; arg_cond = None;
        arg_type = no_mod @@ texpr (tref (0, None), []);
        arg_opt = Some `ResultBang };
      l { arg_id = None; arg_ref = 1; arg_cond = None;
        arg_type = no_mod @@ texpr (tref (1, Some (ConstrRef 0)), []);
        arg_opt = None };
      l { arg_id = None; arg_ref = 2; arg_cond = None;
        arg_type = no_mod @@ trepeat (nvar (VarRef (1, None)), [l {
            r_arg_id = None;
            r_arg_type = tvar (VarRef (0, None));
          }]);
        arg_opt = None };
      ];
      c_ref = 5;
      c_result_type = 5; c_builtin = false; };
    l { c_id = name "int128"; c_magic = 0x84ccf7b7l; c_args = []; c_ref = 6;
      c_result_type = 6; c_builtin = false; };
    l { c_id = name "int256"; c_magic = 0x7bedeb5bl; c_args = []; c_ref = 7;
      c_result_type = 7; c_builtin = false; };
  ]
  ~functions:[]
  ()

let default_constr_ref = Map.length default.constructors_by_name

let%expect_test "show @@ make ()" =
  Caml.print_endline @@ show @@ make ();
  [%expect {|
    Types:
    Constructors:
    Functions: |}]
