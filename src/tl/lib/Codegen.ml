open! Base
open Types

let fix_keyword_conflict name =
  match name with
  | "and"
  | "as"
  | "assert"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "lazy"
  | "let"
  | "match"
  | "method"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with"
  | "mod"
  | "land"
  | "lor"
  | "lxor"
  | "lsl"
  | "lsr"
  | "asr" -> name ^ "_"
  | _ -> name

let is_disallowed_constr : Ast.name -> bool = function
  | Name ( "nat"
         | "int"
         | "long"
         | "double"
         | "string"
         | "bytes"
         | "vector"
         | "boolTrue"
         | "boolFalse"
         | "int128"
         | "int256") -> true
  | _ -> false

let is_disallowed_type : Ast.name -> bool = function
  | Name ( "Type"
         | "Int"
         | "Long"
         | "Double"
         | "String"
         | "Bytes"
         | "Vector"
         | "Bool") -> true
  | _ -> false

let constr_no_args = "E"

let convert_ast_name : Ast.name -> string = function
  | Name n -> n
  | NameNs (ns, n) -> ns ^ "_" ^ n

let convert_name_with_loc : name_with_loc -> string = fun (_, n) ->
  convert_ast_name n

let convert_type_ident (_, tid) =
  match tid with
  | TypeIdBoxed n
  | TypeIdBare n -> convert_ast_name n

let convert_tl_type (Type (name, _)) =
  convert_ast_name name

let convert_comb_name name =
  "TL_" ^ name

let convert_type_name name =
  "TLT_" ^ name

let convert_arg_name name =
  String.uncapitalize @@ fix_keyword_conflict name

let build_type store module_kind ppf (tref, ty) =
  let tname = convert_type_name @@ convert_tl_type ty in
  let tlconstrs = Store.get_constructors_by_type_ref store tref in
  assert (Option.is_some @@ List.hd tlconstrs);
  let f (gcs, ges, gds) (_, { c_magic; c_id; _ }) =
    let magic = c_magic in
    let cname = convert_comb_name @@ convert_name_with_loc c_id in
    let gen_constr ppf () =
      Fmt.pf ppf "| %s of %s.t" cname cname in
    let gen_encode ppf () =
      Fmt.pf ppf "| %s x -> %s.encode_boxed enc x" cname cname in
    let gen_decode ppf () =
      Fmt.pf ppf "| 0x%08lxl -> %s (%s.decode dec)" magic cname cname in
    (gen_constr :: gcs, gen_encode :: ges, gen_decode :: gds)
  in
  let init = ([], [], []) in
  let fmt_tuple = List.fold tlconstrs ~init ~f in
  let[@inline] map3 (a, b, c) f = (f a, f b, f c) in
  let (fmt_constrs, fmt_encodes, fmt_decodes) =
    map3 fmt_tuple (fun x -> Fmt.concat @@ List.rev x) in
  let module_kind_str = match module_kind with
    | `Module -> "module"
    | `ModuleRec -> "module rec"
    | `And -> "and"
  in
  let pp = Fmt.pf ppf "@[\
    @[<v 2>\
    %s %s : sig@,\
      @[<v 2>\
      type%s t =@,\
        @[<v>%a@]\
      @]@,\
      include TLType with type t := t\
    @]@,\
    @[<v 2>\
    end = struct@,\
      @[<v 2>\
      type%s t =@,\
        @[<v>%a@]\
      @]@,\
      type tl_type@,\
      @[<v 2>\
      let encode enc t = match t with@,\
        @[<v>%a@]\
      @]@,\
      @[<v 2>\
      let decode dec =@,\
        let magic = Decoder.read_int32_le dec in@,\
        match magic with@,\
        @[<v>%a@]@,\
        | x -> raise (DeserializationError x)\
      @]\
    @]@,\
    end@]"
  in
  let unboxed = match tlconstrs with
    | [_] -> " [@unboxed]"
    | _ -> ""
  in
  pp
    module_kind_str
    tname
    unboxed
    fmt_constrs ()
    unboxed
    fmt_constrs ()
    fmt_encodes ()
    fmt_decodes ()
;;

let build_var_ref ppf (vref : tl_var_ref) =
  match vref with
  | VarRef (_, Some name) -> Fmt.string ppf @@ fix_keyword_conflict name
  | VarRef (_, None) ->
    failwith "codegen: Unnamed var refs are not supported yet"
  | VarRefUnd name -> Fmt.string ppf @@ fix_keyword_conflict name

let build_type_ref store ppf (tref : tl_type_ref) =
  let name = match tref with
  | TypeRef (_, (r, None)) ->
    convert_tl_type @@ Store.get_type_by_ref store r
  | TypeRef (_, (_, Some cr)) ->
    let _, { c_id; _ } = Store.get_constructor_by_ref store cr in
    convert_ast_name @@ snd c_id
  | TypeRefUnd id -> convert_type_ident id
  | TypeRefBuiltin builtin -> show_builtin_type builtin
  in
  Fmt.string ppf @@ if is_boxed_type_ref tref
  then convert_type_name name
  else convert_comb_name name

let rec build_expr store ppf (expr : tl_expr) =
  match expr with
  | ExprType e -> build_type_expr store ppf e
  | ExprNat e -> build_nat_expr ppf e

and build_type_expr store ppf (_, texpr : tl_type_expr) =
  match texpr with
  | TypeExpr (tref, tparams) ->
    let fmt_tparams = Fmt.list ~sep:Fmt.nop (build_expr store) in
    Fmt.pf ppf "@[<h>%a%a@]" (build_type_ref store) tref fmt_tparams tparams
  | TypeVar vref -> build_var_ref ppf vref
  | TypeRepeat _ -> failwith "codegen: Type repetitions are not supported yet"
  | TypeBare _ -> failwith "codegen: Bare modifiers should be already resolved"

and build_nat_expr ppf (_, nexpr : tl_nat_expr) : unit =
  match nexpr with
  | NatConst n -> Fmt.int ppf n
  | NatVar vref -> build_var_ref ppf vref
  | NatPlus _ -> failwith "codegen: The `+` modifier is not supported yet"

let build_module_name store ppf (e: tl_type_expr) =
  build_type_expr store ppf e

type comb = [ `Constr of tl_constructor | `Func of tl_function ]

let get_comb_id = function
    `Constr (_, c') -> c'.c_id | `Func (_, f') -> f'.f_id
let get_comb_args = function
    `Constr (_, c') -> c'.c_args | `Func (_, f') -> f'.f_args
let get_comb_magic = function
    `Constr (_, c') -> c'.c_magic | `Func (_, f') -> f'.f_magic

let my_pp_list ?(leader = Fmt.nop) ?sep ?(trailer = Fmt.nop) pp ppf = function
  | [] -> ()
  | _ :: _ as xs ->
    leader ppf ();
    Fmt.list ?sep pp ppf xs;
    trailer ppf ()

let build_comb store : _ -> comb Fmt.t =
  let pp_arg_name ppf a' = match a'.arg_id with
    | Some (_, n) -> Fmt.string ppf @@ convert_arg_name n
    (* TODO: *)
    | None -> failwith "codegen: Unnamed args are not supported"
  in
  let pp_locarg_name ppf a = pp_arg_name ppf (snd a) in
  let pp_module_name ppf a' =
    build_module_name store ppf (unwrap_modified a'.arg_type)
  in
  let pp_functor_param ppf e = Fmt.pf ppf "(%a)" (build_expr store) e in
  let pp_functor_appl ~sep ppf (tref, params) =
    Fmt.pf ppf "%a%a%a"
      (build_type_ref store) tref
      sep ()
      (Fmt.list ~sep pp_functor_param) params
  in
  let pp_module_texpr ~sep ppf texpr =
    match texpr with
    | _, TypeExpr (tref, params) -> pp_functor_appl ~sep ppf (tref, params)
    | _ -> build_type_expr store ppf texpr
  in
  let pp_module_decl ppf texpr =
    match texpr with
    | _, TypeExpr (tref, params) ->
      Fmt.pf ppf "module %a = %a"
        (build_module_name store) texpr
        (pp_functor_appl ~sep:(Fmt.any " ")) (tref, params)
    | _ -> ()
  in
  let pp_module_decls ppf comb : unit =
    let args = get_comb_args comb in
    let exprs = List.map args ~f:(fun (_, a') -> a'.arg_type) in
    let exprs = match comb with
      | `Constr _ -> exprs
      | `Func (l, f') -> exprs @ [l, Modified (Some ModBang, f'.f_result_type)]
    in
    let f = function
      | _, Modified (_m, (_, TypeExpr (_, _ :: _) as e)) -> Some e
      | _ -> None
    in
    let exprs = List.filter_map exprs ~f in
    let exprs = List.dedup_and_sort exprs ~compare:compare_tl_type_expr in
    my_pp_list pp_module_decl ~trailer:Fmt.cut ppf exprs
  in
  let pp_result_module ?(is_sig = false) ppf (_, f') =
    let pp_m = if is_sig
      then pp_module_texpr ~sep:Fmt.nop
      else build_module_name store
    in
    match f'.f_result_type with
    | _, TypeVar _ as e ->
      Fmt.pf ppf "module ResultM = %a.ResultM" pp_m e
    | e ->
      Fmt.pf ppf "module ResultM = %a" pp_m e
  in
  let pp_field ?(is_sig = false) ppf (_, a') =
    let opt = match a'.arg_cond with
      | Some _ -> " option"
      | None -> ""
    in
    let [@inline] struct_field () : unit =
      Fmt.pf ppf "%a : %a.t%s;" pp_arg_name a' pp_module_name a' opt
    in
    let [@inline] sig_field () : unit =
      Fmt.pf ppf "%a : %a.t%s;"
        pp_arg_name a'
        (pp_module_texpr ~sep:Fmt.nop) (unwrap_modified a'.arg_type)
        opt
    in
    if is_sig then sig_field () else struct_field ()
  in
  let pp_arg_decode ppf (_, a') =
    match a'.arg_cond with
    | Some (_, Cond (vref, bit)) ->
      Fmt.pf ppf
        "@[<h>let %a = if %a land (1l lsl %d) <> 0l \
        then Some (%a.decode dec) \
        else None in@]"
        pp_arg_name a' build_var_ref vref bit pp_module_name a'
    | None ->
      Fmt.pf ppf "@[<h>let %a = %a.decode dec in@]" pp_arg_name a' pp_module_name a'
  in
  let pp_decode_end ppf args =
    match args with
    | [] ->
      Fmt.string ppf constr_no_args
    | _ :: _ as xs ->
      Fmt.pf ppf "{ @[<h>%a@] }" Fmt.(list ~sep:semi pp_locarg_name) xs
  in
  let pp_arg_encodes ppf (rq_args, cond_args_map) =
    let pp_cond_arg ppf (a', bits) =
      let pp_bit ppf (a'', bit) =
        Fmt.pf ppf
          "match t.%a with Some _ -> %a := !%a lor (1l lsl %d) | None -> ();"
          pp_arg_name a'' pp_arg_name a' pp_arg_name a' bit
      in
      Fmt.pf ppf "\
          let %a = ref 0l in@,\
          @[<v>%a@]@,\
          %a.encode enc !%a;"
        pp_arg_name a'
        (Fmt.list pp_bit) bits
        pp_module_name a'
        pp_arg_name a'
    in
    let pp_common_arg ppf a' =
      match a'.arg_cond with
      | None -> Fmt.pf ppf "%a.encode enc t.%a;" pp_module_name a' pp_arg_name a'
      | Some _ ->
        Fmt.pf ppf "match t.%a with Some x -> %a.encode enc x \
                    | None -> ();"
          pp_arg_name a' pp_module_name a'
    in
    let pp ppf (_, a') =
      let arg_name = Fmt.to_to_string pp_arg_name a' in
      match Map.find cond_args_map arg_name with
      | Some v -> pp_cond_arg ppf (a', v)
      | None -> pp_common_arg ppf a'
    in
    my_pp_list pp ~trailer:Fmt.cut ppf rq_args
  in
  let pp_functor_arg ppf (l, a') =
    let ty = function
      | `ResultBang -> "TLAnyType"
      | `ArgBang -> "TLFunc"
    in
    let f opt =
      let arg_id = Option.map a'.arg_id ~f:snd in
      let e = l, TypeVar (VarRef (a'.arg_ref, arg_id)) in
      Fmt.pf ppf "(%a : %s)" (build_module_name store) e (ty opt)
    in
    Option.iter a'.arg_opt ~f
  in
  fun module_kind ppf comb ->
    let cname =
      convert_comb_name @@ convert_name_with_loc @@ get_comb_id comb in
    let args = get_comb_args comb in
    let module_kind_str = match module_kind with
      | `Module -> "module"
      | `ModuleRec -> "module rec"
      | `And -> "and"
    in
    (* TODO: Add `[@inline]` to the `magic` function? *)
    let magic = get_comb_magic comb in
    (* TODO: Consider at least using ppx_string_interpolation? *)
    let pp = Fmt.pf ppf "@[\
      @[<v 2>\
      %s %s @[<h>%a@]: sig@,\
        @[<v>type t = %t@]@,\
        include %s with type t := t%a\
      @]@,\
      @[<v 2>\
      end = struct@,\
        @[<v>%a@]\
        @[<v>type t = %t@]@,\
        %a\
        type %s@,\
        let magic () = 0x%08lxl@,\
        @[<v 2>\
        let encode enc t =@,\
          %t\
          @[<v>%a@]\
          ()\
        @]@,\
        %t\
        @[<v 2>\
        let decode dec =@,\
          @[<v>%a@]\
          @[<h>%a@]\
        @]\
      @]@,\
      end@]"
    in
    let required_args, optional_args = List.partition_tf args ~f:(
        fun (_, a') -> Option.is_none a'.arg_opt)
    in
    let cond_args_map =
      let f acc (_, a') =
        match a'.arg_cond with
        | Some (_, Cond (vref, bit)) ->
          let f = function Some xs -> (a', bit) :: xs | None -> [a', bit] in
          Map.update acc (Fmt.to_to_string build_var_ref vref) ~f
        | _ -> acc
      in
      List.fold required_args ~init:(Map.empty (module String)) ~f
    in
    let noncond_args = List.filter required_args ~f:(fun (_, a') ->
        let arg_name = Fmt.to_to_string pp_arg_name a' in
        Option.is_none @@ Map.find cond_args_map arg_name
      ) in
    let pp_t ?is_sig ppf = match noncond_args with
      | [] -> Fmt.string ppf constr_no_args
      | _ :: _ as xs ->
        Fmt.pf ppf "{@;<0 2>@[<v>%a@]@,}"
          (Fmt.list @@ pp_field ?is_sig) xs
    in
    let include_module_type = match comb with
      | `Constr _ -> "TLConstr"
      | `Func _ -> "TLFunc"
    in
    let type_marker = match comb with
      | `Constr _ -> "tl_constr"
      | `Func _ -> "tl_func"
    in
    let encode_start ppf = match comb with
      | `Constr _ -> ()
      | `Func _ -> Fmt.pf ppf "Encoder.add_int32_le enc 0x%08lxl;@," magic
    in
    let encode_boxed ppf =
      match comb with
      | `Constr _ -> Fmt.pf ppf
        "let encode_boxed enc t = Encoder.add_int32_le enc 0x%08lxl; encode enc t@,"
        magic
      | `Func _ -> ()
    in
    let result_module ?(trailer = Fmt.nop) ppf comb = (Fmt.using
        (function `Constr _ -> None | `Func f -> Some f)
        (Fmt.(option @@ pp_result_module ++ trailer))) ppf comb
    in
    let open Fmt in
    pp
      module_kind_str
      cname
      (my_pp_list pp_functor_arg ~trailer:sp) optional_args
      (pp_t ~is_sig:true)
      include_module_type
      (match comb with
       | `Constr _ -> Fmt.any ""
       | `Func f -> fun ppf _ -> Fmt.pf ppf " and %a"
           (pp_result_module ~is_sig:true) f) ()
      pp_module_decls comb
      pp_t
      (result_module ~trailer:cut) comb
      type_marker
      magic
      encode_start
      pp_arg_encodes (required_args, cond_args_map)
      encode_boxed
      (my_pp_list pp_arg_decode ~trailer:cut) required_args
      pp_decode_end noncond_args

let cut2 = Fmt.(cut ++ cut)

let build_chain store ppf (trefs : tl_type_ref list) =
  (* TODO: Doesn't work with functors *)
  let trefs' = List.filter_map trefs ~f:(function
      | TypeRef (_, pair) -> Some pair
      | TypeRefUnd _ | TypeRefBuiltin _ -> None
    ) in
  let objs = List.filter_map trefs' ~f:(function
    | (refi, None) ->
        let Type (name, _) as ty = Store.get_type_by_ref store refi in
        if is_disallowed_type name then None else Some (`Type (refi, ty))
    | (_, Some cref) ->
        let (_, c') as c = Store.get_constructor_by_ref store cref in
        if is_disallowed_constr (snd c'.c_id) then None else Some (`Constr c)
  ) in
  let build k ppf = function
    | `Type pair -> build_type store k ppf pair
    | `Constr _ as c -> build_comb store k ppf c
  in
  match objs with
  | [] -> () (* TODO: do something? *)
  | [x] -> build `Module ppf x
  | x :: xs ->
    Fmt.pf ppf "@[<v>%a%a@]"
      (build `ModuleRec) x
      (my_pp_list ~leader:cut2 ~sep:cut2 @@ build `And) xs

module TLGraph = Graph.Make(TypeRefCmp)

(** Not tail-recursive *)
let rec get_texpr_refs e = Loc.value_map_annot e @@ function
  | TypeExpr (ref, exprs) -> ref :: List.concat_map exprs ~f:get_expr_refs
  | TypeRepeat (_nexpr, rargs) ->
    let f (_, rarg') = get_texpr_refs rarg'.r_arg_type in
    List.concat_map rargs ~f
  | TypeVar _ -> []
  | TypeBare e' -> get_texpr_refs e'

and get_expr_refs = function
  | ExprNat _e -> []
  | ExprType e -> get_texpr_refs e

let create_graph (cs : tl_constructor list) : TLGraph.t =
  let get_arg_refs args =
    let f (_, a') = get_texpr_refs @@ unwrap_modified a'.arg_type in
    List.concat_map args ~f
  in
  let f g (_, c') =
    let c_ref =
      TypeRef (Loc.empty, (c'.c_result_type, Some (ConstrRef c'.c_ref))) in
    let g = TLGraph.add_vertex g c_ref in
    let result_ref = TypeRef (Loc.empty, (c'.c_result_type, None)) in
    let g = TLGraph.add_vertex g result_ref in
    let g = TLGraph.add_edge g c_ref result_ref in
    let f g_acc ref =
      let g_acc = TLGraph.add_vertex g_acc ref in
      let g_acc = TLGraph.add_edge g_acc ref c_ref in
      g_acc
    in
    List.fold (get_arg_refs c'.c_args) ~init:g ~f
  in
  List.fold cs ~init:TLGraph.empty ~f

let build : Store.t Fmt.t = fun ppf store ->
  let filter_map fold ~f =
    let g acc x = match f x with Some x' -> x' :: acc | None -> acc in
    fold store ~init:[] ~f:g
  in
  let map fold ~f = fold store ~init:[] ~f:(fun a x -> f x :: a) in
  (* let types = *)
  (*   let f (_ref, ty' as ty) = *)
  (*     Option.some_if (not @@ is_disallowed_type @@ convert_tl_type ty') ty in *)
  (*   filter_map Store.fold_types ~f *)
  (* in *)
  let constrs =
    let f (_, c' as c) =
      (* Option.some_if (not @@ is_disallowed_type @@ snd c'.c_id) (`Constr c) in *)
      Option.some_if (not @@ is_disallowed_constr @@ snd c'.c_id) c in
    filter_map Store.fold_constructors ~f
  in
  let funcs = map Store.fold_functions ~f:(fun f -> `Func f) in
  let graph = create_graph constrs in
  let components = TLGraph.tarjan graph in
  let pp = Fmt.pf ppf "\
(* AUTOGENERATED *)

open! TLRuntime
open! Types
open! Builtin
open! I32

[%@%@%@warning \"-26-27-33\"]
@
@[<v>%a@]

(* -- Functions -- *)
@
@[<v>%a@]\
" in
  let open Fmt in
  pp
    (* (list ~sep:cut2 @@ build_comb store `Module) constrs *)
    (* (list ~sep:cut2 @@ build_type store `Module) types *)
    (list ~sep:cut2 @@ build_chain store) components
    (list ~sep:cut2 @@ build_comb store `Module) funcs

let run = Fmt.to_to_string build
