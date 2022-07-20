open! Base
open Types
open Err.Check

(* Note: Some parts of this file were based on
  https://github.com/Termina1/tlhydra/blob/937ac9742a/TL/Typechecker/Typechecker.idr *)

(* TODO: Add Object as a builtin type? *)

(* Checker State *)
module CState = struct
  type t = {
    errors : Err.Check.t list;
    store : Store.t;
    args : tl_arg list;
    var_ref : int;
  }

  let default = {
    errors = [];
    store = Store.default;
    args = [];
    var_ref = 0;
  }

  let add_error t e = { t with errors = e :: t.errors }
end

(* TODO: Use 'monads' from BAP? *)
module CheckerM : sig
  [@warning "-32"] (* Disable the 'unused' warning *)
  type state = CState.t
  type +'a t
  val run_state  : 'a t -> state -> ('a * state)
  val eval_state : 'a t -> state -> 'a
  val exec_state : 'a t -> state -> state
  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val default : unit t
  include Monad.S with type 'a t := 'a t
end = struct
  type state = CState.t
  type 'a t = state -> ('a * state)
  let[@inline] run_state t s = t s
  let[@inline] eval_state t s = fst (t s)
  let[@inline] exec_state t s = snd (t s)
  let get = fun s -> (s, s)
  let[@inline] put s = fun _ -> ((), s)
  let[@inline] modify f = fun s -> ((), f s)
  let default = put CState.default
  include Monad.Make(struct
    type nonrec 'a t = 'a t
    let[@inline] return a = fun s -> (a, s)
    let bind t ~f = fun s -> let (b, s') = t s in (f b) s'
    let map' t ~f = fun s -> let (b, s') = t s in (f b, s')
    let map = `Custom map'
  end)
end

module StoreCheckerMapM = Store.MapM(CheckerM)

open CheckerM
open CheckerM.Let_syntax

let add_error e = modify (fun s -> CState.add_error s e)

let generate_var_ref : int CheckerM.t =
  let%bind { var_ref; _ } = get in
  let%bind () = modify (fun s -> { s with var_ref = var_ref + 1 }) in
  return var_ref

let get_var_by_name name : tl_arg option CheckerM.t =
  let f (_, a) = match a.arg_id with
    | Some (_, n) -> String.(n = name)
    | None -> false
  in
  let%bind { args; _ } = get in
  return @@ List.find args ~f

let get_var n =
  let%map { args; _ } = get in
  List.find_exn args ~f:(fun (_, a) -> a.arg_ref = n)

let convert_ident (i: Ast.ident): tl_type_ident = Loc.map_annot i @@ function
  | IdBoxed n -> TypeIdBoxed n
  | IdBare n -> TypeIdBare n

let check_cond (loc, CondDef ((nameloc, name), bit_opt): Ast.cond_def) =
  let%bind bit = match bit_opt with
    | None ->
      let%map () = add_error (loc, UnsupportedEmptyBit) in
      0
    | Some (l, bit) ->
      let%map () = if bit > 32 then add_error (l, CondMoreThan32) else return () in
      bit
  in
  match%bind get_var_by_name name with
  | None ->
    let%map () = add_error (nameloc, UndefinedVar name) in
    nameloc, Cond (VarRefUnd name, bit)
  | Some (l, a) ->
    return (l, Cond (VarRef (a.arg_ref, Some name), bit))

let is_valid_nat_var (l, arg) =
  match arg with
  | { arg_cond = Some _; _ } ->
    let%map () = add_error (l, NatVarCantBeCond) in false
  | { arg_type = _, Modified (None, (_, TypeExpr (TypeRefBuiltin BuiltinNat, []))); _ } ->
    return true
  | _ -> return false

let check_var_with_nat_type name =
  match%bind get_var_by_name name with
  | Some (_, v' as v) ->
    if%map is_valid_nat_var v then Some (VarRef (v'.arg_ref, Some name)) else None
  | None -> return None

let min_check_type (tconstr: tl_type_ident) tparams : tl_type_expr =
  match tconstr, tparams with
  | (l, TypeIdBoxed (Name "Type")), [] -> l, TypeExpr (ref_Type, [])
  | (l, TypeIdBare (Name "nat")), [] -> l, TypeExpr (ref_nat, [])
  | (l, _), _ -> l, TypeExpr (TypeRefUnd tconstr, tparams)

(* ;-; *)
let can_be_var_ref = function
  | "long" -> false
  | _ -> true

let check_ident (_, id' as id: Ast.ident) =
  let IdBoxed name | IdBare name = id' in
  match name with
  | Name name_str when can_be_var_ref name_str ->
    begin match%map get_var_by_name name_str with
    | Some (l, var) -> l, TypeVar (VarRef (var.arg_ref, Some name_str))
    | None -> min_check_type (convert_ident id) []
    end
  | NameNs _ | Name _ -> return @@ min_check_type (convert_ident id) []

let rec check_nat_expr (l, e': Ast.expr): tl_nat_expr option CheckerM.t =
  match e' with
  | ENat n -> return @@ Some (l, NatConst n)
  | EIdent (_, IdBoxed (Name name) | _, IdBare (Name name)) ->
    let%map ref_opt = check_var_with_nat_type name in
    Option.(ref_opt >>| fun r -> l, NatVar r)
  | EAppl (x, []) -> check_nat_expr x
  | EOperator ((_, OpPlus), _xs) -> return None (* TODO: *)
  | _ -> return None

let check_type_constr (l, e': Ast.expr) =
  match e' with
  | EIdent id -> return @@ convert_ident id
  | _ ->
    let%map () = add_error (l, InvalidTypeConstr) in
    l, TypeIdBare (Name "invalid")

let rec check_type_expr (l, e': Ast.expr) =
  let op_invalid_args_num op exp got =
    let%map () = add_error (l, WrongNumberOfOperands (op, exp, got)) in
    l, TypeExpr (TypeRefUnd (l, TypeIdBare (Name "invalid")), [])
  in
  match e' with
  | ENat n ->
    let%map () = add_error (l, NatIsNotTypeExpr n) in
    l, TypeExpr (ref_nat, [])
  | EIdent id -> check_ident id
  | EOperator ((_, OpBare), [e]) -> check_type_expr e >>| fun e' -> l, TypeBare e'
  | EOperator ((_, OpBare), xs) -> op_invalid_args_num "%" 1 (List.length xs)
  | EOperator ((_, OpBang), _) -> failwith "`!` modifier is not allowed here"
  | EOperator ((_, OpPlus), [_a; _b]) -> failwith "plus operator" (* TODO: *)
  | EOperator ((_, OpPlus), xs) -> op_invalid_args_num "+" 2 (List.length xs)
  | EAppl (x, []) -> check_type_expr x
  | EAppl ((_, EOperator ((_, OpBare), [x])), xs) ->
    let%map e = check_type_expr (l, EAppl (x, xs)) in
    l, TypeBare e
  | EAppl (x, xs) ->
    let%bind tconstr = check_type_constr x in
    let%bind tparams = all @@ List.map xs ~f:check_expr in
    return @@ min_check_type tconstr tparams
  | EMultiArg (mult_expr_opt, args) ->
    let%bind multiplicity = match mult_expr_opt with
      | Some (l', _ as e) ->
        begin match%bind check_nat_expr e with
        | Some e' -> return e'
        | None ->
          let%map () = add_error (l', RepeatMultIsNotNatExpr) in
          l', NatConst 1
        end
      | None ->
        let%bind { args = prev_args; _ } = get in
        begin match prev_args with
        | (_, arg' as arg) :: _ ->
          if%bind is_valid_nat_var arg
          then return (l, NatVar (VarRef (arg'.arg_ref, None)))
          else
            let name = Option.(arg'.arg_id >>| snd) in
            let%map () = add_error (l, InvalidNatVar name) in
            l, NatConst 1
        | [] ->
          let%map () = add_error (l, RepeatWithoutMultAsFirstArg) in
          l, NatConst 1
        end
    in
    let%bind rargs = all @@ List.map args ~f:check_repeat_arg in
    return (l, TypeRepeat (multiplicity, rargs))

and check_expr (e: Ast.expr) =
  match%bind check_nat_expr e with
  | None -> let%map e' = check_type_expr e in ExprType e'
  | Some e' -> return @@ ExprNat e'

and check_repeat_arg (l, arg': Ast.arg) =
  let%bind () = match arg'.arg_cond_def with
    | Some (l, _) -> add_error (l, RepeatConditional)
    | None -> return ()
  in
  let%bind ty = check_type_expr arg'.arg_type in
  return (l, { r_arg_id = arg'.arg_id; r_arg_type = ty })

let check_modified_type_expr (e: Ast.expr) =
  match e with
  | l, EOperator ((_, OpBang), [a]) ->
    let%map expr = check_type_expr a in
    l, Modified (Some ModBang, expr)
  | l, _ ->
    let%map expr = check_type_expr e in
    l, Modified (None, expr)

let insert_arg (a: tl_arg): unit CheckerM.t =
  modify (fun s -> { s with args = a :: s.args })

(** Returns [true] if a duplicate is found; [false] otherwise. *)
let check_arg_duplicate (name_opt: tl_arg_id option) =
  match name_opt with
  | Some (loc, name') ->
    begin match%bind get_var_by_name name' with
      | Some _ -> let%map () = add_error (loc, DuplicateArg name') in true
      | None -> return false
    end
  | None -> return false

let check_arg (l, ({ arg_id; _ } as arg'): Ast.arg) =
  let%bind dup = check_arg_duplicate arg'.arg_id in
  if dup
  then return ()
  else begin
    let%bind arg_cond = match arg'.arg_cond_def with
      | Some c -> let%map c' = check_cond c in Some c'
      | None -> return None
    in
    let%bind arg_type = check_modified_type_expr arg'.arg_type in
    let%bind arg_ref = generate_var_ref in
    let arg_opt = None in
    let arg = l, { arg_id; arg_ref; arg_cond; arg_type; arg_opt } in
    insert_arg arg
  end

let check_opt_arg (l, arg': Ast.opt_arg) =
  let%bind dup = check_arg_duplicate (Some arg'.opt_arg_id) in
  if dup
  then return ()
  else begin
    let (type_loc, arg_type') = arg'.opt_arg_type in
    match arg_type' with
    | EIdent (_, IdBoxed (Name "Type") | _, IdBare (Name "nat")) ->
      let%bind arg_type = check_modified_type_expr arg'.opt_arg_type in
      let%bind arg_ref = generate_var_ref in
      let arg_id = Some arg'.opt_arg_id in
      let arg_opt = Some `ResultBang in
      let arg = l, { arg_id; arg_ref; arg_cond = None; arg_type; arg_opt } in
      insert_arg arg
    | _ ->
      add_error (type_loc, InvalidOptArgType)
  end

let rec check_type_param (l, param': Ast.expr) =
  match param' with
  | EIdent (idloc, _ as id) ->
    let name = Ast.gen_ident id in
    begin match%bind get_var_by_name name with
      | None ->
        let%map () = add_error (idloc, UndefinedVar name) in ParamType name
      | Some (_, { arg_type = tyloc, arg_type'; _ }) ->
        match arg_type' with
        | Modified (None, (_, TypeExpr (TypeRefBuiltin BuiltinType, []))) ->
          return @@ ParamType name
        | Modified (None, (_, TypeExpr (TypeRefBuiltin BuiltinNat, []))) ->
          return @@ ParamNat name
        | _ ->
          (* TODO: Better error message *)
          let%map () = add_error (tyloc, InvalidTypeParamType) in ParamType name
    end
  | EAppl (x, []) ->
    check_type_param x
  (* TODO: *)
  | _ ->
    let%map () = add_error (l, InvalidTypeParam) in ParamType "invalid"

let check_func_result_type (expr: Ast.expr) =
  let%bind l, texpr' as texpr = check_type_expr expr in
  match texpr' with
  | TypeExpr (ref, _) when is_boxed_type_ref ref -> return texpr
  | TypeExpr _ -> let%map () = add_error (l, BareResultType) in texpr
  | TypeVar _ -> return texpr
  | _ -> let%map () = add_error (l, InvalidResultType) in texpr

let check_constr_result_type (loc, expr' as expr: Ast.expr) =
  let params_comparator x y = match x, y with
    | ParamType _, ParamType _ -> true
    | ParamNat _, ParamNat _ -> true
    | _ -> false
  in
  let%bind { store; _ } = get in
  let expr' = match expr' with EIdent _ -> Ast.EAppl (expr, []) | e -> e in
  match expr' with
  | EAppl ((_, EIdent (_, IdBoxed tconstr')), params) ->
    let%bind tparams = all @@ List.map params ~f:check_type_param in
    begin match Store.get_type_by_name store tconstr' with
      | Some (ref, Type (_, tparams')) ->
        let%bind () =
          if List.equal params_comparator tparams tparams'
          then return ()
          (* TODO: Better error message *)
          else add_error (loc, WrongTypeParams)
        in
        return ref
      | None ->
        let (store', ref) = Store.insert_type store (Type (tconstr', tparams)) in
        let%bind () = modify (fun s -> { s with store = store' }) in
        return ref
    end
  | EAppl ((_, EIdent (idloc, IdBare _)), _) ->
    let%map () = add_error (idloc, BareResultType) in 0
  | _ ->
    let%map () = add_error (loc, InvalidResultType) in 0

(** Checks that all optional arguments are used at least once
    in the result type _not modified_ by `!` (implicit or explicit)
    or in arguments _modified_ by `!`.
    Reverses the [args] list. *)
let validate_opt_args (result_type: modified_type_expr') =
  let list_contains ls ~f =
    let rec go = function
      | [] -> false
      | x :: xs -> f x || go xs
    in
    go ls
  in
  let rec does_expr_contain_var ref (_, expr' : tl_type_expr) =
    let self = does_expr_contain_var ref in
    match expr' with
    | TypeExpr (_, ls) ->
      let f = function
        | ExprNat (_, NatVar (VarRef (r, _))) -> r = ref
        | ExprNat _ -> false
        | ExprType e -> self e
      in
      list_contains ls ~f
    | TypeRepeat (_, ls) ->
      list_contains (List.map ls ~f:(fun (_, a') -> a'.r_arg_type)) ~f:self
    | TypeVar (VarRef (r, _)) -> r = ref
    | TypeVar (VarRefUnd _) -> false
    | TypeBare e -> self e
  in
  let check ref args =
    let arg_in_result = match result_type with
      | Modified (Some ModBang, _) -> false
      | Modified (None, e) -> does_expr_contain_var ref e
    in
    let [@inline] arg_in_args () =
      let f : tl_arg -> bool = function
        | _, { arg_type = _, Modified (Some ModBang, e); _ } ->
          does_expr_contain_var ref e
        | _ -> false
      in
      list_contains args ~f
    in
    match () with
    | _ when arg_in_result -> Some `ResultBang
    | _ when arg_in_args () -> Some `ArgBang
    | _ -> None
  in
  let rec go ~f = function
    (* [xs] as second argument is needed, so [List.map] isn't sufficient *)
    | [] -> []
    | x :: xs -> f x xs :: go ~f xs
  in
  let f (l, a') args =
    let%map arg_opt =
      match a'.arg_opt with
      | None -> return None
      | Some _ as old ->
        begin match check a'.arg_ref args with
        | None -> let%map () = add_error (l, AmbiguousOptArg) in old
        | Some x -> return @@ Some x
        end
    in
    l, { a' with arg_opt }
  in
  let%bind { args = all_args; _ } = get in
  let%bind args' = all @@ go (List.rev all_args) ~f in
  modify (fun s -> { s with args = args' })

let check_combinator_decl (loc, comb' as comb: Ast.combinator_decl) section builtin =
  let%bind (id, magic) = match comb'.comb_id with
    | l, CombIdFull (id, magic_str) ->
      let magic = Int32.of_string ("0x" ^ magic_str) in
      return ((l, id), magic)
    | l, CombIdShort id ->
      let magic = Magic.calculate comb builtin in
      return ((l, id), magic)
    | l, CombIdEmpty ->
      let%map () = add_error (l, Unsupported "combinator without id") in
      let magic = Magic.calculate comb builtin in
      (l, Ast.Name "_"), magic
  in
  let id_str = Ast.gen_name (snd id) in
  let id_loc : Loc.t = fst id in
  let%bind () = modify (fun s -> { s with args = []; var_ref = 0 }) in
  let%bind () = all_unit @@ List.map comb'.comb_opt_args ~f:check_opt_arg in
  let%bind () = all_unit @@ List.map comb'.comb_args ~f:check_arg in
  let section, comb_result = match comb'.comb_result with
    | _, EOperator ((_, OpBang), [e]) -> Functions, e
    | e -> section, e
  in
  match section with
  | Types ->
    let%bind result_ref_i = check_constr_result_type comb_result in
    let%bind result_expr = check_type_expr comb_result in
    let%bind () = validate_opt_args @@ Modified (None, result_expr) in
    let%bind { args; store; _ } = get in
    let c = loc, {
      c_id = id;
      c_magic = magic;
      c_args = args;
      c_ref = Store.fresh_constr_ref store;
      c_result_type = result_ref_i;
      c_builtin = builtin;
    } in
    begin match Store.insert_constructor store c with
    | Some store -> modify (fun s -> { s with store })
    | None ->
      if Store.is_constr_defined_in_prelude c
      then return ()
      else add_error (id_loc, DuplicateConstr id_str)
    end
  | Functions ->
    let%bind result_expr = check_func_result_type comb_result in
    let%bind () = validate_opt_args @@ Modified (Some ModBang, result_expr) in
    let%bind { args; store; _ } = get in
    let f = loc, {
      f_id = id;
      f_magic = magic;
      f_args = args;
      f_result_type = result_expr;
      f_builtin = builtin;
    } in
    begin match Store.insert_function store f with
    | Some store -> modify (fun s -> { s with store })
    | None -> add_error (id_loc, DuplicateFunc id_str)
    end

let check_builtin_combinator_decl (l, comb': Ast.builtin_combinator_decl) section =
  let c = l, { Ast.
    comb_id = comb'.b_comb_id;
    comb_opt_args = [];
    comb_args = [];
    comb_result = fst comb'.b_comb_result, EIdent comb'.b_comb_result;
  } in
  check_combinator_decl c section true

let check_decl section (decl: Ast.decl) : unit CheckerM.t =
  match decl with
  | CombinatorDecl d -> check_combinator_decl d section false
  | BuiltinCombinatorDecl d -> check_builtin_combinator_decl d section
  | PartialApplicationDecl (loc, _) ->
    add_error (loc, Unsupported "Partial application")
  | FinalDecl (loc, _) ->
    add_error (loc, Unsupported "Final declaration") (* TODO: *)

let rec check_compatibility param expr =
  match param, expr with
  | ParamNat _, ExprNat _ -> return ()
  | ParamType _, ExprType (l, texpr') ->
    begin match texpr' with
    | TypeBare e -> check_compatibility param (ExprType e)
    | TypeVar (VarRef (refi, _)) ->
      let%bind (_, var') = get_var refi in
      begin match var'.arg_type with
      | _, Modified (None, (_, TypeExpr (TypeRefBuiltin BuiltinType, []))) ->
        return ()
      | _ ->
        (* TODO: Better error message *)
        add_error (l, IncompatError ("Type", show_modified_type_expr var'.arg_type))
      end
    | TypeVar (VarRefUnd _) -> return () (* Undefined refs are "unsafe" *)
    | TypeExpr _ -> return ()
    | TypeRepeat _ -> add_error (l, IncompatRepeat)
    end
  | ParamNat _, ExprType (l, _) -> add_error (l, IncompatParamNat)
  | ParamType _, ExprNat (l, _) -> add_error (l, IncompatParamType)

let assert_type_params tconstr params exprs =
  let paramslen = List.length params in
  let exprslen = List.length exprs in
  let rec go = function
    | x :: xs, y :: ys ->
      let%bind () = check_compatibility x y in
      go (xs, ys)
    | _, _ ->
      return ()
  in
  if paramslen <> exprslen
  then
    let loc = Loc.concat @@ fst tconstr :: List.map exprs ~f:get_expr_loc in
    add_error (loc,
               WrongNumberOfParams (gen_type_id tconstr, paramslen, exprslen))
  else go (params, exprs)

(** Applies type constructors *)
let check_type (l, tconstr' as tconstr : tl_type_ident) tparams =
  let locs = l :: List.map tparams ~f:get_expr_loc in
  let full_loc = Loc.concat locs in
  let%bind { store; _ } = get in
  match tconstr' with
  | TypeIdBoxed (Name "Type") ->
    let%map () = assert_type_params tconstr [] tparams in
    l, TypeExpr (ref_Type, [])
  | TypeIdBare (Name "nat") ->
    let%map () = assert_type_params tconstr [] tparams in
    l, TypeExpr (ref_nat, [])
  | TypeIdBare name ->
    begin match Store.get_constructor_by_name store name with
    | Some (_, { c_ref; c_result_type = refi; _ }) ->
      let Type (_, exp_params) = Store.get_type_by_ref store refi in
      let%map () = assert_type_params tconstr exp_params tparams in
      full_loc, TypeExpr (TypeRef (l, (refi, Some (ConstrRef c_ref))), tparams)
    | None ->
      let%map () = add_error (l, UndefinedConstr (Ast.gen_name name)) in
      full_loc, TypeExpr (TypeRefUnd tconstr, tparams)
    end
  | TypeIdBoxed name ->
    begin match Store.get_type_by_name store name with
    | Some (refi, Type (_, exp_params)) ->
      let%map () = assert_type_params tconstr exp_params tparams in
      let ref = l, (refi, None) in
      full_loc, TypeExpr (TypeRef ref, tparams)
    | None ->
      let%map () = add_error (l, UndefinedType (Ast.gen_name name)) in
      full_loc, TypeExpr (TypeRefUnd tconstr, tparams)
    end

(** `%T` --> `t` *)
let resolve_bare_type (e: tl_type_expr) =
  let%bind { store; _ } = get in
  let rec go = function
    | _, TypeExpr (ref, _) as e' when is_bare_type_ref ref -> return e'
    | l, TypeExpr (TypeRef (l', (refi, _)), xs) ->
      begin match Store.get_constructors_by_type_ref store refi with
      | [_, c'] ->
        return (l, TypeExpr (TypeRef (l', (refi, Some (ConstrRef c'.c_ref))), xs))
      | [] -> let%map () = add_error (l, BareNoConstructors) in e
      | _ :: _ -> let%map () = add_error (l, BareTooManyConstructors) in e
      end
    | l, TypeExpr (TypeRefUnd (l', TypeIdBoxed name), xs) ->
      let name' = match name with
        | Name n -> Ast.Name (String.uncapitalize n)
        | NameNs (ns, n) -> Ast.NameNs (ns, String.uncapitalize n)
      in
      return (l, TypeExpr (TypeRefUnd (l', TypeIdBare name'), xs))
    | l, TypeExpr (TypeRefBuiltin _, _) ->
      let%map () = add_error (l, BareBuiltin) in e
    | _, TypeBare e' -> go e'
    | l, _ -> let%map () = add_error (l, BareInvalid) in e
  in
  go e

(** Resolves undefined type refs and bare modifiers *)
let resolve =
  let rec mapper : tl_type_expr -> tl_type_expr CheckerM.t = function
    | _, TypeExpr (TypeRefUnd tconstr, tparams) ->
      let%bind tparams' = all @@ List.map tparams ~f:expr_mapper in
      check_type tconstr tparams'
    | l, TypeRepeat (mult, ls) ->
      let f (l', a') =
        let%map ty' = mapper a'.r_arg_type in
        l', { a' with r_arg_type = ty' }
      in
      let%map ls' = all @@ List.map ls ~f in
      l, TypeRepeat (mult, ls')
    | _, TypeBare e -> let%bind e' = mapper e in resolve_bare_type e'
    | _, TypeVar _ | _, TypeExpr _ as e -> return e
  and expr_mapper : tl_expr -> tl_expr CheckerM.t = function
    | ExprType e -> let%map e' = mapper e in ExprType e'
    | ExprNat _ as e -> return e
  in
  let map_arg (l, a') =
    let l', Modified (m, ty) = a'.arg_type in
    let%map ty' = mapper ty in
    l, { a' with arg_type = l', Modified (m, ty') }
  in
  let%bind { store; _ } = get in
  let%bind store = StoreCheckerMapM.constructors store ~f:(fun (l, c') ->
    let%bind () = modify (fun s -> { s with args = c'.c_args }) in
    let%bind c_args = all @@ List.map c'.c_args ~f:map_arg in
    return (l, { c' with c_args })
  ) in
  let%bind store = StoreCheckerMapM.functions store ~f:(fun (l, f') ->
    let%bind () = modify (fun s -> { s with args = f'.f_args }) in
    let%bind f_args = all @@ List.map f'.f_args ~f:map_arg in
    let%bind f_result_type = mapper f'.f_result_type in
    return (l, { f' with f_args; f_result_type })
  ) in
  modify (fun s -> { s with store })

let check (t: Ast.t) =
  (* -- Pass 1 -- *)
  let%bind () = all_unit @@ List.map t.constructors ~f:(check_decl Types) in
  let%bind () = all_unit @@ List.map t.functions ~f:(check_decl Functions) in
  (* -- Pass 2 -- *)
  let%bind () = resolve in
  (* -- *)
  modify (fun s -> { s with errors = List.rev s.errors })

let run ?(store = Store.default) (t: Ast.t): Err.Check.t list * Store.t =
  let s = exec_state (check t) { CState.default with store } in
  (s.errors, s.store)
