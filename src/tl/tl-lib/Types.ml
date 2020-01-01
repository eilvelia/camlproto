open! Base

type tl_section = Functions | Types

type name_with_loc = Ast.name Loc.annot

and tl_type_ident' =
  | TypeIdBoxed of Ast.name
  | TypeIdBare of Ast.name

and tl_type_ident = tl_type_ident' Loc.annot

and tl_type_param = ParamNat of string | ParamType of string
and tl_type = Type of Ast.name * tl_type_param list

and tl_builtin_type =
  | BuiltinType
  | BuiltinNat

and tl_constr_ref =
  | ConstrRef of int [@@unboxed]

and tl_type_ref =
  | TypeRef of (int * tl_constr_ref option) Loc.annot (* TODO: remove loc? *)
  | TypeRefUnd of tl_type_ident
  | TypeRefBuiltin of tl_builtin_type

and tl_var_ref =
  | VarRef of int * string option
  | VarRefUnd of string

and tl_expr =
  | ExprNat of tl_nat_expr
  | ExprType of tl_type_expr

and tl_nat_expr' =
  | NatConst of int
  | NatVar of tl_var_ref
  | NatPlus of tl_nat_expr * tl_nat_expr

and tl_nat_expr = tl_nat_expr' Loc.annot

and tl_type_expr' =
  | TypeExpr of tl_type_ref * tl_expr list
  | TypeRepeat of tl_nat_expr * tl_repeat_arg list
  | TypeVar of tl_var_ref
  | TypeBare of tl_type_expr

and tl_type_expr = tl_type_expr' Loc.annot

and tl_modifier = ModBang

and modified_type_expr' = Modified of tl_modifier option * tl_type_expr

and modified_type_expr = modified_type_expr' Loc.annot

and tl_cond' = Cond of tl_var_ref * int

and tl_cond = tl_cond' Loc.annot

and tl_arg_id = string Loc.annot

and tl_repeat_arg' = {
  r_arg_id: tl_arg_id option;
  r_arg_type: tl_type_expr;
}

and tl_repeat_arg = tl_repeat_arg' Loc.annot

and tl_opt_arg_kind = [ `ResultBang | `ArgBang ]

and tl_arg' = {
  arg_id: tl_arg_id option;
  arg_ref: int;
  arg_cond: tl_cond option;
  arg_type: modified_type_expr;
  arg_opt: tl_opt_arg_kind option;
}

and tl_arg = tl_arg' Loc.annot

(* TODO: merge tl_constructor and tl_function? *)

and tl_constructor' = {
  c_id: name_with_loc;
  c_magic: int32 [@printer fun fmt -> fprintf fmt "0x%lXl"];
  c_args: tl_arg list;
  c_ref: int;
  c_result_type: int;
  c_builtin: bool;
}

and tl_constructor = tl_constructor' Loc.annot

and tl_function' = {
  f_id: name_with_loc;
  f_magic: int32 [@printer fun fmt -> fprintf fmt "0x%lXl"];
  f_args: tl_arg list;
  f_result_type: tl_type_expr;
  f_builtin: bool;
}

and tl_function = tl_function' Loc.annot

[@@deriving show { with_path = false }, equal, compare, sexp, hash]

let gen_type_id' = function
  | TypeIdBoxed n
  | TypeIdBare n -> Ast.gen_name n

let gen_type_id (_, id : tl_type_ident) = gen_type_id' id

let is_boxed_type_id l = Loc.value_map_annot l @@ function
  | TypeIdBoxed _ -> true
  | TypeIdBare _ -> false

let is_boxed_type_ref = function
  | TypeRef (_, (_, Some _)) -> false
  | TypeRef (_, (_, None)) -> true
  | TypeRefBuiltin BuiltinType -> true
  | TypeRefBuiltin BuiltinNat -> false
  | TypeRefUnd id -> is_boxed_type_id id

let is_bare_type_ref r = not @@ is_boxed_type_ref r

let ref_Type = TypeRefBuiltin BuiltinType
let ref_nat = TypeRefBuiltin BuiltinNat

let show_builtin_type : tl_builtin_type -> string = function
  | BuiltinType -> "Type"
  | BuiltinNat -> "nat"

let show_param_type = function
  | ParamType _ -> "Type"
  | ParamNat _ -> "nat"

let get_expr_loc : tl_expr -> Loc.t = function
  | ExprNat (l, _) -> l
  | ExprType (l, _) -> l

let unwrap_modified : modified_type_expr -> tl_type_expr = function
  | _, Modified (_, texpr) -> texpr

module TypeRefCmp = struct
  type t = tl_type_ref
  [@@deriving compare, sexp_of, hash]
  include (val Comparator.make ~compare ~sexp_of_t)
end
