open! Base

(* type var_ref *)

type tl_builtin =
  | TLInt | TLNat | TLLong | TLString | TLDouble | TLTType | TLInt128 | TLInt256

type var_ref = VarRef of string (* id *) * int (* var_num *)

type type_ref =
  | TypeRefBuiltin of tl_builtin
  | TypeRef of string (* id *)

type opt_var_id =
  | VarId of string
  | EmptyVarId

type tl_type_param =
  | TLParamNat of string
  | TLParamType of string

type tl_type =
  | TLType of string * tl_type_param list
  | TLTypeBuiltin of tl_builtin

type conditional_def = {
  id: var_ref;
  nat: int option;
}

type tl_arg = {
  id: opt_var_id;
  cond: conditional_def option;
  argType: tl_type_expr;
}

and tl_nat_expr =
  | NatConst of int
  | NatVariable of var_ref

and tl_type_expr =
  | TypeVariable of var_ref
  | TypeArray of tl_nat_expr (* multiplicity *) * tl_arg list
  | TypeExpr of type_ref (* type *) * tl_type_expr list (* children *)
  | TypeBare of tl_type_expr
  | TypeBang of tl_type_expr

and tl_expr =
  | ExprType of tl_type_expr
  | ExprNat of tl_nat_expr

type comb_kind = TLFunction | TLConstructor

type tl_combinator = {
  id: string;
  magic: int32;
  args: tl_arg list;
  resultType: tl_type_expr;
  kind: comb_kind;
}

(* let _ =
  let comb = {
    id = "req_pq";
    magic = 0x60469778l;
    args = [{
      id = VarId "nonce";
      cond = None;
      argType = TypeExpr (TypeRefBuiltin TLInt128, []);
    }];
    resultType = TypeExpr (TypeRef "ResPQ", []);
    kind = TLFunction;
  } in
  comb *)
