open! Base

type t = {
  constructors: decl list;
  functions: decl list;
}

and name =
  | NameNs of string * string (** [namespace, string] *)
  | Name of string (** [string] *)

and ident' =
  | IdBoxed of name (** uc *)
  | IdBare of name (** lc *)

and ident = ident' Loc.annot

and op' = OpBare (** % *) | OpBang (** ! *) | OpPlus (** + *)

and op = op' Loc.annot

and expr' =
  | ENat of int
  | EIdent of ident
  | EOperator of op * expr list
  | EAppl of expr * expr list
  | EMultiArg of expr option * arg list (** [multiplicity, subargs] *)

and expr = expr' Loc.annot

and decl =
  | CombinatorDecl of combinator_decl
  | BuiltinCombinatorDecl of builtin_combinator_decl
  | PartialApplicationDecl of partial_appl_decl
  | FinalDecl of final_decl

and combinator_ident' =
  | CombIdFull of name * string (** [lc_name_ns, magic] *)
  | CombIdShort of name (** [lc_name_ns] *)
  | CombIdEmpty (** [_] *)

and combinator_ident = combinator_ident' Loc.annot

and opt_arg' = {
  opt_arg_id: string Loc.annot; (** [name] *)
  opt_arg_type: expr;
}

and opt_arg = opt_arg' Loc.annot

and cond_def' = CondDef of string Loc.annot * int Loc.annot option

and cond_def = cond_def' Loc.annot

and arg' = {
  arg_id: string Loc.annot option; (** [name option] *)
  arg_cond_def: cond_def option;
  arg_type: expr;
}

and arg = arg' Loc.annot

and combinator_decl' = {
  comb_id: combinator_ident;
  comb_opt_args: opt_arg list;
  comb_args: arg list;
  comb_result: expr;
}

and combinator_decl = combinator_decl' Loc.annot

and builtin_combinator_decl' = {
  b_comb_id: combinator_ident;
  b_comb_result: ident; (** can be only boxed *)
}

and builtin_combinator_decl = builtin_combinator_decl' Loc.annot

and final_decl' = {
  finalization: finalization;
  final_id: ident; (** can be only boxed *)
}

and final_decl = final_decl' Loc.annot

and finalization' = FinNew | FinFinal | FinEmpty

and finalization = finalization' Loc.annot

and partial_appl_decl' =
  | PartialTypeAppl of ident * expr list (** [ident] can be only boxed *)
  | PartialCombAppl of combinator_ident * expr list
      (** [combinator_ident] can't be full *)

and partial_appl_decl = partial_appl_decl' Loc.annot

[@@deriving show { with_path = false }, equal, compare, sexp, hash]

let rec gen { constructors; functions } =
  let c = String.concat ~sep:"\n" (List.map constructors ~f:gen_decl) in
  let f = String.concat ~sep:"\n" (List.map functions ~f:gen_decl) in
  "---types---\n" ^ c ^ "\n---functions---\n" ^ f ^ "\n"

and gen_name = function
  | NameNs (ns, id) -> ns ^ "." ^ id
  | Name id -> id

and gen_ident' = function
  | IdBoxed name
  | IdBare name -> gen_name name

and gen_ident i = Loc.value_map_annot i gen_ident'

and gen_op op = Loc.value_map_annot op @@ function
  | OpBare -> "%"
  | OpBang -> "!"
  | OpPlus -> "+"

and gen_expr expr = Loc.value_map_annot expr @@ function
  | ENat i -> Int.to_string i
  | EIdent id -> gen_ident id
  | EOperator (op, []) -> gen_op op
  | EOperator (op, [a1]) -> gen_op op ^ gen_expr a1
  | EOperator (op, [a1; a2]) -> "(" ^ gen_expr a1 ^ gen_op op ^ gen_expr a2 ^ ")"
  | EOperator (_, _) -> failwith "Wrong number of operands"
  | EAppl (x, (_ :: _ as xs)) ->
    let xs = String.concat ~sep:" " (List.map xs ~f:gen_expr) in
    "(" ^ gen_expr x ^ " " ^ xs ^ ")"
  | EAppl (x, []) -> gen_expr x
  | EMultiArg (e_opt, args) ->
    let args = String.concat ~sep:" " (List.map args ~f:gen_arg) in
    let e = Option.value_map e_opt ~default:"" ~f:(fun e -> gen_expr e ^ "*") in
    e ^ "[" ^ args ^ "]"

and gen_decl = function
  | CombinatorDecl d -> gen_combinator_decl d
  | BuiltinCombinatorDecl d -> gen_builtin_combinator_decl d
  | PartialApplicationDecl d -> gen_partial_appl_decl d
  | FinalDecl d -> gen_final_decl d

and gen_combinator_ident i = Loc.value_map_annot i @@ function
  | CombIdFull (name, magic) -> gen_name name ^ "#" ^ magic
  | CombIdShort name -> gen_name name
  | CombIdEmpty -> "_"

and gen_opt_arg (_, { opt_arg_id=(_, id); opt_arg_type }) =
  id ^ ": " ^ gen_expr opt_arg_type

and gen_cond_def (_, CondDef ((_, varname), bit_opt)) =
  let n = Option.value_map bit_opt ~default:""
    ~f:(fun (_, bit) -> "." ^ Int.to_string bit) in
  varname ^ n ^ "?"

and gen_arg (_, { arg_id; arg_cond_def; arg_type }) =
  let id = Option.value_map arg_id ~default:"_" ~f:snd in
  let cond_def = Option.value_map arg_cond_def ~default:"" ~f:gen_cond_def in
  let typ = gen_expr arg_type in
  id ^ cond_def ^ ":" ^ typ

and gen_combinator_decl (_, c) =
  let id = gen_combinator_ident c.comb_id in
  let opt_args = List.map c.comb_opt_args ~f:(fun a -> "{" ^ gen_opt_arg a ^ "}") in
  let opt_args = String.concat ~sep:" " opt_args in
  let opt_args = String.(if opt_args = "" then opt_args else " " ^ opt_args) in
  let args = List.map c.comb_args ~f:gen_arg in
  let args = String.concat ~sep:" " args in
  let args = String.(if args = "" then args else " " ^ args) in
  let result = gen_expr c.comb_result in
  id ^ opt_args ^ args ^ " = " ^ result ^ ";"

and gen_builtin_combinator_decl (_, { b_comb_id; b_comb_result }) =
  gen_combinator_ident b_comb_id ^ " ? = " ^ gen_ident b_comb_result ^ ";"

and gen_final_decl (_, { finalization; final_id }) =
  gen_finalization finalization ^ " " ^ gen_ident final_id ^ ";"

and gen_finalization f = Loc.value_map_annot f @@ function
  | FinNew -> "New"
  | FinFinal -> "Final"
  | FinEmpty -> "Empty"

and gen_partial_appl_decl d = Loc.value_map_annot d @@ function
  | PartialTypeAppl (id, args) ->
    let args = String.concat ~sep:" " (List.map args ~f:gen_expr) in
    gen_ident id ^ " " ^ args ^ ";"
  | PartialCombAppl (id, args) ->
    let args = String.concat ~sep:" " (List.map args ~f:gen_expr) in
    gen_combinator_ident id ^ " " ^ args ^ ";"

module ComparableName = struct
  type t = name

  let compare n1 n2 = match n1, n2 with
    | Name s1, Name s2 -> String.compare s1 s2
    | NameNs (n1, s1), NameNs (n2, s2) ->
      begin match String.compare n1 n2 with
      | 0 -> String.compare s1 s2
      | x -> x
      end
    | NameNs _, Name _ -> 1
    | Name _, NameNs _ -> -1

  let sexp_of_t : t -> Sexp.t = function
    | Name n -> Atom n
    | NameNs (ns, n) -> List [Atom ns; Atom n]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let rec crc32_gen_expr expr = Loc.value_map_annot expr @@ function
  | ENat i -> Int.to_string i
  | EIdent (_, IdBare (Name "nat")) -> "#"
  | EIdent id -> gen_ident id
  | EOperator (op, []) -> gen_op op
  | EOperator (op, [a1]) -> gen_op op ^ gen_expr a1
  | EOperator (op, [a1; a2]) -> crc32_gen_expr a1 ^ gen_op op ^ crc32_gen_expr a2
  | EOperator (_, _) -> failwith "Wrong number of operands"
  | EAppl (x, (_ :: _ as xs)) ->
    let xs = String.concat ~sep:" " (List.map xs ~f:crc32_gen_expr) in
    crc32_gen_expr x ^ " " ^ xs
  | EAppl (x, []) -> gen_expr x
  | EMultiArg (e_opt, args) ->
    let args = String.concat ~sep:" " (List.map args ~f:crc32_gen_arg) in
    let e = Option.value_map e_opt ~default:"" ~f:(fun e -> crc32_gen_expr e ^ "*") in
    e ^ "[ " ^ args ^ " ]"

and crc32_gen_combinator_ident i = Loc.value_map_annot i @@ function
  | CombIdFull (name, _magic) -> gen_name name
  | CombIdShort name -> gen_name name
  | CombIdEmpty -> "_"

and crc32_gen_opt_arg (_, { opt_arg_id=(_, id); opt_arg_type }) =
  id ^ ":" ^ crc32_gen_expr opt_arg_type

and crc32_gen_cond_def (_, CondDef ((_, varname), bit_opt)) =
  let n = Option.value_map bit_opt ~default:""
    ~f:(fun (_, bit) -> "." ^ Int.to_string bit) in
  varname ^ n ^ "?"

and crc32_gen_arg (_, { arg_id; arg_cond_def; arg_type }) =
  let cond_def = Option.value_map arg_cond_def ~default:"" ~f:crc32_gen_cond_def in
  let typ = crc32_gen_expr arg_type in
  match arg_id with
  | Some (_, id) -> id ^ cond_def ^ ":" ^ typ
  | None -> cond_def ^ typ

and crc32_gen_combinator_decl (_, c) =
  let id = crc32_gen_combinator_ident c.comb_id in
  let opt_args = List.map c.comb_opt_args ~f:crc32_gen_opt_arg in
  let opt_args = String.concat ~sep:" " opt_args in
  let opt_args = String.(if opt_args = "" then opt_args else " " ^ opt_args) in
  let args = List.map c.comb_args ~f:crc32_gen_arg in
  let args = String.concat ~sep:" " args in
  let args = String.(if args = "" then args else " " ^ args) in
  let result = crc32_gen_expr c.comb_result in
  id ^ opt_args ^ args ^ " = " ^ result

and crc32_gen_builtin_combinator_decl (_, { b_comb_id; b_comb_result }) =
  crc32_gen_combinator_ident b_comb_id ^ " ? = " ^ gen_ident b_comb_result

let%expect_test "crc32_gen_combinator_decl: vector" =
  Caml.print_endline @@ crc32_gen_combinator_decl (Loc.empty, {
    comb_id = Loc.empty, CombIdShort (Name "vector");
    comb_opt_args = [Loc.empty, {
      opt_arg_id = Loc.empty, "x";
      opt_arg_type = Loc.empty, EIdent (Loc.empty, IdBoxed (Name "Type"));
    }];
    comb_args = [Loc.empty, {
      arg_id = None;
      arg_cond_def = None;
      arg_type = Loc.empty, EIdent (Loc.empty, IdBare (Name "nat"));
    }; Loc.empty, {
      arg_id = None;
      arg_cond_def = None;
      arg_type = Loc.empty, EMultiArg (None, [Loc.empty, {
        arg_id = None;
        arg_cond_def = None;
        arg_type = Loc.empty, EIdent (Loc.empty, IdBare (Name "t"))
      }])
    }];
    comb_result =
      Loc.empty, EAppl (
        (Loc.empty, EIdent (Loc.empty, IdBoxed (Name "Vector"))),
        [Loc.empty, EIdent (Loc.empty, IdBare (Name "t"))]);
  });
  [%expect {| vector x:Type # [ t ] = Vector t |}]
