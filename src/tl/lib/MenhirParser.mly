%{

open Ast
open Err.Parse

let convert_ident_ns (ns_opt, id) =
  match ns_opt with
  | Some ns -> NameNs (ns, id)
  | None -> Name id

type section =
  | Constr
  | Fun

let option_join = function
  | Some x -> x
  | None -> None

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

let eappl head tail = match tail with
  | [] -> head
  | xs -> let (loc, _) = head in loc, EAppl (head, xs)

type comb_or_partial =
  | Comb of (opt_arg list * arg list * expr)
  | Part of expr list

let l = Loc.make

(* 'unused variable' warnings don't work correctly with `$loc(id)` *)
[@@@warning "-26-27"]

%}

%token <string> LC_IDENT
%token <string> UC_IDENT
/* %token <string> NAMESPACE_IDENT */
%token <string * string> LC_IDENT_NS
%token <string * string> UC_IDENT_NS
%token <string option * string * string> LC_IDENT_FULL
%token <int> NAT_CONST

%token CONSTRUCTOR_SECTION FUNCTION_SECTION
%token UNDERSCORE
%token COLON
%token SEMICOLON
%token OPEN_PAR CLOSE_PAR
%token OPEN_BRACKET CLOSE_BRACKET
%token OPEN_BRACE CLOSE_BRACE
%token EQUALS
%token HASH
%token QUESTION_MARK
%token PERCENT
%token PLUS
%token LANGLE
%token RANGLE
%token COMMA
%token DOT
%token ASTERISK
%token EXCL_MARK
%token FINAL NEW EMPTY

%token EOF

%left PLUS
/* %nonassoc LC_IDENT UC_IDENT */

%start <Ast.t> tl_program
%%

%inline uc_ident_ns:
  | i=UC_IDENT_NS { let (ns, name) = i in NameNs (ns, name) }
  | i=UC_IDENT { Name i }
;

%inline lc_ident_ns:
  | i=LC_IDENT_NS { let (ns, name) = i in NameNs (ns, name) }
  | i=LC_IDENT { Name i }
;

%inline bare_op:
  | op=PERCENT { l $loc(op), OpBare }
;
%inline bang_op:
  | op=EXCL_MARK { l $loc(op), OpBang }
;
%inline plus_op:
  | op=PLUS { l $loc(op), OpPlus }
;

tl_program:
  | head=decl* tail=declarations* EOF
    {
      let is_constrs x = match fst x with Constr -> true | Fun -> false in
      let (constrs, funcs) = List.partition is_constrs tail in
      {
        constructors = head @ (List.concat @@ List.map snd constrs);
        functions = List.concat @@ List.map snd funcs;
      }
    }
;

declarations:
  | CONSTRUCTOR_SECTION decls=decl*
    { (Constr, decls) }
  | FUNCTION_SECTION decls=decl*
    { (Fun, decls) }
;

%inline boxed_type_ident:
  | id=uc_ident_ns { l $loc(id), IdBoxed id }
;
%inline var_ident:
  | id=LC_IDENT | id=UC_IDENT
    { l $loc(id), id }
;
%inline var_ident_opt:
  | UNDERSCORE { None }
  | id=var_ident { Some id }
;
%inline type_ident:
  | id=boxed_type_ident { id }
  | id=lc_ident_ns { l $loc(id), IdBare id }
  | h=HASH { l $loc(h), IdBare (Name "nat") }
;
%inline etype_ident:
  | id=type_ident { l $loc(id), EIdent id }
;

expr:
  | head=subexpr tail=subexpr+
    { l $sloc, EAppl (head, tail) }
  | head=subexpr
    { head }
;

subexpr:
  | e=subexpr_other
  | e=term { e }
;

%inline subexpr_other:
  /* | nat=NAT_CONST PLUS e=subexpr { EOperator (OpPlus, [ENat nat; e]) } */
  /* | e=subexpr PLUS nat=NAT_CONST { EOperator (OpPlus, [e; ENat nat]) } */
  | e1=subexpr op=plus_op e2=subexpr { l $sloc, EOperator (op, [e1; e2]) }
;

term:
  | OPEN_PAR e=expr CLOSE_PAR
    { e }
  | id=etype_ident LANGLE params=separated_nonempty_list(COMMA, expr) RANGLE
    { l $sloc, EAppl (id, params) }
  | id=etype_ident
    { id }
  | num=NAT_CONST
    { l $sloc, ENat num }
  | op=bare_op t=term
    { l $sloc, EOperator (op, [t]) }
;

type_term:
  | op=bang_op t=term { l $sloc, EOperator (op, [t]) }
  | t=term { t }
;

type_expr:
  | op=bang_op e=expr { l $sloc, EOperator (op, [e]) }
  | e=expr { e }
;

%inline nat_term: t=term { t } ;

%inline multiplicity: t=nat_term { t } ;

decl:
  | d=builtin_combinator_decl
    { BuiltinCombinatorDecl d }
  | d=final_decl
    { FinalDecl d }
  | id=boxed_type_ident LANGLE params=separated_nonempty_list(COMMA, expr) RANGLE SEMICOLON
  | id=boxed_type_ident params=subexpr+ SEMICOLON
    { PartialApplicationDecl (l $sloc, PartialTypeAppl (id, params)) }
  | comb_id=full_comb_name tail=comb_decl_tail(args)
    {
      let (comb_opt_args, comb_args, comb_result) = tail in
      CombinatorDecl (l $sloc,
        { comb_id; comb_opt_args; comb_args; comb_result })
    }
  | comb_id=comb_id tail=comb_or_partial_decl_tail
    {
      match tail with
      | Comb (comb_opt_args, comb_args, comb_result) ->
        CombinatorDecl (l $sloc,
          { comb_id; comb_opt_args; comb_args; comb_result })
      | Part params ->
        PartialApplicationDecl (l $sloc,
          (PartialCombAppl (comb_id, params)))
    }
;

%inline comb_decl_tail(Args):
  | args=Args* EQUALS r=result_type SEMICOLON
    { ([], List.concat args, r) }
  | oargs=opt_args+ args=args* EQUALS r=result_type SEMICOLON
    { (List.concat oargs, List.concat args, r) }
;

comb_or_partial_decl_tail:
  | t=comb_decl_tail(args_other)
    { Comb t }
  | params=subexpr_other+ SEMICOLON
    { Part params }
  | params=type_term+ r_opt=preceded(EQUALS, result_type)? SEMICOLON
    {
      match r_opt with
      | Some r ->
        let args = List.map (fun arg_type ->
          l $sloc,
          { arg_id = None; arg_cond_def = None; arg_type }) params in
        Comb ([], args, r)
      | None ->
        let f = function
          | _, EOperator ((_, OpBang), _) ->
            err $sloc BangInPartAppl
          | _ -> ()
        in
        List.iter f params;
        Part params
    }
;

builtin_combinator_decl:
  | id=full_comb_id QUESTION_MARK EQUALS result=boxed_type_ident SEMICOLON
    { l $sloc, { b_comb_id = id; b_comb_result = result } }
;

final_decl:
  | kind=NEW final_id=boxed_type_ident SEMICOLON
    { l $sloc, { finalization = l $loc(kind), FinNew; final_id } }
  | kind=FINAL final_id=boxed_type_ident SEMICOLON
    { l $sloc, { finalization = l $loc(kind), FinFinal; final_id } }
  | kind=EMPTY final_id=boxed_type_ident SEMICOLON
    { l $sloc, { finalization = l $loc(kind), FinEmpty; final_id } }
;

%inline full_comb_name:
  | id=LC_IDENT_FULL
    {
      let (ns, n, m) = id in
      l $sloc, CombIdFull (convert_ident_ns (ns, n), m)
    }
;

%inline short_comb_name:
  | id=lc_ident_ns { l $loc(id), CombIdShort id }
;

%inline empty_comb_name:
  | UNDERSCORE { l $sloc, CombIdEmpty }
;

comb_id: n=short_comb_name | n=empty_comb_name { n } ;
full_comb_id: n=full_comb_name | n=comb_id { n } ;

opt_args:
  | OPEN_BRACE ids=var_ident+ COLON opt_arg_type=type_expr CLOSE_BRACE
    {
      let f id = l $sloc, { opt_arg_id = id; opt_arg_type } in
      List.map f ids
    }
;

ident_without_ns:
  | n=UC_IDENT { l $sloc, IdBoxed (Name n) }
  | n=LC_IDENT { l $sloc, IdBare (Name n) }
;

ident_without_ns_opt:
  | id=ident_without_ns { Some id }
  | UNDERSCORE { None }
;

args_other:
  | id=ioption(terminated(var_ident_opt, COLON))
    mult=ioption(terminated(multiplicity, ASTERISK))
    ob=OPEN_BRACKET subargs=args* cb=CLOSE_BRACKET
    {
      let arg_id = option_join id in
      let arg_type' = EMultiArg (mult, List.concat subargs) in
      let arg_type = l ($startpos(ob), $endpos(cb)), arg_type' in
      [l $sloc, { arg_id; arg_cond_def = None; arg_type }]
    }
  | id=var_ident_opt COLON arg_cond_def=ioption(conditional_def) arg_type=type_term
    { [l $sloc, { arg_id = id; arg_cond_def; arg_type }] }
  | id=var_ident_opt COLON OPEN_PAR cond=conditional_def arg_type=type_expr CLOSE_PAR
    { [l $sloc, { arg_id = id; arg_cond_def = Some cond; arg_type }] }
  | OPEN_PAR ids=ident_without_ns_opt+ arg_type=preceded(COLON, type_expr) CLOSE_PAR
    {
      let f id_opt = l $sloc, {
        arg_id = option_map (fun id -> Loc.map_annot id gen_ident') id_opt;
        arg_cond_def = None;
        arg_type;
      } in
      List.map f ids
    }
  | OPEN_PAR ids=ident_without_ns_opt+ CLOSE_PAR
    {
      let f = function
        | None ->
          err $loc(ids) UnderscoreInConstrAppl
        | Some (loc, _ as x) -> loc, EIdent x
      in
      let ids = List.map f ids in
      let arg_type = eappl (List.hd ids) (List.tl ids) in
      [l $sloc, { arg_id = None; arg_cond_def = None; arg_type }]
    }
;

args:
  | a=args_other { a }
  | arg_type=type_term
    { [l $sloc, { arg_id = None; arg_cond_def = None; arg_type }] }
;

conditional_def:
  | id=var_ident DOT nat=NAT_CONST QUESTION_MARK
    { l $sloc, CondDef (id, Some (l $loc(nat), nat)) }
  | id=var_ident QUESTION_MARK
    { l $sloc, CondDef (id, None) }
;

result_type:
  | id=boxed_type_ident LANGLE params=separated_nonempty_list(COMMA, subexpr) RANGLE
    { eappl (l $loc(id), EIdent id) params }
  | id=boxed_type_ident params=subexpr*
    { eappl (l $loc(id), EIdent id) params }
  | op=bang_op r=result_type
    { l $sloc, EOperator (op, [r]) }
;
