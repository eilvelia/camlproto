open! Base

let parse lexbuf : (Ast.t, Err.Parse.t) Result.t =
  let open Err.Parse in
  let errloc e = Error (Loc.of_lexbuf lexbuf, e) in
  try Ok (MenhirParser.tl_program Lexer.read lexbuf) with
  | Lexer.Error e ->
    errloc @@ OcamllexError (Lexer.show_error e)
  | MenhirParser.Error ->
    errloc @@ MenhirError
  | Err.Parse.Err e ->
    Error e

let parse_string ?(filename = "") str =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse lexbuf
