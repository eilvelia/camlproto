{

open Lexing
open MenhirParser

type error =
  | IllegalChar of char

let show_error = function
  | IllegalChar c -> Printf.sprintf "Illegal character: \"%c\"" c

exception Error of error

let error e = raise (Error e)

}

let ws = [' ' '\t']+
let newline = "\r\n" | '\r' | '\n'
let lc_letter = ['a'-'z']
let uc_letter = ['A'-'Z']
let digit = ['0'-'9']
let hex_digit = digit | ['a' 'b' 'c' 'd' 'e' 'f']
let underscore = '_'
let letter = lc_letter | uc_letter
let ident_char = letter | digit | underscore

let lc_ident = lc_letter ident_char*
let uc_ident = uc_letter ident_char*
let namespace_ident = lc_ident
let lc_ident_ns = (namespace_ident '.')? lc_ident
let uc_ident_ns = (namespace_ident '.')? uc_ident
let hd = hex_digit
let hexdigits = hd hd? hd? hd? (* *) hd? hd? hd? hd?
(* let lc_ident_full = lc_ident_ns '#' hd hd? hd? hd? hd? hd? hd? hd? *)

rule read =
  parse
  | eof { EOF }
  | ws { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | "//" [^ '\r' '\n' ]* { read lexbuf }
  | "/*" { comment lexbuf; read lexbuf }
  | "---types---" { CONSTRUCTOR_SECTION }
  | "---functions---" { FUNCTION_SECTION }
  | "_" { UNDERSCORE }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "(" { OPEN_PAR }
  | ")" { CLOSE_PAR }
  | "[" { OPEN_BRACKET }
  | "]" { CLOSE_BRACKET }
  | "{" { OPEN_BRACE }
  | "}" { CLOSE_BRACE }
  | "=" { EQUALS }
  | "#" { HASH }
  | "?" { QUESTION_MARK }
  | "%" { PERCENT }
  | "+" { PLUS }
  | "<" { LANGLE }
  | ">" { RANGLE }
  | "," { COMMA }
  | "." { DOT }
  | "*" { ASTERISK }
  | "!" { EXCL_MARK }
  | "Final" { FINAL }
  | "New" { NEW }
  | "Empty" { EMPTY }
  | (namespace_ident as n '.')? (lc_ident as i) '#' (hexdigits as m)
    { LC_IDENT_FULL (n, i, m) }
  | (namespace_ident as n '.') (lc_ident as i) { LC_IDENT_NS (n, i) }
  | (namespace_ident as n '.') (uc_ident as i) { UC_IDENT_NS (n, i) }
  | lc_ident { LC_IDENT (lexeme lexbuf) }
  | uc_ident { UC_IDENT (lexeme lexbuf) }
  | digit+ as num { NAT_CONST (int_of_string num) }
  | (_ as c) { error (IllegalChar c) }

and comment = parse
  | newline { new_line lexbuf; comment lexbuf }
  | "*/" { () }
  | eof { raise Err.Parse.(err
    (lexeme_start_p lexbuf, lexeme_end_p lexbuf) UnterminatedComment) }
  | _ { comment lexbuf }
