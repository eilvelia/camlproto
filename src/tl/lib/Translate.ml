open! Base

let translate : ?store:Store.t -> string -> Err.t list * string =
  fun ?store str ->
  match Parser.parse_string str with
  | Ok ast ->
    let check_errors, store = Checker.run ?store ast in
    let generated = Codegen.run store in
    let errors = List.map check_errors ~f:(fun e -> `CheckError e) in
    (errors, generated)
  | Error e -> ([`ParseError e], "")
