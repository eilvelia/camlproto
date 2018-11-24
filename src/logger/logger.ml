open! Base

(* TODO: Use https://github.com/dbuenzli/logs *)

(* let enable_log = true

let log =
  if enable_log
    then fun name str -> Caml.print_endline (name ^ ": - " ^ str)
    else fun _ _ -> ()

type logger = string -> unit
type logger_name = string

let create (name: string): logger = log name *)

let dump name cs =
  Caml.Printf.printf "%s [%d]:" name (Cstruct.len cs);
  Cstruct.hexdump cs;
  Caml.print_newline ()

let dump_t name cs =
  dump name cs;
  cs
