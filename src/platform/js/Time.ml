open! Base
open Js_of_ocaml

let get_current_time () =
  let time: float = (Js.Unsafe.pure_js_expr "Date")##now () in
  time /. 1000.0
