open! Base

class type js_gzip = object
  (* method compress: Cstruct.buffer -> Typed_array.arrayBuffer Js.t Js.meth *)
  method decompress: Cstruct.buffer -> Typed_array.arrayBuffer Js.t Js.meth
end

let js_gzip: js_gzip Js.t = Js.Unsafe.js_expr "js_gzip"

module Gzip: Types.GZIP = struct
  exception Error of string (* not used *)
  (* let compress cs = js_gzip##compress (Cstruct.to_bigarray cs)
    |> Typed_array.Bigstring.of_arrayBuffer
    |> Cstruct.of_bigarray *)
  let decompress cs = js_gzip##decompress (Cstruct.to_bigarray cs)
    |> Typed_array.Bigstring.of_arrayBuffer
    |> Cstruct.of_bigarray
end
