open! Base

class type js_secure_rand = object
  method rand: int -> Typed_array.arrayBuffer Js.t Js.meth
end

let js_secure_rand: js_secure_rand Js.t = Js.Unsafe.js_expr "js_secure_rand"

let rand_cs size = js_secure_rand##rand size
    |> Typed_array.Bigstring.of_arrayBuffer
    |> Cstruct.of_bigarray
