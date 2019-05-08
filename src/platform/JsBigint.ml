open! Base
open Js_of_ocaml

type bigint

let int2bigInt: int -> bigint = Js.Unsafe.variable "int2bigInt"
(* external bigInt2str: bigint -> Js.js_string Js.t = "bigInt2str" *)
(* external str2bigInt: Js.js_string Js.t -> bigint = "bigInt2str" *)
(* external bitSize: bigint -> int = "bitSize" *)
(* external dup: bigint -> bigint = "dup"
external rightShift_: bigint -> int -> unit = "rightShift_"
external leftShift_: bigint -> int -> unit = "leftShift_" *)
let add: bigint -> bigint -> bigint = Js.Unsafe.variable "add"
let sub: bigint -> bigint -> bigint = Js.Unsafe.variable "sub"
let powMod: bigint -> bigint -> bigint -> bigint = Js.Unsafe.variable "powMod"
let greater: bigint -> bigint -> bool = Js.Unsafe.variable "greater"
let equals: bigint -> bigint -> bool = Js.Unsafe.variable "equals"

(* external arrayBufferFromHex
  : Js.js_string Js.t -> Typed_array.arrayBuffer Js.t = "arrayBufferFromHex"
external arrayBufferToHex
  : Typed_array.arrayBuffer Js.t -> Js.js_string Js.t = "arrayBufferToHex" *)

let bigInt2ArrayBuffer
  : bigint -> Typed_array.arrayBuffer Js.t = Js.Unsafe.variable "bigInt2ArrayBuffer"

(* let arrayBuffer2bigInt
  : Typed_array.arrayBuffer Js.t -> bigint = Js.Unsafe.variable "arrayBuffer2bigInt" *)

let camlBigarray2bigInt
  : Cstruct.buffer -> bigint = Js.Unsafe.variable "camlBigarray2bigInt"

let bigint_zero: bigint = Js.Unsafe.variable "leemon_zero"
let bigint_one: bigint = Js.Unsafe.variable "one"

(* TODO: Some bigint functions don't seem to work *)

module Bigint = struct
  type t = bigint
  exception Overflow (* not used *)
  let zero = bigint_zero
  let one = bigint_one
  (* let size = bitSize *)
  (* let shift_right t n = let new_t = dup t in rightShift_ new_t n; new_t
  let shift_left t n = let new_t = dup t in leftShift_ new_t n; new_t *)
  let (+) = add
  let (-) = sub
  let of_int = int2bigInt
  let powm = powMod
  let (>) = greater
  let (<) t1 t2 = Bool.(greater t1 t2 = false && equals t1 t2 = false)
  let of_cstruct_be cs = camlBigarray2bigInt (Cstruct.to_bigarray cs)
  let to_cstruct_be t = bigInt2ArrayBuffer t
    |> Typed_array.Bigstring.of_arrayBuffer
    |> Cstruct.of_bigarray
end
