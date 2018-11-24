(*
Partially taken from
  https://github.com/mirleft/ocaml-nocrypto/blob/70a31c2d9f15bd04d7ccdb75223f92c4c51416ac/src/numeric.ml

ISC License

Copyright (c) 2014-2016 David Kaloper Meršinjak
Copyright (c) 2018 Bannerets <save14@protonmail.com>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

*)

let imax (a : int) b = if a < b then b else a
let imin (a : int) b = if a < b then a else b

let (//) x y =
  if y < 1 then raise Division_by_zero else
    if x > 0 then 1 + ((x - 1) / y) else 0 [@@inline]

module Z_core = struct
  let bit_bound z = Z.size z * 64
  include Z
  let (lsr) = shift_right
  let (lsl) = shift_left
end

let bits i =
  if i < Z_core.zero then invalid_arg "i < 0";
  let rec scan acc bound = function
    | i when i = Z_core.zero -> acc
    | i when i = Z_core.one  -> acc + 1
    | i ->
        let mid   = bound / 2 in
        let upper = Z_core.(i lsr mid) in
        if upper = Z_core.zero then
          scan acc (bound - mid) i
        else scan (acc + mid) (bound - mid) upper in
  scan 0 Z_core.(bit_bound i) i

(* let of_cstruct_be ?bits cs = *)
let of_cstruct_be cs =
  let open Cstruct in
  let open BE in
  let rec loop acc i = function
    | b when b >= 64 ->
        let x = get_uint64 cs i in
        let x = Z_core.of_int64 Int64.(shift_right_logical x 8) in
        loop Z_core.(x + acc lsl 56) (i + 7) (b - 56)
    | b when b >= 32 ->
        let x = get_uint32 cs i in
        let x = Z_core.of_int32 Int32.(shift_right_logical x 8) in
        loop Z_core.(x + acc lsl 24) (i + 3) (b - 24)
    | b when b >= 16 ->
        let x = Z_core.of_int (get_uint16 cs i) in
        loop Z_core.(x + acc lsl 16) (i + 2) (b - 16)
    | b when b >= 8  ->
        let x = Z_core.of_int (get_uint8 cs i) in
        loop Z_core.(x + acc lsl 8 ) (i + 1) (b - 8 )
    | b when b > 0   ->
        let x = get_uint8 cs i and b' = 8 - b in
        Z_core.(of_int x lsr b' + acc lsl b)
    | _              -> acc in
  (* loop Z_core.zero 0 @@ match bits with
    | None   -> Cstruct.len cs * 8
    | Some b -> imin b (Cstruct.len cs * 8) *)
  loop Z_core.zero 0 (Cstruct.len cs * 8)

let byte1 = Z_core.of_int64 0xffL
and byte2 = Z_core.of_int64 0xffffL
and byte3 = Z_core.of_int64 0xffffffL
and byte7 = Z_core.of_int64 0xffffffffffffffL

let into_cstruct_be n cs =
  let open Cstruct in
  let open BE in
  let rec write n = function
    | i when i >= 7 ->
        set_uint64 cs (i - 7) Z_core.(to_int64 (n land byte7));
        write Z_core.(n lsr 56) (i - 7)
    | i when i >= 3 ->
        set_uint32 cs (i - 3) Z_core.(to_int32 (n land byte3));
        write Z_core.(n lsr 24) (i - 3)
    | i when i >= 1 ->
        set_uint16 cs (i - 1) Z_core.(to_int (n land byte2));
        write Z_core.(n lsr 16) (i - 2)
    | 0 -> set_uint8 cs 0 Z_core.(to_int (n land byte1));
    | _ -> ()
  in
  write n (len cs - 1)

(* let to_cstruct_be ?size n =
  let cs = Cstruct.create_unsafe @@ match size with
    | Some s -> imax 0 s
    | None   -> bits n // 8 in
  (into_cstruct_be n cs ; cs) *)

let to_cstruct_be n =
  let cs = Cstruct.create_unsafe (bits n // 8) in
  (into_cstruct_be n cs ; cs)

include Z_core

let (>) = gt
let (<) = lt
