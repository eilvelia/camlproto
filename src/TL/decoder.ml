open! Base

type t = {
  cs: Cstruct.t;
  mutable offset: int;
}

let of_cstruct (cs: Cstruct.t): t = { cs; offset = 0 }

let read_int32_le t =
  let out = Cstruct.LE.get_uint32 t.cs t.offset in
  t.offset <- t.offset + 4;
  out

let read_int64_le t =
  let out = Cstruct.LE.get_uint64 t.cs t.offset in
  t.offset <- t.offset + 8;
  out

let read_float t =
  let out = Cstruct.LE.get_uint64 t.cs t.offset in
  t.offset <- t.offset + 8;
  Int64.float_of_bits out

let read_byte t = (* as int *)
  let out = Cstruct.get_uint8 t.cs t.offset in
  t.offset <- t.offset + 1;
  out

(* JuanPotato have so many 'lmaos' in his code. *)
(* I like it. *)

let read_len t (len: int) =
  let out = Cstruct.sub t.cs t.offset len in
  t.offset <- t.offset + len;
  out

let skip_len t (len: int) =
  t.offset <- t.offset + len

let read_int128 t = read_len t 16

let read_int256 t = read_len t 32

let read_3_bytes_as_int t =
  let b1 = read_byte t in
  let b2 = read_byte t in
  let b3 = read_byte t in
  (b3 lsl 16) + (b2 lsl 8) + b1

let read_tl_bytes t =
  let len = ref (read_byte t) in
  let bytes_read = ref 0 in

  if !len <= 253 then
    bytes_read := !bytes_read + 1
  else begin
    len := read_3_bytes_as_int t;
    bytes_read := !len + 4
  end;

  let out_cs = read_len t !len in

  let padding = (4 - (!bytes_read % 4)) % 4 in
  skip_len t padding;

  out_cs

let read_tl_string t = read_tl_bytes t |> Cstruct.to_string
