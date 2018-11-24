open! Base

type t = Buffer.t

(* TODO: Use Cstruct.t instead of Buffer.t *)

let create (): t = Buffer.create 128

let unsafe_add_int_ch buf i =
  Buffer.add_char buf (Char.unsafe_of_int i)
let unsafe_add_int32_ch buf i =
  Buffer.add_char buf (Char.unsafe_of_int Int32.(to_int_trunc i))
let unsafe_add_int64_ch buf i =
  Buffer.add_char buf (Char.unsafe_of_int Int64.(to_int_trunc i))

(* Be like vim. *)

let add_int32_le b (i: int32) =
  let open Int32 in
  unsafe_add_int32_ch b (0xffl land i);
  unsafe_add_int32_ch b (0xffl land (i asr 8));
  unsafe_add_int32_ch b (0xffl land (i asr 16));
  unsafe_add_int32_ch b (0xffl land (i asr 24))

let add_int64_le b (i: int64) =
  let open Int64 in
  unsafe_add_int64_ch b (0xffL land i);
  unsafe_add_int64_ch b (0xffL land (i asr 8));
  unsafe_add_int64_ch b (0xffL land (i asr 16));
  unsafe_add_int64_ch b (0xffL land (i asr 24));
  unsafe_add_int64_ch b (0xffL land (i asr 32));
  unsafe_add_int64_ch b (0xffL land (i asr 40));
  unsafe_add_int64_ch b (0xffL land (i asr 48));
  unsafe_add_int64_ch b (0xffL land (i asr 56))

let add_float b (i: float) =
  let bits: int64 = Int64.bits_of_float i in
  add_int64_le b bits

let add_3_bytes_from_int b (i: int) =
  unsafe_add_int_ch b (0xff land i);
  unsafe_add_int_ch b (0xff land (i asr 8));
  unsafe_add_int_ch b (0xff land (i asr 16))

let add_cstruct b (cs: Cstruct.t) =
  (* inefficient *)
  cs |> Cstruct.to_bytes |> Buffer.add_bytes b

exception InvalidLengthError

let add_int128 b (cs: Cstruct.t) =
  if Cstruct.len cs <> 16 then raise InvalidLengthError;
  add_cstruct b cs

let add_int256 b (cs: Cstruct.t) =
  if Cstruct.len cs <> 32 then raise InvalidLengthError;
  add_cstruct b cs

let add_tl_bytes buf (cs: Cstruct.t) =
  let len = Cstruct.len cs in

  if len <= 253 then
    Buffer.add_char buf (Char.unsafe_of_int len)
  else begin
    Buffer.add_char buf '\254';
    add_3_bytes_from_int buf len
  end;

  add_cstruct buf cs;

  let n = (4 - ((len + if len <= 253 then 1 else 0) % 4)) % 4 in
  for _ = 1 to n do
    Buffer.add_char buf '\000'
  done

(* more efficient than `Cstruct.of_string str |> add_tl_bytes buf` *)
let add_tl_string buf str =
  let len = String.length str in

  if len <= 253 then
    Buffer.add_char buf (Char.unsafe_of_int len)
  else begin
    Buffer.add_char buf '\254';
    add_3_bytes_from_int buf len
  end;

  Buffer.add_string buf str;

  let n = (4 - ((len + if len <= 253 then 1 else 0) % 4)) % 4 in
  for _ = 1 to n do
    Buffer.add_char buf '\000'
  done

let encode (f: t -> 'a -> unit) (v: 'a) =
  let buf = create () in
  f buf v;
  buf

let to_buffer (t: t) = t

let to_cstruct (t: t) =
  (* inefficient *)
  t |> Buffer.contents_bytes |> Cstruct.of_bytes
