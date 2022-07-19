type t = Buffer.t

(* TODO: Use a Cstruct.t with predefined size? *)

let create () : t = Buffer.create 128

(* module Int32Infix = struct
  external (land) : int32 -> int32 -> int32 = "%int32_and"
  external (asr) : int32 -> int -> int32 = "%int32_asr"
end

module Int64Infix = struct
  external (land) : int64 -> int64 -> int64 = "%int64_and"
  external (asr) : int64 -> int -> int64 = "%int64_asr"
end *)

let add_int32_le = Buffer.add_int32_le

let add_int64_le = Buffer.add_int64_le

let add_float b f =
  let bits = Int64.bits_of_float f in
  add_int64_le b bits

let add_cstruct b cs =
  (* TODO: inefficient *)
  cs |> Cstruct.to_bytes |> Buffer.add_bytes b

let add_int128 b cs =
  if Cstruct.length cs <> 16 then invalid_arg "add_int128: invalid length";
  add_cstruct b cs

let add_int256 b cs =
  if Cstruct.length cs <> 32 then invalid_arg "add_int256: invalid length";
  add_cstruct b cs

let encode_len buf len =
  if len <= 253 then
    Buffer.add_char buf (Char.unsafe_chr len)
  else
    Buffer.add_int32_le buf @@ Int32.of_int (len lsl 8 + 254)

let add_padding buf len =
  let real_len = if len <= 253 then len + 1 else len in
  let remainder = real_len mod 4 in
  let padding = (4 - remainder) land 0b11 in
  for _ = 1 to padding do
    Buffer.add_char buf '\000'
  done

let add_tl_bytes buf cs =
  let len = Cstruct.length cs in
  encode_len buf len;
  add_cstruct buf cs;
  add_padding buf len

let add_tl_string buf str =
  let len = String.length str in
  encode_len buf len;
  Buffer.add_string buf str;
  add_padding buf len

let encode (f : t -> 'a -> unit) v =
  let buf = create () in
  f buf v;
  buf

let to_buffer = Fun.id

let to_cstruct t =
  (* inefficient *)
  t |> Buffer.to_bytes |> Cstruct.of_bytes
