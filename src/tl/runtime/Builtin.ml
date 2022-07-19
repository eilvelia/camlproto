open Types

module TL_int = struct
  type t = int
  let magic () = 0xa8509bdal
  (* Does not work on 32-bit platforms *)
  let [@inline] encode enc t = Encoder.add_int32_le enc (Int32.of_int t)
  let [@inline] decode dec = Decoder.read_int32_le dec |> Int32.to_int
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TLT_Int = struct
  type t =
    | TL_int of TL_int.t
    [@@unboxed]
  let encode enc t = match t with
    | TL_int x -> TL_int.encode_boxed enc x
  let decode dec =
    let magic = Decoder.read_int32_le dec in
    match magic with
    | x when x = TL_int.magic () -> TL_int (TL_int.decode dec)
    | x -> raise @@ DeserializationError x
  include MakeType(struct
    type nonrec t = t
    let encode = encode
    let decode = decode
  end)
end

module TL_nat = struct
  type t = int32
  let magic () = 0xf2b7df9el (* crc32 of `nat ? = Nat` *)
  let [@inline] encode enc t = Encoder.add_int32_le enc t
  let [@inline] decode dec = Decoder.read_int32_le dec
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TL_long = struct
  type t = int64
  let magic () = 0x22076cbal
  let [@inline] encode enc t = Encoder.add_int64_le enc t
  let [@inline] decode dec = Decoder.read_int64_le dec
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TL_double = struct
  type t = float
  let magic () = 0x2210c154l
  let [@inline] encode enc t = Encoder.add_float enc t
  let [@inline] decode dec = Decoder.read_float dec
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TL_string = struct
  type t = string
  let magic () = 0xb5286e24l
  let [@inline] encode enc t = Encoder.add_tl_string enc t
  let [@inline] decode dec = Decoder.read_tl_string dec
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TL_bytes = struct
  type t = Cstruct.t
  let magic () = 0xe937bb82l (* crc32 of `bytes = Bytes` *)
  let [@inline] encode enc t = Encoder.add_tl_bytes enc t
  let [@inline] decode dec = Decoder.read_tl_bytes dec
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TL_int128 = struct
  type t = Cstruct.t
  let magic () = 0x84ccf7b7l (* crc32 of `int128 4*[ int ] = Int128` *)
  let [@inline] encode enc t = Encoder.add_int128 enc t
  let [@inline] decode dec = Decoder.read_int128 dec
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TL_int256 = struct
  type t = Cstruct.t
  let magic () = 0x7bedeb5bl (* crc32 of `int256 8*[ int ] = Int256` *)
  let [@inline] encode enc t = Encoder.add_int256 enc t
  let [@inline] decode dec = Decoder.read_int256 dec
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TLT_Bool = struct
  type t = bool
  let encode enc t =
    let magic = match t with
      | true -> 0x997275b5l (* boolTrue *)
      | false -> 0xbc799737l (* boolFalse *)
    in
    Encoder.add_int32_le enc magic
  let decode dec =
    let magic = Decoder.read_int32_le dec in
    match magic with
    | 0x997275b5l -> true
    | 0xbc799737l -> false
    | x -> raise @@ DeserializationError x
  include MakeType(struct
    type nonrec t = t
    let encode = encode
    let decode = decode
  end)
end

module TL_vector (A : TLObject) = struct
  type t = A.t list
  let magic () = 0x1cb5c415l
  let encode enc t =
    Encoder.add_int32_le enc (List.length t |> Int32.of_int);
    List.iter (A.encode enc) t
  let decode dec =
    let len = Decoder.read_int32_le dec |> Int32.to_int in
    let xs = ref [] in
    for _ = 1 to len do
      let x = A.decode dec in
      xs := x :: !xs
    done;
    !xs
  include MakeConstr(struct
    type nonrec t = t
    let magic = magic
    let encode = encode
    let decode = decode
  end)
end

module TLT_Vector (A : TLObject) : sig
  type t =
    | TL_vector of TL_vector(A).t
    [@@unboxed]
  include TLType with type t := t
end = struct
  module TL_vectorA = TL_vector(A)
  type t =
    | TL_vector of TL_vectorA.t
    [@@unboxed]
  let encode enc t =
    match t with
    | TL_vector x -> TL_vectorA.encode_boxed enc x
  let decode dec =
    let magic = Decoder.read_int32_le dec in
    match magic with
    | 0x1cb5c415l -> TL_vector (TL_vectorA.decode dec)
    | x -> raise @@ DeserializationError x
  include MakeType(struct
    type nonrec t = t
    let encode = encode
    let decode = decode
  end)
end
