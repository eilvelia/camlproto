open! Base
open Types

module TLInt = MakeConstr(struct
  type t = int
  let magic = 0xa8509bdal
  let encode enc t = Encoder.add_int32_le enc (Int32.of_int_exn t)
  let decode dec = Decoder.read_int32_le dec |> Int.of_int32_exn
end)

module TLTInt = struct
  type t = TLInt of TLInt.t
  include MakeTLTypeR(struct
    type nonrec t = t
    let encode enc t = match t with
      | TLInt x -> TLInt.encode_boxed enc x
    let decode dec =
      let magic = Decoder.read_int32_le dec in
      let open Int32 in
      match magic with
      | x when x = TLInt.magic -> TLInt (TLInt.decode dec)
      | x -> raise @@ DeserializationError ("Invalid Int magic " ^ to_string x)
  end)
end

module TLNat = MakeConstr(struct
  type t = int32
  let magic = 0xf2b7df9el (* crc32 of `nat ? = Nat` *)
  let encode enc t = Encoder.add_int32_le enc t
  let decode dec = Decoder.read_int32_le dec
end)

module TLLong = MakeConstr(struct
  type t = int64
  let magic = 0x22076cbal
  let encode enc t = Encoder.add_int64_le enc t
  let decode dec = Decoder.read_int64_le dec
end)

(* TODO: maybe double (de)serialization doesn't work... I didn't test it. *)
module TLDouble = MakeConstr(struct
  type t = float
  let magic = 0x2210c154l
  let encode enc t = Encoder.add_float enc t
  let decode dec = Decoder.read_float dec
end)

module TLString = MakeConstr(struct
  type t = string
  let magic = 0xb5286e24l
  let encode enc t = Encoder.add_tl_string enc t
  let decode dec = Decoder.read_tl_string dec
end)

module TLBytes = MakeConstr(struct
  type t = Cstruct.t
  let magic = 0xe937bb82l (* crc32 of `bytes = Bytes` *)
  let encode enc t = Encoder.add_tl_bytes enc t
  let decode dec = Decoder.read_tl_bytes dec
end)

module TLInt128 = MakeConstr(struct
  type t = Cstruct.t
  let magic = 0x84ccf7b7l (* crc32 of `int128 4*[ int ] = Int128` *)
  let encode enc t = Encoder.add_int128 enc t
  let decode dec = Decoder.read_int128 dec
end)

module TLInt256 = MakeConstr(struct
  type t = Cstruct.t
  let magic = 0x7bedeb5bl (* crc32 of `int256 8*[ int ] = Int256` *)
  let encode enc t = Encoder.add_int256 enc t
  let decode dec = Decoder.read_int256 dec
end)

(* module Object = struct
  type t = C_object : 'a * (module TLObject with type t = 'a) -> t
  include MakeTLTypeR(struct
    type nonrec t = t
    let encode enc t = match t with
      | C_object (x, m) -> let module M = (val m) in M.encode_boxed enc x
    let decode dec =
      let magic = Decoder.read_int32_le dec in
      let open Int32 in
      match magic with
      (* ... *)
      | x when x = M.magic -> C_object (M.decode dec)
      | x -> raise @@ DeserializationError ("Invalid Object magic " ^ to_string x)
  end)
end *)

module TLBool = MakeTLType(struct
  type t = bool
  let encode enc t =
    let magic = match t with
      | true -> 0x997275b5l (* boolTrue *)
      | false -> 0xbc799737l (* boolFalse *)
    in
    Encoder.add_int32_le enc magic
  let decode dec =
    let magic = Decoder.read_int32_le dec in
    let open Int32 in
    match magic with
    | 0x997275b5l -> true
    | 0xbc799737l -> false
    | x -> raise @@ DeserializationError ("Invalid Bool magic " ^ to_string x)
end)

module C_vector (A: TLObject) = struct
  type t = A.t list
  include MakeConstrR(struct
    type nonrec t = t
    let magic = 0x1cb5c415l
    let encode enc t =
      Encoder.add_int32_le enc (List.length t |> Int32.of_int_exn);
      List.iter t ~f:(fun a -> A.encode enc a)
    let decode dec =
      let len = Decoder.read_int32_le dec |> Int32.to_int_exn in
      let list = ref [] in
      for _ = 1 to len do
        let el = A.decode dec in
        list := el :: !list
      done;
      !list
  end)
end

module Vector (A: TLObject) = struct
  module M_vector = C_vector(A)
  type t = C_vector of M_vector.t
  include MakeTLTypeR(struct
    type nonrec t = t
    let encode enc t = match t with
      | C_vector x -> M_vector.encode_boxed enc x
    let decode dec =
      let magic = Decoder.read_int32_le dec in
      let open Int32 in
      match magic with
      | x when x = M_vector.magic -> C_vector (M_vector.decode dec)
      | x -> raise @@ DeserializationError ("Invalid Vector magic " ^ to_string x)
  end)
end
