open! Base

module type Serializable = sig
  type t
  val encode: Encoder.t -> t -> unit (* encode bare *)
  val encode_boxed: Encoder.t -> t -> unit
end

module type SerMin = sig
  type t
  val encode: Encoder.t -> t -> unit
end

exception DeserializationError of string

module type Deserializable = sig
  type t
  val decode: Decoder.t -> t
end

module type DesMin = sig include Deserializable end

module type SerDes = sig
  type t
  include Serializable with type t := t
  include Deserializable with type t := t
end

module type SerDesMin = sig
  type t
  include SerMin with type t := t
  include DesMin with type t := t
end

type _ tl_comb_kind =
  | TLFunction: [> `TLFunction ] tl_comb_kind
  | TLConstructor: [> `TLConstructor ] tl_comb_kind

type any_comb_kind = [ `TLFunction | `TLConstructor ]

module type TLObject = sig include SerDes end

module type TLComb = sig
  type t
  val kind: any_comb_kind tl_comb_kind
  val magic: int32
  include SerDes with type t := t
end

module type TLConstr = sig
  include TLComb
  val kind: [> `TLConstructor ] tl_comb_kind
end

module type TLFunc = sig
  include TLComb
  val kind: [> `TLFunction ] tl_comb_kind
  module ResultM: TLObject
end

module type TLCombMin = sig
  type t
  val magic: int32
  include SerDesMin with type t := t
end

module ExtendComb (M: TLCombMin) = struct
  open M
  let encode_boxed enc t = Encoder.add_int32_le enc magic; encode enc t
end

module MakeConstr (M: TLCombMin): (TLConstr with type t = M.t) = struct
  include M
  let kind = TLConstructor
  include ExtendComb(M)
end

module MakeConstrR (M: TLCombMin): (TLConstr with type t := M.t) = MakeConstr(M)

module type TLType = sig type tl_type ;; include SerDes end

module type TLTypeMin = sig include SerDesMin end

module MakeTLType (M: TLTypeMin): (TLType with type t = M.t) = struct
  include M
  let encode_boxed = encode
  type tl_type
end

module MakeTLTypeR (M: TLTypeMin): (TLType with type t := M.t) = MakeTLType(M)

module type TLFuncMin = sig
  include TLCombMin
  module ResultM: TLObject
end

module MakeFunc (M: TLFuncMin)
: (TLFunc with type t = M.t and module ResultM = M.ResultM)
= struct
  include M
  let kind = TLFunction
  include ExtendComb(M)
end

module MakeFuncR (M: TLFuncMin)
  : (TLFunc with type t := M.t and module ResultM = M.ResultM)
  = MakeFunc(M)
