module type Serializable = sig
  type t

  val encode : Encoder.t -> t -> unit
  (** [encode enc t] serializes the bare version of [t] *)

  val encode_boxed : Encoder.t -> t -> unit
end

module type SerMin = sig
  type t
  val encode : Encoder.t -> t -> unit
end

module type Deserializable = sig
  type t
  val decode : Decoder.t -> t
end

module type DesMin = sig include Deserializable end

(* TODO: A better error *)
exception DeserializationError of int32

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

module type TLObject = sig include SerDes end

module type TLComb = sig
  type t
  val kind : [ `TLFunction | `TLConstructor ]
  val magic : unit -> int32
  include SerDes with type t := t
end

module type TLCombMin = sig
  type t
  val magic : unit -> int32
  include SerDesMin with type t := t
end

module type TLConstr = sig
  include TLComb
  type tl_constr
end

module type TLFunc = sig
  include TLComb
  module ResultM : TLObject
  type tl_func
end

module type TLFuncMin = sig
  include TLCombMin
  module ResultM : TLObject
end

module type TLType = sig
  type t
  include SerDes with type t := t
  type tl_type
end

module type TLTypeMin = sig include SerDesMin end

module MakeEncodeBoxed (M : TLCombMin) = struct
  open M
  let encode_boxed enc t =
    Encoder.add_int32_le enc (magic ());
    encode enc t
end

module MakeConstr (M : TLCombMin) : TLConstr with type t := M.t = struct
  include M
  type tl_constr
  let kind = `TLConstructor
  include MakeEncodeBoxed(M)
end

module MakeFunc (M : TLFuncMin)
  : TLFunc with type t := M.t and module ResultM := M.ResultM
= struct
  include M
  type tl_func
  let kind = `TLFunction
  include MakeEncodeBoxed(M)
end

module MakeType (M : TLTypeMin) : TLType with type t := M.t = struct
  include M
  let encode_boxed = encode (*_ TODO: calculate magic for types *)
  type tl_type
end
