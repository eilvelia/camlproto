module type TLSerializable = sig
  type t
  val encode : Encoder.t -> t -> unit
end

module type TLDeserializable = sig
  type t
  val decode : Decoder.t -> t
end

(* TODO: A better error *)
exception DeserializationError of int32

module type TLSerDes = sig
  type t
  include TLSerializable with type t := t
  include TLDeserializable with type t := t
end

module type TLAnyType = sig include TLSerDes end
(** A bare or boxed type.
    This is similar to TL's [Type], which is usually used in the
    optional parameters: [comb {X : Type} ... = ...]. *)

module type TLComb = sig
  type t
  val magic : unit -> int32
  include TLSerDes with type t := t
end

module type TLConstr = sig
  type t
  type tl_constr
  include TLComb with type t := t
  val encode_boxed : Encoder.t -> t -> unit
end

(* Note: Instead of [encode_boxed], it is possible to create
   [Boxed : functor (M : TLConstr) -> TLAnyType] that replaces [encode] with
   the boxed version, which would make boxed and unboxed encodings of
   constructors interchangeable in the functions that use
   [TLSerializable] / [TLAnyType] module types.
   However, this abstraction is unneeded at the moment. *)

module type TLFunc = sig
  type t
  type tl_func
  module ResultM : TLAnyType
  include TLComb with type t := t
end

(* TODO: Add magic and encode_boxed for TL boxed types *)

module type TLType = sig
  type t
  type tl_type
  include TLSerDes with type t := t
end
