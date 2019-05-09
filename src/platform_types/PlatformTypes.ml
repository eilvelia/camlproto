open! Base

module type HashFn = sig
  type t
  val init: unit -> t
  val feed: t -> Cstruct.t -> unit
  val get: t -> Cstruct.t
  val digest: Cstruct.t -> Cstruct.t
end

module type AES = sig
  type key
  val ecb_create_key: Cstruct.t -> key
  val ecb_encrypt: key:key -> Cstruct.t -> Cstruct.t
  val ecb_decrypt: key:key -> Cstruct.t -> Cstruct.t
end

module type Crypto = sig
  module SHA1: HashFn
  module SHA256: HashFn
  module AES: AES
end

module type SecureRand = sig
  val rand_cs: int -> Cstruct.t
end

module type Bigint = sig
  type t
  exception Overflow

  (* val zero: t
  val one: t
  val size: t -> int
  val shift_right: t -> int -> t
  val shift_left: t -> int -> t
  val (land): t -> t -> t
  val (+): t -> t -> t
  val (-): t -> t -> t
  val of_int: int -> t
  val of_int32: int32 -> t
  val of_int64: int64 -> t
  val to_int: t -> int
  val to_int32: t -> int32
  val to_int64: t -> int64
  val powm: t -> t -> t -> t
  val lt: t -> t -> bool
  val gt: t -> t -> bool *)

  val zero: t
  val one: t
  val (+): t -> t -> t
  val (-): t -> t -> t
  val of_int: int -> t
  val powm: t -> t -> t -> t
  val (<): t -> t -> bool
  val (>): t -> t -> bool
  val of_cstruct_be: Cstruct.t -> t
  (* val into_cstruct_be: t -> Cstruct.t -> unit *)
  val to_cstruct_be: t -> Cstruct.t
end

module type Gzip = sig
  exception Error of string
  (* val compress: Cstruct.t -> Cstruct.t *)
  val decompress: Cstruct.t -> Cstruct.t
end

module type S = sig
  val get_current_time: unit -> float
  module Crypto: Crypto
  module SecureRand: SecureRand
  module Bigint: Bigint
  module Gzip: Gzip
end
