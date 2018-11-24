open! Base

val get_current_time: unit -> float
(** Get current time in seconds *)

module PlatformCrypto: Types.PLATFORM_CRYPTO
module PlatformSecureRand: Types.PLATFORM_SECURE_RAND
module PlatformBigint: Types.PLATFORM_BIGINT
(* module PlatformBigint = Caml_bigint *)
module Types = Types
