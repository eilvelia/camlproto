open! Base

include (module type of Types)

val get_current_time: unit -> float
(** Get current time in seconds *)

module PlatformCrypto: PLATFORM_CRYPTO
module PlatformSecureRand: PLATFORM_SECURE_RAND
module PlatformBigint: PLATFORM_BIGINT
module PlatformGzip: GZIP
