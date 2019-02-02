(* hi *)

open! Base

include Types

#ifdef JS_OF_OCAML

include Js_time
module PlatformCrypto = Js_crypto.Crypto
module PlatformSecureRand = Js_secure_rand
module PlatformBigint = Js_bigint.Bigint
module PlatformGzip = Js_gzip.Gzip

#else

include Caml_time
module PlatformCrypto = Caml_crypto.Crypto
module PlatformSecureRand = Caml_secure_rand
module PlatformBigint = Caml_bigint
module PlatformGzip = Caml_gzip.Gzip

#endif
