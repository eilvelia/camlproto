(* hi *)

open! Base

#ifdef JS_OF_OCAML

include Js_time
module PlatformCrypto = Js_crypto.JsCrypto
module PlatformSecureRand = Js_secure_rand
module PlatformBigint = Js_bigint.Bigint

#else

include Caml_time
module PlatformCrypto = Caml_crypto.CamlCrypto
module PlatformSecureRand = Caml_secure_rand
module PlatformBigint = Caml_bigint

#endif

module Types = Types
