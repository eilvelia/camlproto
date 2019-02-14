(* hi *)

open! Base

include Types

#ifdef JS_OF_OCAML

let get_current_time = JsTime.get_current_time
module PlatformCrypto = JsCrypto.Crypto
module PlatformSecureRand = JsSecureRand
module PlatformBigint = JsBigint.Bigint
module PlatformGzip = JsGzip.Gzip

#else

let get_current_time = CamlTime.get_current_time
module PlatformCrypto = CamlCrypto
module PlatformSecureRand = CamlSecureRand
module PlatformBigint = CamlBigint
module PlatformGzip = CamlGzip.Gzip

#endif
