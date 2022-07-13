open! Base
open Js_of_ocaml

type sha1_t

class type js_sha1 = object
  method init: unit -> sha1_t Js.meth
  (* method feed: t -> Typed_array.arrayBuffer Js.t -> unit Js.meth *)
  method feed: sha1_t -> Cstruct.buffer -> unit Js.meth
  method get: sha1_t -> Typed_array.arrayBuffer Js.t Js.meth
end

let js_sha1: js_sha1 Js.t = Js.Unsafe.pure_js_expr "js_sha1"

type sha256_t

class type js_sha256 = object
  method init: unit -> sha256_t Js.meth
  method feed: sha256_t -> Cstruct.buffer -> unit Js.meth
  method get: sha256_t -> Typed_array.arrayBuffer Js.t Js.meth
end

let js_sha256: js_sha256 Js.t = Js.Unsafe.pure_js_expr "js_sha1"

type aes_t

class type js_aes = object
  method ecbCreateKey: Cstruct.buffer -> aes_t Js.meth
  method ecbEncrypt: aes_t -> Cstruct.buffer -> Typed_array.arrayBuffer Js.t Js.meth
  method ecbDecrypt: aes_t -> Cstruct.buffer -> Typed_array.arrayBuffer Js.t Js.meth
end

let js_aes: js_aes Js.t = Js.Unsafe.pure_js_expr "js_aes"

module Crypto: PlatformTypes.Crypto = struct
  module SHA1 = struct
    type t = sha1_t
    let init () = js_sha1##init ()
    let feed t cs = js_sha1##feed t (Cstruct.to_bigarray cs)
    let get t = js_sha1##get t
      |> Typed_array.Bigstring.of_arrayBuffer
      |> Cstruct.of_bigarray
    let digest cs =
      let t = init   () in
      feed t cs;
      get t
  end

  module SHA256 = struct
    type t = sha256_t
    let init () = js_sha256##init ()
    let feed t cs = js_sha256##feed t (Cstruct.to_bigarray cs)
    let get t = js_sha256##get t
      |> Typed_array.Bigstring.of_arrayBuffer
      |> Cstruct.of_bigarray
    let digest cs =
      let t = init () in
      feed t cs;
      get t
  end

  module AES = struct
    type key = aes_t
    let ecb_create_key cs = js_aes##ecbCreateKey (Cstruct.to_bigarray cs)
    let ecb_encrypt ~key cs = js_aes##ecbEncrypt key (Cstruct.to_bigarray cs)
      |> Typed_array.Bigstring.of_arrayBuffer
      |> Cstruct.of_bigarray
    let ecb_decrypt ~key cs = js_aes##ecbDecrypt key (Cstruct.to_bigarray cs)
      |> Typed_array.Bigstring.of_arrayBuffer
      |> Cstruct.of_bigarray
  end
end
