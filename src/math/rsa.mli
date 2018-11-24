open! Base

type pub = {
  n: Cstruct.t; (** Modulus *)
  e: Cstruct.t; (** Public exponent *)
}

module RSA: sig
  type t
  val create: pub -> t
  val encrypt: key:t -> Cstruct.t -> Cstruct.t
  (*_ val decrypt: key:key -> Cstruct.t -> Cstruct.t *)
end

val default_keys: pub list

module RsaManager: sig
  type fingerprint = int64
  type key
  type t = key list
  exception FingerprintsNotFound of fingerprint list
  val create_with_pub_keys: pub list -> t
  val default: t
  val find_by_fingerprint: t -> fingerprint -> key
  val find_by_fingerprints: t -> fingerprint list -> key * fingerprint
  val encrypt: key:key -> Cstruct.t -> Cstruct.t
  (*_ val decrypt: key:key -> Cstruct.t -> Cstruct.t *)
end
