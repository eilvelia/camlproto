open! Base

module Make (ECB: PlatformTypes.AES): sig
  val encrypt: Cstruct.t -> Cstruct.t -> Cstruct.t -> Cstruct.t
  (** [encrypt plaintext key iv] *)

  val decrypt: Cstruct.t -> Cstruct.t -> Cstruct.t -> Cstruct.t
  (** [decrypt ciphertext key iv] *)
end
