open! Base

module Make (ECB : PlatformTypes.AES): sig
  val encrypt : key:Cstruct.t -> iv:Cstruct.t -> Cstruct.t -> Cstruct.t
  (** [encrypt ~key ~iv plaintext]. This function does not add padding,
      the size of the [plaintext] must be divisible by 16. *)

  val decrypt : key:Cstruct.t -> iv:Cstruct.t -> Cstruct.t -> Cstruct.t
  (** [decrypt ~key ~iv ciphertext] *)
end
