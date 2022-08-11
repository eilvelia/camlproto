open! Base

val pq_factorize : Cstruct.t -> Cstruct.t * Cstruct.t
(** [pq_factorize n], where [n] is a product of two prime numbers represented
    as a big endian byte array, factorizes [n] into a [(p, q)] pair. *)

val pq_factorize_int64 : int64 -> int64 * int64
