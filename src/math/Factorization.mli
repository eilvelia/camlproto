open! Base

module Make (Platform: PlatformTypes.S): sig
  val pq_prime: int64 -> int64 * int64
  (** [pq_prime n]. Returns [(p, q)]. *)
end
