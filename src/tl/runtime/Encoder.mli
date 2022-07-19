type t = private Buffer.t

val create : unit -> t

val add_int32_le : t -> int32 -> unit

val add_int64_le : t -> int64 -> unit

val add_float : t -> float -> unit

val add_cstruct : t -> Cstruct.t -> unit

val add_int128 : t -> Cstruct.t -> unit

val add_int256 : t -> Cstruct.t -> unit

val add_tl_bytes : t -> Cstruct.t -> unit
(** Uses the TL string encoding *)

val add_tl_string : t -> string -> unit
(** Uses the TL string encoding *)

val encode : (t -> 'a -> unit) -> 'a -> t

val to_buffer : t -> Buffer.t

val to_cstruct : t -> Cstruct.t
