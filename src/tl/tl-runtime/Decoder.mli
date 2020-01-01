type t = private {
  cs: Cstruct.t;
  mutable offset: int;
}

val of_cstruct: Cstruct.t -> t

val read_int32_le: t -> int32

val read_int64_le: t -> int64

val read_float: t -> float

val read_byte: t -> int (*_ as int *)

val read_len: t -> int -> Cstruct.t

val skip_len: t -> int -> unit

val read_int128: t -> Cstruct.t

val read_int256: t -> Cstruct.t

val read_tl_bytes: t -> Cstruct.t

val read_tl_string: t -> string

val to_cstruct: t -> Cstruct.t
