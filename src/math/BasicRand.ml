open! Base

include Random
let () = init 2751212

(* let int128 () =
  let cs = Cstruct.create_unsafe 16 in
  let rand1 = int64 9223372036854775807L in
  let rand2 = int64 9223372036854775807L in
  Cstruct.LE.set_uint64 cs 0 rand1;
  Cstruct.LE.set_uint64 cs 8 rand2;
  cs

let int256 () =
  let cs = Cstruct.create_unsafe 32 in
  let rand1 = int64 9223372036854775807L in
  let rand2 = int64 9223372036854775807L in
  let rand3 = int64 9223372036854775807L in
  let rand4 = int64 9223372036854775807L in
  Cstruct.LE.set_uint64 cs 0 rand1;
  Cstruct.LE.set_uint64 cs 8 rand2;
  Cstruct.LE.set_uint64 cs 16 rand3;
  Cstruct.LE.set_uint64 cs 24 rand4;
  cs *)
