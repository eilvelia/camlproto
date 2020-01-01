type t = int32

external (+) : int32 -> int32 -> int32 = "%int32_add"
external (-) : int32 -> int32 -> int32 = "%int32_sub"
external ( * ) : int32 -> int32 -> int32 = "%int32_mul"
external (/) : int32 -> int32 -> int32 = "%int32_div"

external (land) : int32 -> int32 -> int32 = "%int32_and"
external (lor) : int32 -> int32 -> int32 = "%int32_or"
external (lxor) : int32 -> int32 -> int32 = "%int32_xor"
external (lsl) : int32 -> int -> int32 = "%int32_lsl"
external (asr) : int32 -> int -> int32 = "%int32_asr"
external (lsr) : int32 -> int -> int32 = "%int32_lsr"

let (=) (x : int32) (y : int32) = Int32.compare x y = 0
let (<>) x y = not (x = y) 
