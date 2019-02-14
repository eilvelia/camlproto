open! Base

let () = Nocrypto_entropy_unix.initialize ()

let rand_cs size = Nocrypto.Rng.generate size
