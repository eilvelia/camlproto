module Math = Math.Make(PlatformCaml)

open Math

(* TODO: Use core_bench? It requires a lot of dependencies, though. *)

let () =
  (* Currently about 17/s on my machine *)
  let name = "prime factorization" in
  let res = Benchmark.throughput1 1 ~name pq_factorize_int64 0x1dfaf951107f49dfL in
  Benchmark.tabulate res
