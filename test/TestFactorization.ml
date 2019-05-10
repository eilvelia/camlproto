open OUnit

let pq_prime = Common.Math.Factorization.pq_prime

let suite =
  "pq_prime" >:: (fun _ ->
    let (p, q) = pq_prime 0x1dfaf951107f49dfL in
    assert_equal p 1124459477L;
    assert_equal q 1921201379L
  )
