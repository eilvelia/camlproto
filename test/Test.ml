open OUnit

let suite =
  "Camlproto" >::: [
    TestFactorization.suite;
    TestAesIge.suite;
  ]

let _ = run_test_tt_main suite
