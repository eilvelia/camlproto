open OUnit

let suite =
  "Camlproto" >::: [
    Test_factorization.suite;
    Test_aes_ige.suite;
  ]

let _ = run_test_tt_main suite
