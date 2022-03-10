open OUnit2

let tests =
  "example_haskell" >::: [
    Both_test.tests;
    Iterator_style_test.tests;
    Join_list_test.tests;
    N_plus_k_test.tests;
  ]

let () =
  run_test_tt_main tests
