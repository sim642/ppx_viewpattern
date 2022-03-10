open OUnit2

let tests =
  "example_mimram" >::: [
    Typing_test.tests;
  ]

let () =
  run_test_tt_main tests
