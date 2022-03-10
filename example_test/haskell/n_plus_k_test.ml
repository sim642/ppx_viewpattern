open Example_haskell
open OUnit2

let rec fib_reference = function
  | 0 -> 1
  | 1 -> 1
  | n -> fib_reference (n - 1) + fib_reference (n - 2)

let test_fib =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"fib" QCheck.(0 -- 15) (fun n ->
        N_plus_k.fib n = fib_reference n
      )
  )

let tests =
  "n_plus_k" >::: [
    test_fib;
  ]
