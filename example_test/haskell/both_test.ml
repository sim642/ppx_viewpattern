open Example_haskell
open OUnit2

let[@warning "-partial-match"] f_reference ((h :: t) as xs) = h :: xs @ t

let test_f_function =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"f_function" QCheck.(list int) (fun l ->
        QCheck.assume (l <> []);
        Both.f_function l = f_reference l
      )
  )

let test_f_fun =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"f_fun" QCheck.(list int) (fun l ->
        QCheck.assume (l <> []);
        Both.f_fun l = f_reference l
      )
  )

let test_f_param =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"f_param" QCheck.(list int) (fun l ->
        QCheck.assume (l <> []);
        Both.f_param l = f_reference l
      )
  )

let test_f_let_in =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"f_let_in" QCheck.(list int) (fun l ->
        QCheck.assume (l <> []);
        Both.f_let_in l = f_reference l
      )
  )

let test_f_try =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"f_f_try" QCheck.(list int) (fun l ->
        QCheck.assume (l <> []);
        Both.f_try l = f_reference l
      )
  )

let tests =
  "iterator_style" >::: [
    test_f_function;
    test_f_fun;
    test_f_param;
    test_f_let_in;
    test_f_try;
  ]
