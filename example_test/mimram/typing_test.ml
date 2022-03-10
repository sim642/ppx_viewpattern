open Example_mimram
open OUnit2

open Typing

let test_normalize _ =
  (* if 3 < 2 then 5 else 1 *)
  assert_equal (Int 1) (normalize ((If (Lt (Int 3, Int 2), Int 5, Int 1))));
  (* if 1+(2+3) < 4 then false else 5 *)
  assert_equal (Int 5) (normalize (If (Lt (Add (Int 1, Add (Int 2, Int 3)), Int 4), Bool false, Int 5)));
  (* if 1+(2+3) < 10 then false else 5 *)
  assert_equal (Bool false) (normalize (If (Lt (Add (Int 1, Add (Int 2, Int 3)), Int 10), Bool false, Int 5)))

let test_typable _ =
  (* if 3 < 2 then 5 else 1 *)
  assert_equal true (typable (If (Lt (Int 3, Int 2), Int 5, Int 1)));
  (* if (1 + 2) < 3 then 4 else 5 *)
  assert_equal true (typable (If (Lt (Add (Int 1, Int 2), Int 3), Int 4, Int 5)));
  (* 1 + false *)
  assert_equal false (typable (Add (Int 1, Bool false)));
  (* if true then 1 else false *)
  assert_equal false (typable (If (Bool true, Int 1, Bool false)))

let test_infer _ =
  (* if 3 < 2 then 5 else 1 *)
  assert_equal TInt (infer (If (Lt (Int 3, Int 2), Int 5, Int 1)));
  (* if (1 + 2) < 3 then 4 else 5 *)
  assert_equal TInt (infer (If (Lt (Add (Int 1, Int 2), Int 3), Int 4, Int 5)))

let tests =
  "typing" >::: [
    "normalize" >:: test_normalize;
    "typable" >:: test_typable;
    "infer" >:: test_infer;
  ]
