open OUnit2

let g = function
  | i when i >= 10 -> Some (i - 10)
  | _ -> None

let f1 = function
  | [%view? i when g] -> i

let test_f1 _ =
  assert_equal None (f1 1);
  assert_equal (Some 0) (f1 10);
  assert_equal (Some 5) (f1 15)

let f2 = function
  | [%view? None when g] -> true
  | _ -> false

let test_f2 _ =
  assert_equal true (f2 1);
  assert_equal false (f2 10);
  assert_equal false (f2 15)

let f3 = function
  | [%view? Some i when g] -> i
  | i -> i

let test_f3 _ =
  assert_equal 1 (f3 1);
  assert_equal 0 (f3 10);
  assert_equal 5 (f3 15)

let f4 = function
  | ([%view? i when g], j) -> (i, j)

let test_f4 _ =
  assert_equal (None, 10) (f4 (1, 10));
  assert_equal (Some 0, 10) (f4 (10, 10));
  assert_equal (Some 5, 10) (f4 (15, 10))

let f5 = function
  | (i, [%view? j when g]) -> (i, j)

let test_f5 _ =
  assert_equal (10, None) (f5 (10, 1));
  assert_equal (10, Some 0) (f5 (10, 10));
  assert_equal (10, Some 5) (f5 (10, 15))

let f6 = function
  | ([%view? i when g], [%view? j when g]) -> (i, j)

let test_f6 _ =
  assert_equal (None, Some 0) (f6 (1, 10));
  assert_equal (None, Some 5) (f6 (1, 15));
  assert_equal (Some 0, None) (f6 (10, 1));
  assert_equal (Some 5, None) (f6 (15, 1));
  assert_equal (Some 0, Some 5) (f6 (10, 15))

let f7 = function
  | ([%view? Some i when g], [%view? Some j when g]) -> (i, j)
  | (i, j) -> (i, j)

let test_f7 _ =
  assert_equal (1, 2) (f7 (1, 2));
  assert_equal (1, 10) (f7 (1, 10));
  assert_equal (10, 1) (f7 (10, 1));
  assert_equal (0, 5) (f7 (10, 15))

let f8 = function
  | [%view? Some i when g] when i >= 5 -> i
  | i -> i

let test_f8 _ =
  assert_equal 1 (f8 1);
  assert_equal 10 (f8 10);
  assert_equal 5 (f8 15);
  assert_equal 10 (f8 20)

let f9 = function
  | ([%view? Some i when g], [%view? Some j when g]) when i + j >= 10 -> (i, j)
  | (i, j) -> (i, j)

let test_f9 _ =
  assert_equal (1, 2) (f9 (1, 2));
  assert_equal (1, 10) (f9 (1, 10));
  assert_equal (10, 1) (f9 (10, 1));
  assert_equal (10, 15) (f9 (10, 15));
  assert_equal (5, 5) (f9 (15, 15))

let f10 = function
  | [%view? Some [%view? Some i when g] when g] -> i
  | i -> i

let test_f10 _ =
  assert_equal 1 (f10 1);
  assert_equal 10 (f10 10);
  assert_equal 15 (f10 15);
  assert_equal 0 (f10 20);
  assert_equal 5 (f10 25)

let f11 = function
  | [%view? Some i when function [%view? Some i when g] -> Some i | _ -> None] -> i
  | i -> i

let test_f11 _ =
  assert_equal 1 (f11 1);
  assert_equal 0 (f11 10);
  assert_equal 5 (f11 15)


let tests =
  "ppx_viewpattern" >::: [
    "f1" >:: test_f1;
    "f2" >:: test_f2;
    "f3" >:: test_f3;
    "f4" >:: test_f4;
    "f5" >:: test_f5;
    "f6" >:: test_f6;
    "f7" >:: test_f7;
    "f8" >:: test_f8;
    "f9" >:: test_f9;
    "f10" >:: test_f10;
    "f11" >:: test_f11;
  ]

let () =
  run_test_tt_main tests
