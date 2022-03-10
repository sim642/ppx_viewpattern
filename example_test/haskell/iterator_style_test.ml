open Example_haskell
open OUnit2

let test_length =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"length" QCheck.(list int) (fun l ->
        Iterator_style.length l = List.length l
      )
  )

let test_map =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"map" QCheck.(pair (fun1 Observable.int int) (list int)) (fun (Fun (_, f), l) ->
        Iterator_style.map f l = List.map f l
      )
  )

let test_foldr =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"foldr" QCheck.(triple (fun2 Observable.int Observable.int int) int (list int)) (fun (Fun (_, f), z, l) ->
        Iterator_style.foldr f z l = List.fold_right f l z
      )
  )

let tests =
  "iterator_style" >::: [
    test_length;
    test_map;
    test_foldr;
  ]
