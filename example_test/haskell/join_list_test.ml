open Example_haskell
open OUnit2

let empty = Join_list.Empty
let single x = Join_list.Single x
let join x y = Join_list.Join (x, y)

let arbitrary_jlist (x: 'a QCheck.arbitrary): 'a Join_list.jlist QCheck.arbitrary =
  let jlist_join = join in
  let gen_jlist = QCheck.Gen.(sized @@ fix (fun self n ->
      match n with
      | 0 -> return empty
      | 1 -> map single x.gen
      | n ->
        frequency [
          1, map single x.gen;
          2, map2 jlist_join (self (n / 2)) (self (n / 2));
        ]
    ))
  in
  QCheck.make gen_jlist

let rec length_direct: 'a Join_list.jlist -> int = function
  | Empty -> 0
  | Single _ -> 1
  | Join (x, y) -> length_direct x + length_direct y

let test_length =
  QCheck_ounit.to_ounit2_test (
    QCheck.Test.make ~name:"length" QCheck.(arbitrary_jlist int) (fun jl ->
        Join_list.length jl = length_direct jl
      )
  )

let tests =
  "join_list" >::: [
    test_length;
  ]
