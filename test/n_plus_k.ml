(** https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns#nk-patterns *)

let np k n =
  if k <= n then
    Some (n - k)
  else
    None

let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | [%view? Some n when np 2] -> fib (n + 1) + fib n

let%test _ = fib 0 = 1
let%test _ = fib 1 = 1
let%test _ = fib 2 = 2
let%test _ = fib 3 = 3
let%test _ = fib 4 = 5
let%test _ = fib 5 = 8
let%test _ = fib 6 = 13
