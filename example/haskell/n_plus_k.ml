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
