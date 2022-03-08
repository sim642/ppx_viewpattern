(** https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns#join-lists *)

type 'a jlist =
  | Empty
  | Single of 'a
  | Join of 'a jlist * 'a jlist

type 'a jlistview =
  | Nil
  | Cons of 'a * 'a jlist

let rec view: 'a jlist -> 'a jlistview = function
  | Empty -> Nil
  | Single a -> Cons (a, Empty)
  | Join ([%view? Cons (xh, xt) when view], y) -> Cons (xh, Join (xt, y))
  | Join ([%view? Nil when view], y) -> view y

let rec length: 'a jlist -> int = function
  | [%view? Nil when view] -> 0
  | [%view? Cons (_x, xs) when view] -> 1 + length xs
