(** https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns#iterator-style *)

let[@warning "-redundant-case"] rec length = function
  | [] -> 0
  | _ :: [%view? n when length] -> 1 + n

let%test _ = length [] = 0
let%test _ = length [1] = 1
let%test _ = length [1; 2] = 2
