(** https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns#iterator-style *)

let rec length = function
  | [] -> 0
  | _ :: [%view? n when length] -> 1 + n

let rec map f = function
  | [] -> []
  | x :: [%view? xs when map f] -> f x :: xs

(* This has different argument order from [List.fold_right]. *)
let rec foldr f z = function
  | [] -> z
  | x :: [%view? xs when foldr f z] -> f x xs
