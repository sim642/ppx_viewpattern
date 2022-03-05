(** https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns#both-patterns *)

let both x = (x, x)

[@@@warning "-partial-match"]

let f = function
  | [%view? (xs, h :: t) when both] -> h :: xs @ t

let f2 = fun [%view? (xs, h :: t) when both] -> h :: xs @ t

(* represented as fun in AST as well *)
let f3 [%view? (xs, h :: t) when both] = h :: xs @ t

let%test _ = f [1] = [1; 1]
let%test _ = f [1; 2] = [1; 1; 2; 2]
let%test _ = f [1; 2; 3] = [1; 1; 2; 3; 2; 3]

let b =
  let [%view? (xs, h :: t) when both] = [1; 2; 3] in
  xs = [1; 2; 3] && h = 1 && t = [2; 3]
let%test _ = b


let%test _ =
  let [%view? (xs, h :: t) when both] = [1; 2; 3] in
  xs = [1; 2; 3] && h = 1 && t = [2; 3]


(* TODO: structure let support *)
(* let [%view? (xs, h :: t) when both] = [1; 2; 3]
let%test _ =
  xs = [1; 2; 3] && h = 1 && t = [2; 3] *)
