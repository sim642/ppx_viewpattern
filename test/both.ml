(** https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns#both-patterns *)

let both x = (x, x)

let f = function
  | [%view? (xs, h :: t) when both] -> h :: xs @ t

(* TODO: viewpattern in argument *)
(* let f [%view? (xs, h :: t) when both] = h :: xs @ t *)

let%test _ = f [1] = [1; 1]
let%test _ = f [1; 2] = [1; 1; 2; 2]
let%test _ = f [1; 2; 3] = [1; 1; 2; 3; 2; 3]
