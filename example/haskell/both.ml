(** https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns#both-patterns *)

let both x = (x, x)

(* These are a bit silly because you could just use the standard [(h :: t) as xs] pattern. *)

let f_function = function
  | [%view? (xs, h :: t) when both] -> h :: xs @ t

[@@@warning "-partial-match"]

let f_fun = fun [%view? (xs, h :: t) when both] -> h :: xs @ t

(* This is represented as fun in the AST as well. *)
let f_param [%view? (xs, h :: t) when both] = h :: xs @ t

let f_let_in l =
  let [%view? (xs, h :: t) when both] = l in
  h :: xs @ t
