[@@@warning "-redundant-case"]
[@@@warning "-partial-match"]

let g = function
  | i when i >= 10 -> Some (i - 10)
  | _ -> None

let f1 = function
  | [%view? i when g] -> i

let%test _ = f1 1 = None
let%test _ = f1 10 = Some 0
let%test _ = f1 15 = Some 5

let f2 = function
  | [%view? None when g] -> true
  | _ -> false

let%test _ = f2 1 = true
let%test _ = f2 10 = false
let%test _ = f2 15 = false

let f3 = function
  | [%view? Some i when g] -> i
  | i -> i

let%test _ = f3 1 = 1
let%test _ = f3 10 = 0
let%test _ = f3 15 = 5

let f4 = function
  | ([%view? i when g], j) -> (i, j)

let%test _ = f4 (1, 10) = (None, 10)
let%test _ = f4 (10, 10) = (Some 0, 10)
let%test _ = f4 (15, 10) = (Some 5, 10)

let f5 = function
  | (i, [%view? j when g]) -> (i, j)

let%test _ = f5 (10, 1) = (10, None)
let%test _ = f5 (10, 10) = (10, Some 0)
let%test _ = f5 (10, 15) = (10, Some 5)

let f6 = function
  | ([%view? i when g], [%view? j when g]) -> (i, j)

let%test _ = f6 (1, 10) = (None, Some 0)
let%test _ = f6 (1, 15) = (None, Some 5)
let%test _ = f6 (10, 1) = (Some 0, None)
let%test _ = f6 (15, 1) = (Some 5, None)
let%test _ = f6 (10, 15) = (Some 0, Some 5)

let f7 = function
  | ([%view? Some i when g], [%view? Some j when g]) -> (i, j)
  | (i, j) -> (i, j)

let%test _ = f7 (1, 2) = (1, 2)
let%test _ = f7 (1, 10) = (1, 10)
let%test _ = f7 (10, 1) = (10, 1)
let%test _ = f7 (10, 15) = (0, 5)

let f8 = function
  | [%view? Some i when g] when i >= 5 -> i
  | i -> i

let%test _ = f8 1 = 1
let%test _ = f8 10 = 10
let%test _ = f8 15 = 5
let%test _ = f8 20 = 10

let f9 = function
  | ([%view? Some i when g], [%view? Some j when g]) when i + j >= 10 -> (i, j)
  | (i, j) -> (i, j)

let%test _ = f9 (1, 2) = (1, 2)
let%test _ = f9 (1, 10) = (1, 10)
let%test _ = f9 (10, 1) = (10, 1)
let%test _ = f9 (10, 15) = (10, 15)
let%test _ = f9 (15, 15) = (5, 5)
