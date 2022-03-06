(* let inc x = x + 1

let _ =
  match 5 with
  | [%view? 6 when inc] -> 6
  (* | n -> n *)
  (* | [%view? k when inc] -> k *) *)

type prog =
  | Bool of bool
  | Int of int
  | Add of prog * prog
  | Lt of prog * prog
  | If of prog * prog * prog
  | Pair of prog * prog
  | Fst of prog
  | Snd of prog
  | Unit

let[@warning "-redundant-case"] rec reduce = function
  | Add (Int n1, Int n2) -> Some (Int (n1 + n2))
  | Add ([%view? Some p1' when reduce], p2) -> Some (Add (p1', p2))
  | Add (p1, [%view? Some p2' when reduce]) -> Some (Add (p1, p2'))
  | Lt (Int n1, Int n2) -> Some (Bool (n1 < n2))
  | Lt ([%view? Some p1' when reduce], p2) -> Some (Lt (p1', p2))
  | Lt (p1, [%view? Some p2' when reduce]) -> Some (Lt (p1, p2'))
  | If (Bool true, p1, _p2) -> Some p1
  | If (Bool false, _p1, p2) -> Some p2
  | If ([%view? Some p' when reduce], p1, p2) -> Some (If (p', p1, p2))
  | Pair ([%view? Some p1' when reduce], p2) -> Some (Pair (p1', p2))
  | Pair (p1, [%view? Some p2' when reduce]) -> Some (Pair (p1, p2'))
  | Fst (Pair (p1, _p2)) -> Some p1
  | Fst [%view? Some p' when reduce] -> Some (Fst p')
  | Snd (Pair (_p1, p2)) -> Some p2
  | Snd [%view? Some p' when reduce] -> Some (Snd p')
  | _ -> None

let rec normalize p =
  match reduce p with
  | Some p' -> normalize p'
  | None -> p

(* if 1+(2+3) < 4 then false else 5 *)
let%test _ = normalize (If (Lt (Add (Int 1, Add (Int 2, Int 3)), Int 4), Bool false, Int 5)) = Int 5
(* if 1+(2+3) < 10 then false else 5 *)
let%test _ = normalize (If (Lt (Add (Int 1, Add (Int 2, Int 3)), Int 10), Bool false, Int 5)) = Bool false
let%test _ = normalize (Pair (Snd (Pair (Int 3, Int 2)), Fst (Pair (Int 3, Int 2)))) = Pair (Int 2, Int 3)

type typ =
  | TBool
  | TInt
  | TPair of typ * typ
  | TUnit

exception Type_error

let[@warning "-redundant-case"] rec infer = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Add ([%view? TInt when infer], [%view? TInt when infer]) -> TInt
  | Lt ([%view? TInt when infer], [%view? TInt when infer]) -> TBool
  | If ([%view? TBool when infer], p1, p2) when infer p1 = infer p2 -> infer p1
  | Pair (p1, p2) -> TPair (infer p1, infer p2)
  | Fst [%view? TPair (t1, _t2) when infer] -> t1
  | Snd [%view? TPair (_t1, t2) when infer] -> t2
  | Unit -> TUnit
  | _ -> raise Type_error

let typable p = match infer p with
  | _ -> true
  | exception Type_error -> false

(* if (1 + 2) < 3 then 4 else 5 *)
let%test _ = typable (If (Lt (Add (Int 1, Int 2), Int 3), Int 4, Int 5))
(* 1 + false *)
let%test _ = not (typable (Add (Int 1, Bool false)))
(* if true then 1 else false *)
let%test _ = not (typable (If (Bool true, Int 1, Bool false)))
(* (1 , false) *)
let%test _ = typable (Pair (Int 1, Bool false))
