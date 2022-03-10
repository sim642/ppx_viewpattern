(** https://www.lix.polytechnique.fr/Labo/Samuel.Mimram//teaching/INF551/TD/1.typing.html *)

type prog =
  | Bool of bool
  | Int of int
  | Add of prog * prog
  | Lt of prog * prog
  | If of prog * prog * prog

(* These cases are exactly like reduction rules! *)
let rec reduce = function
  | Add (Int n1, Int n2) -> Some (Int (n1 + n2))
  | Add ([%view? Some p1' when reduce], p2) -> Some (Add (p1', p2))
  | Add (p1, [%view? Some p2' when reduce]) -> Some (Add (p1, p2'))
  | Lt (Int n1, Int n2) -> Some (Bool (n1 < n2))
  | Lt ([%view? Some p1' when reduce], p2) -> Some (Lt (p1', p2))
  | Lt (p1, [%view? Some p2' when reduce]) -> Some (Lt (p1, p2'))
  | If (Bool true, p1, _p2) -> Some p1
  | If (Bool false, _p1, p2) -> Some p2
  | If ([%view? Some p' when reduce], p1, p2) -> Some (If (p', p1, p2))
  | _ -> None

let rec normalize p =
  match reduce p with
  | Some p' -> normalize p'
  | None -> p


type typ =
  | TBool
  | TInt

exception Type_error

(* These cases are exactly like typing rules! *)
let rec infer = function
  | Bool _ -> TBool
  | Int _ -> TInt
  | Add ([%view? TInt when infer], [%view? TInt when infer]) -> TInt
  | Lt ([%view? TInt when infer], [%view? TInt when infer]) -> TBool
  | If ([%view? TBool when infer], [%view? t1 when infer], [%view? t2 when infer]) when t1 = t2 -> t1
  | _ -> raise Type_error

let typable p = match infer p with
  | _ -> true
  | exception Type_error -> false
