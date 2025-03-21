(** Tõeväärtusavaldiste keel.
    Vt. https://courses.cs.ut.ee/t/akt/Main/Alusosa#bool. *)

type t =
  | Var of char (** Muutuja *)
  | Not of t (* not *)
  | Or of t * t (* || *)
  | Imp of t * t (** Implikatsioon *)

(** Väärtuskeskkonnaks on hulk tõestest muutujatest. *)
module CharSet = Set.Make (Char)
type env = CharSet.t


(** Vihje: Kasuta Crashcourse.Basics.implies funktsiooni. *)
let eval (env: env) (e: t): bool =
  (* let eval' = eval env in
  match e with
  | Var x -> CharSet.mem x env
  | Not e -> not (eval' e)
  | Or (e1, e2) -> eval' e1 || eval' e2
  | Imp (e1, e2) -> Crashcourse.Basics.implies (eval' e1) (eval' e2) *)
  let rec eval' e =
    match e with
    | Var x -> CharSet.mem x env
    | Not e -> not (eval' e)
    | Or (e1, e2) -> eval' e1 || eval' e2
    | Imp (e1, e2) -> Crashcourse.Basics.implies (eval' e1) (eval' e2)
  in
  eval' e
