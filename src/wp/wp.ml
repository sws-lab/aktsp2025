(** Nõrgima eeltingimuse arvutamine ja sellega programmi korrektsuse kontrollimine. *)
open Z3
open Symbolic

(** Arvutab lause nõrgima eeltingimuse antud järeltingimuse jaoks.
    Üldist While lauset pole vaja toetada, ainult piisavalt, et testid läbi läheks.
    Vihje: let open Syntax in ...
    Vihje: eval_expr.
    Vihje: Expr.substitute_one.
    Vihje: bool_of_int. *)
let rec wp (stmt: Ast.stmt) (post: Expr.expr): Expr.expr =
  let open Syntax in
  match stmt with
  | Nop -> post
  | Error -> false_
  | Assign (x, e) ->
    let e' = eval_expr e in
    Expr.substitute_one post !x e'
  | Seq (s1, s2) -> wp s1 (wp s2 post)
  | If (c, s1, s2) ->
    let c' = bool_of_int (eval_expr c) in
    implies c' (wp s1 post) && implies (not c') (wp s2 post)
  | While (Num 1, Nop) -> true_
  | _ -> failwith "TODO"


(** Korrektsuse kontrolli tulemus. *)
type verdict =
  | Correct (** Korrektne ehk Error lauset ei täideta. *)
  | Incorrect of Env.t (** Vigane ehk Error lauset täidetakse. Andmetena kaasas keskkond, milles Error lauseni jõutakse. *)
  | Unknown (** Z3 ei oska lahendada. *)
[@@deriving show]

(** Kontrollib, kas programm on korrektne.
    Vt. Toyatp.Bool.check.
    Vihje: wp.
    Vihje: match-i Solver.check tulemust.
    Vihje: env_of_model. *)
let check (stmt: Ast.stmt): verdict =
  let formula = Syntax.(not (wp stmt true_)) in
  match Solver.check solver [formula] with
  | SATISFIABLE -> Incorrect (env_of_model (Option.get (Solver.get_model solver)))
  | UNSATISFIABLE -> Correct
  | UNKNOWN -> Unknown
