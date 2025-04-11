(** Abstraktne väärtustaja ehk abstraktne interpretaator. *)
open Ast

(** Abstraktseid väärtustajaid saab luua kasutades erinevaid täisarve abstraheerivaid domeene.
    Vt. IntDomain. *)
module Make (ID: IntDomain.S) =
struct
  (** Väärtuskeskkonna domeen kasutades antud täisarvude domeeni. *)
  module ED = EnvDomain.Make (ID)

  (** Väärtustab avaldise keskkonnas.
      Vihje: ID.of_int.
      Vihje: ID.of_interval.
      Vihje: ID.eval_binary. *)
  let rec eval_expr (env: ED.t) (expr: expr): ID.t =
    match expr with
    | Num i -> ID.of_int i
    | Var x -> ED.find x env
    | Rand (l, u) -> ID.of_interval (l, u)
    | Binary (l, b, r) ->
      let li = eval_expr env l in
      let ri = eval_expr env r in
      ID.eval_binary li b ri

  (** Väärtustab valvuri (avaldis ja selle oodatav tõeväärtus) keskkonnas.
      Kui valvur on keskkonnaga vastuolus, siis tagastab saavutamatu programmi oleku ED.bot.
      Kui valvuriga saab keskkonna muutujate väärtusi täpsemaks kitsendada, siis võib keskkonda muuta.
      Võib jätta keskkonna muutmata, kuid siis ei kasutata valvurist saadavat lisainfot. *)
  let rec eval_guard (env: ED.t) (expr: expr) (branch: bool): ED.t =
    let expr_value = eval_expr env expr in
    let zero = ID.of_int 0 in
    if branch && ID.leq expr_value zero ||
        not branch && not (ID.leq zero expr_value) then
      ED.bot
    else
      match expr, branch with
      | Binary (Var x, Eq, Num c), true
      | Binary (Var x, Ne, Num c), false ->
        ED.add x (ID.of_int c) env
      | Binary (Var x, Eq, Num c), false
      | Binary (Var x, Ne, Num c), true ->
        let previous = ED.find x env in
        let excluded = ID.exclude c previous in
        ED.add x excluded env
      | Var x, _ -> eval_guard env (Binary (Var x, Ne, Num 0)) branch
      | _ -> env

  module EDFP = Fixpoint.MakeDomain (ED)

  (** Väärtustab lause keskkonnas.
      Vihje: Vea jaoks kasuta failwith funktsiooni.
      Vihje: eval_guard.
      Vihje: While jaoks kasuta püsipunkti moodulit EDFP. *)
  let rec eval_stmt (env: ED.t) (stmt: stmt): ED.t =
    match stmt with
    | Nop -> env
    | Error -> if ED.leq env ED.bot then ED.bot else failwith "eval_stmt: Error"
    | Assign (x, e) ->
      ED.add x (eval_expr env e) env
    | Seq (a, b) ->
      eval_stmt (eval_stmt env a) b
    | If (c, t, f) ->
      let t_refined_env = eval_guard env c true in
      let t_post_env = eval_stmt t_refined_env t in
      let f_refined_env = eval_guard env c false in
      let f_post_env = eval_stmt f_refined_env f in
      ED.join t_post_env f_post_env
    | While (c, b) ->
      let f env' =
        let b_refined_env = eval_guard env' c true in
        eval_stmt b_refined_env b
      in
      let while_result = EDFP.closure f env in
      eval_guard while_result c false
end
