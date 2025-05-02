(** Võrrandisüsteemi kaudu abstraktne interpretatsioon. *)

(** Võrrandisüsteeme saab luua kasutades erinevaid täisarve abstraheerivad domeene.
    Vt. Abseval. *)
module Make (ID: IntDomain.S) =
struct
  (** Abstraktne interpretaator.
      Abseval.eval_stmt me siin tegelikult ei kasuta. *)
  module Abseval = Abseval.Make (ID)
  module ED = Abseval.ED
  open Abseval

  (** Väärtustab juhtvoograafi serva keskkonnas.
      Vihje: Abseval.eval_expr.
      Vihje: Abseval.eval_guard. *)
  let eval_edge (env: ED.t) (edge: Cfg.Edge.t): ED.t =
    match edge with
    | Nop -> env
    | Error -> if ED.leq env ED.bot then ED.bot else failwith "eval_edge: Error"
    | Assign (x, e) -> ED.add x (eval_expr env e) env
    | Guard (c, b) -> eval_guard env c b


  (** Võrrandisüsteem juhtvoograafiga defineeritud programmi jaoks. *)
  module MakeSys (C: sig
      val cfg: Cfg.t (** Juhtvoograaf. *)

      val entry_env: ED.t (** Algne abstraktne väärtuskeskkond. *)
    end) =
  struct
    open C

    module V = Cfg.Node (** Muutujad on juhtvoograafi tipud. *)

    module D = ED (** Väärtused on väärtuskeskkonna domeenist. *)

    let vars = Cfg.nodes cfg (** Kõik juhtvoograafi tipud. *)

    (** Abstraktse väärtustamise võrrandite paremad pooled.
        Vihje: Cfg.pred.
        Vihje: eval_edge. *)
    let f (node: V.t) (get: V.t -> D.t): D.t =
      let initial_env =
        if V.equal node cfg.entry then
          entry_env
        else
          D.bot
      in
      Cfg.pred cfg node
      |> List.map (fun (edge, pred_node) ->
          eval_edge (get pred_node) edge
        )
      |> List.fold_left D.join initial_env
  end
end
