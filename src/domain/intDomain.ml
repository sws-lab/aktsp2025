(** Täisarvude abstraheerimise domeenid. *)

(** Täisarvude domeeni liides. *)
module type S =
sig
  include Domain.S

  (** Loob konkreetsele täisarvule vastava elemendi. *)
  val of_int: int -> t

  (** Loob intervallist juhuarvule vastava elemendi. *)
  val of_interval: int * int -> t

  (** Väärtustab binaarse operaatori. *)
  val eval_binary: t -> Ast.binary -> t -> t

  (** Välistab konkreetse täisarvu. *)
  val exclude: int -> t -> t
end

(** Täisarvude hulkade domeen. *)
module Set =
struct
  include SetDomain.Make (struct type t = int [@@deriving ord, show] end)
  let of_int i = singleton i
  let of_interval (l, u) = S.of_list (List.init (u - l + 1) (fun i -> l + i))

  (** Rakendab binaarset operaatorit kõikvõimalikele paaridele. *)
  let eval_binary s1 b s2 =
    fold (fun i1 acc ->
        fold (fun i2 acc ->
            add (Eval.Concrete.eval_binary i1 b i2) acc
          ) s2 acc
      ) s1 empty

  let exclude i s = remove i s
end

(** Konstantide domeen.
    "Lame" täisarvude võre, mis võimaldab esitada konkreetseid täisarve,
    kuid mitte-konstandid on täiesti tundmatud. *)
module Flat =
struct
  include Domain.Flat (struct type t = int [@@deriving eq, ord, show] end)
  let of_int i = Lift i
  let of_interval ((l, u): int * int): t =
    if l = u then
      Lift l
    else
      Top

  (** Vihje: Eval.Concrete.eval_binary. *)
  let eval_binary (i1: t) (b: Ast.binary) (i2: t): t =
    match i1, i2 with
    | Bot, _
    | _, Bot -> Bot
    | Lift i1, Lift i2 -> Lift (Eval.Concrete.eval_binary i1 b i2)
    | Top, _
    | _, Top -> Top

  let exclude (i: int) (i': t): t =
    match i' with
    | Bot -> Bot
    | Lift i' when i = i' -> Bot
    | Lift i' -> Lift i'
    | Top -> Top
    (* | _ -> i' *)
end

(** Intervallide domeen. *)
module Interval =
struct
  module Interval =
  struct
    type t = int * int [@@deriving eq, ord]

    let pp ppf (l, u) = Format.fprintf ppf "[%d, %d]" l u
    let show = Format.asprintf "%a" pp

    let leq ((l1, u1): t) ((l2, u2): t): bool =
      l2 <= l1 && u1 <= u2

    let join ((l1, u1): t) ((l2, u2): t): t =
      (min l1 l2, max u1 u2)

    let eval_binary ((l1, u1): t) (b: Ast.binary) ((l2, u2): t): t =
      match b with
      | Add -> (l1 + l2, u1 + u2)
      | Lt when u1 < l2 -> (1, 1)
      | Lt when u2 <= l1 -> (0, 0)
      | Eq | Ne | Lt | Le | Gt | Ge -> (0, 1) (* Võrdluse tulemus on 0 või 1, mis on korrektne, aga mitte täpne. Ettepoole saab implementeerida täpsemad juhud kui vaja. *)

      | _ -> failwith "TODO" (* Ei pea implementeerima kõiki operaatoreid, vaid ainult testideks vajalikud. *)
  end
  (* Lisame tehisliku vähima elemendi,
     millega tähistame võimatut täisarvulist väärtust. *)
  include Domain.LiftBot (Interval)

  let of_interval (l, u) = Lift (l, u)
  let of_int i = of_interval (i, i)

  let eval_binary x1 b x2 =
    match x1, x2 with
    | Bot, _
    | _, Bot -> Bot
    | Lift i1, Lift i2 -> Lift (Interval.eval_binary i1 b i2)

  let exclude (i: int) (x: t): t =
    failwith "TODO"
end
