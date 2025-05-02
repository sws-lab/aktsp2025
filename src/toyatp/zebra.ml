(** Zebra Puzzle mõistatuse lahendaja.

    Vt. https://en.wikipedia.org/wiki/Zebra_Puzzle. *)

open Z3

(** Kirje (record) tüüp lahenduse kirjeldamiseks.
    Iga väli sisaldab nimele vastava maja numbrit (1..5) vasakult lugedes. *)
type solution = {
  red: int;
  green: int;
  ivory: int;
  yellow: int;
  blue: int;

  englishman: int;
  spaniard: int;
  ukrainian: int;
  norwegian: int;
  japanese: int;

  coffee: int;
  tea: int;
  milk: int;
  orange_juice: int;
  water: int;

  old_gold: int;
  kools: int;
  chesterfield: int;
  lucky_strike: int;
  parliament: int;

  dog: int;
  snails: int;
  fox: int;
  horse: int;
  zebra: int;
}

let ctx = mk_context [
    ("model", "true");
  ]
let solver = Solver.mk_simple_solver ctx

(** Lahendab Zebra Puzzle mõistatuse. *)
let solve (): solution =
  (* Loome Z3 muutujad lahenduse kõigi tundmatute jaoks.
     Iga muutuja tähistab nimele vastava maja numbrit (1..5) vasakult lugedes. *)
  let red = Arithmetic.Integer.mk_const_s ctx "red" in
  let green = Arithmetic.Integer.mk_const_s ctx "green" in
  let ivory = Arithmetic.Integer.mk_const_s ctx "ivory" in
  let yellow = Arithmetic.Integer.mk_const_s ctx "yellow" in
  let blue = Arithmetic.Integer.mk_const_s ctx "blue" in

  let englishman = Arithmetic.Integer.mk_const_s ctx "englishman" in
  let spaniard = Arithmetic.Integer.mk_const_s ctx "spaniard" in
  let ukrainian = Arithmetic.Integer.mk_const_s ctx "ukrainian" in
  let norwegian = Arithmetic.Integer.mk_const_s ctx "norwegian" in
  let japanese = Arithmetic.Integer.mk_const_s ctx "japanese" in

  let coffee = Arithmetic.Integer.mk_const_s ctx "coffee" in
  let tea = Arithmetic.Integer.mk_const_s ctx "tea" in
  let orange_juice = Arithmetic.Integer.mk_const_s ctx "orange_juice" in
  let milk = Arithmetic.Integer.mk_const_s ctx "milk" in
  let water = Arithmetic.Integer.mk_const_s ctx "water" in

  let old_gold = Arithmetic.Integer.mk_const_s ctx "old_gold" in
  let kools = Arithmetic.Integer.mk_const_s ctx "kools" in
  let chesterfield = Arithmetic.Integer.mk_const_s ctx "chesterfield" in
  let lucky_strike = Arithmetic.Integer.mk_const_s ctx "lucky_strike" in
  let parliament = Arithmetic.Integer.mk_const_s ctx "parliament" in

  let dog = Arithmetic.Integer.mk_const_s ctx "dog" in
  let snails = Arithmetic.Integer.mk_const_s ctx "snails" in
  let fox = Arithmetic.Integer.mk_const_s ctx "fox" in
  let horse = Arithmetic.Integer.mk_const_s ctx "horse" in
  let zebra = Arithmetic.Integer.mk_const_s ctx "zebra" in

  (* Listid sama liiki muutujatest. *)
  let colors = [red; green; ivory; yellow; blue] in
  let nationalities = [englishman; spaniard; ukrainian; norwegian; japanese] in
  let drinks = [coffee; tea; milk; orange_juice; water] in
  let smokes = [old_gold; kools; chesterfield; lucky_strike; parliament] in
  let pets = [dog; snails; fox; horse; zebra] in
  (* List kõigist muutujatest. *)
  let unknowns = colors @ nationalities @ drinks @ smokes @ pets in

  (* "1. There are five houses." *)
  (* Abidefinitsioonid konstantide 1 ja 5 jaoks. *)
  let one = Arithmetic.Integer.mk_numeral_i ctx 1 in
  let five = Arithmetic.Integer.mk_numeral_i ctx 5 in
  (* Loome Z3 avaldised, mis väljendavad, et iga tundmatu väärtus on intervallis [1, 5]. *)
  let c_ranges = List.map (fun unknown ->
      Boolean.mk_and ctx [
          Arithmetic.mk_ge ctx unknown one;
          Arithmetic.mk_le ctx unknown five;
        ]
    ) unknowns
  in

  (* Loome Z3 avalidsed, mis väljendavad ülejäänud mõistatuse tingimusi. *)
  let c_rules = [
      (* "2. The Englishman lives in the red house." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "3. The Spaniard owns the dog." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "4. Coffee is drunk in the green house." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "5. The Ukrainian drinks tea." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "6. The green house is immediately to the right of the ivory house." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "7. The Old Gold smoker owns snails." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "8. Kools are smoked in the yellow house." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "9. Milk is drunk in the middle house." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "10. The Norwegian lives in the first house." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "11. The man who smokes Chesterfields lives in the house next to the man with the fox." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "12. Kools are smoked in the house next to the house where the horse is kept." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "13. The Lucky Strike smoker drinks orange juice." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "14. The Japanese smokes Parliaments." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)


      (* "15. The Norwegian lives next to the blue house." *)
      Boolean.mk_true ctx; (* TODO: Asenda korrektse tingimusega. *)

    ]
  in

  (* Loome Z3 avaldise, mis väljendab, et sama liiki tundmatud on paarikaupa erinevad. *)
  let c_distinct = [
      Boolean.mk_distinct ctx colors;
      Boolean.mk_distinct ctx nationalities;
      Boolean.mk_distinct ctx drinks;
      Boolean.mk_distinct ctx smokes;
      Boolean.mk_distinct ctx pets;
    ]
  in

  (* Tingimused c_ranges ja c_distinct koos tähendavad, et sama liiki tundmatud on mingi permutatsioon arvudest 1, 2, 3, 4, 5. *)

  (* Käivitame Z3 solveri kõigi vajalike tingimustega. *)
  let cs = c_ranges @ c_rules @ c_distinct in
  let status = Solver.check solver cs in
  assert (status = SATISFIABLE); (* See mõistatus peaks olema lahenduv. *)

  let model = Option.get (Solver.get_model solver) in
  (* Õngitseme mudelist välja meie tundmatute Z3 muutujate väärtused OCaml-i täisarvudena ja tagastame kirjena. *)
  let int_of_model x = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x)) in
  {
    red = int_of_model red;
    green = int_of_model green;
    ivory = int_of_model ivory;
    yellow = int_of_model yellow;
    blue = int_of_model blue;

    englishman = int_of_model englishman;
    spaniard = int_of_model spaniard;
    ukrainian = int_of_model ukrainian;
    norwegian = int_of_model norwegian;
    japanese = int_of_model japanese;

    coffee = int_of_model coffee;
    tea = int_of_model tea;
    milk = int_of_model milk;
    orange_juice = int_of_model orange_juice;
    water = int_of_model water;

    old_gold = int_of_model old_gold;
    kools = int_of_model kools;
    chesterfield = int_of_model chesterfield;
    lucky_strike = int_of_model lucky_strike;
    parliament = int_of_model parliament;

    dog = int_of_model dog;
    snails = int_of_model snails;
    fox = int_of_model fox;
    horse = int_of_model horse;
    zebra = int_of_model zebra;
  }
