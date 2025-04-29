open OUnit2
open Constraint
open Constraint.Solver

(** Vt. "Compiler Design: Analysis and Transformation" õpikust, näide 1.5.2. *)
module Example =
struct
  module V =
  struct
    type t = X1 | X2 | X3 [@@deriving eq, ord, hash, show]
  end

  open V

  type element = A | B | C [@@deriving ord, show]

  module D = SetDomain.Make (struct
    type t = element [@@deriving ord, show]
  end)

  let vars = [X1; X2; X3]

  let f x get =
    match x with
    | X1 -> D.union (D.singleton A) (get X3)
    | X2 -> D.inter (get X3) (D.of_list [A; B])
    | X3 -> D.union (get X1) (D.singleton C)


end

(** Jaak Peterson, 2024. *)
module Peterson1 =
struct
  module V =
  struct
    type t = X1 | X2 [@@deriving eq, ord, hash, show]
  end

  open V

  type element = A | B [@@deriving ord, show]

  module D = SetDomain.Make (struct
    type t = element [@@deriving ord, show]
  end)

  let vars = [X1; X2]

  let f x get =
    match x with
    | X1 -> D.union (D.singleton A) (get X2)
    | X2 -> D.inter (get X1) (D.singleton B)


end

(** Jaak Peterson, 2024. *)
module Peterson2 =
struct
  module V =
  struct
    type t = X1 | X2 | X3 [@@deriving eq, ord, hash, show]
  end

  open V

  type element = A | B | C | D [@@deriving ord, show]

  module D = SetDomain.Make (struct
    type t = element [@@deriving ord, show]
  end)

  let vars = [X1; X2; X3]

  let f x get =
    match x with
    | X1 -> D.union (D.of_list [A; B]) (get X3)
    | X2 -> D.inter (get X1) (D.of_list [B; C])
    | X3 -> D.union (get X2) (D.singleton D)


end

(** Vt. "Compiler Design: Analysis and Transformation" õpikust, näide 1.7.3.
    Parandused vt. http://www2.in.tum.de/~seidl/compilers/optimize.pdf, slaid 216. *)
module Liveness =
struct
  module V =
  struct
    type t = L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7 [@@deriving eq, ord, hash, show]
  end

  open V

  type element = X | Y | R | I [@@deriving ord, show]

  module D = SetDomain.Make (struct
    type t = element [@@deriving ord, show]
  end)

  let vars = [L0; L1; L2; L3; L4; L5; L6; L7]

  let f x get =
    match x with
    | L0 -> D.union (D.remove X (get L1)) (D.singleton I)
    | L1 -> D.remove Y (get L2)
    | L2 -> D.union (D.union (get L6) (D.singleton X)) (D.union (get L3) (D.singleton X))
    | L3 -> D.union (D.remove Y (get L4)) (D.of_list [X; Y])
    | L4 -> D.union (D.remove X (get L5)) (D.singleton X)
    | L5 -> get L2
    | L6 -> D.union (get L7) (D.of_list [Y; R])
    | L7 -> D.empty


end



(** Vt. https://www.isa-afp.org/entries/Top_Down_Solver.html, peatükk 5. *)
module Initialized =
struct
  module V =
  struct
    type t = X | Y | Z | W [@@deriving eq, ord, hash, show]
  end

  open V

  type element = A | B [@@deriving ord, show]

  module D = SetDomain.Make (struct
    type t = element [@@deriving ord, show]
    (* TODO: võre peaks olema ümber pööratud *)
  end)

  let vars = [X; Y; Z; W]

  let f x get =
    match x with
    | X -> D.inter (get Y) (get Z)
    | Y -> D.union (get Z) (D.singleton B)
    | Z -> D.inter (D.union (get Y) (D.singleton A)) (D.union (get W) (D.singleton A))
    | W -> D.empty


end

module type Solver = functor (Sys: Sys) -> sig
    module VH: Hashtbl.S with type key = Sys.V.t
    val solve: unit -> Sys.D.t VH.t
  end


module Common_test (Solver: Solver) =
struct

  module ExampleSolver = Solver (Example)

  let test_example _ =
    let open Example in
    let sol = ExampleSolver.solve () in
    let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
    assert_equal (D.of_list [A; C]) (ExampleSolver.VH.find sol X1);
    assert_equal (D.singleton A) (ExampleSolver.VH.find sol X2);
    assert_equal (D.of_list [A; C]) (ExampleSolver.VH.find sol X3)


  module Peterson1Solver = Solver (Peterson1)

  let test_peterson1 _ =
    let open Peterson1 in
    let sol = Peterson1Solver.solve () in
    let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
    assert_equal (D.singleton A) (Peterson1Solver.VH.find sol X1);
    assert_equal (D.empty) (Peterson1Solver.VH.find sol X2)


  module Peterson2Solver = Solver (Peterson2)

  let test_peterson2 _ =
    let open Peterson2 in
    let sol = Peterson2Solver.solve () in
    let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
    assert_equal (D.of_list [A; B; D]) (Peterson2Solver.VH.find sol X1);
    assert_equal (D.singleton B) (Peterson2Solver.VH.find sol X2);
    assert_equal (D.of_list [B; D]) (Peterson2Solver.VH.find sol X3)


  module LivenessSolver = Solver (Liveness)

  let test_liveness _ =
    let open Liveness in
    let sol = LivenessSolver.solve () in
    let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
    assert_equal D.empty (LivenessSolver.VH.find sol L7);
    assert_equal (D.of_list [Y; R]) (LivenessSolver.VH.find sol L6);
    assert_equal (D.of_list [X; Y; R]) (LivenessSolver.VH.find sol L2);
    assert_equal (D.of_list [X; Y; R]) (LivenessSolver.VH.find sol L5);
    assert_equal (D.of_list [X; Y; R]) (LivenessSolver.VH.find sol L4);
    assert_equal (D.of_list [X; Y; R]) (LivenessSolver.VH.find sol L3);
    assert_equal (D.of_list [X; R]) (LivenessSolver.VH.find sol L1);
    assert_equal (D.of_list [I; R]) (LivenessSolver.VH.find sol L0)


  module InitializedSolver = Solver (Initialized)

  let test_initialized _ =
    let open Initialized in
    let sol = InitializedSolver.solve () in
    let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
    assert_equal (D.singleton A) (InitializedSolver.VH.find sol X);
    assert_equal (D.of_list [A; B]) (InitializedSolver.VH.find sol Y);
    assert_equal (D.singleton A) (InitializedSolver.VH.find sol Z);
    assert_equal D.empty (InitializedSolver.VH.find sol W)

  let tests =
    "common" >::: [
      "example" >:: test_example;
      "peterson1" >:: test_peterson1;
      "peterson2" >:: test_peterson2;
      "liveness" >:: test_liveness;
      "initialized" >:: test_initialized;
    ]
end

module Kleene_test =
struct
  module Common = Common_test (Kleene)
  let tests =
    "kleene" >::: [
      Common.tests;
    ]
end

module RoundRobin_test =
struct
  module Common = Common_test (RoundRobin)
  let tests =
    "roundrobin" >::: [
      Common.tests;
    ]
end



let tests =
  "constraint" >::: [
    "solver" >::: [
      Kleene_test.tests;
      RoundRobin_test.tests;

    ];
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
