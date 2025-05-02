open OUnit2
open Toyatp.Zebra

let test_rules _ =
  let solution = solve () in
  assert_bool "2." (solution.englishman = solution.red);
  assert_bool "3." (solution.spaniard = solution.dog);
  assert_bool "4." (solution.coffee = solution.green);
  assert_bool "5." (solution.ukrainian = solution.tea);
  assert_bool "6." (solution.green = solution.ivory + 1);
  assert_bool "7." (solution.old_gold = solution.snails);
  assert_bool "8." (solution.kools = solution.yellow);
  assert_bool "9." (solution.milk = 3);
  assert_bool "10." (solution.norwegian = 1);
  assert_bool "11." (solution.chesterfield = solution.fox + 1 || solution.chesterfield = solution.fox - 1);
  assert_bool "12." (solution.kools = solution.horse + 1 || solution.kools = solution.horse - 1);
  assert_bool "13." (solution.lucky_strike = solution.orange_juice);
  assert_bool "14." (solution.japanese = solution.parliament);
  assert_bool "15." (solution.norwegian = solution.blue + 1 || solution.norwegian = solution.blue - 1)

let test_water _ =
  let solution = solve () in
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 1 solution.water

let test_zebra _ =
  let solution = solve () in
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 5 solution.zebra

let test_solution _ =
  let solution = solve () in
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 1 solution.yellow;
  assert_equal 2 solution.blue;
  assert_equal 3 solution.red;
  assert_equal 4 solution.ivory;
  assert_equal 5 solution.green;

  assert_equal 1 solution.norwegian;
  assert_equal 2 solution.ukrainian;
  assert_equal 3 solution.englishman;
  assert_equal 4 solution.spaniard;
  assert_equal 5 solution.japanese;

  assert_equal 1 solution.water;
  assert_equal 2 solution.tea;
  assert_equal 3 solution.milk;
  assert_equal 4 solution.orange_juice;
  assert_equal 5 solution.coffee;

  assert_equal 1 solution.kools;
  assert_equal 2 solution.chesterfield;
  assert_equal 3 solution.old_gold;
  assert_equal 4 solution.lucky_strike;
  assert_equal 5 solution.parliament;

  assert_equal 1 solution.fox;
  assert_equal 2 solution.horse;
  assert_equal 3 solution.snails;
  assert_equal 4 solution.dog;
  assert_equal 5 solution.zebra

let tests =
  "zebra" >::: [
    "rules" >:: test_rules;
    "water" >:: test_water;
    "zebra" >:: test_zebra;
    "solution" >:: test_solution;
  ]
