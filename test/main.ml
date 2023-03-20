open OUnit2
open Maze_generator

let default_test (name : string) (input : string) : test =
  name >:: fun _ ->
  let _ = Board.print in 
  assert_equal "hi" input

(* let structure_test (name : string) (input : string) : test =
  name >:: fun _ ->

  let _ = Board.print_matrix [["f"];] in 

  assert_equal "hi" input *)

let default_tests = [ default_test "Printing default board" "hi" ]
let tests = "test suite for MS2" >::: List.flatten [ default_tests ]
let _ = run_test_tt_main tests
