open OUnit2
open Maze_generator

let default_test (name : string) (input : string) : test =
  name >:: fun _ ->
  let _ = print_string "\n>>> THE TESTS ARE RUNNING WELL <<<" in

  let _ = Board.matrix in

  assert_equal "hi" input

let default_tests = [ default_test "default_test" "hi" ]
let tests = "test suite for MS2" >::: List.flatten [ default_tests ]
let _ = run_test_tt_main tests
