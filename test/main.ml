open OUnit2
open Maze_generator

let set_matrix_test (name : string) (input : string array array) : test =
  name >:: fun _ ->
  let _ = Board.set_matrix input in 
  let _ = Board.print () in 
  assert_equal !Board.matrix input

let generate_test (name : string) (input : string array array) : test =
  name >:: fun _ ->
  let _ = Board.generate () in 
  let _ = Board.print () in 
  assert_equal !Board.matrix input

let ascii_test (name : string) (input : char) : test =
  name >:: fun _ ->
  let letter = Board.get_letter 'C' in 
  assert_equal input input

let default_tests = [ 
  set_matrix_test "Board.set_matrix_test" ([| [| "a"; "b"; "c" |]; [| "d"; "e"; "f" |]; [| "g"; "h"; "i" |] |]);
  generate_test "Board.generate_test" (Array.make_matrix 10 10 "X");
  ascii_test "Testing get_letter" 'C'
]
let tests = "test suite for MS2" >::: List.flatten [ default_tests ]
let _ = run_test_tt_main tests
