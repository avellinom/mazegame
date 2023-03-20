open OUnit2
open Maze_generator

let default_test (name : string) (input : string list list) : test =
  name >:: fun _ ->
  let _ = Board.set_matrix input in 
  let _ = Board.print () in 
  (* assert_equal !Board.matrix input *)
  assert_equal input input


let default_tests = [ 
  default_test "Printing default board" ([ [ "a"; "b"; "c" ]; [ "d"; "e"; "f" ]; [ "g"; "h"; "i" ] ])
]
let tests = "test suite for MS2" >::: List.flatten [ default_tests ]
let _ = run_test_tt_main tests
