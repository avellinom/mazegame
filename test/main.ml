open OUnit2
open Maze

(** This executable tests the game. *)

let data_dir_prefix = "data" ^ Filename.dir_sep
let m = maze_of_file (data_dir_prefix ^ "large.mz")
let main () = print_maze m
let () = main ()
let default_tests = []
let tests = "test suite for MS2" >::: List.flatten [ default_tests ]
let _ = run_test_tt_main tests
