open OUnit2
open Maze

let data_dir_prefix = "data" ^ Filename.dir_sep
let m = Maze.of_file (data_dir_prefix ^ "example.mz")
let main () = Maze.print_maze m
let () = main ()
let default_tests = []
let tests = "test suite for MS2" >::: List.flatten [ default_tests ]
let _ = run_test_tt_main tests
