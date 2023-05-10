open OUnit2
open Controller

let data_dir_prefix = "data" ^ Filename.dir_sep
let filename = data_dir_prefix ^ "small.mz"
let username = "jennifer"
let g = start_game filename username
let g' = move_down g
let g'' = move_right g'
let g''' = move_left g''

(** This executable tests the game. *)

let () = print_game g'''
(* let default_tests = [] let tests = "test suite for MS2" >::: List.flatten [
   default_tests ] let _ = run_test_tt_main tests *)
