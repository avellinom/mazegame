open OUnit2
open Controller

let data_dir_prefix : string = "data" ^ Filename.dir_sep

(* The filepath to the small maze. *)
let small_fileroute : string = data_dir_prefix ^ "small.mz"

(* The filepath to the large maze. *)
let large_fileroute : string = data_dir_prefix ^ "large.mz"

(* The filepath to the empty maze. *)
let empty_fileroute : string = data_dir_prefix ^ "empty.mz"

(** maze_type represents all the types of mazes available. *)
type maze_type =
  | Small
  | Large
  | Empty

(** [make_game t] makes a Maze Game from a maze type t. *)
let make_game (typ : maze_type) : Controller.t =
  let username = "test user" in
  let maze_filepath =
    match typ with
    | Small -> small_fileroute
    | Large -> large_fileroute
    | Empty -> empty_fileroute
  in
  let ctrl = Controller.start_game maze_filepath username in
  ctrl

(** [game_test n c ps] prints out the maze state of controller c if ps, and
    creates a test with name n. *)
let valid_game_test (name : string) (game_ctrl : Controller.t)
    (print_state : bool) : test =
  if print_state then print_game game_ctrl;
  name >:: fun _ -> assert_equal true true

let controller_tests =
  [
    valid_game_test "User can go right on small maze"
      (make_game Small |> move_right)
      false;
    valid_game_test "User can approach barrier on small maze"
      (make_game Small |> move_right |> move_right)
      false;
    valid_game_test "User can move down on small maze"
      (make_game Small |> move_right |> move_down |> move_right)
      false;
    ( "User cannot move into wall on small maze" >:: fun _ ->
      assert_raises InvalidMove (fun () ->
          make_game Small |> move_right |> move_right |> move_right) );
  ]

let tests = "test suite for MS2" >::: List.flatten [ controller_tests ]
let _ = run_test_tt_main tests
