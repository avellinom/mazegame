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

(** [make_game t n] makes a Maze Game from a maze type t with n images. *)
let make_game (typ : maze_type) (image_count : int) : Controller.t =
  let username = "test user" in
  let maze_filepath =
    match typ with
    | Small -> small_fileroute
    | Large -> large_fileroute
    | Empty -> empty_fileroute
  in
  let ctrl = Controller.start_game maze_filepath username image_count in
  ctrl

(** [game_test n c ps] prints out the maze state of controller c if ps, and
    creates a test with name n. *)
let valid_game_test (name : string) (game_ctrl : Controller.t)
    (print_state : bool) : test =
  if print_state then print_game game_ctrl ANSITerminal.blue;
  name >:: fun _ -> assert_equal true true

(** [invalid_move_game_test n is] returns a test with name n checking that the
    delayed expression raises [IllegalMove] *)
let invalid_move_game_test (name : string)
    (illegal_move_sequence : unit -> Controller.t) : test =
  name >:: fun _ -> assert_raises InvalidMove illegal_move_sequence

(** [solved_game_test n ss] returns a test with name n checking that the delayed
    expression raises [MazeSolved]*)
let solved_game_test (name : string)
    (solved_move_sequence : unit -> Controller.t) : test =
  name >:: fun _ -> assert_raises MazeSolved solved_move_sequence

let controller_tests =
  [
    valid_game_test "User can go right on small maze"
      (make_game Small 0 |> move_right)
      false;
    valid_game_test "User can approach barrier on small maze"
      (make_game Small 0 |> move_right |> move_right)
      false;
    valid_game_test "User can move down on small maze"
      (make_game Small 0 |> move_right |> move_down |> move_right)
      false;
    invalid_move_game_test "User cannot move into right wall on small maze"
      (fun () -> make_game Small 0 |> move_right |> move_right |> move_right);
    solved_game_test
      "Empty maze is easily solvable by tracing left and bottom edges"
      (fun () ->
        make_game Empty 0 |> move_down |> move_down |> move_right |> move_right
        |> move_right |> move_right);
    solved_game_test
      "Empty maze is easily solvabe by tracing top and right edges while the \
       maze is filled with images" (fun () ->
        make_game Empty 5 |> move_right |> move_right |> move_right
        |> move_right |> move_down |> move_down |> move_down);
    solved_game_test
      "More images than available board spaces does not cause problems"
      (fun () ->
        make_game Empty 10000 |> move_right |> move_right |> move_right
        |> move_right |> move_down |> move_down |> move_down);
  ]

let tests = "test suite for MS2" >::: List.flatten [ controller_tests ]
let _ = run_test_tt_main tests
