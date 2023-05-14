open OUnit2
open Lib
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

(* This sequence puts the player one above the goal on the small maze. *)
let get_close_to_small_maze_exit (image_amount : int) : Controller.t =
  make_game Small image_amount
  |> move_down |> move_right |> move_right |> move_right |> move_right
  |> move_down |> move_down |> move_left |> move_left |> move_down |> move_down
  |> move_right |> move_right |> move_down |> move_down |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_right |> move_down |> move_down
  |> move_right |> move_right

(* This sequence puts the player one above the goal on the large maze. *)
let get_close_to_large_maze_exit (image_amount : int) : Controller.t =
  make_game Large image_amount
  |> move_right |> move_right |> move_down |> move_down |> move_down
  |> move_right |> move_right |> move_up |> move_up |> move_right |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_down |> move_down |> move_left
  |> move_left |> move_left |> move_down |> move_down |> move_right
  |> move_right |> move_right |> move_down |> move_down |> move_down
  |> move_down |> move_down |> move_down |> move_down |> move_down |> move_left
  |> move_left |> move_down |> move_down |> move_down |> move_down |> move_right
  |> move_right |> move_right |> move_down |> move_down |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_up |> move_up
  |> move_right |> move_right |> move_right |> move_right |> move_down
  |> move_down |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_right

let controller_tests =
  [
    (* Emmpty maze simple inital movement *)
    valid_game_test "User can go right initially on empty maze"
      (make_game Empty 0 |> move_right)
      false;
    valid_game_test "User can go down initially on empty maze"
      (make_game Empty 0 |> move_down)
      false;
    invalid_move_game_test "User cannot move left initially on empty maze"
      (fun () -> make_game Empty 0 |> move_left);
    invalid_move_game_test "User cannot move up initially on empty maze"
      (fun () -> make_game Empty 0 |> move_up);
    (* Small maze simple initial movement *)
    valid_game_test "User can go right initially on small maze"
      (make_game Small 0 |> move_right)
      false;
    valid_game_test "User can go down initially on small maze"
      (make_game Small 0 |> move_down)
      false;
    invalid_move_game_test "User cannot move left initially on small maze"
      (fun () -> make_game Small 0 |> move_left);
    invalid_move_game_test "User cannot move up initially on small maze"
      (fun () -> make_game Small 0 |> move_up);
    (* Large maze simple initial movement *)
    valid_game_test "User can go right initially on large maze"
      (make_game Large 0 |> move_right)
      false;
    valid_game_test "User can go down initially on large maze"
      (make_game Large 0 |> move_down)
      false;
    invalid_move_game_test "User cannot move left initially on large maze"
      (fun () -> make_game Large 0 |> move_left);
    invalid_move_game_test "User cannot move up initially on large maze"
      (fun () -> make_game Large 0 |> move_up);
    (* Mazes can be generated with images *)
    valid_game_test "Empty maze can have images dropped on it"
      (make_game Empty 3) false;
    valid_game_test "Small maze can have images dropped on it"
      (make_game Small 3) false;
    valid_game_test "Large maze can have images dropped on it"
      (make_game Large 3) false;
    (* Mazes can be generated with more images than available maze locations*)
    valid_game_test "Empty maze survives with extreme image count"
      (make_game Empty 10000) false;
    valid_game_test "Small maze can be constructed with extreme image count"
      (make_game Small 10000) false;
    valid_game_test "Large maze can be constructed with extreme image count"
      (make_game Large 10000) false;
    (* It is OK for a user to be adjacent to a barrier on its right *)
    valid_game_test "User can approach barrier from left on empty maze"
      (make_game Empty 0 |> move_down |> move_right)
      false;
    valid_game_test "User can approach barrier from left on small maze"
      (make_game Small 0 |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from left on large maze"
      (make_game Large 0 |> move_right |> move_right)
      false;
    (* It is OK for a user to be adjacent to a barrier above it *)
    valid_game_test "User can approach barrier from beneath it on empty maze"
      (make_game Empty 0 |> move_down |> move_down |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from beneath it on small maze"
      (make_game Small 0 |> move_down |> move_right |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from beneath it on large maze"
      (make_game Large 0 |> move_right |> move_down |> move_down |> move_down
     |> move_right |> move_right)
      false;
    (* It is OK for a user to be adjacent to a barrier beneath it *)
    valid_game_test "User can approach barrier from above it on empty maze"
      (make_game Empty 0 |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from above it on small maze"
      (make_game Small 0 |> move_down)
      false;
    valid_game_test "User can approach barrier from above it on large maze"
      (make_game Large 0 |> move_down)
      false;
    (* It is OK for a user to be adjacent to a barrier on its left *)
    valid_game_test "User can approach barrier from its right on empty maze"
      (make_game Empty 0 |> move_right |> move_right |> move_right |> move_down)
      false;
    valid_game_test "User can approach barrier from its right on small maze"
      (make_game Small 0 |> move_down |> move_right |> move_right |> move_right
     |> move_right |> move_down)
      false;
    valid_game_test "User can approach barrier from its right on large maze"
      (make_game Large 0 |> move_right |> move_down |> move_down)
      false;
    (* A user cannot move into a wall on its right *)
    invalid_move_game_test
      "User cannot move into wall on its right on empty maze" (fun () ->
        make_game Empty 0 |> move_down |> move_right |> move_right);
    invalid_move_game_test
      "User cannot move into wall on its right on small maze" (fun () ->
        make_game Small 0 |> move_right |> move_right |> move_right);
    invalid_move_game_test
      "User cannot move into wall on its right on large maze" (fun () ->
        make_game Large 0 |> move_right |> move_right |> move_right);
    (* A user cannot move into a wall above it *)
    invalid_move_game_test "User cannot move into wall above it on empty maze"
      (fun () ->
        make_game Empty 0 |> move_down |> move_down |> move_right |> move_right
        |> move_up);
    invalid_move_game_test "User cannot move into wall above it on small maze"
      (fun () ->
        make_game Small 0 |> move_down |> move_right |> move_right |> move_right
        |> move_up);
    invalid_move_game_test "User cannot move into wall above it on large maze"
      (fun () ->
        make_game Large 0 |> move_right |> move_down |> move_down |> move_down
        |> move_right |> move_right |> move_up);
    (* A user cannot move into a wall beneath it *)
    invalid_move_game_test "User cannot move into wall beneath it on empty maze"
      (fun () -> make_game Empty 0 |> move_right |> move_right |> move_down);
    invalid_move_game_test "User cannot move into wall beneath it on small maze"
      (fun () -> make_game Small 0 |> move_down |> move_down);
    invalid_move_game_test "User cannot move into wall beneath it on large maze"
      (fun () -> make_game Large 0 |> move_down |> move_down);
    (* A user cannot move into a wall on its left *)
    invalid_move_game_test
      "User cannot move into wall on its left on empty maze" (fun () ->
        make_game Empty 0 |> move_right |> move_right |> move_right |> move_down
        |> move_left);
    invalid_move_game_test
      "User cannot move into wall on its left on small maze" (fun () ->
        make_game Small 0 |> move_right |> move_right |> move_down |> move_right
        |> move_right |> move_down |> move_left);
    invalid_move_game_test
      "User cannot move into wall on its left on large maze" (fun () ->
        make_game Large 0 |> move_right |> move_down |> move_down |> move_left);
    (* A user cannot move off the board by moving right *)
    invalid_move_game_test
      "User cannot move off the board by moving right on empty maze" (fun () ->
        make_game Empty 0 |> move_right |> move_right |> move_right
        |> move_right |> move_right);
    invalid_move_game_test
      "User cannot move off the board by moving right on small maze" (fun () ->
        get_close_to_small_maze_exit 0 |> move_right);
    invalid_move_game_test
      "User cannot move off the board by moving right on large maze" (fun () ->
        get_close_to_large_maze_exit 0 |> move_right);
    (* A user cannot move off the board by moving up *)
    invalid_move_game_test
      "User cannot move off the board by moving up on empty maze" (fun () ->
        make_game Empty 0 |> move_right |> move_up);
    invalid_move_game_test
      "User cannot move off the board by moving up on small maze" (fun () ->
        make_game Small 0 |> move_right |> move_up);
    invalid_move_game_test
      "User cannot move off the board by moving up on large maze" (fun () ->
        make_game Large 0 |> move_right |> move_up);
    (* A user cannot move off the board by moving down *)
    invalid_move_game_test
      "User cannot move off the board by moving down on empty maze" (fun () ->
        make_game Empty 0 |> move_down |> move_down |> move_down);
    invalid_move_game_test
      "User cannot move off the board by moving down on small maze" (fun () ->
        get_close_to_small_maze_exit 0 |> move_left |> move_down |> move_down);
    invalid_move_game_test
      "User cannot move off the board by moving down on large maze" (fun () ->
        get_close_to_large_maze_exit 0 |> move_left |> move_down |> move_down);
    (* A user cannot move off the board by moving left *)
    invalid_move_game_test
      "User cannot move off the board by moving left on empty maze" (fun () ->
        make_game Empty 0 |> move_down |> move_left);
    invalid_move_game_test
      "User cannot move off the board by moving left on small maze" (fun () ->
        make_game Small 0 |> move_down |> move_left);
    invalid_move_game_test
      "User cannot move off the board by moving left on large maze" (fun () ->
        make_game Large 0 |> move_down |> move_left);
    (* Solvability without images *)
    solved_game_test "User can solve empty maze by tracing top and right edges"
      (fun () ->
        make_game Empty 0 |> move_right |> move_right |> move_right
        |> move_right |> move_down |> move_down);
    solved_game_test
      "User can solve empty maze by tracing left and bottom edges" (fun () ->
        make_game Empty 0 |> move_down |> move_down |> move_right |> move_right
        |> move_right |> move_right);
    solved_game_test "User can solve small maze" (fun () ->
        get_close_to_small_maze_exit 0 |> move_down);
    solved_game_test "User can solve large maze" (fun () ->
        get_close_to_large_maze_exit 0 |> move_down);
    (* Solvability with images *)
    solved_game_test "User can solve empty maze with images" (fun () ->
        make_game Empty 5 |> move_down |> move_down |> move_right |> move_right
        |> move_right |> move_right);
    solved_game_test "User can solve small maze with images" (fun () ->
        get_close_to_small_maze_exit 10 |> move_down);
    solved_game_test "User can solve large maze with images" (fun () ->
        get_close_to_large_maze_exit 25 |> move_down);
  ]

let tests = "Maze Game tests" >::: List.flatten [ controller_tests ]
let _ = run_test_tt_main tests
