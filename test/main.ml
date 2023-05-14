open OUnit2
open Lib
open Controller

let data_dir_prefix : string = "data" ^ Filename.dir_sep

(* The filepath to the tourist maze. *)
let tourist_fileroute : string = data_dir_prefix ^ "tourist.mz"

(* The filepath to the pyramid maze. *)
let pyramid_fileroute : string = data_dir_prefix ^ "pyramid.mz"

(* The filepath to the empty maze. *)
let sphinx_fileroute : string = data_dir_prefix ^ "sphinx.mz"

(* The fileroute to the tomb maze. *)
let tomb_fileroute : string = data_dir_prefix ^ "tomb.mz"

(** maze_type represents all the types of mazes available. *)
type maze_type =
  | Tourist
  | Pyramid
  | Sphinx
  | Tomb

(** [make_game t n] makes a Maze Game from a maze type t with n images. *)
let make_game (typ : maze_type) (image_count : int) : Controller.t =
  let maze_filepath =
    match typ with
    | Tourist -> tourist_fileroute
    | Pyramid -> pyramid_fileroute
    | Sphinx -> sphinx_fileroute
    | Tomb -> tomb_fileroute
  in
  let ctrl = Controller.start_game maze_filepath image_count in
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

(* This sequence puts the player one above the goal on the pyramid maze. *)
let get_close_to_pyramid_maze_exit (image_amount : int) : Controller.t =
  make_game Pyramid image_amount
  |> move_right |> move_down |> move_down |> move_down |> move_down |> move_down
  |> move_down |> move_down |> move_down |> move_down |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_right

(* This sequence puts the player one above the goal on the sphinx maze. *)
let get_close_to_sphinx_maze_exit (image_amount : int) : Controller.t =
  make_game Sphinx image_amount
  |> move_down |> move_right |> move_right |> move_right |> move_right
  |> move_down |> move_down |> move_left |> move_left |> move_down |> move_down
  |> move_right |> move_right |> move_down |> move_down |> move_right
  |> move_right |> move_right |> move_right |> move_right |> move_right
  |> move_right |> move_right |> move_right |> move_down |> move_down
  |> move_right |> move_right

(* This sequence puts the player one above the goal on the tomb maze. *)
let get_close_to_tomb_maze_exit (image_amount : int) : Controller.t =
  make_game Tomb image_amount
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
    (* Tourist maze simple inital movement *)
    valid_game_test "User can go right initially on tourist maze"
      (make_game Tourist 0 |> move_right)
      false;
    valid_game_test "User can go down initially on tourist maze"
      (make_game Tourist 0 |> move_down)
      false;
    invalid_move_game_test "User cannot move left initially on tourist maze"
      (fun () -> make_game Tourist 0 |> move_left);
    invalid_move_game_test "User cannot move up initially on tourist maze"
      (fun () -> make_game Tourist 0 |> move_up);
    (* Pyramid maze simple initial movement *)
    valid_game_test "User can go right initially on pyramid maze"
      (make_game Pyramid 0 |> move_right)
      false;
    valid_game_test "User can go down initially on pyramid maze"
      (make_game Pyramid 0 |> move_down)
      false;
    invalid_move_game_test "User cannot move left initially on pyramid maze"
      (fun () -> make_game Pyramid 0 |> move_left);
    invalid_move_game_test "User cannot move up initially on pyramid maze"
      (fun () -> make_game Pyramid 0 |> move_up);
    (* Sphinx maze simple initial movement *)
    valid_game_test "User can go right initially on sphinx maze"
      (make_game Sphinx 0 |> move_right)
      false;
    valid_game_test "User can go down initially on sphinx maze"
      (make_game Sphinx 0 |> move_down)
      false;
    invalid_move_game_test "User cannot move left initially on sphinx maze"
      (fun () -> make_game Sphinx 0 |> move_left);
    invalid_move_game_test "User cannot move up initially on sphinx maze"
      (fun () -> make_game Sphinx 0 |> move_up);
    (* Tomb maze simple initial movement *)
    valid_game_test "User can go right initially on tomb maze"
      (make_game Tomb 0 |> move_right)
      false;
    valid_game_test "User can go down initially on tomb maze"
      (make_game Tomb 0 |> move_down)
      false;
    invalid_move_game_test "User cannot move left initially on tomb maze"
      (fun () -> make_game Tomb 0 |> move_left);
    invalid_move_game_test "User cannot move up initially on tomb maze"
      (fun () -> make_game Tomb 0 |> move_up);
    (* Mazes can be generated with images *)
    valid_game_test "Tourist maze can have images dropped on it"
      (make_game Tourist 3) false;
    valid_game_test "Pyramid maze can have images dropped on it"
      (make_game Pyramid 3) false;
    valid_game_test "Sphinx maze can have images dropped on it"
      (make_game Sphinx 3) false;
    valid_game_test "Tomb maze can have images dropped on it" (make_game Tomb 3)
      false;
    (* Mazes can be generated with more images than available maze locations*)
    valid_game_test "Tourist maze survives with extreme image count"
      (make_game Tourist 10000) false;
    valid_game_test "Pyramid maze can be constructed with extreme image count"
      (make_game Pyramid 10000) false;
    valid_game_test "Sphinx maze can be constructed with extreme image count"
      (make_game Sphinx 10000) false;
    valid_game_test "Tomb maze can be constructed with extreme image count"
      (make_game Tomb 10000) false;
    (* It is OK for a user to be adjacent to a barrier on its right *)
    valid_game_test "User can approach barrier from left on tourist maze"
      (make_game Tourist 0 |> move_down |> move_right)
      false;
    valid_game_test "User can approach barrier from left on pyramid maze"
      (make_game Pyramid 0 |> move_right |> move_down |> move_down |> move_right
     |> move_right |> move_right |> move_right |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from left on sphinx maze"
      (make_game Sphinx 0 |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from left on tomb maze"
      (make_game Tomb 0 |> move_right |> move_right)
      false;
    (* It is OK for a user to be adjacent to a barrier above it *)
    valid_game_test "User can approach barrier from beneath it on tourist maze"
      (make_game Tourist 0 |> move_down |> move_down |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from beneath it on pyramid maze"
      (make_game Pyramid 0 |> move_down |> move_right |> move_right
     |> move_right)
      false;
    valid_game_test "User can approach barrier from beneath it on sphinx maze"
      (make_game Sphinx 0 |> move_down |> move_right |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from beneath it on tomb maze"
      (make_game Tomb 0 |> move_right |> move_down |> move_down |> move_down
     |> move_right |> move_right)
      false;
    (* It is OK for a user to be adjacent to a barrier beneath it *)
    valid_game_test "User can approach barrier from above it on tourist maze"
      (make_game Tourist 0 |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from above it on pyramid maze"
      (make_game Pyramid 0 |> move_down |> move_right |> move_right
     |> move_right |> move_right |> move_right |> move_right |> move_right
     |> move_right |> move_right)
      false;
    valid_game_test "User can approach barrier from above it on sphinx maze"
      (make_game Sphinx 0 |> move_down)
      false;
    valid_game_test "User can approach barrier from above it on tomb maze"
      (make_game Tomb 0 |> move_down)
      false;
    (* It is OK for a user to be adjacent to a barrier on its left *)
    valid_game_test "User can approach barrier from its right on tourist maze"
      (make_game Tourist 0 |> move_right |> move_right |> move_right
     |> move_down)
      false;
    valid_game_test "User can approach barrier from its right on pyramid maze"
      (make_game Pyramid 0 |> move_down |> move_right |> move_right
     |> move_right |> move_right |> move_right |> move_right |> move_right
     |> move_right |> move_right |> move_right |> move_right |> move_down)
      false;
    valid_game_test "User can approach barrier from its right on sphinx maze"
      (make_game Sphinx 0 |> move_down |> move_right |> move_right |> move_right
     |> move_right |> move_down)
      false;
    valid_game_test "User can approach barrier from its right on tomb maze"
      (make_game Tomb 0 |> move_right |> move_down |> move_down)
      false;
    (* A user cannot move into a wall on its right *)
    invalid_move_game_test
      "User cannot move into wall on its right on tourist maze" (fun () ->
        make_game Tourist 0 |> move_down |> move_right |> move_right);
    invalid_move_game_test
      "User cannot move into wall on its right from pyramid maze" (fun () ->
        make_game Pyramid 0 |> move_down |> move_right |> move_down
        |> move_right |> move_right |> move_right |> move_right |> move_right
        |> move_right |> move_right |> move_right);
    invalid_move_game_test
      "User cannot move into wall on its right on sphinx maze" (fun () ->
        make_game Sphinx 0 |> move_right |> move_right |> move_right);
    invalid_move_game_test
      "User cannot move into wall on its right on tomb maze" (fun () ->
        make_game Tomb 0 |> move_right |> move_right |> move_right);
    (* A user cannot move into a wall above it *)
    invalid_move_game_test "User cannot move into wall above it on tourist maze"
      (fun () ->
        make_game Tourist 0 |> move_down |> move_down |> move_right
        |> move_right |> move_up);
    invalid_move_game_test "User cannot move into wall above it on pyramid maze"
      (fun () ->
        make_game Pyramid 0 |> move_down |> move_right |> move_right
        |> move_right |> move_up);
    invalid_move_game_test "User cannot move into wall above it on sphinx maze"
      (fun () ->
        make_game Sphinx 0 |> move_down |> move_right |> move_right
        |> move_right |> move_up);
    invalid_move_game_test "User cannot move into wall above it on tomb maze"
      (fun () ->
        make_game Tomb 0 |> move_right |> move_down |> move_down |> move_down
        |> move_right |> move_right |> move_up);
    (* A user cannot move into a wall beneath it *)
    invalid_move_game_test
      "User cannot move into wall beneath it on tourist maze" (fun () ->
        make_game Tourist 0 |> move_right |> move_right |> move_down);
    invalid_move_game_test
      "User cannot move into wall beneath it on pyramid maze" (fun () ->
        make_game Pyramid 0 |> move_down |> move_right |> move_right
        |> move_right |> move_right |> move_right |> move_right |> move_right
        |> move_right |> move_right |> move_down);
    invalid_move_game_test
      "User cannot move into wall beneath it on sphinx maze" (fun () ->
        make_game Sphinx 0 |> move_down |> move_down);
    invalid_move_game_test "User cannot move into wall beneath it on tomb maze"
      (fun () -> make_game Tomb 0 |> move_down |> move_down);
    (* A user cannot move into a wall on its left *)
    invalid_move_game_test
      "User cannot move into wall on its left on tourist maze" (fun () ->
        make_game Tourist 0 |> move_right |> move_right |> move_right
        |> move_down |> move_left);
    invalid_move_game_test
      "User cannot move into wall on its left on pyramid maze" (fun () ->
        make_game Pyramid 0 |> move_right |> move_down |> move_down |> move_left);
    invalid_move_game_test
      "User cannot move into wall on its left on sphinx maze" (fun () ->
        make_game Sphinx 0 |> move_right |> move_right |> move_down
        |> move_right |> move_right |> move_down |> move_left);
    invalid_move_game_test "User cannot move into wall on its left on tomb maze"
      (fun () ->
        make_game Tomb 0 |> move_right |> move_down |> move_down |> move_left);
    (* A user cannot move off the board by moving right *)
    invalid_move_game_test
      "User cannot move off the board by moving right on tourist maze"
      (fun () ->
        make_game Tourist 0 |> move_right |> move_right |> move_right
        |> move_right |> move_right);
    invalid_move_game_test
      "User cannot move off the board by moving right on pyramid maze"
      (fun () -> get_close_to_pyramid_maze_exit 0 |> move_right);
    invalid_move_game_test
      "User cannot move off the board by moving right on sphinx maze" (fun () ->
        get_close_to_sphinx_maze_exit 0 |> move_right);
    invalid_move_game_test
      "User cannot move off the board by moving right on tomb maze" (fun () ->
        get_close_to_tomb_maze_exit 0 |> move_right);
    (* A user cannot move off the board by moving up *)
    invalid_move_game_test
      "User cannot move off the board by moving up on tourist maze" (fun () ->
        make_game Tourist 0 |> move_right |> move_up);
    invalid_move_game_test
      "User cannot move off the board by moving up on tourist maze" (fun () ->
        make_game Tourist 0 |> move_right |> move_up);
    invalid_move_game_test
      "User cannot move off the board by moving up on sphinx maze" (fun () ->
        make_game Sphinx 0 |> move_right |> move_up);
    invalid_move_game_test
      "User cannot move off the board by moving up on tomb maze" (fun () ->
        make_game Tomb 0 |> move_right |> move_up);
    (* A user cannot move off the board by moving down *)
    invalid_move_game_test
      "User cannot move off the board by moving down on tourist maze" (fun () ->
        make_game Tourist 0 |> move_down |> move_down |> move_down);
    invalid_move_game_test
      "User cannot move off the board by moving down on pyramid maze" (fun () ->
        get_close_to_pyramid_maze_exit 0 |> move_left |> move_down |> move_down);
    invalid_move_game_test
      "User cannot move off the board by moving down on sphinx maze" (fun () ->
        get_close_to_sphinx_maze_exit 0 |> move_left |> move_down |> move_down);
    invalid_move_game_test
      "User cannot move off the board by moving down on tomb maze" (fun () ->
        get_close_to_tomb_maze_exit 0 |> move_left |> move_down |> move_down);
    (* A user cannot move off the board by moving left *)
    invalid_move_game_test
      "User cannot move off the board by moving left on tourist maze" (fun () ->
        make_game Tourist 0 |> move_down |> move_left);
    invalid_move_game_test
      "User cannot move off the board by moving left on pyramid maze" (fun () ->
        make_game Pyramid 0 |> move_down |> move_left);
    invalid_move_game_test
      "User cannot move off the board by moving left on sphinx maze" (fun () ->
        make_game Sphinx 0 |> move_down |> move_left);
    invalid_move_game_test
      "User cannot move off the board by moving left on tomb maze" (fun () ->
        make_game Tomb 0 |> move_down |> move_left);
    (* Solvability without images *)
    solved_game_test
      "User can solve tourist maze by tracing top and right edges" (fun () ->
        make_game Tourist 0 |> move_right |> move_right |> move_right
        |> move_right |> move_down |> move_down);
    solved_game_test
      "User can solve tourist maze by tracing left and bottom edges" (fun () ->
        make_game Tourist 0 |> move_down |> move_down |> move_right
        |> move_right |> move_right |> move_right);
    solved_game_test "User can solve pyramid maze" (fun () ->
        get_close_to_pyramid_maze_exit 0 |> move_down);
    solved_game_test "User can solve sphinx maze" (fun () ->
        get_close_to_sphinx_maze_exit 0 |> move_down);
    solved_game_test "User can solve tomb maze" (fun () ->
        get_close_to_tomb_maze_exit 0 |> move_down);
    (* Solvability with images *)
    solved_game_test "User can solve tourist maze with images" (fun () ->
        make_game Tourist 5 |> move_down |> move_down |> move_right
        |> move_right |> move_right |> move_right);
    solved_game_test "User can solve pyramid maze with images" (fun () ->
        get_close_to_pyramid_maze_exit 10 |> move_down);
    solved_game_test "User can solve sphinx maze with images" (fun () ->
        get_close_to_sphinx_maze_exit 10 |> move_down);
    solved_game_test "User can solve tomb maze with images" (fun () ->
        get_close_to_tomb_maze_exit 25 |> move_down);
  ]

(* The lowercase English alphabet. *)
let lowercase_alphabet : string = "abcdefghijklmnopqrstuvwxyz"

(** [test_affine_encrypt n p k c] creates a test named n. Encrypting p with k
    should yield c. *)
let test_affine_encrypt (name : string) (plaintext : string)
    (key : Crypt.affine_key) (expected_ciphertext : string) : test =
  name >:: fun _ ->
  assert_equal expected_ciphertext
    (Crypt.affine_encrypt plaintext key)
    ~printer:Fun.id

(** [test_affine_encrypt_bad_key n p k] creates a test named n. Encrypting p
    with k should yield a BadKey exception. In other words, key k does not
    satisfy the precondition for Affine cryptography. *)
let test_affine_encrypt_bad_key (name : string) (plaintext : string)
    (key : Crypt.affine_key) : test =
  name >:: fun _ ->
  assert_raises Crypt.BadKey (fun () -> Crypt.affine_encrypt plaintext key)

let cryptography_tests =
  [
    (* Specific instance (a=1) where Affine turns into a Caesar cipher *)
    test_affine_encrypt "Caesar equivalent, 1 shift right" lowercase_alphabet
      (Crypt.generate_determined_affine_key 1 1)
      "bcdefghijklmnopqrstuvwxyza";
    test_affine_encrypt "Caesar equivalent, 7 shifts right" lowercase_alphabet
      (Crypt.generate_determined_affine_key 1 7)
      "hijklmnopqrstuvwxyzabcdefg";
    test_affine_encrypt "Caesar equivalent, 26 shifts right" lowercase_alphabet
      (Crypt.generate_determined_affine_key 1 26)
      lowercase_alphabet;
    test_affine_encrypt "Caesar equivalent, shift more than 26 times"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 1 70)
      "stuvwxyzabcdefghijklmnopqr";
    (* When a is small and b varies *)
    test_affine_encrypt "In key=(a,b), use small a and zero b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 3 0)
      "adgjmpsvybehknqtwzcfilorux";
    test_affine_encrypt "In key=(a,b), use small a and moderate b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 5 12)
      "mrwbglqvafkpuzejotydinsxch";
    test_affine_encrypt "In key=(a,b), use small a and large b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 3 24)
      "ybehknqtwzcfiloruxadgjmpsv";
    test_affine_encrypt "In key=(a,b), use small a and large b > 26"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 5 10000)
      "qvafkpuzejotydinsxchmrwbgl";
    (* When a is moderate and b varies *)
    test_affine_encrypt "In key=(a,b), use moderate a and zero b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 15 0)
      "apetixmbqfujyncrgvkzodshwl";
    test_affine_encrypt "In key=(a,b), use moderate a and small b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 17 2)
      "ctkbsjarizqhypgxofwnevmdul";
    test_affine_encrypt "In key=(a,b), use moderate a and large b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 15 25)
      "zodshwlapetixmbqfujyncrgvk";
    test_affine_encrypt "In key=(a,b), use moderate a and large b > 26"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 15 9876)
      "wlapetixmbqfujyncrgvkzodsh";
    (* When a is large and b varies *)
    test_affine_encrypt "In key=(a,b), use large a and zero b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 23 0)
      "axurolifczwtqnkhebyvspmjgd";
    test_affine_encrypt "In key=(a,b), use large a and small b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 23 4)
      "ebyvspmjgdaxurolifczwtqnkh";
    test_affine_encrypt "In key=(a,b), use large a and moderate b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 25 13)
      "nmlkjihgfedcbazyxwvutsrqpo";
    test_affine_encrypt "In key=(a,b), use large a and large b"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 25 24)
      "yxwvutsrqponmlkjihgfedcbaz";
    test_affine_encrypt "In key=(a,b), use large a and large b > 26"
      lowercase_alphabet
      (Crypt.generate_determined_affine_key 21 456788)
      "upkfavqlgbwrmhcxsnidytojez";
    (* Affine encryption raises BadKey where applicable *)
    test_affine_encrypt_bad_key
      "In key=(a,b), gcd(a,26) != 1 raises when a is small" lowercase_alphabet
      (Crypt.generate_determined_affine_key 6 2);
    test_affine_encrypt_bad_key
      "In key=(a,b), gcd(a, 26) != 1 raises when a is large" lowercase_alphabet
      (Crypt.generate_determined_affine_key 364 6)
    (* Affine encryption works on lowercase strings with spaces *);
  ]

let tests =
  "Maze Game tests" >::: List.flatten [ controller_tests @ cryptography_tests ]

let _ = run_test_tt_main tests
