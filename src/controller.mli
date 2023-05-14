type t
(** [t] is the abstract type representing a controller for a Maze Game. This is
    where the user provides instructions to the Maze Game. *)

val start_game : string -> int -> bool -> t
(** [start_game f b n] starts a game by generating a maze from file [f]. The
    game will have n images in its maze and one key iff b. Raises: [Failure] if
    f is not a valid maze file. *)

type key_status =
  | NotPlaced
  | NotFound of Crypt.affine_key
  | Found of Crypt.affine_key

val get_key_status : t -> key_status
(** [get_key_status c] returns NotPlaced if a key was never placed on the maze.
    Otherwise, it returns Found if the key was found by the user and NotFound
    otherwise. *)

val print_game : t -> ANSITerminal.style -> unit
(** [print_game c] prints the state of the game to the command line. *)

val string_of_game : t -> string
(** [sting_of_game c] returns the maze board as a string. *)

exception InvalidMove
(** Raised when a move is not legal. For example, moving off the maze or into a
    wall. *)

exception MazeSolved
(** Raised immediately after the user move to the bottom-right-most square in
    the maze. *)

val move_left : t -> t
(** [move_left c] moves the user on the maze left once. Returns a controller,
    identical for c except for that movement. Raises: InvalidMove if the
    movement is not possible.*)

val move_right : t -> t
(** [move_right c] moves the user on the maze right once. Returns a controller,
    identical for c except for that movement. Raises: InvalidMove if the
    movement is not possible.*)

val move_up : t -> t
(** [move_up c] moves the user on the maze up once. Returns a controller,
    identical for c except for that movement. Raises: InvalidMove if the
    movement is not possible.*)

val move_down : t -> t
(** [move_down c] moves the user on the maze down once. Returns a controller,
    identical for c except for that movement. Raises: InvalidMove if the
    movement is not possible.*)
