type t
(** [t] is the abstract type representing a controller for a Maze Game. This is
    where the user provides instructions to the Maze Game. *)

val start_game : string -> string -> int -> t
(** [start_game f s n] starts a game by generating a maze from file f and
    creating a user of name s. The game will have n images in its maze. Raises:
    [Failure] if f is not a valid maze file. *)

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
