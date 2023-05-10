type t
(** [t] is the abstract type representing a controller for a Maze Game. This is
    where the user provides instructions to the Maze Game. *)

val start_game : string -> string -> t
(** [start_game f s] starts a game by generating a maze from file f and creating
    a user of name s. Raises: [Failure] if f is not a valid maze file. *)

val print_game : t -> unit
(** [print_game c] prints the state of the game to the command line. *)

val move_left : t -> t
(** [move_left c] moves the user on the maze left once. *)

val move_right : t -> t
(** [move_right c] moves the user on the maze right once. *)

val move_up : t -> t
(** [move_up c] moves the user on the maze up once. *)

val move_down : t -> t
(** [move_down c] moves the user on the maze up once. *)
