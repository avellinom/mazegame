type t
(** [t] is the abstract type representing a maze. *)

type entry
(** [entry] is the abstract type representing a maze entry. *)

val maze_of_file : string -> t
(** [maze_of_file f] creates a maze type from file f. Raises: [Failure] if f
    does not exist. *)

val print_maze : t -> unit
(** [print_maze m] prints the contents of maze m to the command line. *)
