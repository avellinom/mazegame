type t
(** [t] is the abstract type representing a maze. *)

val of_file : string -> t
(** [of_file filename] generates a maze based on a filename. Raises: TODO if
    file does not exist. *)
val to_list : string -> string list

val print_maze : t -> unit
(** [print_maze m] prints the contents of maze m in the command line. *)
