type entry =
  | Free
  | Wall
  | Goal
  | Picture of Image.t
  | Key of Crypt.affine_key
  | Person of User.t  (** [entry] is the type representing a maze entry. *)

type location = int * int
(** [location] is the type representing a location in the maze. *)

type t
(** [t] is the abstract type representing a maze. *)

val make : string -> t
(** [maze_of_file f] creates a maze type from file f. Raises: [Failure] if f
    does not exist or if the maze file does not properly contain only spaces and
    hashes. *)

val char_of_entry : entry -> char
(** [char_of_entry e] converts an entry of the maze e into a char
    representation. *)

val array_of_maze : t -> entry array array
(** [array_of_maze m] returns maze m as a 2D array. *)

val get_num_rows : entry array array -> int
(** [get_num_rows m] returns the number of rows in 2D matrix m. *)

val get_num_cols : entry array array -> int
(** [get_num_cols m] returns the number of columns in 2D matrix m. *)

val generate_images : t -> int -> t
(** [generate_images m p] generates random images and places them at random
    locations that are available in maze m. p is the approximate amount of
    images to be dropped on the maze. *)
