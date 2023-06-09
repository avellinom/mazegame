(** This module [Image] represents an image that the user can come across in the
    maze and discover. *)

type t = unit -> unit
(** [t] is the type representing an image which exists in the maze. *)

val make_random : unit -> t
(** [make_random ()] displays a random image to the user. *)
