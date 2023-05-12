type turtle = {
  mutable x : int;
  mutable y : int;
  mutable angle : int;
  mutable color : int;
}
(** [turtle] is an object that represents the current location. [x] and [y]
    stands for current coordinates, and [angle] is the direction that the turtle
    is facing. [color] is the color of the trail the turtle will leave as it
    moves.*)

val to_rad : int -> int
(** [to_rad angle] is angle in radians. *)

val forward : turtle -> int -> unit
(** [forward turtle l] draws a line with length [l] in the direction of
    [turtle.angle] *)

val backward : turtle -> int -> unit
(** [backward turtle l] draws a line with length [l] in the opposite direction
    of [turtle.angle] *)

val left : turtle -> int -> unit
(** [left turtle angle] turns the turtle to the left by [angle] degrees. *)

val right : turtle -> int -> unit
(** [right turtle anagle] turns the turtle to the right by [angle] degrees. *)
