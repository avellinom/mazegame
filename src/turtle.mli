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

val make_turtle : int -> int -> int -> int -> turtle
(** [make_turtle x y angle color] is a new turtle at position [x] and [y],
    facing [angle] in [color]. *)

val to_rad : int -> float
(** [to_rad angle] is angle in radians. *)

val forward : turtle -> float -> unit
(** [forward turtle l] draws a line with length [l] in the direction of
    [turtle.angle] *)

val backward : turtle -> float -> unit
(** [backward turtle l] draws a line with length [l] in the opposite direction
    of [turtle.angle] *)

val left : turtle -> int -> unit
(** [left turtle angle] turns the turtle to the left by [angle] degrees. *)

val right : turtle -> int -> unit
(** [right turtle angle] turns the turtle to the right by [angle] degrees. *)
