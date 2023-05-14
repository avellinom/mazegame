open Turtle

type color = int
(** [color] is the representation of color. *)

(** [color_scheme] is a representation of different colors. Int in each type
    represents the gradient of the color, 1 being the lightest and 9 being the
    darkest. Precondition: Int within the type must be between 1 and 9
    inclusive. *)
type color_scheme =
  | Red of int
  | Orange of int
  | Yellow of int
  | LightGreen of int
  | Green of int
  | BlueGreen of int
  | Skyblue of int
  | LightBlue of int
  | Blue of int
  | Purple of int
  | Pink of int
  | RedPink of int
  | BlackWhite of int

val palette : color_scheme -> color
(** [palette c_scheme] is color representative of [c_scheme]. *)

val draw_triangle : turtle -> int -> unit
(** [draw_triangle turtle r] is a triangle that has an endpoint at radius [r]
    away from where [turtle] is facing. *)

val draw_square : turtle -> int -> unit
(** [draw_square turtle r] is a square. The square is determined by where the
    [turtle] is facing. Going radius [r] from where [turtle] is facing will be
    the middle of one side of the square. *)

val draw_circle : turtle -> int -> unit
(** [draw_circle turtle r] is a circle with radius [r] at the location of
    [turtle]. *)

val draw_pentagon : turtle -> int -> unit
(** [draw_pentagon turtle r] is a pentagon that has an endpoint at radius [r]
    away from where [turtle] is facing. *)

val color_triangle : turtle -> int -> int -> unit
(** [color_triangle turtle r c] is a triangle of color [c] that has an endpoint
    at radius [r] away from where [turtle] is facing. *)

val color_square : turtle -> int -> int -> unit
(** [color_square turtle r c] is a square of color [c]. The square is determined
    by where the [turtle] is facing. Going radius [r] from where [turtle] is
    facing will be the middle of one side of the square. *)

val color_pentagon : turtle -> int -> int -> unit
(** [color_pentagon turtle r c] is a pentagon of color [c] that has an endpoint
    at radius [r] away from where [turtle] is facing. *)

val color_circle : turtle -> int -> int -> unit
(** [color_circle turtle r c] is a circle of color [c] with radius [r] at the
    location of [turtle]. *)

type flake = turtle

val init_snowflake : int -> int -> int -> int -> flake
(** [init_snowflake x y angle color] is the [flake]. It marks the an endpoint of
    a snowflake at coordinate ([x], [y]) facing [angle] in [color]. *)

val snowflake_side : flake -> float -> int -> unit
(** [snowflake_side turtle length depth] is one side of a snowflake. *)

val draw_snowflake : flake -> int -> int -> float -> int -> unit
(** [draw_snowflake flake acc sides length depth] is a snowflake with [sides] of
    each [length] with [depth]. Precondition: [sides] must equal [acc]*)

type seed = turtle
(** [seed] is the initial characteristics of a tree. The characteristics include
    initial position, color, and angle that it will start growing. *)

val init_tree : int -> int -> int -> int -> seed
(** [init_tree x y angle color] is the [seed]. It marks the beginning of a tree
    at coordinate ([x], [y]) facing [angle] in [color]. *)

val draw_tree : seed -> int -> float -> int -> unit
(** [draw_tree seed depth length angle] is a tree with initial branch/trunk
    [length]. [angle] determines how wide the branches go. *)
