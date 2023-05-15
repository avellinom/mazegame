open Turtle

type color = int
(** [color] is the representation of color. *)

(** [color_scheme] is a representation of different colors. Int in each type
    represents the gradient of the color, 1 being the lightest and 9 being the
    darkest. Precondition: Int within the type must be between 1 and 9
    inclusive. *)
type color_scheme =
  | Blank
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

(** [pat] represents different kind of patterns of shape, size, and color. *)
type pat =
  | NA
  | Circle of int * color_scheme
  | Triangle of int * color_scheme
  | Square of int * color_scheme
  | Diamond of int * color_scheme
  | Pentagon of int * color_scheme
  (* depth length angle *)
  | Tree of pat * int * float * int * color_scheme
  (* sides, length, depth *)
  | Snowflake of int * float * int * color_scheme

val palette : color_scheme -> color
(** [palette c_scheme] is color representative of [c_scheme]. *)

val draw_triangle : turtle -> int -> int -> unit
(** [draw_triangle turtle r c] is a triangle that has an endpoint at radius [r]
    away from where [turtle] is facing. If [c] is a valid color e.g.
    non-negative, a triangle of color [c] is produced. *)

val draw_square : turtle -> int -> int -> unit
(** [draw_square turtle r c] is a square. The square is determined by where the
    [turtle] is facing. Going radius [r] from where [turtle] is facing will be
    the middle of one side of the square. If [c] is a valid color e.g.
    non-negative, a square of color [c] is produced. *)

val draw_diamond : turtle -> int -> int -> unit
(** [draw_diamond turtle h c] is a diamond that has an endpoint at height [h]
    away from where [turtle] is facing. If [c] is a valid color e.g.
    non-negative, a diamond of color [c] is produced. *)

val draw_circle : turtle -> int -> int -> unit
(** [draw_circle turtle r c] is a circle with radius [r] at the location of
    [turtle]. If [c] is a valid color e.g. non-negative, a circle of color [c]
    is produced. *)

val draw_pentagon : turtle -> int -> int -> unit
(** [draw_pentagon turtle r c] is a pentagon that has an endpoint at radius [r]
    away from where [turtle] is facing. If [c] is a valid color e.g.
    non-negative, a pentagon of color [c] is produced. *)

type flake = turtle

val init_snowflake : int -> int -> int -> int -> flake
(** [init_snowflake x y angle color] is the [flake]. It marks the an endpoint of
    a snowflake at coordinate ([x], [y]) facing [angle] in [color]. *)

val snowflake_side : flake -> float -> int -> unit
(** [snowflake_side turtle length depth] is one side of a snowflake. *)

val draw_snowflake : flake -> int -> int -> float -> int -> color -> unit
(** [draw_snowflake flake acc sides length depth c] is a snowflake of color [c]
    with [sides] of each [length] with [depth]. Precondition: [sides] must equal
    [acc] and [depth should not be more than 4. ]*)

type seed = turtle
(** [seed] is the initial characteristics of a tree. The characteristics include
    initial position, color, and angle that it will start growing. *)

val init_tree : int -> int -> int -> int -> seed
(** [init_tree x y angle color] is the [seed]. It marks the beginning of a tree
    at coordinate ([x], [y]) facing [angle] in [color]. *)

val draw_tree : seed -> unit -> int -> float -> int -> color -> unit
(** [draw_tree seed f depth length angle c] is a tree of color [c] with initial
    branch/trunk [length]. [angle] determines how wide the branches go and [f]
    determines the leaf. *)

val draw_pat : turtle -> pat -> unit
(** [draw_pat turtle pat] draws pattern [pat] where the [turtle] is. *)
