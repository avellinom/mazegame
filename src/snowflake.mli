open Turtle

val init_snowflake : int -> int -> int -> int -> turtle
(** [init_snowflake x y angle color] is the [turtle]. It marks the beginning of
    a snowflake at coordinate ([x], [y]) facing [angle] in [color]. *)

val snowflake_side : turtle -> float -> int -> unit
(** [snowflake_side turtle length depth] is one side of a snowflake. *)

val draw_snowflake : turtle -> int -> int -> float -> unit
(** [draw_snowflake turtle acc sides length] is a snowflake with [sides] number
    of sides, each having [length]. [acc] shoudl equal [sides]. *)
