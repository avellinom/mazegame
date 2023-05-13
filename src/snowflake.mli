open Turtle

val init_snowflake : int -> int -> int -> int -> turtle
(** [init_snowflake x y angle color] is the [turtle]. It marks the beginning of
    a snowflake at coordinate ([x], [y]) facing [angle] in [color]. *)

val snowflake_side : turtle -> float -> int -> unit
(** [snowflake_side turtle length depth] is one side of a snowflake. *)

val draw_snowflake : turtle -> int -> int -> float -> int -> unit
(** [draw_snowflake turtle acc sides length depth] is a snowflake with [sides]
    of each [length] with [depth]. Precondition: [sides] must equal [acc]*)
