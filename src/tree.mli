open Turtle

type seed
(** [seed] is the initial characteristics of a tree. The characteristics include
    initial position, color, and angle that it will start growing. *)

val init_tree : int -> int -> int -> int -> seed
(** [init_tree x y angle color] is the [seed]. It marks the beginning of a tree
    at coordinate ([x], [y]) facing [angle] in [color]. *)

val draw_tree : seed -> int -> float -> int -> unit
(** [draw_tree seed depth length angle] is a tree with initial branch/trunk
    [length]. [angle] determines how wide the branches go. *)
