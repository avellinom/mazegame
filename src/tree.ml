open Graphics
open Turtle

type seed = Turtle.turtle

let init_tree x y angle color = make_turtle x y angle color

let rec draw_tree seed depth length angle =
  if depth = 0 then ()
  else (
    forward seed length;
    left seed angle;
    draw_tree seed (depth - 1) (length *. 0.8) angle;
    right seed (angle * 2);
    draw_tree seed (depth - 1) (length *. 0.8) angle;
    left seed angle;
    backward seed length)
