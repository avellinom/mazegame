open Graphics
open Turtle

let init_tree x y angle color = make_turtle x y angle color

let rec draw_tree turtle depth length angle =
  if depth = 0 then () else forward turtle length;
  left turtle angle;
  draw_tree turtle (depth - 1) (length *. 0.8) angle;
  right turtle (angle * 2);
  draw_tree turtle (depth - 1) (length *. 0.8) angle;
  left turtle angle;
  backward turtle length
