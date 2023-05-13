open Turtle
open Graphics

let init_snowflake x y angle color = make_turtle x y angle color

(* Do not have higher than 4 depth *)
let rec snowflake_side turtle length depth =
  if depth = 0 then forward turtle length
  else
    let new_l = length /. 3. in
    snowflake_side turtle new_l (depth - 1);
    left turtle 60;
    snowflake_side turtle new_l (depth - 1);
    right turtle 120;
    snowflake_side turtle new_l (depth - 1);
    left turtle 60;
    snowflake_side turtle new_l (depth - 1)

let rec draw_snowflake turtle acc sides length depth =
  if acc = 0 then ()
  else (
    snowflake_side turtle length depth;
    right turtle (360 / sides);
    draw_snowflake turtle (acc - 1) sides length depth)

let draw_snowflake_centered turtle acc sides length depth =
  draw_snowflake turtle acc sides length depth
