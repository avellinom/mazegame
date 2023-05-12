open Graphics

type turtle = {
  mutable x : int;
  mutable y : int;
  mutable angle : int;
  mutable color : int;
}

let make_turtle x y angle color = { x; y; angle; color }
let to_rad angle = Float.pi *. 180. *. float_of_int angle

(** [move_turtle turtle l] moves the [turtle] by length [l] in the angle of
    [turtle.angle] while leaving a trail in [turtle.color]. *)
let move_turtle turtle length =
  let rad = to_rad turtle.angle in
  let dx = int_of_float (length *. cos rad) in
  let dy = int_of_float (length *. sin rad) in
  let x' = turtle.x + dx in
  let y' = turtle.y + dy in
  set_color turtle.color;
  moveto turtle.x turtle.y;
  lineto x' y';
  turtle.x <- x';
  turtle.y <- y'

let forward turtle l = move_turtle turtle l
let backward turtle l = move_turtle turtle (-1. *. l)
let left turtle angle = turtle.angle <- turtle.angle + angle
let right turtle angle = turtle.angle <- turtle.angle - angle
