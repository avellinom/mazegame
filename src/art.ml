open Graphics
open Turtle

type color = int

(** [find_endpoint turtle r_float] is the endpoint of an odd-sided polygon that
    is radius [r_float] from where the [turtle] is facing. *)
let find_endpoint turtle r =
  let dx, dy = find_coordinate turtle.angle r in
  let x = turtle.x + dx in
  let y = turtle.y + dy in
  (x, y)

(** [triangle_endpts turtle r] is an array containing endpoints of a triangle.
    Refer to [draw_triangle] spec for what triangle it describes. *)
let triangle_endpts turtle r =
  let r' = float_of_int r in
  let endpoint1 = find_endpoint turtle r' in
  right turtle 120;
  let endpoint2 = find_endpoint turtle r' in
  right turtle 120;
  let endpoint3 = find_endpoint turtle r' in
  right turtle 120;
  [| endpoint1; endpoint2; endpoint3 |]

(** [sqruare_endpts turtle r] is an array containing endpoints of a square.
    Refer to [draw_square] spec for what square it describes. *)
let sqruare_endpts turtle r =
  left turtle 45;
  let new_r = sqrt 2. *. float_of_int r in
  let endpoint1 = find_endpoint turtle new_r in
  left turtle 90;
  let endpoint2 = find_endpoint turtle new_r in
  left turtle 90;
  let endpoint3 = find_endpoint turtle new_r in
  left turtle 90;
  let endpoint4 = find_endpoint turtle new_r in
  left turtle 45;
  [| endpoint1; endpoint2; endpoint3; endpoint4 |]

(** [diamond_endpts turtle w h] is an array containing endpoints of a diamond.
    Refer to [draw_diamond] spec for what diamond it describes. *)
let diamond_endpts turtle h =
  let w = float_of_int h /. 2. in
  let h' = float_of_int h in
  let endpoint1 = find_endpoint turtle h' in
  left turtle 90;
  let endpoint2 = find_endpoint turtle w in
  left turtle 90;
  let endpoint3 = find_endpoint turtle h' in
  left turtle 90;
  let endpoint4 = find_endpoint turtle w in
  left turtle 90;
  [| endpoint1; endpoint2; endpoint3; endpoint4 |]

(** [pentagon_endpts turtle r] is an array containing endpoints of a pentagon.
    Refer to [draw_pentagon] spec for what pentagon it describes. *)
let pentagon_endpts turtle r =
  let r' = float_of_int r in
  let endpoint1 = find_endpoint turtle r' in
  right turtle 72;
  let endpoint2 = find_endpoint turtle r' in
  right turtle 72;
  let endpoint3 = find_endpoint turtle r' in
  right turtle 72;
  let endpoint4 = find_endpoint turtle r' in
  right turtle 72;
  let endpoint5 = find_endpoint turtle r' in
  right turtle 72;
  [| endpoint1; endpoint2; endpoint3; endpoint4; endpoint5 |]

let draw_triangle turtle r =
  let arr = triangle_endpts turtle r in
  draw_poly arr

let draw_square turtle r =
  let arr = sqruare_endpts turtle r in
  draw_poly arr

let draw_diamond turtle h =
  let arr = diamond_endpts turtle h in
  draw_poly arr

let draw_pentagon turtle r =
  let arr = pentagon_endpts turtle r in
  draw_poly arr

let draw_circle turtle r = Graphics.draw_circle turtle.x turtle.y r

let color_circle turtle r c =
  set_color c;
  fill_circle turtle.x turtle.y r;
  set_color turtle.color

let color_triangle turtle r c =
  set_color c;
  let arr = triangle_endpts turtle r in
  fill_poly arr;
  set_color turtle.color

let color_square turtle r c =
  set_color c;
  let arr = sqruare_endpts turtle r in
  fill_poly arr;
  set_color turtle.color

let color_diamond turtle h c =
  set_color c;
  let arr = diamond_endpts turtle h in
  fill_poly arr;
  set_color turtle.color

let color_pentagon turtle r c =
  set_color c;
  let arr = pentagon_endpts turtle r in
  fill_poly arr;
  set_color turtle.color

type flake = turtle

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

type seed = Turtle.turtle

let init_tree x y angle color = make_turtle x y angle color

let rec draw_tree seed f depth length angle =
  if depth = 0 then f
  else (
    forward seed length;
    left seed angle;
    draw_tree seed f (depth - 1) (length *. 0.8) angle;
    right seed (angle * 2);
    draw_tree seed f (depth - 1) (length *. 0.8) angle;
    left seed angle;
    backward seed length)

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

let palette = function
  | Blank -> failwith "Blank can't have color."
  | Red 1 -> rgb 255 204 204
  | Red 2 -> rgb 255 153 153
  | Red 3 -> rgb 255 102 102
  | Red 4 -> rgb 255 51 51
  | Red 5 -> rgb 255 0 0
  | Red 6 -> rgb 204 0 0
  | Red 7 -> rgb 153 0 0
  | Red 8 -> rgb 102 0 0
  | Red 9 -> rgb 51 0 0
  | Red _ -> failwith "Red color scheme int must be between 1 and 9 inclusive."
  | Orange 1 -> rgb 255 229 204
  | Orange 2 -> rgb 255 204 153
  | Orange 3 -> rgb 255 178 102
  | Orange 4 -> rgb 255 153 51
  | Orange 5 -> rgb 255 128 0
  | Orange 6 -> rgb 204 102 0
  | Orange 7 -> rgb 153 76 0
  | Orange 8 -> rgb 102 51 0
  | Orange 9 -> rgb 51 25 0
  | Orange _ ->
      failwith "Orange color scheme int must be between 1 and 9 inclusive."
  | Yellow 1 -> rgb 255 255 204
  | Yellow 2 -> rgb 255 255 153
  | Yellow 3 -> rgb 255 255 102
  | Yellow 4 -> rgb 255 255 51
  | Yellow 5 -> rgb 255 255 0
  | Yellow 6 -> rgb 204 204 0
  | Yellow 7 -> rgb 153 153 0
  | Yellow 8 -> rgb 102 102 0
  | Yellow 9 -> rgb 51 51 0
  | Yellow _ ->
      failwith "Yellow color scheme int must be between 1 and 9 inclusive."
  | LightGreen 1 -> rgb 229 255 204
  | LightGreen 2 -> rgb 204 255 153
  | LightGreen 3 -> rgb 178 255 102
  | LightGreen 4 -> rgb 153 255 51
  | LightGreen 5 -> rgb 128 255 0
  | LightGreen 6 -> rgb 102 204 0
  | LightGreen 7 -> rgb 76 153 0
  | LightGreen 8 -> rgb 51 102 0
  | LightGreen 9 -> rgb 25 51 0
  | LightGreen _ ->
      failwith "LightGreen color scheme int must be between 1 and 9 inclusive."
  | Green 1 -> rgb 204 255 204
  | Green 2 -> rgb 153 255 153
  | Green 3 -> rgb 102 255 102
  | Green 4 -> rgb 204 255 204
  | Green 5 -> rgb 0 255 0
  | Green 6 -> rgb 0 204 0
  | Green 7 -> rgb 0 153 0
  | Green 8 -> rgb 0 102 0
  | Green 9 -> rgb 0 51 0
  | Green _ ->
      failwith "Green color scheme int must be between 1 and 9 inclusive."
  | BlueGreen 1 -> rgb 204 255 229
  | BlueGreen 2 -> rgb 153 255 204
  | BlueGreen 3 -> rgb 102 255 178
  | BlueGreen 4 -> rgb 51 255 153
  | BlueGreen 5 -> rgb 0 255 128
  | BlueGreen 6 -> rgb 0 204 102
  | BlueGreen 7 -> rgb 0 153 76
  | BlueGreen 8 -> rgb 0 102 51
  | BlueGreen 9 -> rgb 0 51 25
  | BlueGreen _ ->
      failwith "BlueGreen color scheme int must be between 1 and 9 inclusive."
  | Skyblue 1 -> rgb 204 255 255
  | Skyblue 2 -> rgb 153 255 255
  | Skyblue 3 -> rgb 102 255 255
  | Skyblue 4 -> rgb 51 255 255
  | Skyblue 5 -> rgb 0 255 255
  | Skyblue 6 -> rgb 0 204 204
  | Skyblue 7 -> rgb 0 153 153
  | Skyblue 8 -> rgb 0 102 102
  | Skyblue 9 -> rgb 0 51 51
  | Skyblue _ ->
      failwith "Skyblue color scheme int must be between 1 and 9 inclusive."
  | LightBlue 1 -> rgb 204 229 255
  | LightBlue 2 -> rgb 153 204 255
  | LightBlue 3 -> rgb 102 178 255
  | LightBlue 4 -> rgb 51 153 255
  | LightBlue 5 -> rgb 0 128 255
  | LightBlue 6 -> rgb 0 102 204
  | LightBlue 7 -> rgb 0 76 153
  | LightBlue 8 -> rgb 0 51 102
  | LightBlue 9 -> rgb 0 25 51
  | LightBlue _ ->
      failwith "LightBlue color scheme int must be between 1 and 9 inclusive."
  | Blue 1 -> rgb 204 204 255
  | Blue 2 -> rgb 153 153 255
  | Blue 3 -> rgb 102 102 255
  | Blue 4 -> rgb 51 51 255
  | Blue 5 -> rgb 0 0 255
  | Blue 6 -> rgb 0 0 204
  | Blue 7 -> rgb 0 0 153
  | Blue 8 -> rgb 0 0 102
  | Blue 9 -> rgb 0 0 51
  | Blue _ ->
      failwith "Blue color scheme int must be between 1 and 9 inclusive."
  | Purple 1 -> rgb 229 204 255
  | Purple 2 -> rgb 204 153 255
  | Purple 3 -> rgb 178 102 255
  | Purple 4 -> rgb 153 51 255
  | Purple 5 -> rgb 127 0 255
  | Purple 6 -> rgb 102 0 204
  | Purple 7 -> rgb 76 0 153
  | Purple 8 -> rgb 51 0 102
  | Purple 9 -> rgb 25 0 51
  | Purple _ ->
      failwith "Purple color scheme int must be between 1 and 9 inclusive."
  | Pink 1 -> rgb 255 204 255
  | Pink 2 -> rgb 255 153 255
  | Pink 3 -> rgb 255 102 255
  | Pink 4 -> rgb 255 51 255
  | Pink 5 -> rgb 255 0 255
  | Pink 6 -> rgb 204 0 204
  | Pink 7 -> rgb 153 0 153
  | Pink 8 -> rgb 102 0 102
  | Pink 9 -> rgb 51 0 51
  | Pink _ ->
      failwith "Pink color scheme int must be between 1 and 9 inclusive."
  | RedPink 1 -> rgb 255 204 229
  | RedPink 2 -> rgb 255 153 204
  | RedPink 3 -> rgb 255 102 178
  | RedPink 4 -> rgb 255 51 153
  | RedPink 5 -> rgb 255 0 127
  | RedPink 6 -> rgb 204 0 102
  | RedPink 7 -> rgb 153 0 76
  | RedPink 8 -> rgb 102 0 51
  | RedPink 9 -> rgb 51 0 25
  | RedPink _ ->
      failwith "RedPink color scheme int must be between 1 and 9 inclusive."
  | BlackWhite 1 -> rgb 255 255 255
  | BlackWhite 2 -> rgb 224 224 224
  | BlackWhite 3 -> rgb 192 192 192
  | BlackWhite 4 -> rgb 160 160 160
  | BlackWhite 5 -> rgb 128 128 128
  | BlackWhite 6 -> rgb 96 96 96
  | BlackWhite 7 -> rgb 64 64 64
  | BlackWhite 8 -> rgb 32 32 32
  | BlackWhite 9 -> rgb 0 0 0
  | BlackWhite _ ->
      failwith "BlackWhite color scheme int must be between 1 and 9 inclusive."

type pat =
  | NA
  | Circle of int * color_scheme
  | Triangle of int * color_scheme
  | Square of int * color_scheme
  | Diamond of int * color_scheme
  | Pentagon of int * color_scheme
  | Tree of pat * int * float * int * color_scheme
  | Snowflake of int * float * int * color_scheme

(* let draw_pat turtle = function | NA -> () | Circle (r, c_scheme) -> |
   Triangle (r, c_scheme) -> | Square (r, c_scheme) -> | Diamond (h, c_scheme)
   -> | Pentagon (r, c_scheme) -> *)
(* | Tree of patt * int * float * int * color_scheme | Snowflake of int * float
   * int * color_scheme *)
