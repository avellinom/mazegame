open Graphics
open Turtle

type color = int

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

type pat =
  | NA
  | Circle of int * color_scheme
  | Triangle of int * color_scheme
  | Square of int * color_scheme
  | Diamond of int * color_scheme
  | Pentagon of int * color_scheme
  | Tree of (unit -> pat) * int * float * int * color_scheme
  | Snowflake of int * float * int * color_scheme

let make_circle r c_scheme = Circle (r, c_scheme)
let make_triangle r c_scheme = Triangle (r, c_scheme)
let make_square r c_scheme = Square (r, c_scheme)
let make_diamond h c_scheme = Diamond (h, c_scheme)
let make_pentagon r c_scheme = Pentagon (r, c_scheme)
let make_tree p depth l angle c_scheme = Tree (p, depth, l, angle, c_scheme)
let make_snowflake sides l depth c_scheme = Snowflake (sides, l, depth, c_scheme)

let palette = function
  | Blank -> -1
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

let draw_triangle turtle r c =
  let arr = triangle_endpts turtle r in
  if c < 0 then draw_poly arr
  else (
    set_color c;
    fill_poly arr;
    set_color turtle.color)

let draw_square turtle r c =
  let arr = sqruare_endpts turtle r in
  if c < 0 then draw_poly arr
  else (
    set_color c;
    fill_poly arr;
    set_color turtle.color)

let draw_diamond turtle h c =
  let arr = diamond_endpts turtle h in
  if c < 0 then draw_poly arr
  else (
    set_color c;
    fill_poly arr;
    set_color turtle.color)

let draw_pentagon turtle r c =
  let arr = pentagon_endpts turtle r in
  if c < 0 then draw_poly arr
  else (
    set_color c;
    fill_poly arr;
    set_color turtle.color)

let draw_circle turtle r c =
  if c < 0 then Graphics.draw_circle turtle.x turtle.y r
  else (
    set_color c;
    fill_circle turtle.x turtle.y r;
    set_color turtle.color)

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

let rec draw_snowflake turtle acc sides length depth c =
  if acc = 0 then ()
  else
    let orig_c = turtle.color in
    turtle.color <- c;
    snowflake_side turtle length depth;
    right turtle (360 / sides);
    draw_snowflake turtle (acc - 1) sides length depth c;
    turtle.color <- orig_c

let random_gradient () = Random.int 9 + 1

type seed = Turtle.turtle

let init_tree x y angle color = make_turtle x y angle color

let rec draw_tree seed p depth length angle c =
  if depth = 0 then
    let pat = p () in
    draw_pat seed pat
  else
    let orig_c = seed.color in
    seed.color <- c;
    forward seed length;
    left seed angle;
    draw_tree seed p (depth - 1) (length *. 0.8) angle c;
    right seed (angle * 2);
    draw_tree seed p (depth - 1) (length *. 0.8) angle c;
    left seed angle;
    backward seed length;
    seed.color <- orig_c

and draw_pat turtle p =
  match p with
  | NA -> ()
  | Circle (r, c_scheme) -> draw_circle turtle r (palette c_scheme)
  | Triangle (r, c_scheme) -> draw_triangle turtle r (palette c_scheme)
  | Square (r, c_scheme) -> draw_square turtle r (palette c_scheme)
  | Diamond (h, c_scheme) -> draw_diamond turtle h (palette c_scheme)
  | Pentagon (r, c_scheme) -> draw_pentagon turtle r (palette c_scheme)
  | Tree (p', depth, l, angle, c_scheme) ->
      draw_tree turtle p' depth l angle (palette c_scheme)
  | Snowflake (sides, l, depth, c_scheme) ->
      draw_snowflake turtle sides sides l depth (palette c_scheme)

(** [pick_color ()] is a random color scheme generator. This generator sticks
    with the same color after the color being randomly chosen - only the
    gradient varies. *)
let pick_color () =
  Random.self_init ();
  match Random.int 14 with
  | 0 -> fun () -> Blank
  | 1 -> fun () -> Red (random_gradient ())
  | 2 -> fun () -> Orange (random_gradient ())
  | 3 -> fun () -> Yellow (random_gradient ())
  | 4 -> fun () -> LightGreen (random_gradient ())
  | 5 -> fun () -> Green (random_gradient ())
  | 6 -> fun () -> BlueGreen (random_gradient ())
  | 7 -> fun () -> Skyblue (random_gradient ())
  | 8 -> fun () -> LightBlue (random_gradient ())
  | 9 -> fun () -> Blue (random_gradient ())
  | 10 -> fun () -> Purple (random_gradient ())
  | 11 -> fun () -> Pink (random_gradient ())
  | 12 -> fun () -> RedPink (random_gradient ())
  | 13 -> fun () -> BlackWhite (random_gradient ())
  | _ -> failwith "pick_one error"

(** [pick_two ()] is two randomly chosen color schemes. *)
let pick_two () =
  let f = pick_color () in
  let g = pick_color () in
  fun () ->
    Random.self_init ();
    match Random.int 2 with
    | 0 -> f ()
    | _ -> g ()

(** [floor ()] draws the floor for random tree. *)
let floor () =
  Random.self_init ();
  match Random.int 4 with
  | 0 ->
      set_color (palette (Red 9));
      fill_rect 0 0 800 50
  | 1 ->
      set_color (palette (Orange 6));
      fill_rect 0 0 800 50
  | 2 ->
      set_color (palette (LightGreen 7));
      fill_rect 0 0 800 50
  | _ ->
      set_color (palette (Green 7));
      fill_rect 0 0 800 50

(** [p ()] is a random pattern. *)
let p =
  let f = pick_color () in
  Random.self_init ();
  match Random.int 5 with
  | 0 ->
      fun () ->
        let c = f () in
        make_circle 6 c
  | 1 ->
      fun () ->
        let c = f () in
        make_triangle 6 c
  | 2 ->
      fun () ->
        let c = f () in
        make_square 6 c
  | 3 ->
      fun () ->
        let c = f () in
        make_diamond 10 c
  | _ ->
      fun () ->
        let c = f () in
        make_pentagon 6 c

(** [decrease_leaf f] decreases leaf size. *)
let decrease_leaf f =
  match f () with
  | Circle (_, c) -> make_circle 4 c
  | Triangle (_, c) -> make_triangle 4 c
  | Square (_, c) -> make_square 4 c
  | Diamond (_, c) -> make_diamond 7 c
  | Pentagon (_, c) -> make_pentagon 4 c
  | _ -> failwith "leaf error"

(** [fallen_leaf_aux f seed acc] draws fallen leaves of a random tree. *)
let rec fallen_leaf_aux f seed acc =
  if acc = 0 then ()
  else
    let x, y, ang =
      Random.self_init ();
      (Random.int (800 - (90 * 2)) + 90, Random.int 49 + 1, Random.int 181)
    in
    seed.x <- x;
    seed.y <- y;
    seed.angle <- ang;
    f ();
    fallen_leaf_aux f seed (acc - 1)

(** [i()] is random float between 0.5 to 0.9 exlcusive. *)
let i () =
  Random.self_init ();
  Random.float 0.4 +. 0.5

(** [make_two_branch seed a depth length f] makes two branches. *)
let make_two_branch seed a depth length f =
  let b = Random.int 36 + 10 in
  right seed b;
  f seed p (depth - 1) (length *. i ()) b;
  let c = Random.int 36 + 10 in
  right seed c;
  f seed p (depth - 1) (length *. i ()) c;
  left seed (c + b - a);
  backward seed length

(** [make_one_branch seed a depth length f] makes one branche. *)
let make_one_branch seed a depth length f =
  let c = Random.int 36 + 10 in
  right seed (c * 2);
  f seed p (depth - 1) (length *. 0.8) c;
  left seed (c + c - a);
  backward seed length

(** [random_tree_aux seed p depth length angle] is a helper function for
    [make_random_tree]. *)
let rec random_tree_aux seed p depth length angle =
  if depth = 0 then
    let pat = p () in
    draw_pat seed pat
  else (
    (match depth with
    | num when num > 0 -> set_line_width num
    | _ -> set_line_width 1);
    forward seed length;
    (* Keeps branch going too far down or up. *)
    if current_y () < 150 || current_x () > 750 || current_x () < 50 then (
      random_tree_aux seed p 0 length 0;
      backward seed length)
    else (
      Random.self_init ();
      let n = Random.int 4 in
      let a = Random.int 36 + 10 in
      left seed a;
      random_tree_aux seed p (depth - 1) (length *. i ()) a;
      if n = 0 then make_two_branch seed a depth length random_tree_aux
      else make_one_branch seed a depth length random_tree_aux))

let make_random_tree () =
  Random.self_init ();
  let random_pick = Random.int 2 in
  let color = if random_pick = 0 then Orange 8 else Orange 1 in
  let depth = Random.int 10 + 2 in
  let angle = Random.int 36 + 10 in
  let seed = init_tree 400 50 90 (palette color) in
  if random_pick = 0 then set_color (palette (Skyblue 1))
  else set_color (palette (Blue 8));
  fill_rect 0 0 1000 1000;
  floor ();
  random_tree_aux seed p depth 150. angle;
  let random_seed = init_tree 0 0 0 black in
  let g () = draw_pat random_seed (p |> decrease_leaf) in
  fallen_leaf_aux g random_seed 80
