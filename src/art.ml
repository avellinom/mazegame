open Graphics
open Turtle

(** [find_endpoint_odd turtle r_float] is the endpoint of an odd-sided polygon
    that is radius [r_float] from where the [turtle] is facing. *)
let find_endpoint turtle r =
  let rad = to_rad turtle.angle in
  let dx = r *. cos rad |> int_of_float in
  let dy = r *. sin rad |> int_of_float in
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

(** [draw_triangle turtle r] is a triangle that has an endpoint radius [r] away
    from where [turtle] is facing. *)
let draw_triangle turtle r =
  let arr = triangle_endpts turtle r in
  draw_poly arr

(** [draw_square turtle r] is a square. The square is determined by where the
    [turtle] is facing. Going radius [r] from where [turtle] is facing will be
    the middle of one side of the square. *)
let draw_square turtle r =
  let arr = sqruare_endpts turtle r in
  draw_poly arr

(** [draw_pentagon turtle r] is a pentagon that has an endpoint radius [r] away
    from where [turtle] is facing. *)
let draw_pentagon turtle r =
  let arr = pentagon_endpts turtle r in
  draw_poly arr
