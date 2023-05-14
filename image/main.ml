open Graphics
open Lib
open Art
open Turtle

(** This executable provides an area to test out image generation. *)

let make_tree () =
  open_graph " 480x570";
  let turtle = init_tree 250 50 90 black in
  draw_tree turtle 10 100. 30;
  ignore (wait_next_event [ Button_down ]);
  close_graph ()

let make_snowflake () =
  open_graph " 1000x1000";
  let turtle = init_snowflake 500 500 90 white in
  (* snowflake_side turtle 100. 4; *)
  color_pentagon turtle 200 red;
  draw_snowflake turtle 6 6 60. 2;
  change_color turtle blue;
  draw_triangle turtle 100;
  change_color turtle black;
  draw_square turtle 300;
  draw_pentagon turtle 200;
  let seed = init_tree 250 50 90 black in
  draw_tree seed 10 100. 30;
  change_color turtle white;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  forward turtle 150.;
  draw_triangle turtle 30;
  backward turtle 150.;
  left turtle 10;
  ignore (wait_next_event [ Button_down ]);
  close_graph ()

let () = make_snowflake ()
