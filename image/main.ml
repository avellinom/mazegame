open Graphics
open Lib
open Tree
open Snowflake
open Art

(** This executable provides an area to test out image generation. *)

let make_tree () =
  open_graph " 480x570";
  let turtle = init_tree 250 50 90 black in
  draw_tree turtle 10 100. 30;
  ignore (wait_next_event [ Button_down ]);
  close_graph ()

let make_snowflake () =
  open_graph " 1000x1000";
  let turtle = init_snowflake 500 500 90 black in
  (* snowflake_side turtle 100. 4; *)
  color_pentagon turtle 200 green;
  draw_snowflake turtle 6 6 60. 2;
  draw_triangle turtle 100;
  draw_square turtle 300;
  draw_pentagon turtle 200;
  ignore (wait_next_event [ Button_down ]);
  close_graph ()

let () = make_snowflake ()
