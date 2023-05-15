open Graphics
open Lib
open Art
open Turtle

(** This executable provides an area to test out image generation. *)

let show_tree () =
  open_graph " 1000x1000";
  set_color blue;
  fill_rect 0 0 1000 1000;
  let turtle = init_tree 500 50 90 white in
  let p () = make_diamond 10 (Yellow (Random.int 7 + 1)) in
  let t = make_tree p 10 100. 30 (BlackWhite 1) in
  draw_pat turtle t;
  let flake = init_snowflake 500 500 90 white in
  let snow = make_snowflake 3 50. 3 (BlackWhite 1) in
  draw_pat flake snow;
  ignore (wait_next_event [ Button_down ]);
  close_graph ()

let () = show_tree ()
