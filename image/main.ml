open Graphics
open Lib
open Tree

(** This executable provides an area to test out image generation. *)

let make_tree () =
  open_graph " 480x570";
  let turtle = init_tree 250 50 90 black in
  draw_tree turtle 10 100. 30;
  ignore (wait_next_event [ Button_down ]);
  close_graph ()

let () = make_tree ()
