open Graphics
open Lib
open Tree

(** This executable provides an area to test out image generation. *)

let main () =
  open_graph " 480x570";
  let turtle = init_tree 250 250 90 black in
  draw_tree turtle 7 30. 100;
  ignore (wait_next_event [ Button_down ]);
  close_graph ()

let () = main ()
