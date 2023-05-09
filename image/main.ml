open Graphics
(** This executable provides an area to test out image generation. *)

let blink_dot () =
  let c = rgb 255 255 0 in
  set_color c;
  plot 4 4;
  fill_rect 100 100 3 4;
  fill_circle 200 200 50

let () =
  open_graph " 480x270";
  blink_dot ();
  ignore (wait_next_event [ Button_down ]);
  close_graph ()
