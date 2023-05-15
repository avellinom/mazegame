open Graphics
open Art

type t = unit -> unit

let make_random () : t =
 fun () ->
  open_graph " 600x700";
  set_color (palette (Skyblue 1));
  fill_rect 0 0 1000 1000;
  Random.self_init ();
  make_random_tree ();
  ignore (wait_next_event [ Button_down ]);
  close_graph ()
