open Graphics
open Art

type t = unit

let make_random () : t =
  open_graph " 1000x1000";
  set_color (palette (Skyblue 1));
  fill_rect 0 0 1000 1000;
  Random.self_init ();
  make_random_tree ();
  ignore (wait_next_event [ Button_down ]);
  close_graph ()
