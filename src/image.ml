open Graphics
open Art

type t = unit -> unit

let make_random () : t =
 fun () ->
  open_graph " 800x800";
  make_random_tree ();
  ignore (wait_next_event [ Button_down ]);
  close_graph ()
