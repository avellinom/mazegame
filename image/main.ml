(** This executable provides an area to test out image generation. *)

open Graphics
open Lib
open Art
open Turtle

let show_tree () =
  let turtle = init_tree 500 50 90 white in
  let p () = make_diamond 10 (Yellow (Random.int 7 + 1)) in
  let t = make_tree p 10 100. 30 (BlackWhite 1) in
  draw_pat turtle t;
  let flake = init_snowflake 500 500 90 white in
  let snow = make_snowflake 3 50. 3 (BlackWhite 1) in
  draw_pat flake snow

let test_stuff () =
  let turtle = init_tree 500 50 90 white in
  let p =
    let f = pick_color () in
    match Random.int 5 with
    | 0 ->
        fun () ->
          let c = f () in
          make_circle 7 c
    | 1 ->
        fun () ->
          let c = f () in
          make_triangle 7 c
    | 2 ->
        fun () ->
          let c = f () in
          make_square 7 c
    | 3 ->
        fun () ->
          let c = f () in
          make_diamond 10 c
    | _ ->
        fun () ->
          let c = f () in
          make_pentagon 7 c
  in
  draw_pat turtle (p ())

(* let () = open_graph " 1000x1000"; set_color (palette (RedPink 9)); fill_rect
   0 0 100 1000; test_stuff (); test_stuff (); test_stuff (); make_random_tree
   (); ignore (wait_next_event [ Button_down ]); close_graph () *)

let () =
  open_graph " 600x700";
  set_color (palette (Skyblue 1));
  fill_rect 0 0 1000 1000;
  Random.self_init ();
  make_random_tree ();
  ignore (wait_next_event [ Button_down ]);
  close_graph ()
