open Graphics;;

open_graph " 400x400"

let random_ocean x y =
  let r = Random.int 100 in
  let b = 150 + Random.int 105 in
  let g = 100 + Random.int 155 in
  let size = Random.int 6 in
  set_color (rgb r g b);
  fill_circle x y size

let random_sun x y =
  print_endline (string_of_int (y - (size_y () / 3)));
  let r = Random.full_int (555 - (y - (size_y () / 3)) - 50) + 50 in
  let g = Random.full_int (300 - (y - (size_y () / 3))) in
  let size = Random.int 6 in
  set_color (rgb r g 0);
  fill_circle x y size
;;

Random.self_init ();

for y = (size_y () - 1) / (3 * 4) downto 0 do
  for x = (size_x () - 1) / 2 downto 0 do
    random_ocean (4 * x) (4 * y)
  done
done
;;

for y = (size_y () - 1) / 12 to (size_y () - 1) / 4 do
  for x = (size_x () - 1) / 2 downto 0 do
    random_sun (4 * x) (4 * y)
  done
done
;;

ignore (wait_next_event [ Button_down ]);
close_graph ()

(* Ideas: 1. Anchor points - choose a few anchor points where colors are
   dark/light, and blend them - +- around a random color i.e if randomly chosen
   was red do around red, this makes more color scheme possible *)
