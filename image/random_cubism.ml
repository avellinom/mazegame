open Graphics

let random_color () =
  let r = Random.int 256 in
  let g = Random.int 256 in
  let b = Random.int 256 in
  rgb r g b

let random_dot () =
  let x = Random.int 640 in
  let y = Random.int 480 in
  let size = Random.int 5 + 1 in
  let color = random_color () in
  set_color color;
  fill_circle x y size

let random_circle () =
  let x = Random.int 640 in
  let y = Random.int 480 in
  let radius = Random.int 50 + 1 in
  let color = random_color () in
  set_color color;
  fill_circle x y radius

let random_rect () =
  let x = Random.int 640 in
  let y = Random.int 480 in
  let width = Random.int 50 + 1 in
  let height = Random.int 50 + 1 in
  let color = random_color () in
  set_color color;
  fill_rect x y width height

let random_cubism () =
  open_graph " 640x480";
  set_color white;
  fill_rect 0 0 640 480;
  for _ = 1 to 1500 do
    let r = Random.int 4 in
    match r with
    | 0 -> random_dot ()
    | 1 -> random_circle ()
    | 2 -> random_rect ()
    | 3 -> random_dot ()
    | _ -> ()
  done;
  ignore (wait_next_event [ Button_down ]);
  close_graph ()

let () = random_cubism ()
