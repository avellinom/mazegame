type entry =
  | Free
  | Wall
  | Goal
  | Picture of Image.t
  | Key of Crypt.affine_key
  | Person of User.t

type location = int * int
type t = entry array array

let char_of_entry (e : entry) : char =
  match e with
  | Free -> ' '
  | Wall -> '#'
  | Goal -> 'G'
  | Picture _ -> 'i'
  | Key _ -> 'k'
  | Person _ -> 'p'

(** [to_list f] converts a filename f into a list of strings where each element
    of the list is a row of the file. *)
let to_list (filename : string) : string list =
  try
    let file = open_in filename in
    let rec fill_list acc =
      try
        let line = input_line file in
        fill_list (line :: acc)
      with End_of_file -> acc
    in
    let reversed_order = fill_list [] in
    List.rev reversed_order
  with Sys_error _ -> failwith "File does not exist."

let make (filename : string) : t =
  let string_list = to_list filename in
  let num_rows = List.length string_list in
  let num_cols = string_list |> List.hd |> String.length in
  let matrix = Array.make_matrix num_rows num_cols Free in
  let rec row_loop s_list row_count =
    match s_list with
    | hd :: tl ->
        (* fill the nested array inside the matrix *)
        for col_count = 0 to num_cols - 1 do
          let new_entry = hd.[col_count] in
          let variant_entry =
            match new_entry with
            | ' ' -> Free
            | '#' -> Wall
            | 'G' -> Goal
            | _ -> failwith "Maze entry is not a space, hash, or G."
          in
          matrix.(row_count).(col_count) <- variant_entry
        done;
        row_loop tl (row_count + 1)
    | _ -> ()
  in
  row_loop string_list 0;
  matrix

let get_num_rows (m : t) : int = Array.length m
let get_num_cols (m : t) : int = Array.length m.(0)

let array_of_maze (m : t) : entry array array =
  let num_rows = get_num_rows m in
  let num_cols = get_num_cols m in
  let new_array = Array.make_matrix num_rows num_cols Free in
  for row_i = 0 to num_rows - 1 do
    new_array.(row_i) <- Array.copy m.(row_i)
  done;
  new_array

module RandomSet = Set.Make (struct
  type t = int * int

  (* This enforces the random order of the set. Note: [Random.self_init ()]
     makes the placement of the images in the maze different for each run of the
     program. *)
  let compare x y =
    Random.self_init ();
    Random.int 3 - 1
end)

let generate_images (m : t) (desired_image_count : int) : t =
  let free_locations = ref RandomSet.empty in
  Array.iteri
    (fun row_i row ->
      Array.iteri
        (fun col_i entry ->
          match entry with
          | Free when row_i != 0 && col_i != 0 ->
              free_locations := RandomSet.add (row_i, col_i) !free_locations
          | _ -> ())
        row)
    m;
  let desired_image_amount =
    min desired_image_count (RandomSet.cardinal !free_locations)
  in
  let images_added = ref 0 in
  RandomSet.iter
    (fun random_location ->
      if !images_added < desired_image_amount then begin
        let x, y = random_location in
        m.(x).(y) <- Picture (Image.make_random ());
        incr images_added
      end
      else ())
    !free_locations;
  m
