type entry =
  | Free
  | Wall
  | Goal
  | Person of User.t

type location = int * int
type t = entry array array

let char_of_entry (e : entry) : char =
  match e with
  | Free -> ' '
  | Wall -> '#'
  | Goal -> 'G'
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
            | _ -> failwith "Maze entry is not a space of a hash."
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

let hashtable_of_maze (m : t) : (location, entry) Hashtbl.t =
  let hashtable = Hashtbl.create (get_num_rows m * get_num_cols m) in
  Array.iteri
    (fun row_i row ->
      Array.iteri
        (fun col_i entry -> Hashtbl.add hashtable (row_i, col_i) entry)
        row)
    m;
  hashtable

let array_of_maze (m : t) : entry array array =
  let num_rows = get_num_rows m in
  let num_cols = get_num_cols m in
  let new_array = Array.make_matrix num_rows num_cols Free in
  for row_i = 0 to num_rows - 1 do
    new_array.(row_i) <- Array.copy m.(row_i)
  done;
  new_array
