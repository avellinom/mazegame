type t = char array array

(** [to_list fname] returns a list of strings of each line in the file fname in
    their original order. *)
let to_list (filename : string) : string list =
  let file = open_in filename in
  let rec fill_list acc =
    try
      let line = input_line file in
      fill_list (line :: acc)
    with End_of_file -> acc
  in
  let reversed_order = fill_list [] in
  List.rev reversed_order

let of_file (filename : string) : t =
  let string_list = to_list filename in
  let num_rows = List.length string_list in
  let num_cols = string_list |> List.hd |> String.length in
  let matrix = Array.make_matrix num_rows num_cols 'x' in
  let rec row_loop s_list row_count =
    match s_list with
    | hd :: tl ->
        (* fill the nested array inside the matrix *)
        for col_count = 0 to num_cols - 1 do
          matrix.(row_count).(col_count) <- hd.[col_count]
        done;
        row_loop tl (row_count + 1)
    | _ -> ()
  in
  row_loop string_list 0;
  matrix

let print_maze (maze : t) : unit =
  let print_maze_row row =
    (* helper function to print each row of the matrix *)
    Array.iter (fun ele -> print_char ele) row;
    print_endline ""
  in
  print_endline "";
  Array.iter (fun array -> print_maze_row array) maze
