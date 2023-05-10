type entry =
  | Free
  | Wall
(* TODO: add image later *)

type t = entry array array

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

let maze_of_file (filename : string) : t =
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
            | _ -> failwith "Todo write err for bad entry"
          in
          matrix.(row_count).(col_count) <- variant_entry
        done;
        row_loop tl (row_count + 1)
    | _ -> ()
  in
  row_loop string_list 0;
  matrix

let print_maze (maze : t) : unit =
  let print_maze_row row =
    (* helper function to print each row of the matrix *)
    Array.iter
      (fun entry ->
        match entry with
        | Free -> print_char ' '
        | Wall -> print_char 'W')
      row;
    print_endline ""
  in
  print_endline "";
  Array.iter (fun array -> print_maze_row array) maze
