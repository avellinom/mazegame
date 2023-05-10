type maze_array = Maze.entry array array

type t = {
  mz_array : maze_array;
  user_location : Maze.location;
  user : User.t;
}

(** A helper function that determines whether a location is 'Free' in the
    hashmap. *)
let location_is_free (mz_array : maze_array) (loc : Maze.location) : bool =
  match loc with
  | x, y
    when x >= 0 && y >= 0
         && x < Maze.get_num_rows mz_array
         && y < Maze.get_num_cols mz_array -> begin
      match mz_array.(x).(y) with
      | Free -> true
      | _ -> false
    end
  | _ -> false

let start_game (filename : string) (username : string) : t =
  let mz = Maze.maze_of_file filename in
  let mz_array = Maze.array_of_maze mz in
  let usr = User.make_user username in
  match location_is_free mz_array (0, 0) with
  | true ->
      mz_array.(0).(0) <- Person usr;
      { mz_array; user_location = (0, 0); user = usr }
  | false -> failwith "Error creating maze. "

type move =
  | Left
  | Right
  | Up
  | Down

let move_user (c : t) (move : move) : t =
  let x_diff, y_diff =
    match move with
    | Left -> (0, -1)
    | Right -> (0, 1)
    | Up -> (-1, 0)
    | Down -> (1, 0)
  in
  match c with
  | { mz_array; user_location; user } -> (
      let x, y = user_location in
      let x', y' =
        match (x, y) with
        | x, y -> (x + x_diff, y + y_diff)
      in
      match location_is_free mz_array (x', y') with
      | true ->
          mz_array.(x).(y) <- Free;
          mz_array.(x').(y') <- Person user;
          { c with user_location = (x', y') }
      | false -> c)

let move_left (c : t) : t = move_user c Left
let move_right (c : t) : t = move_user c Right
let move_up (c : t) : t = move_user c Up
let move_down (c : t) : t = move_user c Down

(** A helper function to print the current state of the maze. *)
let print_maze (maze : Maze.entry array array) : unit =
  let print_maze_row row =
    (* helper function to print each row of the matrix *)
    Array.iter
      (fun entry ->
        let char_entry = Maze.char_of_entry entry in
        print_char char_entry)
      row;
    print_endline ""
  in
  print_endline "";
  Array.iter (fun array -> print_maze_row array) maze

let print_game (c : t) : unit =
  match c with
  | { mz_array; user_location; user } -> print_maze mz_array
