type maze_array = Maze.entry array array

type key_status =
  | NotPlaced
  | NotFound of Crypt.affine_key
  | Found of Crypt.affine_key

type t = {
  mz_array : maze_array;
  user_location : Maze.location;
  user : User.t;
  key_maybe : key_status;
}

exception InvalidMove
exception MazeSolved

(** [location_is_free arr loc] determines whether a location is [Maze.Free] in
    [arr].*)
let location_is_free (mz_array : maze_array) (loc : Maze.location) : bool =
  match loc with
  | x, y
    when x >= 0 && y >= 0
         && x < Maze.get_num_rows mz_array
         && y < Maze.get_num_cols mz_array -> begin
      match mz_array.(x).(y) with
      | Free -> true
      | Goal -> true
      | Picture _ -> true
      | Key _ -> true
      | _ -> false
    end
  | _ -> false

let start_game (filename : string) (num_images : int) (drop_key : bool) : t =
  let mz = Maze.make filename in
  let mz = Maze.generate_images mz num_images in
  let mz_array = Maze.array_of_maze mz in
  let usr = User.make () in
  match location_is_free mz_array (0, 0) with
  | true ->
      if drop_key then begin
        let mz_with_key, gen_key = Maze.generate_key mz in
        let mz_array_with_key = Maze.array_of_maze mz in
        mz_array_with_key.(0).(0) <- Person usr;
        {
          mz_array = mz_array_with_key;
          user_location = (0, 0);
          user = usr;
          key_maybe = NotFound gen_key;
        }
      end
      else begin
        mz_array.(0).(0) <- Person usr;
        { mz_array; user_location = (0, 0); user = usr; key_maybe = NotPlaced }
      end
  | false -> failwith "Error creating maze."

let get_key_status (c : t) : key_status = c.key_maybe

(** [move] represents the directions in which a user can make a move by one maze
    location. *)
type move =
  | Left
  | Right
  | Up
  | Down

(** [move_user c move] returns a new controller, which is the same as c but with
    the user updated to a new location in direction [move]. Raises: InvalidMove
    if the move is not possible (e.g., moving into a wall or off the grid). *)
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
          let new_key_status =
            match mz_array.(x').(y') with
            | Key aff_key -> Found aff_key
            | _ -> c.key_maybe
          in
          begin
            match mz_array.(x').(y') with
            | Picture display_picture -> display_picture ()
            | _ -> ()
          end;
          mz_array.(x').(y') <- Person user;
          { c with user_location = (x', y'); key_maybe = new_key_status }
      | false -> raise InvalidMove)

let check_solved (c : t) : t =
  match c with
  | { mz_array; user_location; user } ->
      let bottom_right_index =
        (Maze.get_num_rows mz_array - 1, Maze.get_num_cols mz_array - 1)
      in
      if bottom_right_index = user_location then raise MazeSolved else c

let move_left (c : t) : t = check_solved (move_user c Left)
let move_right (c : t) : t = check_solved (move_user c Right)
let move_up (c : t) : t = check_solved (move_user c Up)
let move_down (c : t) : t = check_solved (move_user c Down)

let string_of_game (c : t) : string =
  match c with
  | { mz_array; user_location; user } ->
      let buffer = Buffer.create 16 in
      Array.iter
        (fun row ->
          Array.iter
            (fun entry ->
              let char_entry = Maze.char_of_entry entry in
              Buffer.add_char buffer char_entry)
            row;
          Buffer.add_char buffer '\n')
        mz_array;
      Buffer.contents buffer

let print_game (c : t) (color_style : ANSITerminal.style) : unit =
  match c with
  | { mz_array; user_location; user } ->
      let board_string = string_of_game c in
      ANSITerminal.print_string [ color_style ] ("\n" ^ board_string)
