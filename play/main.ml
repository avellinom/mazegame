open Controller
open ANSITerminal

(** The common prefix for reading from files. *)
let data_dir_prefix : string = "data" ^ Filename.dir_sep

(** Supported sizes for the world. *)
type size =
  | Small
  | Large

(** The console accepts inputs of this form. *)
type instruction =
  | Play of size
  | Quit

(** The dominant color of the console. *)
let console_color : ANSITerminal.style = red

(* The supplemental color of the console. *)
let console_subcolor : style = magenta

(* The color that the maze is displayed in. *)
let maze_displaycolor : style = blue

(** [begin_console ()] begins the console. It displays the initial instructions
    of the game to the user. *)
let begin_console () : unit =
  print_string [ console_color ] "\n -- Welcome to the Maze Game! -- \n";
  print_string [ console_subcolor ] "Here's how everything works: \n";
  print_string [ console_subcolor ]
    "  1. This is the Maze Game console. From here, you'll be able to start a \
     game or quit this console. \n";
  print_string [ console_subcolor ]
    "  2. When beginning a game, you are able to select a maze size {small, \
     large}.\n";
  print_string [ console_subcolor ]
    "  3. During the game, you may find images. These treasures are randomly \
     generated and they will be displayed to you on completion of the maze.\n";
  print_string [ console_color ] "\n -- Let's begin! -- \n"

(** [stop_console_msg ()] displays a message to the user saying that the console
    is stopping. *)
let stop_console_msg () : unit =
  print_string [ console_color ] "Stopping console...\n"

(** [bad_request_msg ()] displays a message to the user saying that they have
    inputted an invalid instruction. *)
let bad_request_msg () : unit =
  print_string [ console_subcolor ] "This input is not quite right.\n"

(** [process_raw_selection ()] takes in user input and outputs their
    instruction. It continually re-prompts the user for valid input if they make
    mistakes. *)
let rec process_raw_selection () : instruction =
  print_string [ console_subcolor ]
    "Please make a selection {play <size>, quit}:\n";
  print_string [ console_subcolor ] "> ";
  match read_line () with
  | exception End_of_file ->
      bad_request_msg ();
      process_raw_selection ()
  | s -> begin
      let split_s = String.split_on_char ' ' s in
      let s_lst_no_spaces = List.filter (fun e -> String.empty <> e) split_s in
      match s_lst_no_spaces with
      | "play" :: size -> begin
          match size with
          | [ value ] -> begin
              match value with
              | "small" -> Play Small
              | "large" -> Play Large
              | _ ->
                  print_string [ console_subcolor ]
                    "That maze size is not available. Available sizes are \
                     {small, large}. ";
                  process_raw_selection ()
            end
          | _ ->
              bad_request_msg ();
              process_raw_selection ()
        end
      | [ "quit" ] -> Quit
      | _ ->
          bad_request_msg ();
          process_raw_selection ()
    end

(** The user can move in 4 directions to traverse the maze. *)
type direction =
  | Left
  | Right
  | Up
  | Down

(** [process_raw_movement c] prompts the user for input and returns the
    direction in which they want to move. *)
let rec process_raw_movement (game_ctrl : Controller.t) : direction =
  Controller.print_game game_ctrl maze_displaycolor;
  print_string [ console_subcolor ]
    "Where would you like to go? Hit `a` for left, `d` for right, `w` for up, \
     and `s` for down.";
  print_string [ console_subcolor ] "> ";
  match read_line () with
  | exception End_of_file ->
      bad_request_msg ();
      process_raw_movement game_ctrl
  | direction -> begin
      let cleaned_direction = String.lowercase_ascii direction in
      match cleaned_direction with
      | "a" -> Left
      | "d" -> Right
      | "w" -> Up
      | "s" -> Down
      | _ ->
          print_string [ console_subcolor ]
            "That direction is not available. Available sizes are {left, \
             right, up, down}.";
          process_raw_movement game_ctrl
    end

let rec perform_movement (game_ctrl : Controller.t) (dir : direction) : unit =
  try
    let game_ctrl' =
      match dir with
      | Left -> Controller.move_left game_ctrl
      | Right -> Controller.move_right game_ctrl
      | Up -> Controller.move_up game_ctrl
      | Down -> Controller.move_down game_ctrl
    in
    let dir' = process_raw_movement game_ctrl' in
    perform_movement game_ctrl' dir'
  with e ->
    print_string [ console_subcolor ]
      "That move is not in bounds. Please try again.\n";
    let dir' = process_raw_movement game_ctrl in
    perform_movement game_ctrl dir'

(** [perform_instruction i] performs instruction i. It either starts the game of
    quits the console. *)
let perform_instruction (input : instruction) : unit =
  print_string [ console_color ] "Instruction received.\n";
  match input with
  | Play sz ->
      let maze_size =
        match sz with
        | Small -> "small.mz"
        | Large -> "large.mz"
      in
      let filepath = data_dir_prefix ^ maze_size in
      let game_ctrl = Controller.start_game filepath "todo: some user name" in
      print_string [ console_subcolor ]
        "Here is the maze. Note that you are 'p' and start in the top left \
         corner.";
      let first_move_dir = process_raw_movement game_ctrl in
      perform_movement game_ctrl first_move_dir
  | Quit -> stop_console_msg ()

(* Begin the Maze Game console. *)
let () =
  begin_console ();
  let user_instruction = process_raw_selection () in
  perform_instruction user_instruction
