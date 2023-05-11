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
  | s -> (
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
                    "This maze size is not available. Available sizes are \
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
          process_raw_selection ())

let rec process_movement (game_ctrl : Controller.t) : unit = failwith "todo"

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
      process_movement game_ctrl
  | Quit -> stop_console_msg ()

(* Begin the Maze Game console. *)
let () =
  begin_console ();
  let user_instruction = process_raw_selection () in
  perform_instruction user_instruction
