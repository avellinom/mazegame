open Lib
open Controller
open ANSITerminal

(** The common prefix for reading from files. *)
let data_dir_prefix : string = "data" ^ Filename.dir_sep

(** Supported types for the maze. *)
type maze_typ =
  | Tourist
  | Pyramid
  | Sphinx
  | Tomb

(** The console accepts inputs of this form. *)
type instruction =
  | Play of maze_typ
  | Quit

(** The dominant color of the console. *)
let console_color : ANSITerminal.style = red

(* The supplemental color of the console. *)
let console_subcolor : style = magenta

(* The color that the maze is displayed in. *)
let maze_displaycolor : style = magenta

(* The camel that is displayed at the start of the program. *)
let camel_art : string =
  {|
                                                        =--_
                                         .-""""""-.     |* _)
                                        /          \   /  /
                                       /            \_/  /
           _                          /|                /
       _-'"/\                        / |    ____    _.-"            _
    _-'   (  '-_            _       (   \  |\  /\  ||           .-'".".
_.-'       '.   `'-._   .-'"/'.      "   | |/ /  | |/        _-"   (   '-_
             '.      _-"   (   '-_       \ | /   \ |     _.-'       )     "-._
           _.'   _.-'       )     "-._    ||\\   |\\  '"'        .-'
         '               .-'          `'  || \\  ||))
jjs__  _  ___  _ ____________ _____  ___ _|\ _|\_|\\/ _______________  ___   _
                       c  c  " c C ""C  " ""  "" ""
                   c       C
              C        C
                   C
    C     c
  |}

(* The lone pyramid that is displayed at the beginning of the tourist level. *)
let lone_pyramid_art : string =
  {|
  /\
  ___                  /  \                  ___
 /   \     __         /    \         __     /
/     \   /  \   _   / <()> \   _   /  \   /
       \_/    \_/ \_/________\_/ \_/    \_/
 __________________/__I___I___\________________
                  /_I___I___I__\
                 /I___I___I___I_\
                /___I___I___I___I\
               /__I___I___I___I___\
              /_I___I___I___I___I__\
             /I___I___I___I___I___I_\
            /___I___I___I___I___I___I\
           /__I___I___I___I___I___I___\
          /_I___I___I___I___I___I___I__\
  |}

(* The sphinx that is displayed at the beginning of the sphinx level. *)
let sphinx_art : string =
  {|
                _-_
             /'. .'\   /\.
        /\. /(|`/ !)\ /:_\/.
       /:_\|  \ = /  |__:_\/ .
            `  ---  .__:___\/./
           /---------\___:__\/
          /-----------\
     ____/----   -/    \__
    / / / / |     | \ \ \ \
  |}

(* The tomb that is displayed at the beginning of the tomb level. *)
let pharaoh_art : string =
  {|
          _....._
         .\:\:/:/.
        /=\_.@._/=\
       /==/ _ _ \==\
      /==(   -   )==\
      |===\  =  /===|
      \====|-V-|====/
       \===|   |===/
        |==|   |==|
        '=.|   |.='
  |}

(** [begin_console ()] begins the console. It displays the initial instructions
    of the game to the user. *)
let begin_console () : unit =
  print_string [ console_color ] "\n\t\t\t -- Welcome to Egypt! -- \n";
  print_string [ console_color ] camel_art;
  print_string [ console_subcolor ] "\nHere's how everything works: \n";
  print_string [ console_subcolor ]
    "  1. This is the Maze Game console. From here, you'll be able to select a \
     maze or quit this console. \n";
  print_string [ console_subcolor ]
    "  2. When beginning a game, you are able to select a maze type {tourist, \
     pyramid, sphinx, tomb}. They are ordered at increasing levels of \
     difficulty.\n\
    \ It is recommended that you play them in order of increasing difficulty.\n";
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
    "Please make a selection {play <type>, quit}:\n";
  print_string [ console_subcolor ] "> ";
  match read_line () with
  | exception End_of_file ->
      bad_request_msg ();
      process_raw_selection ()
  | s -> begin
      let split_s = String.split_on_char ' ' s in
      let s_lst_no_spaces = List.filter (fun e -> String.empty <> e) split_s in
      match s_lst_no_spaces with
      | "play" :: typ -> begin
          match typ with
          | [ typ ] -> begin
              match typ with
              | "tourist" -> Play Tourist
              | "pyramid" -> Play Pyramid
              | "sphinx" -> Play Sphinx
              | "tomb" -> Play Tomb
              | _ ->
                  print_string [ console_subcolor ]
                    "That maze type is not available. Available types are \
                     {tourist, pyramid, sphinx, tomb}. ";
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
     and `s` for down.\n";
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
  with
  | InvalidMove ->
      print_string [ console_subcolor ]
        "That move is not in bounds. Please try again.\n";
      let dir' = process_raw_movement game_ctrl in
      perform_movement game_ctrl dir'
  | MazeSolved ->
      print_string [ console_color ] "Congratulations! You won the Maze Game.\n";
      let images_found = Controller.get_all_collected_images game_ctrl in
      List.iteri
        (fun index element ->
          print_endline ("some image " ^ string_of_int index))
        images_found;
      (* TODO: update this to reveal images*)
      print_string [ console_color ] "Welcome back to the console.\n";
      print_string [ console_subcolor ]
        "\n\
         Remember: The available selections are {tourist, pyramid, sphinx, \
         tomb}.\n";
      let user_instruction = process_raw_selection () in
      perform_instruction user_instruction

(** [perform_instruction i] performs instruction i. It either starts the game of
    quits the console. *)
and perform_instruction (input : instruction) : unit =
  print_string [ console_color ] "Instruction received.\n";
  match input with
  | Play typ ->
      let maze_typ, image_count =
        match typ with
        | Tourist ->
            print_string [ console_color ] "\n\t\t   -- TOURIST -- \n";
            print_string [ console_color ] lone_pyramid_art;
            print_string [ console_color ]
              "\n\
               A tourist I see... Let's see if you can dance around the \
               pyramid Giza.\n\n";
            ("tourist.mz", 0)
        | Pyramid -> ("pyramid.mz", 0)
        | Sphinx ->
            print_string [ console_color ] "\n\t -- SPHINX -- \n";
            print_string [ console_color ] sphinx_art;
            print_string [ console_color ]
              "\nYou're ambitious. Let's see you against the sphinx.\n";
            print_string [ console_color ]
              "She may drop you some images (i). Collect them and her ancient \
               artifacts will be revealed to you.\n\n";
            ("sphinx.mz", 5)
        | Tomb ->
            print_string [ console_color ] "\n\t -- TOMB -- \n";
            print_string [ console_color ] pharaoh_art;
            print_string [ console_color ] "\nAt last. The pharaoh's tomb.\n";
            print_string [ console_color ]
              "Deep inside, you find a strange string of text.\n";
            print_string [ console_color ]
              "Can you find the key (k) to decrypt this text?\n\n";
            ("tomb.mz", 5)
      in
      let filepath = data_dir_prefix ^ maze_typ in
      let game_ctrl =
        Controller.start_game filepath "todo: some user name"
          image_count (* TODO: CHANGE THESE INPUTS *)
      in
      print_string [ console_subcolor ]
        "Here is the maze. Note that you are 'p' and start in the top left \
         corner.\n\
         Your goal is to make it to G.";
      let first_move_dir = process_raw_movement game_ctrl in
      perform_movement game_ctrl first_move_dir
  | Quit -> stop_console_msg ()

(* Begin the Maze Game console. *)
let () =
  begin_console ();
  let user_instruction = process_raw_selection () in
  perform_instruction user_instruction
