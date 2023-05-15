open Lib
open Controller
open ANSITerminal

(** The common prefix for reading from files. *)
let data_dir_prefix : string = "data" ^ Filename.dir_sep

let data_ascii_art_prefix : string =
  "data" ^ Filename.dir_sep ^ "ascii-art" ^ Filename.dir_sep

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
let maze_displaycolor : style = cyan

(* [string_of_file f] converts the contents of filename f into a string.
   Citation:
   https://stackoverflow.com/questions/53839695/how-do-i-read-the-entire-content-of-a-given-file-into-a-string*)
let string_of_file (filename : string) : string =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* The camel that is displayed at the start of the program. *)
let camel_art : string =
  string_of_file (data_ascii_art_prefix ^ "intro-camel.txt")

(* The lone pyramid that is displayed at the beginning of the tourist level. *)
let lone_pyramid_art : string =
  string_of_file (data_ascii_art_prefix ^ "tourist-pyramid.txt")

(* The pyramid with cats that is displayed at the beginning of the pyramid
   level. *)
let pyramid_cats_art : string =
  string_of_file (data_ascii_art_prefix ^ "pyramid-cats.txt")

(* The sphinx that is displayed at the beginning of the sphinx level. *)
let sphinx_art : string =
  string_of_file (data_ascii_art_prefix ^ "sphinx-cat.txt")

(* The tomb that is displayed at the beginning of the tomb level. *)
let pharaoh_art : string =
  string_of_file (data_ascii_art_prefix ^ "tomb-pharaoh.txt")

(* The top secret message that is encrypted and decrypted with the key in the
   maze. *)
let secret_message : string = "tutankhamun was a camel"
let encrypted_message : string ref = ref ""

(** [begin_console ()] begins the console. It displays the initial instructions
    of the game to the user. *)
let begin_console () : unit =
  Fancy.fancy_message "xxxxxxxxxxxxxxxxxxxxxxxx";
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
            "That direction is not available. Available moves are {left, \
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
      print_string [ console_color ]
        "Congratulations! You traversed the maze.\n";
      begin
        match Controller.get_key_status game_ctrl with
        | NotFound _ ->
            print_string [ console_color ]
              "You did not find the key. Better luck next time.\n"
        | NotPlaced -> ()
        | Found key ->
            print_string [ console_color ] "Wow! You found the key.\n";
            print_string [ console_color ] "Let's see the message: \n";
            let decrypted_secret_message =
              Crypt.affine_decrypt !encrypted_message key
            in
            print_string [ yellow ]
              ("\n\t" ^ decrypted_secret_message ^ "\t\n\n")
      end;

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
        | Pyramid ->
            print_string [ console_color ] "\n\t\t\t -- PYRAMID -- \n";
            print_string [ console_color ] pyramid_cats_art;
            print_string [ console_color ]
              "\nCan you navigate around Giza, Khafre, and Menkaure?\n\n";
            ("pyramid.mz", 0)
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
        if typ = Tomb then begin
          let game_ctrl_with_key =
            Controller.start_game filepath image_count true
          in
          match Controller.get_key_status game_ctrl_with_key with
          | NotPlaced -> failwith "Key must have been placed"
          | Found _ -> failwith "Key can not initially be found"
          | NotFound affine_key ->
              encrypted_message :=
                Crypt.affine_encrypt secret_message affine_key;
              print_string [ yellow ] ("\t" ^ !encrypted_message ^ "\t\n\n");
              game_ctrl_with_key
        end
        else Controller.start_game filepath image_count false
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
