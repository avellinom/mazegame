(* ░█▀█░█▀▄░█▀▀░█▀▄░█▀▀░█▀▀░█▀▀░█░█░▀█▀░▀▀█░█░█░█░░░█▄█░█▀█░█▀█░█▀█░▄▀▄░█▀▄░█▀▀░▀█▀░█░█░█░█░█░█░█░█░█░█░▀▀█
   ░█▀█░█▀▄░█░░░█░█░█▀▀░█▀▀░█░█░█▀█░░█░░░░█░█▀▄░█░░░█░█░█░█░█░█░█▀▀░█\█░█▀▄░▀▀█░░█░░█░█░▀▄▀░█▄█░▄▀▄░░█░░▄▀░
   ░▀░▀░▀▀░░▀▀▀░▀▀░░▀▀▀░▀░░░▀▀▀░▀░▀░▀▀▀░▀▀░░▀░▀░▀▀▀░▀░▀░▀░▀░▀▀▀░▀░░░░▀\░▀░▀░▀▀▀░░▀░░▀▀▀░░▀░░▀░▀░▀░▀░░▀░░▀▀▀

   Text style from:
   https://textkool.com/en/test-ascii-art-generator?text=Your%20text%20here%20 *)

(** [alphabet] is a 2D-array representation of the alphabet. *)
let alphabet =
  [|
    [| "░█▀█"; "░█▀█"; "░▀░▀" |];
    [| "░█▀▄"; "░█▀▄"; "░▀▀░" |];
    [| "░█▀▀"; "░█░░"; "░▀▀▀" |];
    [| "░█▀▄"; "░█░█"; "░▀▀░" |];
    [| "░█▀▀"; "░█▀▀"; "░▀▀▀" |];
    [| "░█▀▀"; "░█▀▀"; "░▀░░" |];
    [| "░█▀▀"; "░█░█"; "░▀▀▀" |];
    [| "░█░█"; "░█▀█"; "░▀░▀" |];
    [| "░▀█▀"; "░░█░"; "░▀▀▀" |];
    [| "░▀▀█"; "░░░█"; "░▀▀░" |];
    [| "░█░█"; "░█▀▄"; "░▀░▀" |];
    [| "░█░░"; "░█░░"; "░▀▀▀" |];
    [| "░█▄█"; "░█░█"; "░▀░▀" |];
    [| "░█▀█"; "░█░█"; "░▀░▀" |];
    [| "░█▀█"; "░█░█"; "░▀▀▀" |];
    [| "░█▀█"; "░█▀▀"; "░▀░░" |];
    [| "░█░█"; "░█▄█"; "░▀░▀" |];
    [| "░█▀▄"; "░█▀▄"; "░▀░▀" |];
    [| "░█▀▀"; "░▀▀█"; "░▀▀▀" |];
    [| "░▀█▀"; "░░█░"; "░░▀░" |];
    [| "░█░█"; "░█░█"; "░▀▀▀" |];
    [| "░█░█"; "░▀▄▀"; "░░▀░" |];
    [| "░█░█"; "░█▄█"; "░▀░▀" |];
    [| "░█░█"; "░▄▀▄"; "░▀░▀" |];
    [| "░█░█"; "░░█░"; "░░▀░" |];
    [| "░▀▀█"; "░▄▀░"; "░▀▀▀" |];
    [| "░░"; "░░"; "░░" |];
  |]

(** [get_letter c] returns the string array representation of character c. *)
let get_letter (c : char) : string array =
  let index =
    if int_of_char c == 32 then 26 (* space character *)
    else int_of_char c - int_of_char 'a'
  in
  (* normal characters *)
  alphabet.(index)

(** [explode s] converts string s into a list of characters in it. *)
let explode s = List.init (String.length s) (String.get s)

(** [print_line cl i] is a helper method to support printing in [fancy_message]. *)
let rec print_line char_lst l =
  match char_lst with
  | [] ->
      print_string "\n";
      []
  | h :: t ->
      print_string (get_letter h).(l);
      print_line t l

let fancy_message str =
  let _ = print_string "\n" in
  for i = 0 to 2 do
    let _ = print_line (explode str) i in
    ()
  done
