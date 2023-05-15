

(* ASCII Text Generation *)
(*

    ░█▀█░█▀▄░█▀▀░█▀▄░█▀▀░█▀▀░█▀▀░█░█░▀█▀░▀▀█░█░█░█░░░█▄█░█▀█░█▀█░█▀█░▄▀▄░█▀▄░█▀▀░▀█▀░█░█░█░█░█░█░█░█░█░█░▀▀█
    ░█▀█░█▀▄░█░░░█░█░█▀▀░█▀▀░█░█░█▀█░░█░░░░█░█▀▄░█░░░█░█░█░█░█░█░█▀▀░█\█░█▀▄░▀▀█░░█░░█░█░▀▄▀░█▄█░▄▀▄░░█░░▄▀░
    ░▀░▀░▀▀░░▀▀▀░▀▀░░▀▀▀░▀░░░▀▀▀░▀░▀░▀▀▀░▀▀░░▀░▀░▀▀▀░▀░▀░▀░▀░▀▀▀░▀░░░░▀\░▀░▀░▀▀▀░░▀░░▀▀▀░░▀░░▀░▀░▀░▀░░▀░░▀▀▀   

    Text style from:
    
    https://textkool.com/en/test-ascii-art-generator?text=Your%20text%20here%20
*)

(** stores the 2D-array representation of the alphabet *)
val alphabet : string array array

(* Return the string array representation of a char c*)
val get_letter : char -> string array

(* Converts a string s into a char list *)
val explode : string -> char list

val print_line : char list -> int -> 'a list 

(* Prints the fancy message to the console
   [USE THIS FUNCTION]*)
val fancy_message : string -> unit
