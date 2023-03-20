

module type BOARD = sig

  (* The matrix representing the Board *)
  val matrix : string list list 

  (* Prints the matrix to the console *)
  val print : unit 

  (* Returns a string representation of the matrix*)
  val matrix_to_string : string 
end



module Board : BOARD = struct
  let matrix = [ [ "a"; "b"; "c" ]; [ "d"; "e"; "f" ]; [ "g"; "h"; "i" ] ]

  let rec line_to_string lst =
    match lst with
    | [] -> ""
    | h :: t ->
        h ^ " " ^ (line_to_string t)
    
  let rec printAll mtx = 
    match mtx with 
    | [] -> ""
    | h :: t ->
      ("\n  ") ^ (line_to_string h) ^ (printAll t)

  let matrix_to_string =
    ("\n" ^ (printAll matrix) ^ "\n")

  let print = 
    print_string matrix_to_string

end
