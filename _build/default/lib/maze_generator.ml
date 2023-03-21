

module type BOARD = sig

  (* The matrix representing the Board *)
  val matrix : string list list ref

  (* Prints the matrix to the console *)
  val print : unit 

  (* Returns a string representation of the matrix *)
  val matrix_to_string : string 

  (* Sets the matrix to the matrix given in the argument *)
  val set_matrix : string list list -> unit
end



module Board : BOARD = struct
  let matrix = ref [[">>> EMPTY MATRIX <<<"]]

  
  let set_matrix (mtx : string list list) =
    matrix := mtx (* TODO: FIGURE OUT WHY THIS DOESN'T WORK! *)

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
    ("\n" ^ (printAll !matrix) ^ "\n")

  let print = 
    print_string matrix_to_string

end
