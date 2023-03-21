module type BOARD = sig
  (* The matrix representing the Board *)
  val matrix : string array array ref

  (* Prints the matrix to the console *)
  val print : unit -> unit

  (* Returns a string representation of the matrix *)
  val matrix_to_string : unit -> string

  (* Sets the matrix to the matrix given in the argument *)
  val set_matrix : string array array -> unit

  (* Generates a valid, solvable maze "#" represent Walls "." represent Paths
     "S" represents the Start "E" represents the End *)
  val generate : unit -> unit
end

module Board : BOARD = struct
  let matrix = ref [| [| ">>> EMPTY MATRIX <<<" |] |]
  let set_matrix (mtx : string array array) = matrix := mtx
  let line_to_string arr = Array.fold_left (fun x y -> x ^ y ^ " ") "" arr

  let printAll (mtx : string array array) =
    Array.fold_left (fun x y -> x ^ line_to_string y ^ "\n  ") "" mtx

  let matrix_to_string () = "\n\n  " ^ printAll !matrix ^ "\n"
  let print () = print_string (matrix_to_string ())

  (** TODO: Implement function that generates a solvable maze. "#" represent
      Walls "." represent Paths "S" represents the Start "E" represents the End *)
  let generate () =
    matrix :=
      Array.make_matrix 10 10 "X" (* Placeholder method that fills Array*)
end
