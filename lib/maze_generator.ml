

module type BOARD = sig

  (* The matrix representing the Board *)
  val matrix : string list list 

  (* Prints the Board to the console *)
  val print : unit 

  (* Prints a matrix given as an argument *)
  val print_matrix : string list list -> unit
end



module Board : BOARD = struct
  let matrix = [ [ "a"; "b"; "c" ]; [ "d"; "e"; "f" ]; [ "g"; "h"; "i" ] ]

  let rec print_line lst =
    match lst with
    | [] -> ()
    | h :: t ->
        print_string (h ^ " ");
        print_line t
    
  let rec printAll mtx = 
    match mtx with 
    | [] -> ()
    | h :: t ->
      print_string "\n  ";
      print_line h;
      printAll t

  let print = 
    let _ = print_string "\n" in 
    let _= printAll matrix in 
    print_string "\n"

  let print_matrix mtx =
    let _ = print_string "\n" in 
    let _ = printAll mtx in 
    print_string "\n"
end
