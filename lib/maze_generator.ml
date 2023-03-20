module Board = struct
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
    printAll matrix
end
