module Board = struct
  let matrix = [ [ "a"; "b"; "c" ]; [ "d"; "e"; "f" ]; [ "g"; "h"; "i" ] ]

  let rec print_line lst =
    match lst with
    | [] -> ()
    | h :: t ->
        print_string h;
        print_string " ";
        print_line t

  let print_first_line = print_line (List.hd matrix)
end
