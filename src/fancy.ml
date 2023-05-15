

    let alphabet = [|
      [|
        "░█▀█";
        "░█▀█";
        "░▀░▀"
      |];
      [|
        "░█▀▄";
        "░█▀▄";
        "░▀▀░"
      |];
      [|
        "░█▀▀";
        "░█░░";
        "░▀▀▀"
      |];
      [|
        "░█▀▄";
        "░█░█";
        "░▀▀░"
      |];
      [|
        "░█▀▀";
        "░█▀▀";
        "░▀▀▀"
      |];
      [|
        "░█▀▀";
        "░█▀▀";
        "░▀░░"
      |];
      [|
        "░█▀▀";
        "░█░█";
        "░▀▀▀"
      |];
      [|
        "░█░█";
        "░█▀█";
        "░▀░▀"
      |];
      [|
        "░▀█▀";
        "░░█░";
        "░▀▀▀"
      |];
      [|
        "░▀▀█";
        "░░░█";
        "░▀▀░"
      |];
      [|
        "░█░█";
        "░█▀▄";
        "░▀░▀"
      |];
      [|
        "░█░░";
        "░█░░";
        "░▀▀▀"
      |];
      [|
        "░█▄█";
        "░█░█";
        "░▀░▀"
      |];
      [|
        "░█▀█";
        "░█░█";
        "░▀░▀"
      |];
      [|
        "░█▀█";
        "░█░█";
        "░▀▀▀"
      |];
      [|
        "░█▀█";
        "░█▀▀";
        "░▀░░"
      |];
      [|
        "░█░█";
        "░█▄█";
        "░▀░▀"
      |];
      [|
        "░█▀▄";
        "░█▀▄";
        "░▀░▀"
      |];
      [|
        "░█▀▀";
        "░▀▀█";
        "░▀▀▀"
      |];
      [|
        "░▀█▀";
        "░░█░";
        "░░▀░"
      |];
      [|
        "░█░█";
        "░█░█";
        "░▀▀▀"
      |];
      [|
        "░█░█";
        "░▀▄▀";
        "░░▀░"
      |];
      [|
        "░█░█";
        "░█▄█";
        "░▀░▀"
      |];
      [|
        "░█░█";
        "░▄▀▄";
        "░▀░▀"
      |];
      [|
        "░█░█";
        "░░█░";
        "░░▀░"
      |];
      [|
        "░▀▀█";
        "░▄▀░";
        "░▀▀▀"
      |];
      [|
        "░░";
        "░░";
        "░░"
      |];
    |]
    
    let get_letter (c : char) : string array =
      let index = 
        if (int_of_char c) == 32 then 26 (* space character *)
        else ((int_of_char c) - (int_of_char 'a')) in (* normal characters *)
      alphabet.(index)
    
    let explode s = List.init (String.length s) (String.get s)
    
    let rec print_line char_lst l = match char_lst with 
      | [] -> print_string "\n"; []
      | h :: t -> print_string ((get_letter h).(l)); print_line t l
    
    let fancy_message str = 
      let _ = print_string "\n" in 

      for i = 0 to 2 do
        let _ = print_line (explode str) i in ()
      done 