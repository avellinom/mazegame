type affine_key = int * int

(* All the available betas for an affine key (alpha,beta). *)
let numbers_coprime_with_26 = [| 1; 3; 5; 7; 9; 11; 15; 17; 19; 21; 23; 25 |]

exception BadKey

let generate_determined_affine_key (a : int) (b : int) : affine_key = (a, b)

let generate_random_affine_key () : affine_key =
  let random_index = Random.int (Array.length numbers_coprime_with_26) in
  let alpha = numbers_coprime_with_26.(random_index) in
  let beta = Random.int 26 in
  (alpha, beta)

(** [int_of_char c] converts lowercase character c as 'a' -> 0, 'b' -> 1, etc. 
    Requires: c is in {'a', 'b', ..., 'z'} *)
let char_to_int (c : char) : int = Char.code c - Char.code 'a'

(** [int_to_char i] converts integer i to a character as 0 -> 'a', 1 -> 'b',
    etc. *)
let int_to_char (i : int) : char =
  let i = i mod 26 in
  Char.chr (i + Char.code 'a')

let affine_encrypt (plaintext : string) (key : affine_key) : string =
  let alpha, beta = key in
  if Array.mem alpha numbers_coprime_with_26 then begin
    let plaintext = String.lowercase_ascii plaintext in
    let ciphertext = Buffer.create 16 in
    String.iter
      (fun c ->
        if c = ' ' then Buffer.add_char ciphertext c
        else
          Buffer.add_char ciphertext
            (int_to_char ((alpha * char_to_int c) + beta)))
      plaintext;
    Buffer.contents ciphertext
  end
  else raise BadKey
