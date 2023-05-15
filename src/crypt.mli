(** This module [Crypt] implements the Affine cryptosystem. *)

type affine_key

exception BadKey
(** Raised when there is an attempt to encrypt or decrypt text with a key=(a,b)
    where gcd(a,26) != 1. *)

val generate_determined_affine_key : int -> int -> affine_key
(** [generate_determined_affine_key a b] returns an affine key with parameters
    (a,b). Requires: a and b are non-negative. *)

val generate_random_affine_key : unit -> affine_key
(** [generate_random_affine_key ()] returns a key used with the affine cipher.
    It is of the form k=(a,b) where gcd(a,26)=1 and a is in {1, ..., 26}. 
    a and b are also non-negative. *)

val affine_encrypt : string -> affine_key -> string
(** [affine_encrypt p k] encrypts the plaintext p with key k. Requires: p
    contains lowercase English letters and/or spaces. *)

val affine_decrypt : string -> affine_key -> string
(** [affine_decrypt c k] decrypts the ciphertext c with key k. Requires: c is a
    possible ciphertext generated by Crypt.affine_encrypt. *)
