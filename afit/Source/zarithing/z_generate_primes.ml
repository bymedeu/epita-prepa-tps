(** Generating primes *)

open Z

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
   A light version done in-class.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n upper limit of elements in the list of big integers.
 *)
let init_eratosthenes n =
  let rec init_eratosthenes_rec e elem =
    match e with
    | _ when elem > n -> []
    | _ -> elem :: init_eratosthenes_rec (succ e) (succ (succ elem))
  in
  (succ one) :: init_eratosthenes_rec zero (succ (succ one));;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  let rec eratosthenes_rec lst =
    match lst with
    | [] -> []
    | x :: xs ->
        let rec erato_rec lst1 lst_era =
          match lst1 with
          | [] -> []
          | y :: ys ->
              if y mod x = zero then erato_rec ys lst_era
              else y :: erato_rec ys (y :: lst_era)
        in
        x :: eratosthenes_rec (erato_rec xs [])
  in
  eratosthenes_rec (init_eratosthenes n);;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file = 
  let oc = open_out file in
  let rec write_list_rec l =
    match l with
    | [] -> close_out oc
    | x :: xs -> Printf.fprintf oc "%s\n" (Z.to_string x); write_list_rec xs
  in
  write_list_rec li;;



(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = 
  write_list (eratosthenes n) file;;


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c = 
  let rec create_list_rec l =
    match input_line_opt in_c with
    | None -> close_in in_c; l
    | Some s -> create_list_rec (Z.of_string s :: l)
  in
  create_list_rec [];;



(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = 
  let ic = open_in file in
  create_list ic;;



(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(* Generating couples of primes numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive big integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec double_primes_rec lst =
    match lst with
    | [] -> []
    | x :: xs ->
        if isprime (succ (mul x (succ one))) then
          (x, succ (mul x (succ one))) :: double_primes_rec xs
        else
          double_primes_rec xs
  in
  double_primes_rec (eratosthenes limit)
