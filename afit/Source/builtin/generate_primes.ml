(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  let rec init_eratosthenes_rec k ele =
    match k with
    | _ when ele > n -> []
    | _ -> ele :: init_eratosthenes_rec (k + 1) (ele + 2)
  in
  2 :: init_eratosthenes_rec 0 3

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
              if y mod x = 0 then erato_rec ys lst_era
              else y :: erato_rec ys (y :: lst_era)
        in
        x :: eratosthenes_rec (erato_rec xs [])
  in
  eratosthenes_rec (init_eratosthenes n);;
(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in
  let rec aux = function
    | [] -> close_out oc
    | e :: l ->
        let str = string_of_int e in
        Printf.fprintf oc "%s\n" str;
        aux l
  in
  aux li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes limit file =
  let primes = eratosthenes limit in
  write_list primes file


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
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None
  in
  let rec loop () =
    match try_read () with
    | Some s -> int_of_string s :: loop ()
    | None ->
        close_in ic;
        []
  in
  loop ()

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

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime = 
  let rec double_primes_rec lst =
    match lst with
    | [] -> []
    | x :: xs ->
        if isprime (2 * x + 1) then
          (x, 2 * x + 1) :: double_primes_rec xs
        else
          double_primes_rec xs
  in
  double_primes_rec (eratosthenes limit);;


(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec helper lst =
    match lst with
    | [] -> []
    | x :: xs ->
        if isprime (x + 2) then
          (x, x + 2) :: helper xs
        else
          helper xs
  in
  helper (eratosthenes limit);;
