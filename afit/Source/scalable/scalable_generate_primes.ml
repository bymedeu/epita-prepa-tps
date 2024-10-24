(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
    let rec init_eratosthenes_rec k ele =
        if ele >> n then []
        else ele :: init_eratosthenes_rec (add_b k [0;1]) (add_b ele [0;0;1])
    in [0;0;1] :: init_eratosthenes_rec [] [0;1;1]

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
    let rec sieve primes = match primes with
        | [] -> []
        | p :: rest ->
            let is_multiple x = mod_b x p = [] in
            let filtered = List.filter (fun x -> not (is_multiple x)) rest in
            p :: sieve filtered
    in sieve (init_eratosthenes n)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)

let int_list_to_string_list list =
    List.map string_of_int list
   
let write_list li file = 
    let oc = open_out file in
    let rec write_list_rec l = match l with
        | [] -> close_out oc
        | h::t -> output_string oc (String.concat "," (int_list_to_string_list h) ^ "\n");
                  write_list_rec t
    in write_list_rec li;;



(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = 
    let primes = eratosthenes n in
    write_list primes file


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ();;




(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)

let string_list_to_int_list list =
    List.map int_of_string list
let read_list_primes file = 
    let ic = open_in file in
    let rec read_list_rec l = match input_line_opt ic with
        | None -> close_in ic; l
        | Some line -> read_list_rec (string_list_to_int_list (String.split_on_char ',' line) :: l)
    in read_list_rec [];;



(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t;;

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t;;

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
    let double_primes limit isprime =
        let rec double_primes_rec acc tab = match tab with
            | [] -> List.rev acc
            | e::l ->
                let next_prime = add_b (mult_b [0;0;1] e) [0;1] in
                if isprime next_prime then
                    double_primes_rec ((e, next_prime) :: acc) l
                else
                    double_primes_rec acc l
        in
        double_primes_rec [] (eratosthenes limit)

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
    let rec twin_primes_rec acc tab = match tab with
        | [] -> List.rev acc
        | e::l ->
            let next_prime = add_b e [0;0;1] in
            if isprime next_prime then
                twin_primes_rec ((e, next_prime) :: acc) l
            else
                twin_primes_rec acc l
    in
    twin_primes_rec [] (eratosthenes limit)
