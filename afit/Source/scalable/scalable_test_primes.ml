(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n =
    let rec is_prime_rec nbr di =
        match nbr with
        | _ when nbr <<= [0;0;1] ->
            if n = [0;0;1] then
                true
            else
                false
        | _ when mod_b nbr di = [] ->
            false
        | _ when mult_b di di >> nbr ->
            true
        | _ ->
            is_prime_rec nbr (add_b di [0;1])
    in
    is_prime_rec n [0;1;0]

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
    let rec is_pseudo_prime_rec p test_seq =
        match test_seq with
        | [] -> true
        | n :: l ->
                if mod_power n p p <> mod_b n p then
                    false
                else
                    is_pseudo_prime_rec p l
    in
    is_pseudo_prime_rec p test_seq
