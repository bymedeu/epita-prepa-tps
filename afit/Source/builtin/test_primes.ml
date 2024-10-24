(** Testing for primality *)
open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)

let is_prime n =
    let rec is_prime_rec num divisor =
        match num with
        | _ when num <= 2 ->
                if n = 2 then
                    true
                else
                    false
        | _ when modulo num divisor = 0 ->
                false
        | _ when divisor * divisor > num ->
                true
        | _ -> is_prime_rec num (divisor + 1)
    in
    is_prime_rec n 2




(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
    let rec is_pseudo_prime_rec p test_seq =
        match test_seq with
        | [] -> true
        | n :: l ->
                if mod_power n p p != modulo n p then
                    false
                else
                    is_pseudo_prime_rec p l
    in
    is_pseudo_prime_rec p test_seq
