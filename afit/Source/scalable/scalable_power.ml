(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
 let pow x n =  if n << [] then
    invalid_arg("n must be positive")
else
   let rec pow_rec x n = 
   if n = [] then
       [0;1]
   else
       mult_b x ((pow_rec x (diff_b n [0;1])))
in pow_rec x n;;



(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let rec power x n =
    match (x, n) with
    | ([], _) -> []
    | (_, []) -> [0; 1]
    | (_, [0; 1]) -> x
    | (e::l, [0; 0; 1]) -> mult_b (0::l) (0::l)
    | _ when mod_b n [0; 0; 1] = [] ->
        power (mult_b x x) (quot_b n [0; 0; 1])
    | _ -> mult_b x (power (mult_b x x) (quot_b n [0; 0; 1]))
;;
 
 
(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let rec mod_power x n m =
    if x = [] then
        []
    else if n = [] then
        [0; 1]
    else if mod_b n [0; 0; 1] = [] then
        let yes = mod_power x (quot_b n [0; 0; 1]) m in
        let yes = mod_b (mult_b yes yes) m in
        mod_b (add_b yes m) m
    else
        let yes = mod_b x m in
        let yes = mult_b yes (mod_b (mod_power x (diff_b n [0; 1]) m) m) in
        mod_b (add_b yes m) m
;;
(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
    if x = [] || x = [0;1] then
        x
    else
        let u = mod_b n (diff_b p [0;1]) in
        mod_power x u p
;;
