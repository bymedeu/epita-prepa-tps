(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let rec aux key n =
    if mod_b key n = [] then
        (n, quot_b key n)
    else
        aux key (diff_b n [0; 1]);;

let break key =
    let (a, _) = key in
    let aa = from_int (to_int a) in
    let a = float_of_int (to_int a) in
    let r_key = from_int (int_of_float (floor (sqrt a))) in
    if mod_b r_key [0; 0; 1] = [0; 1] then
        aux aa (diff_b r_key [0; 1])
    else
        aux aa r_key;;
