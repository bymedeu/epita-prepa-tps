(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let rec break_key k n =
    if (k mod n) = 0 then
        (n, k / n)
    else
        break_key k (n - 1)
 
let break key =
    let (x, _) = key in
    let x = float_of_int x in
    let r_key = sqrt x in
    if modulo (int_of_float (floor r_key)) 2 = 1 then
        break_key (int_of_float (floor x)) (int_of_float (floor r_key) - 1)
    else
        break_key (int_of_float (floor x)) (int_of_float (floor r_key))
