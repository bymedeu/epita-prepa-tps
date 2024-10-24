(** Testing for primality *)

open Z
open Z_power

(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)
let is_prime n =
    let rec is_prime_rec nbr di =
        if pred nbr <= one then
            if pred n = one then
                true
            else
                false
        else if n mod di = zero then
            false
        else if mul di di > nbr then
            true
        else
            is_prime_rec nbr (succ di)
    in is_prime_rec n (succ one);;
