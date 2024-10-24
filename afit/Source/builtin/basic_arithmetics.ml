(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b =
    if a<0 && b<0 then
        gcd (a*(-1)) (b*(-1))
      else let x = a mod b in
           let rec pgcd a b x = match x with
             |0 -> b
             |_-> pgcd b x (b mod x)
                in pgcd a b x;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let rec bezout a b u v u' v' = match b with
    |0 -> (u, v, a)
    |_-> let q = a/b in
         bezout b (a-b*q) u' v' (u-q*u') (v-q*v')
  in bezout a b 1 0 0 1;;
