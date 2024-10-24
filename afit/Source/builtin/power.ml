(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
 let pow x n =
    let  a=1 in
    let rec recpow n a = match n with
      |0 -> a
      |_ -> recpow (n-1) (a*x)
        in recpow n a;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
 let power x n = 
    let rec recpower x n = match n with
        |0 -> 1
        |n when n mod 2 = 0 -> recpower (x * x ) (n /2)
        |_-> x * recpower (x* x) (n/ 2)
    in recpower x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
 let mod_power x n m =
    if n = 0 then 1
    else
      let rec recmod_p a b = match a with
        | a when a = n -> b
        | a -> recmod_p (a +1) ((x *b) mod m) in 
    
        if (recmod_p 1 (x mod m)) < 0 then 
            recmod_p 1 (x mod m) + m
        else 
            recmod_p 1 (x mod m);;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
 let prime_mod_power x n p =
    let rec primerec n p = if (n<p) then
                                  mod_power x n p
                                else
                                 if( ((power x n) mod p)=0) then
                                  x mod p
                                 else
                                  primerec (n-(p-1)) p in primerec n p;;
  
