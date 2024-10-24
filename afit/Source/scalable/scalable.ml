(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let get_sign x = if x >= 0 then 1 else -1;;

let divide a b = let quotient = a / b and remainder = a mod b in
    if remainder >= 0 then
        quotient
    else
        quotient - (get_sign b);;

let calculate_modulo a b = let remainder = a mod b in
    if remainder >= 0 then
        remainder
    else
        remainder + (get_sign b) * b;;

let from_int x =
    let rec from_int_rec nbr =
        match nbr with
        | 0 -> []
        | _ -> calculate_modulo nbr 2 :: from_int_rec (divide nbr 2)
    in
    if get_sign x = -1 then
        1 :: from_int_rec (-x)
    else if x = 0 then
        []
    else
        0 :: from_int_rec x
    ;;

let pow x n =
    let rec pow_rec acc x n =
        if n = 0 then
            acc
        else if n mod 2 = 0 then
            pow_rec acc (x * x) (n / 2)
        else
            pow_rec (acc * x) (x * x) ((n - 1) / 2)
    in
    if n < 0 then
        invalid_arg "n must be non-negative"
    else
        pow_rec 1 x n
;;



(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA =
    let rec rec_to_int nbr pw = match nbr with
        | [] -> 0
        | e::l -> e * (pow 2 pw) + rec_to_int l (pw+1)
    in
    match bA with
    | [] -> 0
    | e::l -> if e = 0 then rec_to_int l 0 else -(rec_to_int l 0)
;;

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
    let rec print_b_rec arr = match arr with
        | [] -> ""
        | e::l -> (print_b_rec l) ^ (string_of_int e)
    in
    match bA with
    | [] -> print_string "0"
    | 1::l -> print_string ("-" ^ (print_b_rec l))
    | e::l -> print_string (print_b_rec l)
;;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let reverse lst =
    let rec reverse_helper lst acc = match lst with
        | [] -> acc
        | x :: xs -> reverse_helper xs (x :: acc)
    in reverse_helper lst [];;

let rec shift bit_array d = match d with
    | 0 -> bit_array
    | _ -> shift (0 :: bit_array) (d - 1);;

let shift_left lst d = reverse (shift (reverse lst) d);;

let tail lst = match lst with
    | [] -> []
    | _ :: xs -> xs;;

let compare_n nA nB =
    let lenA = List.length nA and lenB = List.length nB in
    if lenA < lenB then
        -1
    else if lenA > lenB then
        1
    else
        let a = reverse nA and b = reverse nB in
        let rec compare_n_rec nA nB = match (nA, nB) with
            | ([], []) -> 0
            | ([], _) -> -1
            | (_, []) -> 1
            | (x :: xs, y :: ys) when x > y -> 1
            | (x :: xs, y :: ys) when x < y -> -1
            | (x :: xs, y :: ys) -> compare_n_rec xs ys
        in compare_n_rec a b;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB =
    let rep = compare_n nA nB in
    rep = 1;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB =
    let rep = compare_n nA nB in
    rep = -1;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =
    let rep = compare_n nA nB in
    rep >= 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =
    let rep = compare_n nA nB in
    rep = 0 || rep = -1;;
(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
    let rec compare_b_rec bA bB = match (bA, bB) with
        | ([], []) -> 0
        | ([], e :: l) when e = 1 -> 1
        | ([], e :: l) when e = 0 -> -1
        | (e :: l, []) when e = 0 -> 1
        | (e :: l, []) when e = 1 -> -1
        | (ea :: la, eb :: lb) when ea = 1 && eb = 1 -> -(compare_n la lb)
        | (ea :: la, eb :: lb) when ea < eb -> 1
        | (ea :: la, eb :: lb) when ea > eb -> -1
        | (ea :: la, eb :: lb) -> compare_n la lb
        | _ -> 0
    in compare_b_rec bA bB;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
    let rep = compare_b bA bB in
    rep = 1;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB =
    let rep = compare_b bA bB in
    rep = -1;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB =
    let rep = compare_b bA bB in
    rep >= 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =
    let rep = compare_b bA bB in
    rep <= 0;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
    match bA with
    | [] -> 1
    | 1 :: _ -> -1
    | _ :: _ -> 1;;


(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
    match bA with
    | [] -> bA
    | e :: l -> if e = 1 then bA else 0 :: bA;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a =
    match a with
    | a when a < 2 -> 0
    | _ -> 1;;

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a =
    match a with
    | 1 | 3 -> 1
    | _ -> 0;;

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a);;

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
    let rec add_n_rec na nb r =
        match (na, nb) with
        | ([], []) ->
                if r = 0 then
                    []
                else
                    [r]
        | ([], e::l) ->
                if r = 0 then
                    e :: add_n_rec [] l 0
                else
                    if e = 1 then
                        0 :: add_n_rec [] l 1
                    else
                        1 :: add_n_rec [] l 0
        | (e::l, []) ->
                if r = 0 then
                    e :: add_n_rec l [] 0
                else
                    if e = 1 then
                        0 :: add_n_rec l [] 1
                    else
                        1 :: add_n_rec l [] 0
        | (eA::lA, eB::lB) ->
                let aux sum =
                    match sum with
                    | 0 -> 0 :: add_n_rec lA lB 0
                    | 1 -> 1 :: add_n_rec lA lB 0
                    | 2 -> 0 :: add_n_rec lA lB 1
                    | 3 -> 1 :: add_n_rec lA lB 1
                    | _ -> [0]
                in aux (eA + eB + r)
    in add_n_rec nA nB 0;;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)

let remove_zero lst =
    let reversed_lst = reverse lst in
    match reversed_lst with
    | [] -> []
    | (x::xs) ->
        if x = 1 then
            lst
        else
            let rec remove_zero_rec l = 
                match l with
                | [] -> []
                | (y::ys) when y = 0 -> remove_zero_rec ys
                | (y::ys) -> reverse l
            in remove_zero_rec (reverse lst);;


    let diff_n nA nB =
        let rec diff_n_rec na nb r =
            match (na, nb) with
            | ([], []) -> []
            | (e::l, []) ->
                    if r = 0 then
                        e :: diff_n_rec l [] 0
                    else
                        if e = 1 then
                            0 :: diff_n_rec l [] 0
                        else
                            1 :: diff_n_rec l [] 1
            | (eA::lA, eB::lB) ->
                    let aux (a, b, r) =
                        match (a, b, r) with
                        | (0, 0, 0) -> 0 :: diff_n_rec lA lB 0
                        | (1, 0, 0) -> 1 :: diff_n_rec lA lB 0
                        | (1, 1, 0) -> 0 :: diff_n_rec lA lB 0
                        | (0, 1, 0) -> 1 :: diff_n_rec lA lB 1
                        | (1, 0, 1) -> 0 :: diff_n_rec lA lB 0
                        | (1, 1, 1) -> 1 :: diff_n_rec lA lB 1
                        | (0, 1, 1) -> 0 :: diff_n_rec lA lB 1
                        | (0, 0, 1) -> 1 :: diff_n_rec lA lB 1
                        | _ -> failwith "diff_n error 0"
                    in
                    aux (eA, eB, r)
            | _ -> failwith "diff_n error 1"
        in
        remove_zero (diff_n_rec nA nB 0);;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bitarrayA bitarrayB =
  match (bitarrayA, bitarrayB) with
  | ([], elementB :: restB) -> bitarrayB
  | (elementA :: restA, []) -> bitarrayA
  | (elementA :: restA, elementB :: restB) when elementA = 0 && elementB = 0 ->
      0 :: add_n restA restB
  | (elementA :: restA, elementB :: restB) when elementA = 1 && elementB = 1 ->
      1 :: add_n restA restB
  | (elementA :: restA, elementB :: restB) when elementA <> elementB && compare_n restA restB = 0 ->
      []
  | (elementA :: restA, elementB :: restB) ->
      let add_bitarrays_aux bitarrayA bitarrayB carry =
        match (bitarrayA, bitarrayB, carry) with
        | (elementA :: restA, elementB :: restB, 1) when elementA = 0 && elementB = 1 ->
            0 :: add_n restA restB
        | (elementA :: restA, elementB :: restB, 1) when elementA = 1 && elementB = 0 ->
            1 :: add_n restA restB
        | (elementA :: restA, elementB :: restB, 1) when elementA = 1 && elementB = 1 ->
            1 :: add_n restA restB
        | (elementA :: restA, elementB :: restB, -1) when elementA = 0 && elementB = 1 ->
            1 :: add_n restB restA
        | (elementA :: restA, elementB :: restB, -1) when elementA = 1 && elementB = 0 ->
            0 :: add_n restB restA
        | (elementA :: restA, elementB :: restB, -1) when elementA = 1 && elementB = 1 ->
            1 :: add_n restA restB
        | _ -> failwith "add_bitarrays: error 0"
      in
      add_bitarrays_aux bitarrayA bitarrayB (compare_n bitarrayA bitarrayB)
  | _ -> failwith "add_bitarrays: error 1";;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =
    match (bA, bB) with
    | ([], []) -> []
    | ([], e :: l) when e = 1 -> 0 :: l
    | ([], e :: l) when e = 0 -> 1 :: l
    | (e :: l, []) when e = 0 -> bA
    | (e :: l, []) when e = 1 -> 1 :: l
    | (e1 :: l1, e2 :: l2) when l1 = l2 && e1 = 0 && e2 = 1 -> []
    | (e1 :: l1, e2 :: l2) when l1 = l2 && e1 = 1 && e2 = 0 -> []
    | (eA :: lA, eB :: lB) ->
            let diff_b_bis bA bB cm =
                match (bA, bB, cm) with
                | (eA :: lA, eB :: lB, 0) when eA = 1 && eB = 1 -> []
                | (eA :: lA, eB :: lB, 0) when eA = 0 && eB = 0 -> []
                | (eA :: lA, eB :: lB, 1) when eA = 0 && eB = 0 -> 0 :: diff_n lA lB
                | (eA :: lA, eB :: lB, 1) when eA = 0 && eB = 1 -> 0 :: add_n lA lB
                | (eA :: lA, eB :: lB, 1) when eA = 1 && eB = 0 -> 1 :: add_n lA lB
                | (eA :: lA, eB :: lB, 1) when eA = 1 && eB = 1 -> 1 :: diff_n lA lB
                | (eA :: lA, eB :: lB, -1) when eA = 0 && eB = 0 -> 1 :: diff_n lB lA
                | (eA :: lA, eB :: lB, -1) when eA = 1 && eB = 0 -> 1 :: add_n lA lB
                | (eA :: lA, eB :: lB, -1) when eA = 1 && eB = 1 -> 0 :: diff_n lB lA
                | (eA :: lA, eB :: lB, -1) when eA = 0 && eB = 1 -> 0 :: add_n lA lB
                | _ -> failwith "diff_b error 0"
            in
            remove_zero (diff_b_bis bA bB (compare_n lA lB))
    | _ -> failwith "diff_b error 1";;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)


let mult_n nA nB =
    let rec mult_n_rec m a sum =
        match m with
        | [] -> sum
        | e :: l when e = 0 -> mult_n_rec l (shift a 1) sum
        | e :: l -> mult_n_rec l (shift a 1) (add_n a sum)
    in
    mult_n_rec nA nB []

let mult_b bA bB =
    match (bA, bB) with
    | ([], []) -> []
    | (e :: l, []) -> []
    | ([], e :: l) -> []
    | (eA :: lA, eB :: lB) ->
            let mult_b_bis bA bB cm =
                match (bA, bB, cm) with
                | (eA :: lA, eB :: lB, 0) when eA = eB -> 0 :: mult_n lA lB
                | (eA :: lA, eB :: lB, 1) when eA = eB -> 0 :: mult_n lA lB
                | (eA :: lA, eB :: lB, -1) when eA = eB -> 0 :: mult_n lB lA
                | (eA :: lA, eB :: lB, 0) -> 1 :: mult_n lA lB
                | (eA :: lA, eB :: lB, 1) -> 1 :: mult_n lA lB
                | (eA :: lA, eB :: lB, -1) -> 1 :: mult_n lB lA
                | _ -> failwith "mult_b error 0"
            in
            mult_b_bis bA bB (compare_n lA lB)


let rec shift bA d = match d with
    | 0 -> bA
    | _ -> shift (0 :: bA) (d - 1);;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let quot_n nA nB =
    let s = List.length nA - List.length nB in
    let rec quot_n_rec a b res sh =
        match res with
        | _ when a = [] && sh = -1 -> remove_zero res, true
        | _ when sh = -1 -> remove_zero res, false
        | _ when a >>= b ->
                quot_n_rec (diff_b a b) (tail b) (1 :: res) (sh - 1)
        | _ -> quot_n_rec a (tail b) (0 :: res) (sh - 1)
    in
    quot_n_rec (0 :: nA) (0 :: shift nB s) [] s
    

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =
    match (bA, bB) with
    | ([], []) -> failwith "quot_b: division by 0"
    | ([], e :: l) -> []
    | (e :: l, []) -> failwith "quot_b: division by 0"
    | (_, _) when bA = bB -> [0; 1]
    | (eA :: lA, eB :: lB) when lA = lB && eA <> eB -> [1; 1]
    | _ when bA <<! bB ->
            let quot_aux bA bB =
                match (bA, bB) with
                | (eA :: lA, eB :: lB) when eA = 0 && eB = 0 -> []
                | (eA :: lA, eB :: lB) when eA = 0 && eB = 1 -> [0]
                | (eA :: lA, eB :: lB) when eA = 1 && eB = 0 -> [1; 1]
                | (eA :: lA, eB :: lB) when eA = 1 && eB = 1 -> [0; 1]
                | _ -> failwith "quot_b: erreur aux"
            in
            quot_aux bA bB
    | (eA :: lA, eB :: lB) ->
            let quot_b_bis bA bB cm =
                match (bA, bB, cm) with
                | (eA :: lA, eB :: lB, 1) when eA = 0 && eB = 0 ->
                        let (result, equal) = quot_n lA lB in
                        0 :: result
                | (eA :: lA, eB :: lB, 1) when eA = 0 && eB = 1 ->
                        let (result, equal) = quot_n lA lB in
                        if equal = true then
                            1 :: result
                        else
                            1 :: result
                | (eA :: lA, eB :: lB, 1) when eA = 1 && eB = 0 ->
                        let (result, equal) = quot_n lA lB in
                        if equal = true then
                            1 :: result
                        else
                            add_b (1 :: result) [1; 1]
                | (eA :: lA, eB :: lB, 1) when eA = 1 && eB = 1 ->
                        let (result, equal) = quot_n lA lB in
                        if equal = true then
                            0 :: result
                        else
                            add_b (0 :: result) [0; 1]
                | (eA :: lA, eB :: lB, -1) when eA = 0 && eB = 0 ->
                        let (result, equal) = quot_n lA lB in
                        result
                | (eA :: lA, eB :: lB, -1) when eA = 0 && eB = 1 ->
                        let (result, equal) = quot_n lA lB in
                        result
                | (eA :: lA, eB :: lB, -1) when eA = 1 && eB = 0 -> [1; 1]
                | (eA :: lA, eB :: lB, -1) when eA = 1 && eB = 1 -> [0; 1]
                | _ -> failwith "quot_b: erreur 0"
            in
            quot_b_bis bA bB (compare_n lA lB)


(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB =
    match (bA, bB) with
    | _ when bA = bB -> []
    | _ when mult_b (quot_b bA bB) bB = bA -> []
    | _ -> diff_b bA (mult_b (quot_b bA bB) bB)



(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB =
    let quotient = quot_b bA bB in
    let remainder = mod_b bA bB in
    (quotient, remainder)
