(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
    let len = from_int (String.length str) in
    let bitss = from_int bits in
    let rec encode_rec i j sum =
        if i = [1; 1] || j >= add_b len [0; 1] then
            sum
        else
            let char_index = int_of_char str.[to_int i] in
            let power_of_bits = power [0; 0; 1] bitss in
            let encoded_char = mult_b (from_int char_index) (power power_of_bits j) in
            encode_rec (diff_b i [0; 1]) (add_b j [0; 1]) (add_b sum encoded_char)
    in
    encode_rec (diff_b len [0; 1]) [] [];;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =
    let rec decode_rec msg bits =
        match msg with
        | [] -> ""
        | _ ->
            let pw = power [0; 0; 1] bits in
            let quotient = quot_b msg pw in
            let remainder = mod_b msg pw in
            decode_rec quotient bits ^ Char.escaped (char_of_int (to_int remainder))
    in
    decode_rec msg (from_int bits);;
