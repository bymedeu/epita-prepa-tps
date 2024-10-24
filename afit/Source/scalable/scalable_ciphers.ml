(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)


let rec euclide_extended a b =
    if a = [] then
        (b, [], [0;1])
    else
        let (g, y, x) = euclide_extended (mod_b b a) a in
        (g, (diff_b x (mult_b (quot_b b a) y)), y);;

let inv_modulo e phi =
    let g, c, d = euclide_extended e phi in
    if g <> [0;1] then
        invalid_arg "inv_modulo linverse modulo does not exist"
    else
        mod_b c phi;;


let generate_keys_rsa p q =
    let n = mult_b p q in
    let phi = mult_b (diff_b p [0;1]) (diff_b q [0;1]) in
    let e = [0;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1] in
    let d = inv_modulo e phi in
    ((n, e), (n, d))

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p =
    let key = from_int (Random.int (to_int p)) in
    if mod_power key p p <> [0; 1] then
        (key, p)
    else
        public_data_g p

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
 let generate_keys_g (g, p) =
    let keyP = from_int(Random.int(to_int(p))) in
    (prime_mod_power g keyP p, keyP);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
    let key = Random.int (to_int p - 2) + 2 in
    let g_power = mod_power g (from_int key) p in
    let msg_power = mult_b msg (mod_power kA (from_int key) p) in
    (g_power, msg_power);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)

let decrypt_g (msgA, msgB) a (g, p) =
    quot_b msgB (prime_mod_power msgA a p)
;;
