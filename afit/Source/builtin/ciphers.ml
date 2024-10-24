(** Ciphers
    Builtin integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
    let rec encrypt_cesar_helper lst = 
        match lst with
        | [] -> []
        | x::xs -> modulo (k + x) (b + 1) :: encrypt_cesar_helper xs
    in encrypt_cesar_helper m;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b =
    let rec helper lst =
        match lst with
        | [] -> []
        | x :: xs -> modulo (x - k) (b + 1) :: helper xs
    in
    helper m;;


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let rec extended_euclidean_algorithm a b =
     if a = 0 then
         (b, 0, 1)
     else
         let (g, y, x) = extended_euclidean_algorithm (b mod a) a in
         (g, x - (b / a) * y, y);;

let inverse_modulo e phi =
    let gcd, c, d = extended_euclidean_algorithm e phi in
    if gcd != 1 then
        invalid_arg "e and phi are not coprime"
    else
        modulo c phi;;

let generate_keys_rsa p q =
    let m = q * p in
        let phi = (p - 1) * (q - 1) in
            let e = 65537 in
                let d = inverse_modulo e phi in
                ((m, e), (m, d));;

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
    let random_key = Random.int p in
    if mod_power p random_key p <> 1 then
        (random_key, p)
    else
        public_data_g p;;

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
    let key_p = Random.int p in
    (prime_mod_power g key_p p, key_p);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
    let random_key = Random.int (p - 2) + 2 in
    let encrypted_msg = (prime_mod_power g random_key p, msg * (prime_mod_power kA random_key p)) in
    encrypted_msg;;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
    let decrypted_msg = msgB / prime_mod_power msgA a p in
    decrypted_msg;;
