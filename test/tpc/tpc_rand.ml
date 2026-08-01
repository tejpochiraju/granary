(* A 48-bit linear congruential generator with the parameters used by
   POSIX drand48.  Chosen over Stdlib.Random so that a seed pins the dataset
   independently of the stdlib's PRNG implementation, which is not stable
   across OCaml releases. *)

type t = { mutable state : int }

(* The 48-bit state and the [1 lsl 48] mask below need a 63-bit [int].  On a
   32-bit target the mask wraps and the same seed would produce a different
   dataset, silently voiding the reproducibility contract this module exists
   for — so refuse to load instead (#504). *)
let () =
  if Sys.int_size < 63
  then
    failwith
      (Printf.sprintf
         "Tpc_rand: needs a 63-bit int for its 48-bit LCG state; this platform's int is \
          %d bits"
         Sys.int_size)
;;

let modulus = 1 lsl 48
let multiplier = 0x5DEECE66D
let increment = 0xB
let pp fmt t = Format.fprintf fmt "Tpc_rand.t { state = %d }" t.state
let create ~seed = { state = seed lxor multiplier land (modulus - 1) }

let next_bits t bits =
  t.state <- ((t.state * multiplier) + increment) land (modulus - 1);
  t.state lsr (48 - bits)
;;

(* 31 random bits, i.e. a non-negative int under 2^31. *)
let next_int t = next_bits t 31

let int_between t ~lo ~hi =
  if lo > hi then invalid_arg "Tpc_rand.int_between: lo > hi";
  let span = hi - lo + 1 in
  lo + (next_int t mod span)
;;

let float_between t ~lo ~hi ~decimals =
  let scale = int_of_float (10.0 ** float_of_int decimals) in
  let lo_i = int_of_float (Float.round (lo *. float_of_int scale)) in
  let hi_i = int_of_float (Float.round (hi *. float_of_int scale)) in
  float_of_int (int_between t ~lo:lo_i ~hi:hi_i) /. float_of_int scale
;;

let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789, "

(* Built with an explicit left-to-right loop rather than [String.init].  The
   character function draws from the LCG, so the string's value depends on the
   order in which it is applied.  [String.init] does document "increasing index
   order" on the toolchain this project pins, but making the order local rather
   than inherited from a stdlib guarantee keeps the reproducibility contract
   readable and immune to that documentation changing. *)
let a_string t ~lo ~hi =
  let n = int_between t ~lo ~hi in
  let last = String.length alphabet - 1 in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.unsafe_set b i alphabet.[int_between t ~lo:0 ~hi:last]
  done;
  Bytes.unsafe_to_string b
;;

let pick t choices =
  let n = Array.length choices in
  if n = 0 then invalid_arg "Tpc_rand.pick: empty choices";
  choices.(int_between t ~lo:0 ~hi:(n - 1))
;;

let phone t ~nation =
  Printf.sprintf
    "%02d-%03d-%03d-%04d"
    (nation + 10)
    (int_between t ~lo:100 ~hi:999)
    (int_between t ~lo:100 ~hi:999)
    (int_between t ~lo:1000 ~hi:9999)
;;
