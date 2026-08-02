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
let digits = "0123456789"

(* Built with an explicit left-to-right loop rather than [String.init].  The
   character function draws from the LCG, so the string's value depends on the
   order in which it is applied.  [String.init] does document "increasing index
   order" on the toolchain this project pins, but making the order local rather
   than inherited from a stdlib guarantee keeps the reproducibility contract
   readable and immune to that documentation changing. *)
let string_from t ~symbols ~lo ~hi =
  let n = int_between t ~lo ~hi in
  let last = String.length symbols - 1 in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.unsafe_set b i symbols.[int_between t ~lo:0 ~hi:last]
  done;
  Bytes.unsafe_to_string b
;;

let a_string t ~lo ~hi = string_from t ~symbols:alphabet ~lo ~hi
let n_string t ~lo ~hi = string_from t ~symbols:digits ~lo ~hi

let pick t choices =
  let n = Array.length choices in
  if n = 0 then invalid_arg "Tpc_rand.pick: empty choices";
  choices.(int_between t ~lo:0 ~hi:(n - 1))
;;

(* Each draw is bound to its own [let], in the order the fields appear, rather
   than written inline as arguments of [Printf.sprintf].  OCaml does not specify
   the evaluation order of a function's arguments, and every draw advances the
   LCG, so inlined it was toolchain-dependent which draw landed in which field —
   the same seed produced different s_phone and c_phone values under a different
   compiler or flambda setting, silently voiding this module's reproducibility
   contract (#509).  Same hazard as [a_string] above and [nurand] below. *)
let phone t ~nation =
  let area = int_between t ~lo:100 ~hi:999 in
  let exchange = int_between t ~lo:100 ~hi:999 in
  let line = int_between t ~lo:1000 ~hi:9999 in
  Printf.sprintf "%02d-%03d-%03d-%04d" (nation + 10) area exchange line
;;

(* TPC-C's non-uniform key distribution:
     NURand(A, x, y) = (((random(0,A) | random(x,y)) + C) mod (y - x + 1)) + x
   The bitwise-or is what creates the skew — it drives bits high, so the
   masked-and-wrapped result clusters, and clustering is what makes the
   benchmark contend.  The spec picks C once per run at random; taking it as a
   parameter keeps a seed sufficient to pin the whole workload.

   The two draws are bound in explicit statements, random(0,a) first, rather
   than written inline as operands of [lor].  Both draws mutate the LCG state,
   and OCaml does not specify evaluation order for the operands of an infix
   operator — inlined, which stream value plays random(0,a) and which plays
   random(x,y) would be toolchain-dependent, silently breaking the seed's
   cross-platform reproducibility contract.  This is the same hazard
   [a_string] avoids above by using an explicit loop instead of
   [String.init]. *)
let nurand t ~a ~x ~y ~c =
  if x > y then invalid_arg "Tpc_rand.nurand: x > y";
  if a < 0 then invalid_arg "Tpc_rand.nurand: a < 0";
  if c < 0 then invalid_arg "Tpc_rand.nurand: c < 0";
  let span = y - x + 1 in
  let r_a = int_between t ~lo:0 ~hi:a in
  let r_xy = int_between t ~lo:x ~hi:y in
  (((r_a lor r_xy) + c) mod span) + x
;;

let last_name_syllables =
  [| "BAR"; "OUGHT"; "ABLE"; "PRI"; "PRES"; "ESE"; "ANTI"; "CALLY"; "ATION"; "EING" |]
;;

let last_name n =
  if n < 0 || n > 999 then invalid_arg "Tpc_rand.last_name: n out of [0,999]";
  last_name_syllables.(n / 100)
  ^ last_name_syllables.(n / 10 mod 10)
  ^ last_name_syllables.(n mod 10)
;;
