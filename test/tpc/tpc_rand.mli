(** Seeded pseudo-random primitives for TPC-derived data generation (#482).

    A self-contained linear congruential generator, not [Stdlib.Random], so a
    given seed produces the same dataset on every platform and OCaml version.
    Generation must be reproducible for benchmark numbers to be comparable
    across runs and machines. *)

type t

(** [pp fmt t] prints the generator's opaque internal LCG state, for
    debugging only — it is not part of the reproducibility contract. *)
val pp : Format.formatter -> t -> unit

(** [create ~seed] starts a generator stream.  Distinct seeds give distinct
    streams; equal seeds give byte-identical output. *)
val create : seed:int -> t

(** [int_between r ~lo ~hi] is a uniform integer in the closed interval
    [\[lo, hi\]].  Requires [lo <= hi]; raises [Invalid_argument] otherwise. *)
val int_between : t -> lo:int -> hi:int -> int

(** [float_between r ~lo ~hi ~decimals] is a uniform value in [\[lo, hi\]]
    quantized to [decimals] places — the spec's fixed-point money and rate
    columns. *)
val float_between : t -> lo:float -> hi:float -> decimals:int -> float

(** [a_string r ~lo ~hi] is the spec's random alphanumeric string: a length
    uniform in [\[lo, hi\]], each character drawn uniformly from the 64-symbol
    alphabet (letters, digits, comma, space). *)
val a_string : t -> lo:int -> hi:int -> string

(** [pick r choices] selects one element uniformly.  Raises
    [Invalid_argument] on an empty array. *)
val pick : t -> string array -> string

(** [phone r ~nation] builds the spec's 15-character phone number
    ["CC-AAA-BBB-CCCC"], where the country code is [nation + 10]. *)
val phone : t -> nation:int -> string

(** [nurand t ~a ~x ~y ~c] is the TPC-C non-uniform random distribution
    [(((random(0,a) lor random(x,y)) + c) mod (y - x + 1)) + x], always within
    [\[x, y\]].  The skew it produces is what makes the benchmark contend; a
    uniform draw would not.  [c] is the spec's per-run constant, taken as a
    parameter so that a seed pins the whole workload.  Requires [x <= y],
    [a >= 0], and [c >= 0] — a negative [c] would make OCaml's [mod] return a
    negative remainder, pushing the result below [x]; raises
    [Invalid_argument] if any precondition is violated. *)
val nurand : t -> a:int -> x:int -> y:int -> c:int -> int

(** [last_name n] is the spec's customer surname for [n] in [\[0, 999\]]: the
    concatenation of three syllables drawn from a fixed ten-element table,
    running from ["BARBARBAR"] to ["EINGEINGEING"].  Pure — it draws no
    randomness.  Raises [Invalid_argument] outside [\[0, 999\]]. *)
val last_name : int -> string
