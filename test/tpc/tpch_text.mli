(** The TPC-H §4.2.2.1 text pool (#482).

    Comment columns in the TPC-H schema are random substrings of a large pool
    of grammar-generated English-like text.  The grammar matters rather than
    being decoration: Q13 filters on the [special] and [requests] tokens this
    grammar produces, so a pool of arbitrary noise would make it degenerate.

    Q16's [Customer]/[Complaints] markers do {b not} come from this grammar.
    They are spliced into [s_comment] by {!Tpch_gen}, which plants five of
    each per 10 000 suppliers as the specification requires. *)

(** [pool r ~size] generates at least [size] bytes of grammar text.  The
    result is deterministic in the generator's seed.  Build it once and share
    it across all comment columns — regenerating per row is both slow and
    contrary to the spec. *)
val pool : Tpc_rand.t -> size:int -> string

(** [substring ~pool r ~lo ~hi] takes a random substring of [pool] whose
    length is uniform in [\[lo, hi\]], as the spec's comment columns require.

    Degenerate case: if the drawn length exceeds [String.length pool], the
    result is truncated to [pool] itself (never raises) and its length can
    then be shorter than [lo]. This cannot happen in intended use — the pool
    is always built far larger than any comment's [hi] — but callers passing
    a [pool] smaller than [hi] should not rely on the [\[lo, hi\]] length
    contract holding. *)
val substring : pool:string -> Tpc_rand.t -> lo:int -> hi:int -> string
