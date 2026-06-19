(** Incremental equi-join over Z-sets (#417 Phase 2).

    Join is {e bilinear}, so its delta is
    [Δ(L ⋈ R) = ΔL ⋈ R + L ⋈ ΔR + ΔL ⋈ ΔR].  An operator value maintains the
    integrated inputs [L] and [R] (indexed by join key) and, given a delta on
    either side, returns the matching output delta and folds it into a
    materialized output — so the join is kept current in time proportional to the
    change, not the size of [L] and [R].

    Output carries {e bag} multiplicity: a matched pair contributes the product
    of its input weights.  Set / [SELECT DISTINCT] semantics, if wanted, are a
    {!Zset.S.distinct} applied to the output, not built in here. *)

(** What to join: the two input and one output Z-set domains, the join key, and
    how to extract keys / combine a matched pair into an output element. *)
module type SPEC = sig
  (** Left input Z-set. *)
  module Left : Zset.S

  (** Right input Z-set. *)
  module Right : Zset.S

  (** Output Z-set (the joined rows). *)
  module Out : Zset.S

  (** The equi-join key type. *)
  type key

  (** Total order on keys (used to index the integrated inputs). *)
  val compare_key : key -> key -> int

  (** The join key of a left element. *)
  val key_left : Left.elt -> key

  (** The join key of a right element. *)
  val key_right : Right.elt -> key

  (** Combine a matched [(left, right)] pair into an output element.  The output
      weight is the product of the input weights. *)
  val combine : Left.elt -> Right.elt -> Out.elt
end

(** [Make (S)] builds an incremental equi-join operator for spec [S]. *)
module Make (S : SPEC) : sig
  (** A stateful join operator: the integrated left/right inputs plus the
      materialized output. *)
  type t

  (** A fresh operator with empty inputs and output. *)
  val create : unit -> t

  (** [step t ~left ~right] integrates the input deltas [left] and [right] (pass
      {!Zset.S.zero} for a side with no change) and returns the resulting output
      delta, also folding it into {!output}. *)
  val step : t -> left:S.Left.t -> right:S.Right.t -> S.Out.t

  (** The current materialized join of everything integrated so far. *)
  val output : t -> S.Out.t
end
