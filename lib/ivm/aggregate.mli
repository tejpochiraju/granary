(** Incremental grouped aggregation over Z-sets (#417 Phase 2).

    Maintains a [SUM]-style aggregate per group: each input element contributes
    [measure element * weight] to its group's running value, and the group's
    {e existence} is tracked by its total weight (so [COUNT] is the special case
    [measure = fun _ -> 1]).  Given an input delta, {!step} returns the output
    delta that retracts each changed group's old [(group, value)] row and inserts
    its new one — keeping the result relation current without rescanning the
    input.  A group's row is present iff its total weight is [> 0], and each
    present group yields exactly one output row.

    The measure is {e additive}, which is what makes the running total
    maintainable from a delta (this covers [COUNT], [SUM], and [AVG] as
    sum-with-count).  Order-sensitive aggregates ([MIN]/[MAX]) cannot be
    expressed this way — a retraction can lower the current extremum, which needs
    the full group, not a running scalar — and are out of scope here. *)

(** What to aggregate: the input/output Z-set domains, the grouping key, the
    per-element measure, and how to render a [(group, value)] result row. *)
module type SPEC = sig
  (** Input Z-set (the rows being aggregated). *)
  module In : Zset.S

  (** Output Z-set (the [(group, value)] result rows). *)
  module Out : Zset.S

  (** The grouping-key type. *)
  type group

  (** Total order on groups. *)
  val compare_group : group -> group -> int

  (** The group an input element belongs to. *)
  val group_of : In.elt -> group

  (** The element's contribution to its group's aggregate ([1] for [COUNT], the
      summed column for [SUM]).  The running value is [Σ measure * weight]. *)
  val measure : In.elt -> int

  (** Build the output row from a group and its aggregate value. *)
  val result : group -> int -> Out.elt
end

(** [Make (S)] builds an incremental grouped-aggregate operator for spec [S]. *)
module Make (S : SPEC) : sig
  (** A stateful aggregate operator: per-group running totals plus the
      materialized output relation. *)
  type t

  (** A fresh operator with no groups. *)
  val create : unit -> t

  (** [step t delta] folds the input [delta] into the per-group totals and
      returns the output delta (retract/insert of changed group rows), also
      folding it into {!output}. *)
  val step : t -> S.In.t -> S.Out.t

  (** The current materialized [(group, value)] relation. *)
  val output : t -> S.Out.t
end
