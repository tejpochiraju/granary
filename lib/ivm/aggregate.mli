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
    the full group, not a running scalar — and are out of scope here.

    {2 Memory: the net-zero retention ceiling (#423)}

    A group is dropped from the operator's internal map only when {e both} its
    total weight and its running value reach zero.  A group whose weights cancel
    to zero while its value does not is kept on purpose: the value is not
    recoverable from the delta feed, so dropping it would silently corrupt the
    group if it later revives.  #423 decided to accept that retention and state
    its bound rather than compact or age it out; [docs/IVM_MEMORY.md] carries the
    long form and the measurement.  In summary:

    - {b The bound is on distinct groups, not on updates.}  The map holds at most
      one entry per group, so retention is bounded by the number of distinct
      groups the operator has ever been shown.  A view churning a fixed key set
      forever retains at most that key set; only unbounded {e key cardinality} is
      unbounded memory.
    - {b The cost is 9 words per retained group}, plus whatever the caller's
      [group] value costs — 72 bytes on a 64-bit target for an immediate key,
      measured, and stable because it is one map node plus one two-field record.
    - {b Retention needs a negative weight.}  If every element of a group carries
      a non-negative cumulative weight, a total weight of zero forces every
      individual weight to zero and hence a value of zero, which prunes.  So an
      input stream that only ever retracts what it has inserted — which is what
      a base-table change feed produces — never retains anything.  Signed
      weights, as produced by a composed operator or by a retraction with no
      matching insertion, are what reach this state.
    - {b Retention needs two measures in one group.}  The value is
      [Σ measure * weight]; if [measure] is constant [c] across the group, that
      is [c * Σ weight], which is zero exactly when the total weight is.  COUNT
      ([measure = fun _ -> 1]) therefore never retains, whatever the weights.

    {!Make.retained_groups} reports the live count, so a long-running embedding
    can observe its own ceiling rather than infer it. *)

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

  (** [retained_groups t] is the number of groups [t] is holding state for,
      including the net-zero groups that contribute no output row (see the
      retention ceiling above).  It is always at least the number of rows in
      {!output}, and never exceeds the number of distinct groups [t] has been
      shown.  Introspection for memory accounting; it costs one map traversal
      and reads no aggregate values. *)
  val retained_groups : t -> int
end
