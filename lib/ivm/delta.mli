(** Lift row-mutation events into Z-set deltas (#417 Phase 3).

    This is the bridge from a change feed (e.g. the engine's row-level delta feed,
    #419) to the incremental operators: an inserted element is [+1], a deleted
    element is [-1], and an update is the retraction of the old element plus the
    insertion of the new ([-old +new]) — so an update to an identical element is a
    no-op, and a sequence of events folds to a single Z-set delta. *)

(** One change to a relation, over the element type a view ranges on (typically a
    projection of a stored row). *)
type 'elt event =
  | Insert of 'elt (** a row entered the relation *)
  | Delete of 'elt (** a row left the relation *)
  | Update of 'elt * 'elt (** a row changed: [Update (old, new)] *)

(** [Make (Z)] lifts events over [Z.elt] into [Z.t] deltas. *)
module Make (Z : Zset.S) : sig
  (** [of_event e] is the Z-set delta for a single event: [+1] for {!Insert},
      [-1] for {!Delete}, [-old +new] for {!Update}. *)
  val of_event : Z.elt event -> Z.t

  (** [of_events es] is the sum of {!of_event} over [es] (cancellations
      collapse, as in any Z-set). *)
  val of_events : Z.elt event list -> Z.t
end
