(** Z-sets — the core data structure of DBSP-style incremental computation
    (#417 Phase 1).

    A Z-set over a domain is a finitely-supported map from elements to signed
    integer {e weights}: an element present with weight [+1] is "in the set
    once", [+n] "n times", and a negative weight is a {e retraction}.  Inserts
    and deletes from the row-level delta feed (#419) lift to a Z-set delta
    ([Inserted] → [+1], [Deleted] → [-1]), and the incremental operators (Phase
    2) are functions over Z-sets that commute with [add], so a view is maintained
    by applying operators to the input delta rather than recomputing.

    A Z-set is kept {e canonical}: an element with total weight [0] is never
    stored, so two Z-sets are {!equal} iff they have the same elements with the
    same nonzero weights. *)

(** The element domain of a Z-set: a totally ordered, printable type. *)
module type ELEMENT = sig
  (** The element type. *)
  type t

  (** A total order on elements (keys are deduplicated by it). *)
  val compare : t -> t -> int

  (** Pretty-print an element (used by {!S.pp}). *)
  val pp : Format.formatter -> t -> unit
end

(** The operations a Z-set supports. *)
module type S = sig
  (** The element type this Z-set ranges over. *)
  type elt

  (** A Z-set: a finitely-supported [elt -> int] weight map, kept canonical (no
      zero-weight entries). *)
  type t

  (** The empty Z-set (additive identity). *)
  val zero : t

  (** [is_zero z] is [true] iff [z] has no elements (equivalently, total weight
      everywhere is 0). *)
  val is_zero : t -> bool

  (** [singleton e w] is the Z-set mapping [e] to [w] (and [zero] if [w = 0]). *)
  val singleton : elt -> int -> t

  (** [of_list pairs] sums the weights of repeated elements; entries that sum to
      [0] are dropped. *)
  val of_list : (elt * int) list -> t

  (** [to_list z] returns the [(element, nonzero-weight)] pairs, ordered by the
      element {!ELEMENT.compare}. *)
  val to_list : t -> (elt * int) list

  (** [weight z e] is the weight of [e] in [z] ([0] if absent). *)
  val weight : t -> elt -> int

  (** [add a b] is the pointwise sum of weights, dropping any that cancel to
      [0].  Commutative, associative, with identity {!zero}. *)
  val add : t -> t -> t

  (** [negate z] flips the sign of every weight (the additive inverse:
      [add z (negate z) = zero]). *)
  val negate : t -> t

  (** [sub a b] is [add a (negate b)]. *)
  val sub : t -> t -> t

  (** [scale n z] multiplies every weight by [n] ([zero] when [n = 0]). *)
  val scale : int -> t -> t

  (** [map f z] applies [f] to each element, summing the weights of elements
      that collide onto the same image (and dropping any that cancel).  A linear
      operator: [map f (add a b) = add (map f a) (map f b)]. *)
  val map : (elt -> elt) -> t -> t

  (** [filter p z] keeps the entries whose element satisfies [p], weights
      unchanged.  A linear operator. *)
  val filter : (elt -> bool) -> t -> t

  (** [distinct z] is the set-semantics projection: every element with weight
      [> 0] is mapped to weight [1]; elements with weight [<= 0] are dropped.
      Idempotent. *)
  val distinct : t -> t

  (** [support z] is the list of elements with nonzero weight, ordered by
      {!ELEMENT.compare}. *)
  val support : t -> elt list

  (** [cardinality z] is the number of distinct elements with nonzero weight. *)
  val cardinality : t -> int

  (** [total_weight z] is the sum of all weights (may be negative). *)
  val total_weight : t -> int

  (** [iter f z] applies [f element weight] to each entry, in element order. *)
  val iter : (elt -> int -> unit) -> t -> unit

  (** [fold f z acc] folds [f element weight] over the entries, in element
      order. *)
  val fold : (elt -> int -> 'a -> 'a) -> t -> 'a -> 'a

  (** Structural equality (canonical form makes this exact set/weight
      equality). *)
  val equal : t -> t -> bool

  (** Pretty-print a Z-set as [{e1: w1, e2: w2, …}]. *)
  val pp : Format.formatter -> t -> unit
end

(** [Make (E)] builds a Z-set module over element domain [E]. *)
module Make (E : ELEMENT) : S with type elt = E.t
