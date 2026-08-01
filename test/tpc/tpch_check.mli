(** Answer cross-check for the TPC-H-derived benchmark (#482).

    Compares the rows two engines returned for the same query and classifies
    the outcome into the CSV's [cross_check] token. Lives in the library rather
    than in [bench_tpch] so that the classification — which decides whether a
    run exits non-zero — is unit-testable without [sqlite3]. *)

(** The outcome of comparing one query's two answers. *)
type t =
  | Agree (** the answers match and are non-empty *)
  | Agree_both_empty
  (** both engines returned zero rows. Reported apart from {!Agree} because
          two empty answers compare equal whatever the query means: at SF 0.001
          Q2 and Q20 read "ok" for two rounds of review and were wrong at
          SF 0.01 (#492). *)
  | Mismatch of string
  (** the answers disagree; the string describes the first difference *)
  | Errored
  (** a query the catalogue asserts is runnable returned no rows because an
          engine rejected it. Distinct from {!Skipped} so a query that *starts*
          erroring is not indistinguishable from a deliberate skip (#502). *)
  | Skipped (** the query is [Skipped] in the catalogue; no comparison was made *)

(** [pp fmt t] prints the outcome and, for a mismatch, its report — for test
    failure messages and debugging. *)
val pp : Format.formatter -> t -> unit

(** [label t] is the CSV [cross_check] token: ["ok"], ["ok-both-empty"],
    ["MISMATCH"], ["error"], or ["skipped"]. *)
val label : t -> string

(** [is_failure t] is whether this outcome must make the run exit non-zero: a
    wrong answer, or an error on a query the catalogue asserts runs. *)
val is_failure : t -> bool

(** [field_eq a b] compares two rendered column values. Equal text is equal;
    otherwise the values compare numerically, but only when at least one side
    is written as a float. Two integer literals that differ as text ([{"07"}]
    vs [{"7"}]) are a real disagreement between engines' TEXT rendering, not
    float noise (#504). *)
val field_eq : string -> string -> bool

(** [row_eq a b] is [field_eq] over two rows of equal arity. *)
val row_eq : string list -> string list -> bool

(** [unordered_compare number] is whether query [number] truncates with LIMIT
    under an ORDER BY that is not a total order, so that its answers must be
    compared as multisets rather than sequences. *)
val unordered_compare : int -> bool

(** [compare_rows ~unordered granary sqlite] is [None] when the answers agree,
    and [Some report] describing the first disagreement otherwise. With
    [~unordered:true] the comparison is on the multiset of rows. *)
val compare_rows : unordered:bool -> string list list -> string list list -> string option

(** [classify ~number ~runnable ~granary ~sqlite] is the cross-check outcome
    for query [number], where each answer is [None] when that engine did not
    produce rows and [runnable] is whether the catalogue asserts the query
    runs (i.e. its verdict is not [Skipped]). *)
val classify
  :  number:int
  -> runnable:bool
  -> granary:string list list option
  -> sqlite:string list list option
  -> t
