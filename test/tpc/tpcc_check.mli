(** The TPC-C clause-3.3 consistency conditions, as a pure oracle (#500).

    TPC-C defines four consistency conditions that must hold across the
    warehouse/district/orders/new_order/order_line tables at any point a
    benchmark run is quiesced. {b All four are evaluated the same way here:}
    {!condition.queries} asks the engine only for plain rows and bare
    [GROUP BY] aggregates, and {!condition.check} — a pure OCaml function,
    and therefore directly unit-testable without an engine — joins them over
    the {e union} of both sides' keys. The engine still does all the
    scanning, grouping and aggregation; only the final comparison is
    client-side.

    That uniform shape is load-bearing, not a stylistic preference. The
    obvious alternative — one SQL query per condition returning zero rows on
    success, with a correlated scalar subquery in the [WHERE] clause — is a
    vacuous oracle for two independent reasons:

    - {b #485}: an unqualified outer-column reference inside a correlated
      subquery silently evaluates to NULL instead of erroring, turning
      condition 1's predicate into [w_ytd <> NULL] — never true, zero rows,
      a guaranteed {!Holds} over any database however corrupt.
    - Even fully qualified, [x <> (SELECT SUM/MAX ...)] is NULL, and so not
      returned, and so a pass, whenever the subquery matches {e zero rows}.
      A warehouse with no [district] rows passed condition 1 for any
      [w_ytd]; a district with no [orders] and no [new_order] rows passed
      condition 2 for any [d_next_o_id]. A [GROUP BY] over [new_order]
      alone has the same defect from the other end: it produces no group for
      a district that has vanished from [new_order], so condition 3 stopped
      examining that district entirely. PR 2's Delivery profile deletes
      [new_order] rows, so a drained district is reachable in the real mix.

    Both holes close the same way, and that is what every condition now does:
    drive the check from the table that {e must} have the row (warehouse for
    1, district for 2, 3 and 4), aggregate the other side with a plain
    [GROUP BY], and treat a key present on one side but missing on the other
    as an explicit, reported outcome rather than as an absence of evidence.

    Condition 4 needs the driving side too, even though its two aggregates are
    symmetric: joined against each other alone, a district that lost
    {e both} its [orders] and its [order_line] rows contributes no key to
    either side and passes vacuously. Condition 2's [orders] half catches that
    district today, so the oracle as a whole is not blind — but a condition
    must not rely on a different condition to notice its own subject
    disappearing.

    {b #507} is a further, unrelated reason conditions 3 and 4 could not be
    single queries in any case: granary's aggregation planner rejects any
    [GROUP BY] projection item that is not a bare grouped column, a bare
    aggregate call, or a window function, so both [MAX(x) - MIN(x)] and a
    subquery beside a sum are rejected outright with "complex expression in
    aggregated projection not supported". Fixing #507 does {e not} license a
    revert to a single query: only the client-side join closes the zero-rows
    hole.

    Two empty groups are not the same thing, and the difference is decided
    per condition rather than by default:

    - On a {e driving} side (warehouse for 1, district for 2, 3 and 4) an
      empty result is reported as a violation. Those tables always have rows
      in a real run, so an empty one means the check stopped seeing real data.
    - On the [new_order] {e aggregate} side a district with no rows is a
      legitimate steady state — Delivery deletes [new_order] rows, so a
      fully delivered district has none — and is consciously skipped, with
      the reasoning recorded at the site. An empty [orders] group is the
      opposite: orders are never deleted, so it is a violation.

    Condition 1 compares REAL money, so it uses a documented half-cent
    tolerance. That is necessary rather than lax: [w_ytd] is one accumulator
    while [SUM(d_ytd)] re-sums ten separately accumulated values, so the two
    take different rounding paths and an exact [=] would report a violation
    after a handful of Payments. The boundary is pinned by a unit test.

    Unparseable aggregates and malformed rows are likewise reported rather
    than dropped: a dropped row would leave its key silently absent, which is
    exactly the shape of the hole above.

    [test/test_tpcc_load.ml]'s "the conditions can fail" cases are the
    regression guard. They break each invariant against a real loaded
    population — both by perturbing a value {e and} by deleting a whole
    group — and require {!Violated}; they are what caught both holes. *)

(** One of the four numbered conditions: its 1-based [number], a
    human-readable [description] of the invariant it checks, the [queries]
    to run (in order — two for conditions 1 and 3, three for conditions 2 and
    4; the first is always the driving table's rows), and the pure [check] that
    decides the outcome from each query's rows (in [queries] order): [None]
    means the invariant holds, [Some report] means it was violated and names
    the offending warehouse or district and the two numbers that disagree. *)
type condition =
  { number : int
  ; description : string
  ; queries : string list
  ; check : string list list list -> string option
  }

(** The four clause-3.3 conditions, in order 1 through 4. *)
val conditions : condition list

(** The outcome of running one condition's [queries] through its [check]. *)
type outcome =
  | Holds (** [check] returned [None]: the invariant holds *)
  | Violated of string
  (** [check] returned [Some report]: the string names the offending
      district and the two numbers that disagree *)
  | Not_run
  (** a condition's queries did not execute at all. Distinct from a check
      that ran and found no violation: a check that silently stopped running
      must not read as a pass (#502). *)

(** [pp fmt outcome] prints the outcome for test failure messages and
    debugging. *)
val pp : Format.formatter -> outcome -> unit

(** [classify condition ~rows] is the outcome of having run every query in
    [condition.queries] (in order) and gotten back [rows] (one entry per
    query, in the same order): {!Holds} when [condition.check rows] is
    [None], {!Violated} with its report otherwise. Callers that could not
    execute a query at all should use {!Not_run} directly rather than calling
    [classify]. *)
val classify : condition -> rows:string list list list -> outcome

(** [label outcome] is the report token: ["ok"], ["VIOLATED"], or
    ["not-run"]. *)
val label : outcome -> string

(** [is_failure outcome] is whether this outcome must make the run exit
    non-zero. Both {!Violated} and {!Not_run} are failures — a condition that
    stopped executing is not a pass. *)
val is_failure : outcome -> bool
