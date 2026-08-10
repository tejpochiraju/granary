(** TPC-C transaction profile catalogue and input generation (#500).

    The spec defines five transaction profiles, each with a fixed mix weight
    and its own input parameters. This module is the catalogue and the
    per-profile input generator; the SQL control flow that executes an input
    against granary is Task 7, not here.

    Determinism matters here exactly as it does in {!Tpcc_gen}: every
    {!Tpc_rand} draw in {!gen_input} is bound to its own [let], in the order
    it is meant to happen, never combined inline as operands of one
    expression (record field, tuple element, function argument) — OCaml
    leaves that evaluation order unspecified, and each draw advances the
    generator, so an unspecified order would make a seed no longer pin the
    workload across toolchains. *)

(** How faithfully a profile's transaction could be expressed in granary's SQL
    dialect. Mirrors {!Tpch_queries.verdict}. *)
type verdict =
  | Native (** runs on granary as the spec writes it; the SQL is untouched *)
  | Rewritten of string (** runs after a transformation; the string explains it *)
  | Skipped of string
  (** cannot be expressed; the string names the missing capability and cites
      the Forgejo issue tracking it *)

(** The five TPC-C transaction profiles. *)
type kind =
  | New_order
  | Payment
  | Order_status
  | Delivery
  | Stock_level

(** A catalogued profile: its kind, name, spec mix weight, and portability
    verdict. *)
type profile =
  { kind : kind
  ; name : string (** lower_snake_case, e.g. ["new_order"] *)
  ; weight : int (** the spec's mix weight; the five weights sum to 100 *)
  ; verdict : verdict
  }

(** All five profiles, in spec order, with their spec weights (New_order 45,
    Payment 43, Order_status 4, Delivery 4, Stock_level 4).

    Every {!field-verdict} here is measured, not assumed: [test_tpcc_smoke.ml]
    runs each profile against a loaded W=1 granary database and checks the
    four clause-3.3 consistency conditions afterwards. New_order, Payment,
    Order_status and Delivery are {!Native} — they run the spec's statements
    untouched. Stock_level is {!Rewritten}, and for one reason only: the spec's
    comma-join is spelled [INNER JOIN], since granary's FROM clause takes a
    single table plus explicit joins ({b #486}). It is the only profile with a
    join at all, which is why it is the only one carrying a rewrite. It used to
    carry a second one — [COUNT(DISTINCT s_i_id)] was a granary parse error
    ({b #491}), so the engine returned the deduplicated ids and the count was
    taken client-side — but #491 is fixed and the distinct count now runs in
    the engine. No profile is {!Skipped}. *)
val all : profile list

(** [verdict_label v] is the CSV token: ["native"], ["rewritten"], or
    ["skipped"]. The tokens are comma-free so they need no CSV quoting. *)
val verdict_label : verdict -> string

(** [pick r profiles] draws one profile from [profiles], weighted by
    {!field-weight} among those whose {!field-verdict} is not {!Skipped}.
    Excluding [Skipped] profiles and drawing against only the runnable
    survivors' weights keeps their ratios to one another correct — drawing
    against the original weights would silently produce a mix with a hole in
    it where the skipped profile's share went unused. Raises
    [Invalid_argument "Tpcc_txn.pick: no runnable profile"] if every profile
    in [profiles] is [Skipped]. *)
val pick : Tpc_rand.t -> profile list -> profile

(** One order line requested by a {!New_order} transaction. *)
type new_order_line =
  { ol_i_id : int
  ; ol_supply_w_id : int
  ; ol_quantity : int
  }

(** The item id {!gen_input} substitutes into the last line of the spec's 1%
    intentional-rollback {!New_order}: one past {!Tpcc_gen.items}, so it
    matches no generated row and the transaction reaches its [ROLLBACK]
    through the same path a real miss would. Derived from {!Tpcc_gen.items}
    rather than written as a literal — a change to the item cardinality must
    move this with it, or the "invalid" id starts matching a real item and
    the rollback path stops being exercised at all. Exported so tests and
    drivers can build that case without re-deriving the number. *)
val invalid_item_id : int

(** How a {!Payment} or {!Order_status} transaction selects its customer:
    directly by id, or by last name (the spec's 60% case, resolved against
    the population by a lookup on the name). *)
type customer_selector =
  | By_id of int
  | By_last_name of string

(** Parameters for one transaction, one variant per profile. All fields are
    concrete — no [Obj.t], no association lists — so Task 7 can pattern-match
    directly on the shape it needs. *)
type input =
  | New_order_input of
      { w_id : int
      ; d_id : int
      ; c_id : int
      ; lines : new_order_line list
        (** always at least one line; the spec's ol_cnt is [List.length lines] *)
      ; rollback : bool
        (** the spec's 1% intentional rollback case: true means the last line's
          [ol_i_id] is invalid by construction, which must make the whole
          transaction roll back rather than partially apply *)
      }
  | Payment_input of
      { w_id : int
      ; d_id : int
      ; customer_w_id : int (** the warehouse owning the selected customer *)
      ; customer_d_id : int (** the district owning the selected customer *)
      ; customer : customer_selector
      ; amount : float
      }
  | Order_status_input of
      { w_id : int
      ; d_id : int
      ; customer : customer_selector
      }
  | Delivery_input of
      { w_id : int
      ; carrier_id : int
      }
  | Stock_level_input of
      { w_id : int
      ; d_id : int
      ; threshold : int
      }

(** [pp fmt input] prints [input]'s constructor and fields, for debugging and
    for readable test-failure messages (Task 7's mock driver in particular). *)
val pp : Format.formatter -> input -> unit

(** The three NURand run constants of TPC-C 2.1.6.1, one per use. The spec
    chooses them {e independently} — a single shared constant is not what it
    says, and for [c_last] is precisely what it forbids. *)
type run_constants =
  { nurand_c_id : int
    (** [C] for the [c_id] draw, [A = 1023]; any value in [\[0,1023\]] *)
  ; nurand_ol_i_id : int
    (** [C] for the [ol_i_id] draw, [A = 8191]; any value in [\[0,8191\]] *)
  ; nurand_c_last : int
    (** [C] for the [c_last] draw, [A = 255]. Constrained: the {e run}
        constant must differ from the {e load} constant ({!Tpcc_gen.c_load})
        by a delta in [\[65,119\]], excluding 96 and 112, so that the run's
        hot surnames do not coincide with the load's. See
        {!default_run_constants}. *)
  }

(** The delta this module puts between {!Tpcc_gen.c_load} and the [c_last] run
    constant: 85. Any value in [\[65,119\]] except 96 and 112 satisfies the
    spec; 85 is mid-range and far from both exclusions, so a future change to
    {!Tpcc_gen.c_load} cannot drift the delta onto one of them. *)
val c_last_run_delta : int

(** The run constants this harness uses. [nurand_c_last] is {!Tpcc_gen.c_load}
    offset by {!c_last_run_delta}, with the sign chosen to stay inside [\[0,255\]];
    [nurand_c_id] and [nurand_ol_i_id] are arbitrary but fixed values in
    their ranges.

    Fixed rather than drawn from the workload's own {!Tpc_rand.t}: a constant
    drawn from that stream would depend on how many draws preceded it, so an
    unrelated change to a profile's draw order would silently retarget the hot
    set and a seed would no longer pin the workload. *)
val default_run_constants : run_constants

(** [gen_input r ~warehouses ~constants profile] draws the parameters for one
    transaction of [profile]'s kind, per the spec's per-profile rules:

    - {!New_order}: [w_id] uniform in [\[1, warehouses\]]; [d_id] uniform
      [\[1,10\]]; [c_id] via [nurand ~a:1023 ~x:1 ~y:3000 ~c:constants.nurand_c_id]; a
      line count uniform in [\[5,15\]]; each line's item id via
      [nurand ~a:8191 ~x:1 ~y:100000 ~c:constants.nurand_ol_i_id], its [supply_w_id]
      remote
      (a warehouse other than [w_id]) in 1% of lines when [warehouses > 1],
      else always [w_id], and its quantity uniform [\[1,10\]]; and a 1%
      chance of [rollback = true], which invalidates the last line's item id
      to exercise the rollback path.
    - {!Payment}: [w_id], [d_id] as above; the customer selected by id (40%,
      via the same NURand as New_order's [c_id]) or by last name (60%, via
      [last_name (nurand ~a:255 ~x:0 ~y:999 ~c:constants.nurand_c_last)]); a remote
      customer warehouse/district in 15% of transactions when
      [warehouses > 1], else always [w_id]/[d_id]; [amount] uniform
      [\[1.0, 5000.0\]] to 2 decimals.
    - {!Order_status}: [w_id], [d_id]; customer by id (40%) or last name
      (60%), as in Payment.
    - {!Delivery}: [w_id]; [carrier_id] uniform [\[1,10\]].
    - {!Stock_level}: [w_id], [d_id]; [threshold] uniform [\[10,20\]].

    Every bound written as a number above is spelled in the implementation as
    the corresponding {!Tpcc_gen} constant — [\[1,10\]] for a district id is
    {!Tpcc_gen.districts_per_warehouse}, [y = 3000] is
    {!Tpcc_gen.customers_per_district}, [y = 100000] is {!Tpcc_gen.items} —
    so that changing the generated population retargets the workload with it
    rather than leaving the two silently disagreeing.

    [constants] are the run constants; pass {!default_run_constants} unless
    deliberately varying them.

    An earlier version of this signature took one shared [constant_c] and
    claimed it "must be the same NURand [C] the dataset was generated with …
    or the transactions' customer lookups target a distribution the loaded
    data does not actually follow". {b That claim was false}, and it inverted
    the spec. False, because {!Tpcc_gen} assigns [last_name (c_id - 1)] to the
    first 1,000 customers of every district, so all 1,000 surnames exist in
    every district and {e any} [c_last] run constant resolves to real rows.
    Inverted, because 2.1.6.1 requires the run constant to DIFFER from the
    load constant by the delta rule above: the point is precisely that the
    run's hot set must not coincide with the load's. *)
val gen_input
  :  Tpc_rand.t
  -> warehouses:int
  -> constants:run_constants
  -> profile
  -> input

(** One SQL statement with its positional parameters. Parameters are never
    concatenated into [sql] by the profiles themselves — an engine that binds
    them does so directly, and one driven by literal SQL goes through
    {!render}. *)
type stmt =
  { sql : string (** the statement text, with [?] for each parameter *)
  ; params : Tpc_value.t list (** one value per [?], in order *)
  }

(** One result row, each column rendered by the engine's canonical text
    conversion — the same shape {!Bench_report.ENGINE.query_rows} returns. *)
type row = string list

(** The engine operations a transaction profile needs. Keeping the profiles
    behind this record puts all the control flow in this library, where it is
    unit-testable against a recording mock, and keeps it out of the benchmark
    driver. *)
type ops =
  { query : stmt -> row list Lwt.t (** run a statement and read back its rows *)
  ; exec : stmt -> unit Lwt.t (** run a statement for effect *)
  }

(** [count_placeholders sql] counts the [?] characters in [sql]. Exposed so a
    caller that binds parameters directly, rather than substituting them
    through {!render}, can still check its own arity against the same count
    {!render} uses. *)
val count_placeholders : string -> int

(** [render s] substitutes [s.params] into [s.sql], left to right, each
    rendered by {!Tpc_value.literal} — for engines driven by literal SQL
    rather than bound parameters.

    Raises
    [Invalid_argument "Tpcc_txn.render: %d placeholders but %d parameter(s)"]
    when the [?] count and the parameter count differ. A mismatch must raise
    rather than substitute what it can: a silently short-substituted statement
    is a different, still-runnable query, which would quietly measure the
    wrong thing. Every [?] in [sql] counts, including one inside a string
    literal — no profile in this module writes such a literal. *)
val render : stmt -> string

(** Raised when a statement that the spec guarantees returns a row returned
    none, or returned a column that will not parse. The payload names the
    lookup and the column.

    Every value a profile reads back and feeds into a later statement is
    required in this sense. Substituting a default instead would not keep a
    drifted population running — it would turn a broken read into a plausible
    number and commit it: a missing [d_next_o_id] defaulting to 1 inserts an
    order at a colliding id after the counter was already bumped, and a
    missing [c_id] defaulting to 0 moves [w_ytd] and [d_ytd] while crediting
    nobody, which {!Tpcc_check}'s condition 1 cannot see because both columns
    moved together.

    The one genuinely absent value — Delivery's [MIN(no_o_id)] over a district
    with no undelivered order — is SQL NULL by design and never raises; it is
    the profile's "nothing to deliver" case. That NULL is the {e only}
    absence tolerated there: [MIN] always returns exactly one row, so a
    {e zero-row} result means the query broke and raises like any other
    required read. Conflating the two would let Delivery commit an empty
    transaction, report success at full throughput, and leave all four
    consistency conditions holding while delivering nothing. *)
exception Missing_value of string

(** [run ops input] executes one transaction of [input]'s profile through
    [ops].

    The transaction boundary belongs to the profile, not to the caller: [run]
    issues its own [BEGIN] and [COMMIT] (or [ROLLBACK]) through [ops.exec].
    {!Delivery} is the exception in shape — it processes ten districts, each
    in its own transaction, so it issues ten [BEGIN]/[COMMIT] pairs rather
    than one enclosing pair.

    Between a [BEGIN] and its [COMMIT] this function awaits nothing but
    [ops.query] and [ops.exec]: granary holds a non-reentrant single-writer
    lock for that whole window, so an unrelated await there would turn writer
    contention into a permanent hang rather than an error.

    A {!New_order_input} whose last line carries the spec's deliberately
    invalid item id finds no [item] row, issues [ROLLBACK], and returns
    normally — the 1% rollback path is part of the workload, not a failure.

    If any statement fails — an [ops] error, or {!Missing_value} — the profile
    issues [ROLLBACK] and then re-raises. The transaction boundary belongs to
    the profile, so the failure path does too: letting an exception out with
    the transaction still open would leave granary's non-reentrant writer lock
    held, which is the same permanent hang described above by another route.
    A failure of that [ROLLBACK] is swallowed, so the original exception is
    what the caller sees. Delivery rolls back only the district's own
    transaction; the districts already committed stay committed. *)
val run : ops -> input -> unit Lwt.t
