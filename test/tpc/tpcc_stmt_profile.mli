(** Per-statement {e service-time} attribution for the TPC-C harness (#714).

    granary serialises writers: [Rwlock] lets many readers run concurrently and
    never blocks them, and only writers exclude each other.  A TPC-C
    transaction is explicit and multi-statement, and {!Granary.Db.begin_txn}
    takes the writer lock at [BEGIN], so shrinking a transaction's elapsed time
    is what raises throughput.  This module attributes that elapsed time across
    the statement shapes the profiles issue, so the work of shrinking it can
    start from a measurement.

    {b What is measured is service time — elapsed wall clock from call to
    promise resolution — and the in-lock / out-of-lock split is {e not}
    measured and cannot be seen from here.}  Two places in the engine make that
    split real, and both cut across the largest rows:

    - [commit_wal] releases the writer lock {e before} it fsyncs
      ([lib/store/store.ml:2166] precedes [group_commit_sync]), and the default
      [sync_mode] is [`Full], so a large and unknown share of a [COMMIT]'s time
      is held outside the critical section — and [group_commit_sync] coalesces
      it across concurrent committers, so it is the part that does {e not}
      serialise.
    - A [BEGIN] that finds the lock held is {e waiting}, not working, and the
      profiler cannot tell that from work.

    Do not read the output as an attribution of lock-hold time.  Establishing
    that split needs instrumentation inside [Store] that does not exist.

    {b Read the numbers only from a run at [GRANARY_TPCC_TERMINALS=1].}  One
    terminal removes queueing behind a {e sibling terminal}, which is necessary:
    with more terminals the figures silently absorb inter-terminal contention,
    which the sweep in [docs/benchmarks/BENCHMARKS-TPCC.md] reports and which
    this module cannot separate.  It does {e not} establish "in-lock work", nor
    even "no lock wait" — one terminal is not one writer, because
    [maybe_autockpt_after_commit] dispatches the autocheckpoint through
    [Lwt.async] and that fiber takes the writer lock with nobody awaiting it.

    The accumulator itself is unconditional — {!record} always records.  The
    on/off gate is {!enabled}, consulted at the single instrumentation site in
    {!Tpcc_txn.run}, which keeps the arithmetic here directly testable. *)

(** [true] when [GRANARY_TPCC_STMT_PROFILE] is set to a non-empty string.

    Read {e once}, at module initialisation.  Once rather than per call so the
    disabled path costs one bool test, and so a mid-run environment change
    cannot produce a half-profiled run. *)
val enabled : bool

(** [record ~profile ~sql ~rows ~secs] adds one statement observation.

    [profile] must be one of {!Tpcc_txn.all}'s [name] fields, because
    {!summaries} joins on it against [Tpcc_driver.profile_stats.name].  [sql]
    is the {e raw, unrendered} shape string — the same key
    {!Tpcc_conn}'s statement cache uses (#697) — so that one shape accumulates
    one entry rather than one per distinct parameter value.  [rows] is the
    number of rows a query returned, and [0] for a non-query. *)
val record : profile:string -> sql:string -> rows:int -> secs:float -> unit

(** [reset ()] discards every observation.  Called between the driver's
    warm-up and measured windows so cold-cache first touches and the one-off
    [Db.prepare] per shape do not contaminate steady-state means. *)
val reset : unit -> unit

(** One accumulated statement shape within one profile.  [total_ms] and
    [mean_ms] are milliseconds; [pct_of_profile] is [total_ms] as a percentage
    of every statement recorded under the same [profile]. *)
type entry =
  { profile : string
  ; sql : string
  ; calls : int
  ; rows : int
  ; total_ms : float
  ; mean_ms : float
  ; pct_of_profile : float
  }

(** [ranked ()] returns every entry, costliest profile first and costliest
    statement first within each profile.  Ties break on the SQL text, so the
    order is stable across runs rather than dependent on hash-table iteration
    order. *)
val ranked : unit -> entry list

(** One {e family} of statement keys: several {!entry} values within one profile
    whose SQL differs only in an embedded number, rolled up.  [shape] is the SQL
    with every maximal run of digits replaced by a single ['#'], [members] is
    how many distinct keys were merged (always [> 1]; see {!families}), and
    [calls], [rows], [total_ms] and [pct_of_profile] are the members' totals. *)
type family =
  { profile : string
  ; shape : string
  ; members : int
  ; calls : int
  ; rows : int
  ; total_ms : float
  ; pct_of_profile : float
  }

(** [families ()] rolls up statement keys that differ only in an embedded
    number, costliest profile first and costliest family first within it.

    {!record} keys on the raw SQL, which is right for a prepared shape and wrong
    for a {e generated} one: {!Tpcc_txn.stock_select_sql} interpolates the
    district into a {e column name} ([s_dist_%02d]), so NewOrder's stock read
    arrives as ten keys and {!ranked} shows ten rows of 0.39-0.79% where the
    shape is really 326.7 ms over 3264 calls — 5.85% of the profile, 4th
    overall.  A reader who ranks by row understates it by its fan-out factor,
    and nothing in the per-key table reveals that.

    Only families of {e more than one} key are returned, so a run whose shapes
    are all constant gets an empty list and {!report} prints no such section.

    The rollup is {e reported}, never folded back into {!ranked} or {!to_csv}:
    the per-key rows are the measurement and this is an interpretation of it,
    and the digit rule is a heuristic — two statements differing only in a
    numeric {e literal} are distinct shapes to the planner but merge here.
    Consuming the CSV and re-applying the rule is therefore always possible;
    the reverse would not be. *)
val families : unit -> family list

(** Per-profile coverage: how much of the driver's own measured service time
    the recorded statements account for.  [attributed_pct] is
    [statements_total_ms] as a percentage of [driver_service_ms], and is
    deliberately {e not} clamped — a figure above 100 is a real signal, and a
    figure well below it says the cost is driver or scheduling overhead rather
    than engine work, which is the difference between a table worth acting on
    and one worth discarding.

    A [summary] reads other than 100 for three reasons:

    - {b Genuinely partial coverage.} The recorded statements are a real
      subset (or superset, under clock skew or a driver window narrower than
      the recorded statements) of the profile's service time — the ordinary,
      expected case.
    - {b Retries and failures inflate the numerator without touching the
      denominator.} {!record} fires on every attempt, including one that
      raises and one that is then retried ([Tpcc_driver.attempt] re-draws and
      recurses, and each retry issues its own [BEGIN]), while
      [Tpcc_driver.stats_of_acc] divides [driver_service_ms] by the count of
      {e successful} attempts only — so a failed or retried transaction
      inflates [statements_total_ms] without inflating [driver_service_ms].
      Checking [BEGIN] against [COMMIT] + [ROLLBACK] does {e not} detect this:
      {!Tpcc_txn.with_rollback} issues its [ROLLBACK] through the same
      instrumented [ops], so a failed attempt still contributes one [BEGIN]
      and one [ROLLBACK] and the equality survives while the numerator is
      inflated — only a failure of the [COMMIT] itself would break it. The
      exact check is instead: every attempt issues exactly one [BEGIN], and
      [Tpcc_driver.attempt] retries by recursing into a fresh attempt with its
      own [BEGIN], so [BEGIN]'s call count equals the driver's own [committed]
      count ([Tpcc_driver.profile_stats.committed], from the driver's summary
      output — not the CSV — and recoverable as
      [driver_service_ms / service_ms]) if and only if no attempt failed or
      was retried. In the published run this holds: [new_order] has 330
      [BEGIN] calls and 5598.118 / 16.964 = 330 committed, so that run is
      unaffected.
    - {b A zero denominator with a non-zero numerator renders as
      {!Float.infinity}, never [0.0].} [driver_service_ms] is [0.0] both when
      the profile is simply absent from a caller's [~service_ms] argument (see
      {!summaries} below) and when the driver never reached
      {!Tpcc_driver.record_success} for that profile — e.g. every attempt
      raised and exhausted [GRANARY_TPCC_RETRIES] — while
      [statements_total_ms] can still be non-zero, because {!record} fired on
      every one of those attempts' statements regardless of whether the
      attempt itself succeeded. That profile is {e entirely} unaccounted, and
      reporting [0.0] would read as "nothing to see" instead of "everything
      is missing". [statements_total_ms] is [0.0] too only when {!record}
      never fired for the profile at all — but then the profile does not
      appear in {!summaries}'s result at all (it is derived from {!record}'s
      accumulator, see below), so this combination cannot arise in practice;
      [attributed_pct] would stay [0.0] in that case. *)
type summary =
  { profile : string
  ; statements_total_ms : float
  ; driver_service_ms : float
  ; attributed_pct : float
  }

(** [summaries ~service_ms] pairs each profile's recorded statement time with
    that profile's total driver service time, looked up by name from
    [service_ms].  A profile absent from [service_ms] gets a
    [driver_service_ms] of [0.0]; see {!summary} for what [attributed_pct]
    then becomes rather than dividing by zero.  Ordered by profile name. *)
val summaries : service_ms:(string * float) list -> summary list

(** [report ?lock ~service_ms ()] renders {!ranked}, then {!families} when it is
    non-empty, then [lock] when given, then {!summaries}, as a human-readable
    block for stderr.  Returns a one-line notice (with [lock] still rendered
    before it) when nothing was recorded.

    [lock] is the writer-lock accounting for the same measured interval,
    from {!Tpcc_conn.lock_stats} — the split this module's own numbers cannot
    show, since service time spans the lock in both directions.  Omitted for an
    engine that has no such accounting, which is every engine but granary. *)
val report
  :  ?lock:Granary_store.Lock_stats.report
  -> service_ms:(string * float) list
  -> unit
  -> string

(** [to_csv ?lock ~path ~service_ms ()] writes CSV tables to [path], separated
    by blank lines: {!ranked} first, then {!summaries}.  Several tables in one
    file because they are one measurement, and separating them invites reading
    the per-statement table without its own coverage figure.

    [lock] (#718) appends three more: one row per writer-lock acquisition site,
    then the contention matrix, then an integrity row.  The site rows repeat
    [clock_installed] rather than stating it once, because it is what
    distinguishes "nothing waited" from "nothing was measured" and a row of
    this file is read on its own — cut out by grep or awk — more often than the
    file is read whole. *)
val to_csv
  :  ?lock:Granary_store.Lock_stats.report
  -> path:string
  -> service_ms:(string * float) list
  -> unit
  -> unit
