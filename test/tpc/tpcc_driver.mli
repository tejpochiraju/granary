(** Zero-think-time TPC-C saturation driver (#500).

    N concurrent Lwt terminals issue the spec's weighted transaction mix back
    to back against a pool of engine {e workers}, until a wall-clock deadline.
    Reports per-profile throughput and latency percentiles. The headline
    figure is {b NewOrder transactions per second}; it is {b never} tpmC —
    the SQL is rewritten for granary's dialect, there are no keying or think
    times, and there is no audit or pricing disclosure.

    {2 Why a worker pool, and why granary's is one deep}

    A terminal is an Lwt task, not a process: granary is single-domain. A
    TPC-C transaction is inherently multi-statement and explicit, and a
    granary [Db.t] holds exactly {e one} explicit-transaction slot
    ([Db.begin_txn] answers ["transaction already active"] to a second
    [BEGIN]). Two terminals interleaving [BEGIN]/[COMMIT] on one handle would
    not merely error — the second terminal's [COMMIT] would commit the
    first's half-finished work ({b #555}). So a worker must never run two
    transactions at once, and this driver guarantees that: {!run} takes a {e list} of
    workers and serializes each one, handing a terminal a free worker for the
    duration of one transaction.

    That makes the concurrency granary actually has {e visible in the
    measurement} rather than hidden by it. With a one-worker pool — granary
    today — adding terminals cannot raise throughput, only queueing delay,
    and {!field-wait_ms} reports exactly that delay separately from
    {!field-service_ms}. A terminal sweep that flatlines is the honest result
    and the intended one; it is not a knob to tune until the number looks
    better. See the {!field-wait_ms} / {!field-service_ms} split below for
    how to read it.

    {2 Determinism}

    Every terminal draws from its own {!Tpc_rand.t}, seeded from
    {!field-seed} and the terminal's index, so the mix a terminal issues does
    not depend on how the scheduler interleaved it with its peers. What a run
    is {e not} is reproducible in outcome: a saturation run is bounded by
    wall-clock time, so the transaction {e count} varies between runs even at
    a fixed seed. The seed pins which transactions are issued, in order, per
    terminal — not how many. *)

(** How a run is configured. Built from the environment by
    {!config_from_env}; constructed directly by tests. *)
type config =
  { warehouses : int (** the TPC-C scale factor; the loaded population's *)
  ; terminals : int (** concurrent Lwt terminals *)
  ; seconds : float (** measurement interval, in wall-clock seconds *)
  ; warmup_seconds : float
    (** untimed interval run before measurement, discarded from the result.
        Zero skips warm-up entirely. *)
  ; seed : int (** base seed; terminal [i] draws from [seed + i] *)
  ; max_retries : int
    (** how many times one transaction is retried after a failure before it
        is recorded as failed. Zero means no retry. *)
  }

(** [pp_config fmt c] prints [c]'s fields on one line, for run headers and
    test failure messages. *)
val pp_config : Format.formatter -> config -> unit

(** [config_from_env ()] reads [GRANARY_TPCC_WAREHOUSES] (default 1),
    [GRANARY_TPCC_TERMINALS] (default 4), [GRANARY_TPCC_SECONDS] (default
    10), [GRANARY_TPCC_WARMUP_SECONDS] (default 1), [GRANARY_TPC_SEED]
    (default 42) and [GRANARY_TPCC_RETRIES] (default 3). Each falls back to
    its default when unset or unparseable, and every numeric knob is clamped
    to its legal range ([warehouses], [terminals] at least 1; the two
    intervals and [max_retries] at least 0), so a typo shortens a run rather
    than producing a nonsensical one. *)
val config_from_env : unit -> config

(** Executes one transaction to completion, including its own transaction
    boundary — [Tpcc_txn.run ops] is the intended instance. The driver never
    calls a worker re-entrantly: one worker runs at most one transaction at a
    time (see the module header). A raise means the transaction failed and is
    subject to retry. *)
type worker = Tpcc_txn.input -> unit Lwt.t

(** Latency percentiles of one profile, in milliseconds. Computed by
    nearest-rank over the recorded samples; all four are [0.0] when nothing
    was recorded. *)
type percentiles =
  { p50 : float
  ; p95 : float
  ; p99 : float
  ; max : float
  }

(** What one profile did during the measurement interval.

    {!field-wait_ms} and {!field-service_ms} are the load-bearing pair:
    [service_ms] is time inside the worker (the engine's own cost) and
    [wait_ms] is time a terminal spent queued for a free worker. Their sum is
    what a terminal experiences, and it is [latency]'s subject. On a
    one-worker pool [wait_ms] absorbs essentially all of the extra terminals'
    time, which is precisely the observation a terminal sweep is for. *)
type profile_stats =
  { name : string (** the profile's name, from {!Tpcc_txn.profile} *)
  ; attempted : int (** transactions started, including those later retried *)
  ; committed : int
    (** transactions that completed without raising. Includes the spec's 1%
        intentional-rollback NewOrder, which is a successful completion of
        the workload's rollback path, not a failure — {!field-rolled_back}
        counts those separately. *)
  ; rolled_back : int
    (** of {!field-committed}, how many were the intentional-rollback case *)
  ; retried : int (** retry attempts made, summed over all transactions *)
  ; failed : int (** transactions that exhausted their retries *)
  ; latency : percentiles (** end-to-end, i.e. wait + service *)
  ; mean_ms : float (** mean end-to-end latency *)
  ; wait_ms : float (** mean time queued for a free worker *)
  ; service_ms : float (** mean time executing inside a worker *)
  }

(** The result of one measurement interval. *)
type result =
  { config : config
  ; workers : int (** the worker-pool depth the run used *)
  ; elapsed_s : float
    (** measured wall clock, from the first terminal starting to the last
        finishing. Not exactly {!field-seconds}: a transaction in flight when
        the deadline passes runs to completion. Every rate in this module is
        computed against this, not against the requested interval. *)
  ; per_profile : profile_stats list (** in {!Tpcc_txn.all} order *)
  ; errors : (string * int) list
    (** distinct failure texts and their counts, most frequent first, so a
        run that fails does not merely report a number *)
  }

(** [tps stats ~elapsed_s] is [stats.committed] divided by [elapsed_s], or
    [0.0] when [elapsed_s] is not positive. *)
val tps : profile_stats -> elapsed_s:float -> float

(** [new_order_per_sec r] is the headline metric: committed NewOrder
    transactions per second over {!field-elapsed_s}. [0.0] if no NewOrder
    profile ran. This is {b not} tpmC and must never be labelled as such. *)
val new_order_per_sec : result -> float

(** [percentiles samples] is the nearest-rank p50/p95/p99 and maximum of
    [samples] (in milliseconds). [samples] is not mutated — it is copied
    before sorting. All zeros for an empty array. *)
val percentiles : float array -> percentiles

(** [run config ~workers] runs the mix for [config.warmup_seconds] untimed,
    then for [config.seconds] measured, and returns the measured interval's
    statistics.

    Each of [config.terminals] terminals loops: draw a profile by the spec's
    weights ({!Tpcc_txn.pick}), draw its input ({!Tpcc_txn.gen_input}),
    acquire a free worker, and execute. A raise from the worker is retried on
    a {e freshly drawn} input up to [config.max_retries] times — redrawing
    rather than replaying, because a deterministic failure would otherwise
    consume every retry to no purpose while a contention failure is equally
    well served by the next transaction. Retries are counted, never folded
    silently into throughput.

    Raises [Invalid_argument] if [workers] is empty or [config.terminals] is
    below 1. *)
val run : config -> workers:worker list -> result Lwt.t

(** [summary r] is the multi-line human-readable report: the configuration,
    a per-profile table, the NewOrder/sec headline, and — when the pool is
    one worker deep — the note recording that the terminal count cannot raise
    throughput on it. Written for stderr, alongside the CSV on stdout. *)
val summary : result -> string

(** The CSV header for {!csv_rows}. *)
val csv_columns : string list

(** [csv_rows r ~host ~engine] is one rendered CSV line per profile, matching
    {!csv_columns}. *)
val csv_rows : result -> host:string -> engine:string -> string list
