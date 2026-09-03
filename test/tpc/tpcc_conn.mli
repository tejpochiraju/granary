(** A granary connection the TPC-C driver can drive asynchronously (#500).

    {!Granary_engine} cannot serve the driver: every one of its operations
    calls [Lwt_main.run] internally, and {!Tpcc_driver.run} is itself an Lwt
    computation, so using it inside a terminal would nest [Lwt_main.run] in
    [Lwt_main.run]. This module wraps the same [Db.t] the other way round —
    Lwt-native operations, with the synchronous {!Bench_report.ENGINE} view
    kept only for the load phase, which runs before any terminal exists.

    One connection is one [Db.t] and therefore one explicit-transaction slot.
    Handing the same connection to two of {!Tpcc_driver.run}'s workers would
    reintroduce exactly the hazard that driver's pool exists to prevent — see
    its module header. *)

(** The synchronous view, used by [Tpcc_schema.Load] during the untimed load
    phase. Its [open_db ~dir] creates [tpcc.db] beneath [dir], removing any
    previous database and WAL there. *)
include Bench_report.ENGINE

(** [pp fmt t] prints the connection's database path, for debugging and test
    failure messages. *)
val pp : Format.formatter -> t -> unit

(** [ops t] is the Lwt-native operation record the transaction profiles run
    against — no [Lwt_main.run] anywhere beneath it, so it is safe inside a
    driver terminal. *)
val ops : t -> Tpcc_txn.ops

(** [worker_handle t] mints a fresh connection sharing [t]'s underlying store,
    via {!Granary.Db.create_worker_handle}: its own [explicit_txn] slot and its
    own prepared-statement cache, but the same on-disk data and the same
    single-writer [Rwlock]. Two connections returned from this function (or one
    of them and [t]) can each drive one of {!Tpcc_driver.run}'s workers without
    the collision/poison hazard a shared [Db.t] would have — see #555/#589/#632/
    #633 and the "Running explicit transactions from more than one fiber"
    section of the top-level [CLAUDE.md].

    {b Must only be called after the load phase.} DDL run on [t] is invisible
    to a handle minted before it runs (per-handle catalog), so calling this
    before {!Tpcc_schema.Load} completes would hand a terminal a connection
    that cannot see the freshly created tables.

    {b Never call [close] on the returned handle (or on [t]) while any sibling
    handle is still in use.} [Db.close] releases the shared [Store.t]
    unconditionally — it is not reference-counted — so closing any one of them
    tears down every other handle's store out from under it. Close exactly one
    connection, once, after every worker built from it is done. *)
val worker_handle : t -> t Lwt.t

(** [lock_stats t] snapshots the writer lock's wait/hold accounting (#718) for
    the store behind this connection.

    This is what separates {e holding} the engine's one global critical section
    from time merely spent inside a statement, which {!Tpcc_stmt_profile}'s
    service time cannot: [COMMIT] releases the lock before it fsyncs, and a
    [BEGIN] that finds the lock held is waiting rather than working.

    {b One call covers the whole run.}  Every connection from
    {!worker_handle} shares this one's store, the lock belongs to the store,
    and so does the accounting — there is nothing to sum across terminals, and
    calling this on two sibling connections returns the same numbers.

    Durations are real here: {!open_db} installs [Unix.gettimeofday] as the
    store's clock. *)
val lock_stats : t -> Granary_store.Lock_stats.report

(** [reset_lock_stats t] discards every observation {!lock_stats} would report,
    so a run's warm-up window does not contaminate its measured one.  Called
    from {!Tpcc_driver.run}'s [?on_measured_start] hook, at the same point it
    resets {!Tpcc_stmt_profile}. *)
val reset_lock_stats : t -> unit
