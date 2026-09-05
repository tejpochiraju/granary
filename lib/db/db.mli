(** Public API for granary — pure-OCaml in-memory SQL engine (Phase 0). *)

type t

(** #634: the invalidation cohort shared by every handle over one
    {!Granary_store.Store.t}.  {!vacuum} closes that store and swaps a rebuilt
    one into the handle that ran it; every other handle in the cohort is thereby
    dead, and is marked so its next statement is refused with an error naming
    VACUUM rather than failing later on a closed store or a stale rowid counter.
    Opaque; obtained from {!cohort} and passed to {!of_store}. *)
type store_cohort

(** Pretty-print a summary: path, active schema, savepoint depth, total changes. *)
val pp : Format.formatter -> t -> unit

(** #634: this handle's invalidation cohort, to pass to {!of_store} for a second
    handle over the same store.  {!create_worker_handle} does it for you. *)
val cohort : t -> store_cohort

(** Re-export value type for convenience. *)
type value = Granary_encoding.Row.value =
  | V_int of int64
  | V_text of string
  | V_null
  | V_real of float
  | V_blob of bytes

type row = Granary_encoding.Row.t (* value array *)

type error =
  | Parse of string
  (** SQL syntax error.  Since #487 the message carries the position — line,
          column and byte offset — and the offending token; for example
          [syntax error at line 2, column 8 (byte offset 21): unexpected token FORM].
          It deliberately names no expected-token set: the grammar resolves
          ~290 shift/reduce conflicts arbitrarily, so the automaton state at
          failure does not correspond to an honest one. *)
  | Sema of Granary_sql.Sema.error (** name/type error *)
  | Runtime of string (** unexpected internal error *)
  | History_unavailable
  (** #266: an as-of read was requested on a database opened without
          [~as_of_history:true]. *)
  | History_pruned
  (** #266: the as-of target predates the retained history floor (see
          {!history_pin}), so the snapshot is no longer available. *)

(** #239: per-query cost/stats signal for an external cost-based cache,
    re-exported from {!Granary_sql.Exec.query_stats}.  [rows_examined] is the
    rows pulled from a base table/index scan (the work signal — a full scan
    behind a selective filter reads [rows_examined = N] returning one row);
    [rows_returned] is the result size once the stream is drained; [used_index]
    is the plan-time fact that the base access is an index/rowid/FTS seek rather
    than a full scan.  [index_entries] (#546) is the number of index entries an
    index lookup walked — the work an index seek adds over the scan it replaces,
    which [rows_examined] cannot show because it counts only the table rows both
    plans end up reading.  See {!query_with_stats} / {!iter_with_stats}. *)
type query_stats = Granary_sql.Exec.query_stats =
  { mutable rows_examined : int
  ; mutable rows_returned : int
  ; mutable index_entries : int
  ; mutable used_index : bool
  }

(** Open a fresh in-memory database.  [clock] is an optional Unix-timestamp
    provider used by SQL date/time functions when invoked with the literal
    ['now'].  Omit for MirageOS-friendly cores that have no Unix dependency. *)
val open_in_memory : ?clock:(unit -> float) -> unit -> t Lwt.t

(** Open a SQL engine on any block device given as I/O callbacks.
    Use with [Granary_mirage_block.Mirage_backend.Make(B)] to build
    the callbacks from a [Mirage_block.S] device.  Pass [~n_pages:0L]
    for Mirage adapters; the adapter handles device-capacity bounds
    internally.  On success, [~close] is called later by [Db.close]; on
    {b any} [Error] return (#753/#763) [~close] is called here, once,
    before the error is returned — [Granary_store.Store.open_block] never
    wires it into a store record on that path (there is no store), so this
    is the only place it can be invoked, and skipping it would leak the
    caller's underlying handle (fd, block device, ...) on every refused
    open.  The caller must not call [~close] itself in that case.  This
    call is guarded: an exception or a rejected promise from [~close] is
    swallowed rather than surfaced, so a misbehaving [~close] cannot
    clobber the store error this function is in the middle of returning.

    [clock] and [durability] are open-options forwarded to [of_store]:
    [durability] sets the database-wide durability knob (full/batched/off)
    before the catalog is loaded.

    {b #753: [init_if_corrupt] defaults to [false].}  It is forwarded
    verbatim to {!Granary_store.Store.open_block}, whose header check cannot
    distinguish "a zeroed, never-before-used device" from "an existing
    device whose header failed to parse" — both look like a missing header
    and are the same code path.  So the choice is really "may this open
    format the device if its header is unreadable," not "is this device
    known to be corrupt":

    - [false] (the default): an unreadable header — corrupt {e or} freshly
      zeroed — is refused rather than silently reformatted.  [Db]'s
      {!error} type has no dedicated variant for this (every
      {!Granary_store.Store.error} is wrapped as [Runtime]), so the failure
      surfaces as [Error (Runtime msg)] where [msg] names
      [Header_error(both header pages corrupt)] — check the message text,
      not the variant, to distinguish this from other [Runtime] failures.
      For a persisted, already-provisioned database this is the safe
      choice: a media fault, a partial write, a wrong device path, or a
      device belonging to a different application must not be mistaken for
      "nothing here yet" and quietly wiped into an empty store that looks
      healthy to the caller.
    - [true]: {b provisioning only — destroys whatever is on the device.}
      An unreadable header is treated as fresh and the device is
      initialised.  Reach for this only at a deliberate one-time
      provisioning step (first boot of a new device), never as a
      steady-state open path, since it cannot tell "new" from "corrupt"
      any better than [false] can — it just resolves the ambiguity the
      other way.

    This is the platform-agnostic entry point.  Unix file convenience
    constructors ([open_file] / [open_file_wal]) live in the [granary.unix]
    driver library so this core carries no [unix] dependency (#170). *)
val open_block
  :  ?init_if_corrupt:bool
  -> ?geom:Granary_storage.Geometry.t
  -> ?clock:(unit -> float)
  -> ?durability:Granary_store.Store.durability
  -> read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> resize:(n_pages:int64 -> (unit, string) result Lwt.t)
  -> n_pages:int64
  -> close:(unit -> unit Lwt.t)
  -> unit
  -> (t, error) result Lwt.t

(** Wrap an already-open {!Granary_store.Store.t} as a database handle,
    loading its catalog, views, and triggers.  [file_path] records the
    on-disk path for file-backed handles so VACUUM can rebuild in place;
    omit it for in-memory / arbitrary block devices.  [durability] sets the
    database-wide durability knob (full/batched/off) on the store before the
    catalog is loaded — use this to configure the store's fsync policy at
    open time without needing a raw store handle afterwards.  Used by the
    [granary.unix] driver and by ATTACH.

    #589/#633: it is safe to call this over a store that is ALREADY open under
    another [Db.t].  The rowid allocator lives on the
    {!Granary_store.Store.t} ({!Granary_store.Store.rowid_counters}), so the two
    handles share one allocator over the one set of data trees by construction —
    there is no longer an argument to pass, and therefore none to forget.  (It
    used to be [?rowid_counters], and forgetting it cost silent row loss plus
    durable index corruption.)  {!create_worker_handle} remains the intended way
    in, because it is about more than the allocator.

    [cohort] (#634) joins the new handle to an existing handle's invalidation
    cohort, and IS still owed by any caller reaching [of_store] over a store that
    is already open: {!vacuum} closes the store and swaps in a rebuilt one, so
    every OTHER handle over that store is left pointing at a closed store.
    Handles in one cohort are invalidated together and report it through
    {!stale_after_vacuum}; a handle opened without [cohort] starts its own. *)
val of_store
  :  ?clock:(unit -> float)
  -> ?durability:Granary_store.Store.durability
  -> ?file_path:string
  -> ?cohort:store_cohort
  -> Granary_store.Store.t
  -> t Lwt.t

(** File operations the engine needs for ATTACH and VACUUM.  The core has no
    OS/filesystem dependency (#170); a platform driver (e.g. [granary.unix])
    supplies these via {!set_file_provider}.  Without a provider, ATTACH and
    VACUUM fail with a clear error. *)
type file_provider =
  { open_store :
      ?geom:Granary_store.Store.Geometry.t
      -> ?as_of_history:bool
      -> path:string
      -> unit
      -> (Granary_store.Store.t, Granary_store.Store.error) result Lwt.t
    (** [geom] (#176) is the geometry to CREATE a fresh file with — VACUUM
        passes the source's geometry so the rebuilt file keeps its page_size and
        reserved bytes.  Ignored when opening an existing file (its geometry is
        peeked from the header).  [as_of_history] (default [false]) enables the
        opened store's as-of commit log. *)
  ; remove_file : string -> unit (** best-effort unlink; ignore if absent *)
  ; rename_file : string -> string -> unit (** atomic rename over the target *)
  }

(** Install the process-wide file provider used by ATTACH and VACUUM. *)
val set_file_provider : file_provider -> unit

(** Close the database, flushing and releasing the underlying store. *)
val close : t -> unit Lwt.t

(** Create a lightweight worker handle sharing the same underlying store.
    Equivalent to [of_store (store t)] — since #633 the rowid allocator comes
    from the store itself, so the two handles share it automatically. Used by
    Jepsen-style concurrent workloads where each worker needs its own
    [explicit_txn] without exposing the raw store.

    #555: this is the supported way to run explicit transactions from more than
    one fiber. Each handle gets its own transaction slot, and because they share
    one [Store.t] they share its single-writer [Rwlock], so a second fiber's
    [BEGIN]/write {e blocks} until the first commits rather than contaminating it
    or poisoning anything. Neither handle is ever poisoned by the other —
    {!transaction_poisoned} is about two fibers sharing {e one} handle.

    {b #589 (fixed): rowid allocation is shared, so writes to any table shape are
    safe.} Each handle still gets a fresh {e catalog}, but no longer a fresh
    {e rowid allocator}: the counters travel with the store, keyed by tree id.
    Before that fix, two counters over one data tree handed out the same
    engine-assigned rowid twice and the second write silently overwrote the
    first — a lost row on a plain rowid table, and on a [TEXT PRIMARY KEY] table
    an index entry left pointing at the wrong row (wrong answers, not merely
    missing ones). Pinned across the full table-shape matrix by
    [test/test_worker_handle_589.ml].

    Its remaining limits, none of which corrupt anything:
    - DDL executed on one handle is {b invisible} to the other's schema cache.
      That one really is inherent to the per-handle catalog: create a table on
      the parent and the worker cannot see it until it is reopened.
    - ATTACHed schemas and the active-schema setting are per-handle; a worker
      handle starts with none of the parent's.
    - Reactive views ARE reconstructed for a worker handle — from the same
      persisted catalog definitions the parent's came from — but each
      handle's copy is independently rebuilt rather than shared: its delta-
      engine state is recomputed from the current base-table contents rather
      than copied, and it starts with {b none} of the parent's registered
      callbacks ({!register_view_callback} callbacks are never persisted or
      inherited). #757: what IS shared across every handle over one store,
      including a worker handle's independently-rebuilt copy, is a view
      name's {!reactive_view_generation} identity — a worker handle created
      after the parent already has view [v] live reuses the SAME generation
      the parent recorded, rather than minting a fresh one and falsely
      signalling a drop-and-recreate that never happened. What generation
      identity does {b not} give a worker handle is live visibility into a
      sibling's LATER drop-and-recreate of [v] — that is the same
      DDL-invisibility this bullet already describes, and a handle only
      picks up a sibling's recreate by re-deriving its view of the store (a
      fresh {!create_worker_handle}, or reopening).
    - Write transactions {b serialize} on the shared [Rwlock] rather than
      overlapping, and read-only transactions do not overlap a writer either.
      Genuine concurrency needs #555 option 1.
    - {b A {!vacuum} on ANY handle over this store kills every other one} (#634).
      VACUUM closes the store and swaps in a rebuilt file, which the siblings
      cannot follow. Since #634 that is loud rather than silent: the surviving
      handles are refused with an error naming VACUUM, {!stale_after_vacuum}
      reports it, and the only thing left to do with one is {!close} it and take
      a fresh worker off the handle that ran the VACUUM.

    Raises [Failure] if [t] is itself stale (see {!stale_after_vacuum}). *)
val create_worker_handle : t -> t Lwt.t

(** Number of WAL fsyncs performed since open.  Returns 0 for non-WAL
    databases.  Exposed for #77 group-commit testing. *)
val wal_sync_count : t -> int

(** Re-export of the internal-events type (#382). *)
module Event = Granary_store.Store.Event

(** Register/clear the internals-monitor observer on this database's store.
    No-op for in-memory databases.  The callback should be cheap and
    non-blocking (see {!Granary_store.Store.set_event_callback}). *)
val set_event_callback : t -> (Event.t -> unit) option -> unit

(** [tree_of_table t name] is the storage tree id backing table [name] in the
    active schema, or [None] if no such table exists or the table has no real
    on-disk tree (e.g. an ephemeral/CTE table, or a legacy zero-tid columnar
    table, which carry a negative sentinel tree id). Used by the internals
    monitor to filter page events by table (#385). *)
val tree_of_table : t -> string -> int option

(** [schema t] is a read-only projection of the active in-memory catalog backing
    [t] — table names, column names/types/constraints, and index metadata. See
    {!Schema} for the exposed surface.

    #433: this replaces the former [catalog] accessor, which handed out the
    {e live} {!Granary_catalog.Catalog.t}. That type is a read-write surface
    ([create_table], [drop_table], [add_column], [set_last_inserted_rowid],
    [store], …), so a consumer could mutate the schema — or reach the raw
    store — out of band from SQL DDL and the WAL. "Do not mutate it" was a doc
    comment, not a constraint; now it is a type.

    The projection is a {e live view}, not a snapshot: it holds the same
    catalog the handle executes against, so DDL run through SQL is visible
    through it immediately. It does {e not} survive the catalog swap {!vacuum}
    performs, so do not cache one across a VACUUM — every other handle over the
    store is dead after that anyway (#634). *)
val schema : t -> Schema.t

(** [plan t sql] parses, binds and plans [sql] against [t]'s active schema
    {e without executing it}. Read-only: nothing is written and no transaction
    is opened. It exists because planning a statement against a handle's own
    catalog was the one legitimate use of the removed [catalog] accessor that a
    schema projection cannot serve (the binder and planner take a
    [Catalog.t]) — the planner's tests inspect the chosen access path this way.
    Routing follows {!execute}: the statement is planned against the handle its
    active schema selects. *)
val plan : t -> string -> (Granary_sql.Plan.op, error) result Lwt.t

(** Rebuild the database file in place: copies every tree from the
    current file into a fresh sibling [path ^ ".vacuum-tmp"], then
    atomically renames it over the original.  This drops free-list
    pages and re-packs everything densely.

    Only works on file-backed databases (opened via the [granary.unix]
    driver); in-memory and arbitrary-block-device handles raise [Failure],
    as does any handle when no file provider is installed (see
    {!set_file_provider}).

    Must not be called inside an explicit transaction.  Any open
    prepared statements created from the previous file {b on this handle} will
    continue to work logically but observe the recompacted file.  Phase 37 /
    #120.

    {b #634: every OTHER handle over the same store dies here.}  VACUUM closes
    the store and opens the rebuilt file into {e this} handle only, so any handle
    from {!create_worker_handle} — and any statement prepared on one — is left
    over a closed store and a pre-VACUUM rowid allocator.  They are now marked
    stale: every statement on them is refused with an error naming VACUUM,
    {!stale_after_vacuum} answers [true], and {!close} on one is a safe no-op
    (the store is already closed).  There is no ROLLBACK recovery, unlike #555's
    poison — the caller must take a fresh worker handle off {e this} one.  Called
    on a handle that is itself stale, VACUUM raises [Failure].

    {b #757: reactive-view generations survive VACUUM without colliding.}
    VACUUM swaps in a brand-new underlying store, and a fresh store's
    generation counter would otherwise restart at 0 — unlike the rowid
    allocator, which reseeds itself correctly from the copied tree data, a
    generation has nothing durable to recover it from. Before the swap
    becomes visible on [t], the old store's generation bookkeeping is carried
    into the new one ([Granary_store.Store.rv_carry_over_generations]), so a
    generation minted before this VACUUM and one minted after it can never
    collide, and {!reactive_view_generation} keeps its "strictly increasing
    for this name, forever" guarantee across a VACUUM. This handle's own
    {!reactive_view_generation} answers for a view unaffected by the VACUUM
    do not change (nothing about {e this} handle's registry entries is
    touched, only the counter that mints FUTURE ones) — the DDL-visibility
    caveat above is what governs whether a SIBLING handle's later
    drop-and-recreate is visible here, exactly as it does outside a VACUUM. *)
val vacuum : t -> unit Lwt.t

(** #634: whether this handle — or any ATTACHed schema on it — has been
    invalidated by a {!vacuum} run on a {e sibling} handle over the same store.

    VACUUM rebuilds the database file and re-seats only the handle that ran it.
    Every other handle in the cohort ({!create_worker_handle}, or {!of_store}
    passed [~cohort]) still points at the store VACUUM closed, and its catalog's
    rowid counters describe the pre-VACUUM file.  Rather than let that surface
    later as a closed-store failure or a rowid collision, such a handle is marked
    stale and refuses everything: reads, writes, DDL, prepared [run]/[iter],
    [BEGIN], [COMMIT] {e and} [ROLLBACK].

    The [ROLLBACK] exemption that makes {!transaction_poisoned} recoverable
    deliberately does {e not} apply: there is no state to roll back to, the store
    is gone.  {!close} is the only supported operation on a stale handle, and it
    is a no-op on the already-closed store.  Get a fresh handle from the one that
    ran the VACUUM. *)
val stale_after_vacuum : t -> bool

(** [lock_stats t] snapshots the writer-lock wait/hold accounting (#718) for the
    store this handle sits on.

    It is the measurement that separates time spent {e holding} the engine's one
    global critical section from time merely spent inside a statement — a split
    no service-time profile can see, because [COMMIT] releases the lock before
    it fsyncs and a [BEGIN] that finds the lock held is waiting rather than
    working.  Durations are [0.] unless a clock was installed at open time (the
    [?clock] argument of {!open_file}/{!of_store}); the report's
    [clock_installed] field distinguishes that from "nothing waited".

    Under ATTACH this reports {e this} handle's store only, and does not fold
    over the attached schemas the way {!transaction_poisoned} does: each
    attached schema is a different store with a different lock, so a sum across
    them would describe no lock at all. *)
val lock_stats : t -> Granary_store.Lock_stats.report

(** [reset_lock_stats t] discards every observation {!lock_stats} would report,
    for this handle's store — so a benchmark can exclude its warm-up window from
    the measured one. *)
val reset_lock_stats : t -> unit

(** #555: whether this connection has been {e poisoned} by an overlapping
    explicit transaction — either the top-level handle or any ATTACHed schema.

    A [Db.t] holds exactly one explicit-transaction slot and every statement
    resolves its transaction from it, so two fibers sharing one handle cannot
    hold two transactions.  When a [BEGIN] arrives while another explicit
    transaction is already active on the handle, that [BEGIN] fails {e and} the
    handle is poisoned: every subsequent statement on it is rejected with a
    [Runtime] error.

    {2 What this does and does not guarantee}

    It {e narrows} the hazard; it does not close it.  While the poison is set,
    the losing fiber cannot read or write inside the winner's transaction and
    cannot commit the winner's half-finished work.  But the poison covers only
    the window between the failed [BEGIN] and the first [ROLLBACK] — and
    [ROLLBACK] is the prescribed recovery.  Once it clears, the slot is free
    again and the original contamination is reachable with the fibers exchanged:
    the recovering fiber can [BEGIN] afresh and the fiber whose transaction was
    aborted, never having been told, writes into the new one.  See #584 for the
    exact sequence, and #585 for the scoped-transaction combinator that is the
    enabling change for a real fix.

    So this is a loud failure in place of a silent one over a bounded window,
    not a safe way to share a handle.  Do not treat it as permission to run
    explicit transactions concurrently on one [Db.t] — use
    {!create_worker_handle} for that.

    {2 Recovery}

    [ROLLBACK] aborts whatever transaction was in flight and clears the poison,
    and it is the only thing that does.  Not [COMMIT] — that is exactly the
    operation that must not be allowed to succeed here — and not finalizing a
    statement.  Under ATTACH, the two routing statements that could otherwise
    have dropped a poisoned schema out from under the caller,
    [PRAGMA active_database = …] and [DETACH DATABASE], are both refused while
    the active schema is poisoned, so this stays a single-exit contract.  Any
    fiber may issue the [ROLLBACK]; the handle has no notion of transaction
    ownership, which is the limitation #585 removes.  It clears the flag even
    when no transaction remains to abort, so a handle can never be left
    permanently unusable.

    Note this dooms the {e winner's} transaction too.  The engine cannot tell
    the two fibers apart, so it cannot let [COMMIT] through without letting the
    wrong fiber's [COMMIT] through.

    {2 Under ATTACH}

    Poisoning is per-handle: each attached schema is its own [Db.t] with its own
    slot, [BEGIN] routes to the active schema, and a poisoned ["aux"] does not
    stop statements that route to ["main"].  This predicate reports whether
    {e any} of them is poisoned, so an application polling it for recovery is
    not told [false] about a genuinely poisoned connection.  Leaving a poisoned
    schema — via [PRAGMA active_database = …] or [DETACH DATABASE] — is refused,
    so the [ROLLBACK] that recovers it cannot be routed away from it; arriving at
    one stays legal.

    This does not make ATTACH safe in general.  The poison fires only on a
    {e collision}, and one [Db.t] under ATTACH legitimately holds two
    transaction slots, so switching schema mid-transaction makes a statement
    autocommit into the wrong schema with no collision and no poison.  That is
    pre-existing and tracked as #598.

    {2 Autocommit}

    Unaffected, as long as no explicit transaction is used: sharing a [Db.t]
    across fibers without [BEGIN] never poisons it. *)
val transaction_poisoned : t -> bool

(** #585: run [body] inside an explicit transaction with a scoped extent.

    [with_transaction t body] issues [BEGIN], runs [body t], and then [COMMIT]s
    on success or [ROLLBACK]s and re-raises on exception. It is the
    exception-safe replacement for hand-written
    [BEGIN] / … / [COMMIT] sequences, and — more importantly — it is the first
    place the engine has a transaction {e extent} rather than a sequence of
    unrelated statements.

    {2 Why the extent matters}

    #555/#584 are not "the slot is not locked"; they are "the engine cannot tell
    which fiber owns the slot". A statement-at-a-time API gives it nothing to
    attach an owner to: [Lwt.with_value] can only tag a fiber for the dynamic
    extent of a callback, and a bare [Db.execute db "BEGIN"] has no enclosing
    scope to be that extent. This combinator supplies one, and mints an owner
    token into it (readable via {!in_transaction_scope}). Binding every
    {e statement} to its owner is still #555 option 1's work; what the token
    decides today is listed below.

    {2 Return value}

    [Ok v] where [v] is [body]'s result, once the [COMMIT] has succeeded. [Error]
    if the [BEGIN] or the [COMMIT] failed. An exception raised by [body] is
    {e re-raised} after the rollback, never converted into [Error] — a caller
    that wants it as a value should catch it inside [body].

    {2 Nesting: refused, deliberately}

    Calling [with_transaction] again on the same handle from inside the extent —
    directly, or from a fiber spawned inside [body], which inherits the token —
    returns [Error (Runtime …)] without opening anything, without rolling
    anything back, and {b without poisoning the handle}. The outer transaction is
    untouched and the outer scope carries on normally.

    The two alternatives were rejected because each has to lie to the inner
    caller:

    - {b Savepoint.} The inner scope's "commit" would be a [RELEASE], so
      returning from it would not mean durable, and the outer scope could still
      discard all of it. Code that reads [with_transaction] as "committed when it
      returns" would be wrong in a way no type catches.
    - {b No-op join.} The inner scope's rollback-on-exception would abort the
      {e outer} transaction while returning control to code that believes only
      its own work was undone — silent, unbounded loss of the outer scope's
      writes.

    Refusal is the only answer that is true. If a partial undo point is what you
    want, [SAVEPOINT] / [RELEASE] / [ROLLBACK TO] say so explicitly.

    Note this is the one question the owner token answers precisely today: the
    token proves the caller is the fiber that opened the outer transaction, so
    the engine can refuse {e without} the #555 poison, which exists only for the
    case where it cannot tell the fibers apart.

    {2 Interaction with the #555 poison}

    Unchanged, and deliberately not routed around.

    - A second fiber calling [with_transaction] on a handle that already holds an
      explicit transaction is {e not} the nesting case (it has no token): its
      [BEGIN] fails and poisons the handle exactly as a bare [BEGIN] would. The
      combinator reports the [Error] and returns.
    - When the [BEGIN] fails, no [ROLLBACK] is issued. [ROLLBACK] is the sole
      exit from the poisoned state and it stays the {e caller's} to issue —
      this combinator must not become a second exit.
    - When the [COMMIT] fails, no compensating [ROLLBACK] is issued either. The
      arms that leave a transaction uncommittable ([#286] in-txn DDL, a violated
      deferred FK) already roll it back themselves, and on a poisoned handle the
      recovery is the caller's.
    - The rollback-on-exception path {e does} issue [ROLLBACK] — but that is the
      prescribed exit itself, applied to this scope's own transaction, not an
      additional one.

    One consequence of that last point is worth stating, because "this scope's
    own transaction" is not the whole truth: [ROLLBACK] clears the {e handle's}
    poison flag unconditionally (#555 made it unconditional so a handle can
    never be stranded), and the poison is connection state rather than
    transaction state. So if another fiber's [BEGIN] collided and poisoned the
    handle while this scope was running, and this scope's body then {e raises},
    the rollback it issues also clears the poison that fiber was told to recover
    from — and that fiber's own recovery [ROLLBACK] then answers
    ["no active transaction"]. Nothing is lost or committed wrongly; the
    recovering fiber is simply told the handle is already clean, which it is.
    Making the clear conditional would strand the handle instead, so this is
    inherited from #555 rather than introduced here.

    {2 #584 is narrowed here, not closed}

    If another fiber's [ROLLBACK] aborts this scope's transaction — whether it
    then opens its own in the freed slot or leaves it empty — the scope's owner
    token no longer matches the handle. This combinator detects that at scope
    exit and issues {b neither} [COMMIT] nor [ROLLBACK], returning [Error]
    instead: either would have acted on state that is no longer this scope's,
    which is exactly #584's failure.

    The detection holds for {e both} spellings of the displacing fiber — a bare
    [Db.execute db "BEGIN"] and a second [with_transaction] — because the token
    is invalidated by every path that ends a transaction, not merely by this
    combinator's own exit. A [BEGIN] (and [SAVEPOINT]'s auto-begin) clears it
    when it fills the slot; [COMMIT], [ROLLBACK], the internal forced rollback
    and [RELEASE]'s auto-commit clear it when they empty it. A token therefore
    never outlives the transaction it names.

    That covers the scope's own boundaries. It does {b not} cover statements
    {e inside} [body]: those still resolve the transaction from the handle's
    mutable slot, so a displaced scope's writes land in the other fiber's
    transaction before the boundary check reports the loss.

    So this is not permission to share a handle across fibers. Use
    {!create_worker_handle}, which gives each fiber its own slot and blocks on
    the shared writer lock instead of contaminating.

    {2 The body must not manage the transaction itself}

    [body] owns the statements, not the transaction. A [BEGIN] inside it fails
    and poisons. A [COMMIT] or [ROLLBACK] inside it empties the slot {e and}
    clears the scope's owner token, so the scope exit takes the displacement
    branch above: [Error], and no second [COMMIT] is issued. The body's work is
    committed (or discarded) exactly as the body asked, but the [Error] is the
    caller's only signal that the scope did not end the way it looks like it
    did. That is the intended outcome rather than a wart — the alternative would
    be a blind [COMMIT] of whatever now occupies the slot. [SAVEPOINT] /
    [RELEASE] / [ROLLBACK TO] are fine and leave the transaction in place.

    {2 ATTACH}

    The [BEGIN] routes to the active schema, as it always does, so the
    transaction belongs to that schema's sub-handle — and the owner token is
    stamped on {e that} sub-handle, not on the top-level handle the call was
    made on, because the sub-handle is the one whose slot the begin/commit/
    rollback paths maintain. Since #598 refuses a [PRAGMA active_database]
    switch while any schema holds a transaction, the routing cannot move under
    an open scope, so the handle the token lives on is stable for the whole
    extent. A consequence: nesting is refused per {e schema}, so a scope open on
    ["main"] does not by itself refuse one on an attached ["aux"] — but reaching
    ["aux"] would need the routing switch #598 already refuses. *)
val with_transaction : t -> (t -> 'a Lwt.t) -> ('a, error) result Lwt.t

(** #585: whether the {e calling fiber} is inside a {!with_transaction} extent
    that currently owns [t]'s explicit-transaction slot.

    True only when the fiber's inherited owner token equals the token stored on
    the handle the active schema routes to, so it answers [false] for a fiber
    that never entered a scope, for a fiber whose scope has exited, and for a
    fiber whose scope was displaced by another's transaction (#584). It also
    answers [false] inside a transaction opened by a bare [BEGIN] — an unscoped
    transaction has no owner to report, and a [BEGIN] arriving in the slot
    actively clears whatever token was there.

    This is the observable half of the owner token, exposed for tests and for
    library code that needs to know whether it may open a transaction or is
    already inside one. It is {e not} a general "is a transaction open" predicate;
    it says nothing about a transaction this fiber does not own. *)
val in_transaction_scope : t -> bool

(** Execute a DDL or DML statement (CREATE TABLE, INSERT, UPDATE, ...).
    Returns [Ok ()] on success, [Error e] on failure.

    {b [PRAGMA not_null_repair] is a write and belongs here (#588).} It DELETEs
    rows, but it used to be dispatched as a query, so this call answered
    ["use Exec.query for read operations"] and deleted nothing — the natural
    call for a mutating statement did nothing but error, while the read API was
    the one that destroyed data. Both entry points now perform it: [execute]
    (and {!execute_change_count}) reports the number of rows DELETED as the
    statement's change count, while {!query} streams the per-column
    [(table, column, count)] report. Its read-only half,
    [PRAGMA not_null_check], stays on {!query} alone, and a repair reached under
    {!query_as_of} — or any read-only snapshot — is refused at the statement
    level rather than failing inside the storage layer. *)
val execute : t -> string -> (unit, error) result Lwt.t

(** Like [execute], but returns the rows-affected count.  For DDL the
    count is [0]; for INSERT it is [1]; for UPDATE it is the number of
    rows whose contents were modified. *)
val execute_change_count : t -> string -> (int, error) result Lwt.t

(** Execute a query (SELECT).
    Returns [Ok stream] on success, [Error e] on failure.
    The stream is lazy — rows are produced on demand. *)
val query : t -> string -> (row Lwt_stream.t, error) result Lwt.t

(** #239: like {!query}, but also returns a per-query cost/stats record
    ([rows_examined], [rows_returned], [used_index]) for an external cost-based
    cache.  The record's counters are populated as the returned stream is
    consumed — read them once it is fully drained.  See
    {!Granary_sql.Exec.query_stats}. *)
val query_with_stats : t -> string -> (row Lwt_stream.t * query_stats, error) result Lwt.t

(** [query_as_of t target sql] (#266) runs a read-only [sql] query against the
    database as it existed at [target] (a past txn id or timestamp).  Requires
    the db to have been opened with [~as_of_history:true]; otherwise the result
    carries [History_unavailable].  As-of reads require an active retention floor
    set via {!history_pin}: with no floor every target resolves to
    [History_pruned], and the floor is not retroactive — pin before the writes you
    want to retain across.  [History_pruned] also when [target] predates the
    retained floor.  Schema is read at HEAD: a query whose table had DDL after
    [target] may misinterpret older rows (schema-as-of is out of scope, #266).
    As-of resolves against whichever database the statement routes to — MAIN, or
    the active ATTACHed schema (#412).  A single query cannot span MAIN and an
    attached db at one [target]: their commit orders are independent, so routing
    consults exactly one store's history. *)
val query_as_of
  :  t
  -> Granary_store.History.target
  -> string
  -> (row Lwt_stream.t, error) result Lwt.t

(** [history_pin ?schema t ~txn_id] (#266/#412) sets the retention floor at
    [txn_id] for [schema] (default ["main"]; otherwise a currently ATTACHed
    schema): the committed root at or before [txn_id] is retained so
    {!query_as_of} can reach it.  No-op when as-of history is not enabled.
    @raise Invalid_argument if [schema] is neither ["main"] nor attached. *)
val history_pin : ?schema:string -> t -> txn_id:int64 -> unit

(** [history_floor ?schema t] (#266/#412) is [schema]'s current retention floor
    txn id, or [None] when no floor is pinned (or as-of history is not enabled).
    @raise Invalid_argument if [schema] is neither ["main"] nor attached. *)
val history_floor : ?schema:string -> t -> int64 option

(** [history_release ?schema t] (#266/#412) clears [schema]'s retention floor,
    allowing pruning of previously pinned historical roots.
    @raise Invalid_argument if [schema] is neither ["main"] nor attached. *)
val history_release : ?schema:string -> t -> unit

(** [history_log ?schema t] (#266/#412) is [schema]'s recorded commit history
    (the [<path>.aslog] sidecar), oldest first.  Empty when as-of history is not
    enabled.
    @raise Invalid_argument if [schema] is neither ["main"] nor attached. *)
val history_log : ?schema:string -> t -> Granary_store.History.record list Lwt.t

(** #240: the set of user tables whose {e observable contents} a statement
    changed — including tables touched indirectly by triggers and FK cascades,
    and (since #405) by row-rewriting or destructive DDL.  Sorted and
    deduplicated; internal/system tables are excluded.  Enables an external read
    cache to invalidate exactly the tables whose answers moved.

    {b Reported.}

    - Row-level DML: INSERT / UPDATE / DELETE, upserts, FK cascades, trigger
      bodies, columnar and FTS writes, and [PRAGMA not_null_repair].
    - [DROP TABLE] — marks the dropped table.
    - [ALTER TABLE], in all four forms — [DROP COLUMN] (which physically
      rewrites every stored row), [ADD COLUMN] (which leaves the bytes alone but
      widens every row a reader sees, so a cached result has the wrong arity),
      [RENAME COLUMN], and [RENAME TABLE] (which marks {e both} the old name and
      the new one).

    {b Not reported}, because no existing table's contents change: [CREATE
    TABLE] and [CREATE VIRTUAL TABLE] (the new table is empty); [CREATE INDEX]
    and [DROP INDEX] (query answers are identical — only plans differ); view
    and trigger DDL; [VACUUM] (a physical rebuild that preserves every row);
    [ATTACH] / [DETACH].

    {b Over-approximate, deliberately.}  Like the rest of this accumulator the
    marks are {e not} undone by a [ROLLBACK] (#666), so a rolled-back statement
    may still report what it touched.  A superfluous invalidation costs one
    re-read; a missing one serves a wrong answer from cache. *)
type dirty_tables = string list

(** #417 Phase 0: one row-level mutation in the delta feed.  [rowid] is the row's
    int64 rowid; [row]/[old_row]/[new_row] are the full {!row}s involved.
    Re-exported from {!Granary_sql.Exec.row_change}. *)
type row_change = Granary_sql.Exec.row_change =
  | Inserted of
      { rowid : int64
      ; row : row
      }
  | Deleted of
      { rowid : int64
      ; row : row
      }
  | Updated of
      { rowid : int64
      ; old_row : row
      ; new_row : row
      }

(** #417: the per-table row-level deltas a write statement produced — [(table,
    changes)] pairs sorted by table name, each table's changes in application
    order.  Same name/scope rules as {!dirty_tables} for user tables and internal
    [sqlite_…] objects, but this feed carries {e row} deltas only: DDL is
    reported by {e name} through {!dirty_tables} (#405) and contributes no
    entries here. *)
type table_changes = (string * row_change list) list

(** Like {!execute}, but also returns the {!dirty_tables} the statement mutated.
    The list is empty for a no-op write (e.g. [INSERT OR IGNORE] that inserts
    nothing) and for DDL that leaves every existing table's contents alone
    ([CREATE TABLE], [CREATE INDEX] / [DROP INDEX], view and trigger DDL); it is
    {e non}-empty for [DROP TABLE] and every [ALTER TABLE] form.  See the
    {!dirty_tables} scope note for the exact split. *)
val execute_with_dirty : t -> string -> (dirty_tables, error) result Lwt.t

(** Like {!execute_change_count}, but also returns the {!dirty_tables} the
    statement mutated alongside the rows-affected count. *)
val execute_change_count_with_dirty
  :  t
  -> string
  -> (int * dirty_tables, error) result Lwt.t

(** #417: like {!execute_with_dirty}, but returns the row-level {!table_changes}
    delta feed (rowid + old/new rows) instead of just the table names — the input
    an incremental view-maintenance layer consumes.  Empty for no-op writes, and
    empty for DDL — which {!execute_with_dirty} reports by name (#405) but which
    produces no row-level deltas.

    {b Phase 0 coverage.}  The feed covers ordinary rowid-table DML: INSERT,
    UPDATE, DELETE, REPLACE (delete-old + insert-new), UPSERT [DO UPDATE], and
    rows mutated indirectly by FK cascades and triggers.  It does {b not} yet
    carry row-level deltas for FTS5 or columnar tables — those still report only
    their {e name} via {!execute_with_dirty} and are absent here.  A consumer that
    must invalidate on FTS/columnar writes uses {!execute_with_dirty} alongside
    this; full FTS/columnar row capture is a follow-up (#418).

    {b Rowid identity.}  Deltas are keyed by rowid.  An UPDATE that changes a
    row's INTEGER-PK alias {e moves} it to a new rowid — a change of identity —
    so it surfaces as a {!Deleted} of the old rowid paired with an {!Inserted} at
    the new one, not an {!Updated}.  Conversely, a REPLACE that overwrites the
    {e same} alias rowid is a physical in-place write and surfaces as a single
    {!Inserted} of that rowid (no paired {!Deleted}); a consumer treats "inserted
    a rowid it already holds" as a replace.

    {b Multiple deltas per rowid.}  Within one statement a rowid may appear in
    more than one change — e.g. a self-referential FK [ON UPDATE CASCADE] whose
    cascade updates a row that the statement {e also} matches directly.  Consumers
    must fold deltas in order rather than assume one per rowid (#418). *)
val execute_with_changes : t -> string -> (table_changes, error) result Lwt.t

(** #746: an opaque handle for one registered reactive-view callback, returned
    by {!register_view_callback} and consumed by {!unregister_view_callback}.

    It carries the view it was registered against, so there is no view-name
    argument to get wrong at removal time, and it is minted from a
    process-global counter, so a handle presented to a different {!t} over the
    same store matches nothing rather than removing an unrelated callback. *)
type view_callback

(** Render a handle as [view#id].  For logging and test failure messages; the
    id is an opaque serial number with no meaning beyond identity. *)
val pp_view_callback : Format.formatter -> view_callback -> unit

(** #427: register [cb] to fire after every commit that changes reactive view
    [view_name]'s materialisation.  [cb] receives the native {!row_change} diffs
    applied to [_rv_<view_name>] (Deleted/Inserted pairs; a changed aggregate row
    is a delete of the old plus an insert of the new).  Commits that leave the
    view unchanged — and DDL — fire nothing.

    #437: returns [Error (`Unknown_view view_name)] — attaching nothing — when
    [view_name] is not a live reactive view, so a caller wiring hooks from
    config can tell a typo from a successful registration.  Validity is decided
    at the instant of registration; prefer this over pre-checking with
    {!is_reactive_view}.

    The error is a closed polymorphic variant rather than the module-wide
    {!error} deliberately: this is the only failure this function has, and it is
    not a SQL error.  Widening {!error} would add a case that no other function
    returning [(_, error) result] can ever produce, and would force every
    existing exhaustive match to handle it.  A caller chaining against [error]
    maps it explicitly, e.g.
    [Result.map_error (fun (`Unknown_view v) -> Runtime ("unknown view " ^ v))].

    Callbacks are held by the registry entry, so [DROP REACTIVE VIEW] discards
    them: a callback registered against a dropped view stops firing, and
    re-registering reports [`Unknown_view].

    {b #746: registration is O(1) and returns a handle.}  It used to append with
    [@], copying the whole list per registration, so [n] registrations cost
    O(n^2) — measured at 4.00x allocation per doubling of [n], and 3.4s for
    20 000 registrations downstream.  The list is now held newest-first and
    reversed at the firing site.

    {b Callbacks fire in registration order, sequentially, and that is
    contractual.}  It was already the observable behaviour and preserving it
    across the O(1) rewrite is free, so a caller may rely on a callback
    registered earlier running (and completing — the firing site awaits each in
    turn) before one registered later on the same view.  Ordering between
    {e different} views in one flush is not specified.

    {b The returned handle is the only way to detach one callback.} Before #746
    nothing removed a single callback: [DROP REACTIVE VIEW] was the only
    removal path (#469), so a caller re-wiring callbacks from data — a hook
    table, a config reload — leaked a dead closure per re-wire that was still
    invoked on every change and had to decide for itself that it was stale.

    {b #757: the [Ok] case also returns the view's generation at the instant
    of registration.} [view_name] answers {e liveness} ("is there a reactive
    view of this name right now?"), not {e identity} ("is this the same view I
    registered against?") — a [DROP REACTIVE VIEW v; CREATE REACTIVE VIEW v
    AS ...] leaves [v] live throughout, but the new incarnation starts with no
    callbacks of its own, so a caller holding a handle from before the drop is
    stuck on a dead entry with nothing to tell it to re-register. Comparing the
    generation returned here against a later {!reactive_view_generation} call
    is that signal: equal means still the same incarnation, different means
    the name was recreated and the handle should be dropped and re-registered.
    See {!reactive_view_generation} for the monotonicity guarantee this
    depends on.

    {b #766 (open, deliberately not closed here): a TOCTOU gap in that
    re-registration pattern.} Nothing stops a SECOND drop-and-recreate landing
    between a caller's generation comparison and its re-registration call, so
    the re-registration can itself land against an incarnation that is
    already stale by the time this function returns. Closing it needs an
    atomic check-and-register (an optional expected-generation argument that
    fails on mismatch rather than silently registering), which is a further
    signature change beyond this one; tracked rather than rushed. *)
val register_view_callback
  :  t
  -> view_name:string
  -> (row_change list -> unit Lwt.t)
  -> (view_callback * int, [ `Unknown_view of string ]) result

(** #746: detach the callback [h] names.  Returns [true] if it was still
    registered and has now been removed, [false] if it was not — because it was
    already unregistered, because its view was dropped (which discards every
    callback on it), or because [h] came from a different {!t}.  Idempotent:
    unregistering twice is not an error, it just answers [false] the second
    time.

    {b Mid-flush semantics (#746).}  The set of callbacks a notification fires
    is snapshotted when that view's notification batch begins.  So a callback
    that unregisters itself — or another callback on the same view — from
    inside its own invocation:

    - always completes the invocation it is in;
    - does not affect who else is invoked in {e that} batch: a callback removed
      by an earlier callback in the same batch still runs for that batch;
    - is not invoked again from the batch after it, in this flush or any later
      one.

    That is a snapshot, not a tombstone and not a refusal: removal is always
    accepted and never raises, and the visible effect is simply deferred to the
    next batch.  A registration made from inside a callback behaves the same
    way — it starts firing from the next batch.

    {b #766 (open, deliberately not closed here): [false] is also returned
    when [h]'s view was dropped and a DIFFERENT incarnation recreated under
    the same name} — the lookup is by view name, so a stale [h] finds the
    new incarnation's (unrelated) callback list, fails to find [h]'s id in
    it, and this reports [false] exactly as it would for "already
    unregistered". A caller that needs to tell the two apart can compare
    {!reactive_view_generation} itself; making this function do so natively
    would need a 3-way result and is tracked, not rushed, in the same
    follow-up as the TOCTOU note on {!register_view_callback}. *)
val unregister_view_callback : t -> view_callback -> bool

(** #437: the names of the live reactive views, sorted.  Read from the in-memory
    registry, so — unlike probing the catalog for [_rv_<name>] — a user table
    named [_rv_<x>] does not make [x] look live, and neither does a persisted
    view whose stored SQL failed to re-parse at open (which leaves its [_rv_]
    table behind and logs a warning).

    #469: a view leaves this list when [DROP REACTIVE VIEW name] retires it —
    the only supported removal path.  The two statements a caller would most
    plausibly reach for instead are rejected rather than silently half-working:
    [DROP TABLE _rv_<name>] and [DROP VIEW <name>] on a live reactive view both
    error and point at [DROP REACTIVE VIEW].  Both guards key off the registry,
    so they protect *live* views only: for a persisted view whose stored SQL
    failed to re-parse at open (and which is therefore absent from the
    registry) neither fires — [DROP TABLE _rv_<name>] succeeds and
    [DROP VIEW <name>] still no-ops — but [DROP REACTIVE VIEW] recovers such a
    view regardless, erasing its catalog row and tolerating an already-missing
    [_rv_] table.  That is not a guarantee that the registry can never drift
    from the catalog — by design (see the spec's
    non-goals) nothing yet stops [ALTER TABLE _rv_<name> RENAME TO …] or
    [DROP TABLE <base_table>] from desynchronising them.
    Reactive-view DDL is immediate rather than transactional: a
    [ROLLBACK] after a drop does not bring the view back.  #473: inside an
    explicit transaction, [DROP REACTIVE VIEW] is rejected outright (not
    staged, not executed early) — it returns an error rather than
    participating in the transaction at all, so there is nothing for
    [ROLLBACK] or [COMMIT] to interact with; run it in autocommit.
    [CREATE REACTIVE VIEW] inside an explicit transaction is *not* similarly
    guarded and currently hangs — it is not "symmetric" with the drop case;
    see #473, which also tracks that gap. *)
val reactive_view_names : t -> string list

(** #437: whether [name] is a live reactive view, i.e. is in
    {!reactive_view_names}. *)
val is_reactive_view : t -> string -> bool

(** #757: [None] when [name] is not currently a live reactive view {e in this
    handle's registry} (the same condition {!is_reactive_view} reports as
    [false]); [Some g] when it is live, where [g] identifies {e which
    incarnation} of [name] this handle knows as live right now.

    {b Identity, not liveness.} {!reactive_view_names} and {!is_reactive_view}
    both answer "is there a reactive view named [name] right now?" — and both
    keep answering "yes" across a [DROP REACTIVE VIEW name; CREATE REACTIVE
    VIEW name AS ...], because the name never stops being live. They cannot
    tell a caller that the view underneath the name changed out from under
    it. [g] can: it is minted fresh every time [name] gets a new registry
    entry, whether that is the first-ever [CREATE REACTIVE VIEW name ...] or a
    recreate after a drop, so two calls to this function returning different
    [Some] values for the same [name] means the view was dropped and
    recreated in between, even if nothing else observable distinguishes the
    two calls.

    {b Scope of the guarantee: per store, not per process, and current as of
    each handle's own last sync.} The generation is minted from a counter that
    lives on the underlying store ([Granary_store.Store.rv_next_generation]),
    not on this module, and the store also records which generation is
    current for each name ([Store.rv_generations]) — {e not} a counter or map
    private to one {!t}. Every {!t} sharing one store, including one produced
    by {!create_worker_handle}, therefore mints and reuses generations from
    the SAME source: a worker handle created after a sibling already has view
    [v] live reconstructs its own registry entry for [v] from the persisted
    definition ({!create_worker_handle} always does; see its doc) and picks
    up the SAME generation the sibling already recorded, rather than minting
    a fresh one and falsely signalling a drop-and-recreate that never
    happened. Within one store, for one fixed [name], every generation it is
    ever assigned across its whole lifetime is strictly greater than every
    generation assigned to it before, {e regardless of how many other views
    are created or dropped in between and regardless of how many handles are
    doing the creating and dropping} — the counter is store-wide and never
    reset, so unrelated churn can never make two different incarnations of
    [name] collide on the same value or make a later incarnation's value go
    backwards.

    What this does {e not} give you is live cross-handle DDL propagation: a
    handle's own copy of the generation for [name] is only as fresh as that
    handle's own registry, which is populated at that handle's creation (or
    its own [CREATE]/[DROP]) and is not automatically refreshed when a
    {e sibling} handle drops and recreates [name] afterwards — the same
    per-handle DDL-visibility caveat {!create_worker_handle} already documents
    for tables, views and triggers (#589/#633/#634). A handle that needs to
    observe a sibling's recreate has to re-derive its view of the store (a
    fresh {!create_worker_handle}, or reopening), exactly as it would to see
    the sibling's other DDL.

    {b Composes with {!register_view_callback}.} A caller registers a callback
    and records the generation returned alongside its handle; later, comparing
    that recorded generation against a fresh call to this function (on the
    SAME {!t} the callback was registered on) tells it whether to keep using
    the handle (equal) or to treat it as stale and re-register (different, or
    [None] if the name isn't even live any more on this handle). This is the
    identity signal #746's handle-and-liveness pair could not provide on its
    own. *)
val reactive_view_generation : t -> string -> int option

(** #752: one row-mutation delivered to a {!register_row_hook} callback,
    modeled on the [NEW]/[OLD] pair a SQL trigger body sees.

    - INSERT: [old_row = None], [new_row = Some _].
    - DELETE: [old_row = Some _], [new_row = None].
    - UPDATE: both [Some _].

    [table] is always the table the hook was registered against — a
    convenience for a callback shared across more than one
    {!register_row_hook} call, so it need not close over the table name
    itself to tell which registration fired it. *)
type row_mutation =
  { table : string
  ; new_row : row option
  ; old_row : row option
  }

(** #752: an opaque handle for one registered OCaml row-mutation hook,
    returned by {!register_row_hook} and consumed by {!unregister_row_hook}.
    Mirrors {!view_callback} (#746/#437): it carries the (table, timing,
    event) key it was registered under, so there is no way to present it
    together with a mismatched key at removal time, and its id is minted from
    a counter shared by every {!t} over the same store
    ({!Granary_store.Store.row_hooks}, #752 review round 3 — see
    {!register_row_hook}'s doc comment), so a handle presented to a {!t} over
    a DIFFERENT store matches nothing rather than removing an unrelated hook. *)
type row_hook

(** Render a handle as [table:timing:event#id] (e.g. [orders:before:insert#3]).
    For logging and test failure messages; the id is an opaque serial number
    with no meaning beyond identity. *)
val pp_row_hook : Format.formatter -> row_hook -> unit

(** #752: register [fn] to run on every [event] mutation of [table] at
    [timing], as an OCaml closure rather than a SQL [CREATE TRIGGER] body —
    the seam camel#75 needs for a GADT-certified program's [before:]/[after:]
    hooks, which are typed OCaml, not SQL, by design (camel#1).

    Returns [Error (`Unknown_table table)] — registering nothing — when
    [table] does not exist at the instant of the call, mirroring
    {!register_view_callback}'s [`Unknown_view] (#437): a caller wiring hooks
    from config can tell a typo from a successful registration. The error is a
    closed polymorphic variant rather than the module-wide {!error} for the
    same reason #437 gives — this is {!register_row_hook}'s only failure
    class, and widening {!error} would add a case no other function returning
    [(_, error) result] can produce.

    {b Returns [Error (`Columnstore_unsupported table)] for a [USING
    COLUMNSTORE] table (#752 review).} The columnar write path calls
    [Col_store.insert_rows] directly for INSERT, bypassing [before_hook]/
    [after_hook] entirely, and refuses UPDATE/DELETE outright — so a hook
    registered on a columnstore table could never fire, for any [timing] or
    [event]. Refusing at registration matches the precedent set by a
    GENERATED column on a [USING COLUMNSTORE] table being refused at DDL
    (#660) rather than silently accepted and then never honoured.

    {b A hook is tied to the table it was registered against, not to the
    name — DROP TABLE purges it; RENAME migrates it, from ANY execution path
    and for EVERY handle sharing this store (#752 review round 3).} The
    registry backing this is {!Granary_store.Store.row_hooks}, not a table on
    [t] — the same move #757 made for reactive-view generation identity, and
    for the identical reason: a table-keyed registry entry must stay correct
    no matter which of possibly several {!t} handles sharing one store
    ({!create_worker_handle}, #589/#633) performs the DROP/RENAME, or fires
    the hook, or (for {!unregister_row_hook}) removes it. The purge/migrate
    itself lives inside the ONE function each statement shape funnels through
    ([Sql.Exec.execute_drop_table] / [execute_alter_table]) — reached alike
    by a one-shot {!execute}, a {!prepare}d statement, and a trigger body's
    nested DML (which a Db.ml-only mechanism checked in an earlier revision
    of this feature missed) — so no future DDL-executing path can bypass it
    by construction, not by remembering to call a bookkeeping function.
    Applied once the DDL statement that names [table] actually {e succeeds}
    (a failed or rolled-back DROP/RENAME leaves the registry as if it never
    ran — see the ROLLBACK paragraph below):
    - [DROP TABLE table] removes every hook registered on it, for every
      [timing]/[event]. Without this, [DROP TABLE t; CREATE TABLE t (...)]
      with a different shape would silently reattach a stale hook to the new
      table — and since a [`Before] hook can veto, a misattached one is a
      correctness/security gap, not just a leak.
    - [ALTER TABLE table RENAME TO new_name] re-keys every hook registered
      on it to fire under [new_name] instead — {e not} a refusal, unlike
      #609's view/trigger dependency check. #609 refuses because a view or
      trigger is stored as raw, unparsed SQL text with no name to rewrite in
      place; that is a technical necessity, not a chosen policy. A row hook
      is a structured in-memory registration keyed by name, so re-keying it
      is exact and lossless, and a [row_mutation] fired after the rename
      reports [table = new_name] — refusing here would only strand a
      caller's hook (and camel's [reject!] veto with it) for no correctness
      reason.
    - VACUUM rebuilds the store's data into a wholly new
      {!Granary_store.Store.t}; without a dedicated carry-over (mirroring
      {!Granary_store.Store.rv_carry_over_generations}, called from the same
      site) every registered hook would silently vanish across the rebuild.
      {!create_worker_handle}'s existing VACUUM-invalidation caveat still
      applies to everything else about a sibling handle's view of the store.

    {b Registering inside an explicit transaction is undone by its ROLLBACK
    (#752 review).} [register_row_hook] is an OCaml call, not a SQL statement,
    so it does not automatically share in a transaction's rollback the way
    DDL run through {!execute} does. Without special handling,
    [BEGIN; CREATE TABLE t(...); (* register_row_hook here *); ROLLBACK]
    would leave the hook attached even though [t] never really existed — and
    a {e later, unrelated} [CREATE TABLE t] would silently reattach it. When
    a transaction is open at the moment of registration, the registration is
    therefore undone if that transaction rolls back (including a
    [ROLLBACK TO] that unwinds past the point of registration), via the same
    #269 schema-undo mechanism the view/trigger caches use. Registering
    outside an explicit transaction is unaffected — there is no rollback to
    protect against.

    {b Veto ([`Before] only).}  [fn] returns [(unit, string) result Lwt.t].
    [Ok ()] lets the write proceed (or, for [`After], simply completes).
    [Error msg] is a {b veto} for a [`Before] hook: the write this statement
    was about to make never happens and [msg] surfaces to the statement's
    caller as a {!Runtime} error — {e exactly} the path a failing
    [BEFORE INSERT] trigger body already takes today, because a [`Before] veto
    is implemented as the identical [Lwt.fail]: the write path's
    [before_hook]/[after_hook] pair does not distinguish a hook that raised
    from a hook that returned [Error]; both unwind the same way.

    {b What "unwind" gets you, precisely — this is narrower than #631's
    OR-IGNORE guarantee and deliberately not widened here.}  In autocommit,
    a veto rolls back the statement's own transaction outright: the primary
    write, and anything the vetoing hook itself wrote via nested DML on this
    [t], are both gone. Inside an explicit transaction the caller opened,
    only the {e primary} write is prevented — a [`Before] hook's own nested
    DML, if it performed any before returning [Error], is {b not} specially
    unwound, for the same reason a raising [BEFORE INSERT] trigger's own
    nested DML is not: #631's statement-level savepoint exists only for the
    non-raising [OR IGNORE]/NOT NULL {e skip} path ([execute_insert] takes it
    exactly when [on_conflict = Some Ast.CA_ignore]), not for a raise, and a
    raise's partial effects surviving a borrowed transaction is a pre-existing,
    documented property of that path (see [execute_insert]'s exception
    handler) — not a new gap this feature introduces, and not one this issue
    closes. A hook wanting true no-residue nested DML on veto should perform
    it, if at all, only after every other [`Before] hook and check has
    already had a chance to veto — or avoid nested DML from a vetoing path
    entirely.

    {b The common case is unaffected by that caveat.}  A [`Before] hook that
    coexists with a SQL [BEFORE] trigger on the same table vetoes {e before}
    the trigger ever runs (see Ordering, below) — the trigger's body,
    including any nested DML it would have performed, simply never executes,
    which is a stronger and simpler guarantee than rolling it back.

    {b [`After] hook errors also abort the statement}, deliberately the same
    as [`Before]'s veto rather than a log-and-continue: an [`After] hook runs
    inside the same not-yet-committed write transaction a [`Before] hook and a
    SQL [AFTER] trigger body run in (all three are threaded through the one
    [before_hook]/[after_hook] pair {!make_trigger_hook} already built for SQL
    triggers), so there is no "the write is already durable" case that would
    make swallowing the error meaningful — an [AFTER] trigger's body failing
    aborts the statement today for the identical reason, and giving [`After]
    row hooks quieter failure semantics than that would be a surprising
    asymmetry with no upside. A hook that truly wants log-and-continue
    semantics can catch its own errors and always return [Ok ()].

    {b Ordering against SQL [CREATE TRIGGER]s (design decision, #752).} OCaml
    row hooks and SQL triggers on the same (table, timing, event) are
    sandwiched, not interleaved by a shared registration order: at [`Before]
    every registered row hook runs {b before} any matching SQL [BEFORE]
    trigger; at [`After] every matching SQL [AFTER] trigger runs {b before}
    the row hooks. Row hooks are therefore always the outer gate — a
    [`Before] veto pre-empts a SQL trigger's nested DML for that statement
    entirely, and an [`After] hook always observes the row after every SQL
    [AFTER] trigger this statement fired has already applied its own writes.

    {b Multiple hooks on the same (table, timing, event) fire in registration
    order}, sequentially — the firing site awaits each in turn — matching
    {!register_view_callback}'s #746 contract for view callbacks.

    {b Not persisted, but SHARED across handles over one store (#752 review
    round 3 — revised from an earlier revision of this feature).} Unlike a
    SQL trigger, a row hook is an in-memory OCaml closure: it does not
    survive closing and reopening the database. Unlike that earlier
    revision, though, it is {e not} private to the {!t} that registered it —
    a sibling handle from {!create_worker_handle} sees it, fires it, and can
    purge/migrate it via its own DROP/RENAME, because the registry lives on
    the shared {!Granary_store.Store.t} (see the paragraph above). This is a
    deliberate divergence from {!register_view_callback}, whose callbacks
    stay private per handle — a reactive-view callback is a per-subscriber
    observation, where a row hook (especially a [`Before] veto) is closer to
    a table-level invariant that should hold no matter which handle is
    writing.

    {b A row hook's own nested DML re-entering this same hook is bounded,
    like SQL trigger recursion (#752 review round 3, item 4).} If [fn] itself
    performs DML (via {!execute}/{!execute_change_count} on this same [t])
    that re-triggers the same or another row hook, the nesting is capped at
    the same depth {!Db}'s SQL-trigger recursion guard uses, on an
    independent counter — so a self-recursing hook fails cleanly with a
    bounded "recursion limit exceeded" message instead of exhausting the
    OCaml call stack. A row hook and a SQL trigger nesting into each other in
    one call chain each get their own full budget rather than sharing one.

    {b A hook that raises is treated exactly like one that returns [Error]
    (#752 review).} [fn]'s documented failure path is [Error msg], but a hook
    that raises an OCaml exception instead ([Not_found], a failed pattern
    match, ...) gets the identical treatment rather than escaping the
    [(_, error) result] contract every other failure in this library upholds:
    the exception is caught and re-raised as [Failure], the one exception
    class {!execute}/{!execute_change_count} already turn into
    [Error (Runtime _)]. The resulting message is prefixed with the timing,
    event and table (e.g. ["before row hook on 't': <msg>"], or
    ["... raised: <exn>"] for a genuine exception) rather than given its own
    {!error} variant — deliberately: this fires from deep inside the write
    path rather than at a single call boundary, and adding a case to the
    small, closed {!error} type would force every existing exhaustive match
    on it to grow an arm only this one feature can produce, which is exactly
    what #437/#746 chose closed polymorphic variants over for
    {!register_view_callback}. A caller that needs to tell a hook's veto from
    an unrelated internal error can match the [Runtime] message text. *)
val register_row_hook
  :  t
  -> table:string
  -> timing:[ `Before | `After ]
  -> event:[ `Insert | `Update | `Delete ]
  -> (row_mutation -> (unit, string) result Lwt.t)
  -> (row_hook, [ `Unknown_table of string | `Columnstore_unsupported of string ]) result

(** #752: detach the row hook [h] names — for every handle sharing [h]'s
    store (#752 review round 3): the removal acts on
    {!Granary_store.Store.row_hooks}, so a sibling handle stops seeing [h]
    fire too. Idempotent and never raises: calling it a second time, on a
    hook whose table no longer has any hooks registered, or on a handle
    minted over a DIFFERENT store, is simply a no-op — unlike
    {!unregister_view_callback} this reports no [bool], since a row hook has
    no analogous "mid-flush" caller who needs to know whether its own
    removal request was the one that mattered.

    {b Undone by an enclosing transaction's ROLLBACK, symmetrically with
    {!register_row_hook} (#752 review round 3, item 3).} Calling this inside
    an explicit transaction that later rolls back re-attaches [h] exactly as
    it was — otherwise [BEGIN; unregister_row_hook h; ROLLBACK] would leave
    [h] permanently detached even though nothing else about the transaction
    survived. Registering outside an explicit transaction is unaffected. *)
val unregister_row_hook : t -> row_hook -> unit

(** #387: the projected output column names for a row-returning [sql], without
    executing it.  Parses and binds [sql] against the current schema and returns
    a best-effort name per result column: a SELECT alias or bare column name
    where known, [*] expanded to the source table's columns, and a positional
    [col_N] placeholder for anonymous expressions.  Intended for header display
    (e.g. the REPL); the list length matches the row width [query] produces for
    the same statement. *)
val query_columns : t -> string -> (string list, error) result Lwt.t

(** #264: stream a logical SQL dump (a {{:https://sqlite.org/cli.html#dump}.dump}-style
    export) of the database to [sink], one chunk at a time.

    The output is a self-contained SQL script that recreates the database when
    replayed statement-by-statement through {!execute}: [PRAGMA foreign_keys=OFF;],
    then for each table its [CREATE TABLE] followed by [INSERT] statements for
    its rows, then each FTS5 virtual table's [CREATE VIRTUAL TABLE] followed by
    [INSERT]s for its content, then explicit indexes, views (in dependency order)
    and triggers. Tables are emitted in creation order. Generated-column values
    are omitted (recomputed on insert); implicit PRIMARY KEY indexes the
    [CREATE TABLE] already implies are not re-emitted.

    The whole script is wrapped in [BEGIN]/[COMMIT] (after the leading [PRAGMA
    foreign_keys=OFF;], which stays outside the transaction), so a restore
    applies atomically — matching [sqlite3 .dump] (#281). This became possible
    once #269 removed the DDL-in-transaction deadlock; every statement the dump
    emits is transactional, so there is no per-statement-autocommit fallback. A
    consequence: replay the script as-is — do {e not} wrap it in your own
    [BEGIN]/[COMMIT], as the engine rejects a nested transaction.

    {b Point-in-time row data (#274).} Every table's row data is read through
    one read-only snapshot of the committed state, taken when the dump begins
    (when an explicit transaction is active, its view is used instead), so the
    emitted rows reflect a single consistent moment even if other writers commit
    concurrently while the dump runs — matching [sqlite3 .dump], which previously
    differed by reading each table in its own snapshot (a torn dump). View and
    trigger DDL is read through that same view too (#322/#323/#329): the shared
    snapshot in autocommit, or the explicit transaction when one is active — so
    the schema section is always consistent with the row data (point-in-time
    against a concurrent [CREATE]/[DROP VIEW|TRIGGER] commit, and read-your-own
    -writes inside a transaction). Table/index DDL and the AUTOINCREMENT high-water
    still come from the in-memory catalog (a separate consistency surface), so for
    a fully consistent result those should be quiescent during the dump.

    {b FTS5 content is dumped (#319/#330).} An FTS5 table's [CREATE VIRTUAL TABLE]
    is emitted followed by [INSERT INTO t(rowid, ..)] statements for its stored
    content, so a replay re-inserts the original rows (rebuilding the index) with
    their {e original rowids} — a delete leaves a rowid gap that the restore
    preserves rather than re-packing, matching how [sqlite3 .dump] round-trips an
    FTS table.

    {b Composite / table-level PRIMARY KEYs round-trip as PRIMARY KEYs (#533).}
    They used to be downgraded to a plain [UNIQUE] index — the data survived but
    the restored schema reported no PRIMARY KEY and permitted NULLs. The key's
    column order is recovered from its implicit index (the only place it is
    recorded), so the [CREATE TABLE] carries a real [PRIMARY KEY (a, b)] and its
    index is not emitted separately.

    {b A database whose rows contradict its own schema is refused (#548).} A file
    written before #530 can hold NULL in a column the engine now declares NOT
    NULL — a table-level [PRIMARY KEY (k)], every column of a composite
    [PRIMARY KEY (a, b)], or a column added by [ALTER TABLE ... ADD COLUMN ...
    PRIMARY KEY]. Rendering the current declaration and then emitting those rows
    produces a script that dies on the first offending row. Rather than emit it,
    [dump] fails with [Error (Runtime msg)] naming the table and the column.
    The alternative — silently emitting the column
    without its NOT NULL — would restore, but by downgrading the schema without
    saying so.

    {b The diagnostic points at [PRAGMA not_null_check] (#583).} The refusal is
    raised from inside the row stream, so it names the violation it stopped on
    and knows nothing about the rest of the file; repairing table by table off
    successive dump failures is what #563's report mode exists to prevent. The
    message therefore leads with [PRAGMA not_null_check] (every offending
    (table, column, count) in one pass) and [PRAGMA not_null_repair] (deletes
    them through the ordinary delete path, so indexes and ON DELETE cascades are
    honoured), and keeps the hand-written [UPDATE] that preserves the rows and
    [DELETE] that drops them as the manual escape hatch. Detection rides along with the row stream, so a healthy database
    pays no extra read; a refused dump has already emitted [BEGIN] and is closed
    with [ROLLBACK], so a streaming sink is left with a script that undoes itself
    rather than one that half-applies.

    [schema_only] omits all [INSERT]s; [data_only] omits all DDL (leaving only
    the row [INSERT]s). Both modes carry the same [BEGIN]/[COMMIT] wrapper as a
    full dump. Passing both yields an essentially empty dump. Neither can trip
    the #548 refusal: [schema_only] emits no rows, and [data_only] emits no
    schema for them to contradict — so [data_only] remains the escape hatch for
    getting the data out of an unrepaired file. *)
val dump
  :  t
  -> ?schema_only:bool
  -> ?data_only:bool
  -> sink:(string -> unit Lwt.t)
  -> unit
  -> (unit, error) result Lwt.t

(** #264: like {!dump}, but collects the whole dump into a single string.
    Convenient for small databases; for large ones prefer {!dump} with a
    streaming [sink] to avoid materializing the entire script in memory. *)
val dump_to_string
  :  t
  -> ?schema_only:bool
  -> ?data_only:bool
  -> unit
  -> (string, error) result Lwt.t

(** Format an [error] value for human-readable output. *)
val pp_error : Format.formatter -> error -> unit

(** A compiled prepared statement.  Can be reused with different
    parameter bindings via [run] and [iter]. *)
type stmt

(** Compile [sql] into a prepared statement.  Returns [Error] if the
    SQL cannot be parsed or the schema check fails. *)
val prepare : t -> string -> (stmt, error) result Lwt.t

(** Execute a write statement (INSERT, UPDATE, DELETE) with the given
    positional parameter values.  Returns the rows-affected count. *)
val run : stmt -> params:value list -> (int, error) result Lwt.t

(** Like {!run}, but also returns the {!dirty_tables} the prepared write
    mutated alongside the rows-affected count. *)
val run_with_dirty : stmt -> params:value list -> (int * dirty_tables, error) result Lwt.t

(** Execute a read statement (SELECT) with the given positional
    parameter values.  Returns a stream of result rows.

    Inside an explicit transaction the stream observes the transaction's own
    uncommitted writes (read-your-own-writes, #262).  The stream is a stable
    snapshot taken when iteration begins: rows written to the same table later
    in the transaction are not retroactively seen by an in-flight stream, and
    the stream remains safe to drain after a subsequent write or [COMMIT]. *)
val iter : stmt -> params:value list -> (row Lwt_stream.t, error) result Lwt.t

(** #239: like {!iter}, but also returns a per-query cost/stats record populated
    as the returned stream is consumed.  See {!Granary_sql.Exec.query_stats}. *)
val iter_with_stats
  :  stmt
  -> params:value list
  -> (row Lwt_stream.t * query_stats, error) result Lwt.t

(** Release resources held by a prepared statement.  No-op in this
    implementation, but should be called for forward compatibility. *)
val finalize : stmt -> unit Lwt.t

(** Look up the 0-based slot index for a named parameter.
    Returns [None] if the name was not found in the prepared statement. *)
val param_slot : stmt -> string -> int option

(** Build a parameter array from a named association list.
    Unnamed or unknown parameters are left as [V_null]. *)
val params_of_named : stmt -> (string * value) list -> value array
