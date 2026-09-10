(** Ordered byte-keyed store.
    Phase 0: in-memory [Bytes_map] per [tree_id].
    Phase 1: CoW B+-tree on BLOCK behind the same interface.

    Two backends are available:
    - [create ()] — in-memory [Bytes_map] (no size limits on keys/values).
      This is the legacy Phase 0 backend, retained for tests and
      ephemeral use cases that exceed B+-tree leaf-cell size limits.
    - [open_block ~...] — CoW B+-tree over any BLOCK device, given as I/O
      callbacks. Persistent across reopen. Keys ≤ 512 bytes, values ≤ 1024
      bytes. Unix-file convenience constructors live in the [granary.unix]
      driver library so this core stays platform-agnostic (#170). *)

type t

(** Page geometry (#95), re-exported so callers can name {!Geometry.t} without
    depending on [granary.storage] directly. *)
module Geometry = Granary_storage.Geometry

(** Pretty-print the store's backend kind (Mem or Btree). *)
val pp : Format.formatter -> t -> unit

(** The page geometry this store is backed by (#176).  The in-memory backend
    reports {!Geometry.default}; a B+-tree store reports the geometry peeked or
    chosen at open time.  VACUUM reads this to rebuild at the same page_size. *)
val geometry : t -> Geometry.t

(** #338/#752 (review round 5): [true] once {!close} has signalled teardown
    on this exact store object — set at the very start of {!close}, before
    any of its own awaits, and never cleared, since a closed store is never
    reused (a VACUUM that swaps in a rebuilt store gives the caller a
    brand-new {!t}, not a resurrection of this one). {!rw_begin}/{!ro_begin}
    already refuse once this is set; this is the same check, exposed so a
    caller that mutates store-level state directly — without opening a
    transaction — can apply it too.  This is exactly [Db]'s situation for its
    row-hook registry accessors: a sibling handle whose [t.store] still names
    a store a concurrent {!Db.vacuum} has already closed (but not yet swapped
    out on the vacuuming handle — a real window, since {!Db.vacuum} awaits
    between the two) could otherwise register or unregister a hook against a
    registry nobody will ever look at again, with no error. Always [false]
    for the in-memory backend, which has no teardown state and on which
    {!Db.vacuum} refuses to run at all. *)
val is_closing : t -> bool

(** Phantom types for transaction modes. *)
type ro

type rw

(** A transaction handle, phantom-typed by mode. *)
type 'a txn

(** Each tree is an independent ordered key→value map.
    System trees use IDs 0–15; user tables use 16+. *)
type tree_id = int

(** #589/#633: the rowid allocator's live state for this store's data trees,
    keyed by TREE ID.  It lives here rather than on a catalog because a tree id
    is only meaningful within one {!t}, and because every catalog opened over
    one store must share exactly one allocator: two counters over one data tree
    hand the same rowid out twice, and the second write silently overwrites the
    first.  Two stores (an ATTACHed schema, say) are two sets of trees and so
    two tables, which is correct by construction. *)
type rowid_counters = (tree_id, int64) Hashtbl.t

(** This store's rowid allocator state.  Every catalog opened over the same
    store gets this same table — there is nothing to pass by hand and nothing
    to forget (#633). *)
val rowid_counters : t -> rowid_counters

(** #757: per-reactive-view-name generation identity, shared by every
    [Db.t]/catalog handle opened over this store — the same rationale as
    {!rowid_counters}: a reactive view's registry entry is reconstructed
    independently by each handle (a fresh top-level open, or a worker handle
    from {!Store} sharing this store via [Db.create_worker_handle]), and two
    independent reconstructions of the SAME live incarnation of a view must
    agree on its identity, or a sibling handle would see a view it never
    dropped or recreated as a fresh incarnation.  Keyed by view name; the
    value is the generation most recently minted for that name's current live
    incarnation.  Not persisted — like {!rowid_counters}, this is process-local
    bookkeeping a fresh handle either finds already populated (a sibling got
    there first) or populates itself (the first handle over this store to see
    this name). *)
type rv_generations = (string, int) Hashtbl.t

(** This store's reactive-view generation map.  Every [Db.t] opened over the
    same store gets this same table — mirrors {!rowid_counters} above. *)
val rv_generations : t -> rv_generations

(** #757: mint a fresh generation, unique for the lifetime of this store and
    never reused, even across drops and recreates of any reactive view name.
    Shared by every handle over this store the same way {!rowid_counters} is,
    so two sibling handles can never mint colliding values — whether for the
    same name or different ones. *)
val rv_next_generation : t -> int

(** #757: carry [from]'s reactive-view generation bookkeeping forward into
    [to_] — every recorded name/generation pair, and the counter raised to at
    least [from]'s.  For {!Db.vacuum} to call right after it rebuilds a
    store's data into a fresh backing file and BEFORE any [Db.t] starts
    minting generations against the new [t]: a freshly-opened store (this
    function's own [create]/[open_block]) always starts with an EMPTY
    {!rv_generations} and its counter at 0, because unlike {!rowid_counters}
    — whose correct value is always recoverable by rescanning the copied data
    tree or reading a persisted AUTOINCREMENT high-water mark — a generation
    is pure process-local bookkeeping with nothing durable to reconstruct it
    from; {!rv_next_generation}'s copied tree data for [sys_reactive_views]
    carries the view SQL forward but not this counter. Without this call, a
    rebuilt store's counter restarting at 0 lets a generation minted before
    the VACUUM and one minted after collide on the same integer, which
    directly breaks the "strictly greater than every generation ever assigned
    to this name before" guarantee {!Db.reactive_view_generation} documents.
    Idempotent and safe to call on a [to_] that already has entries — an
    existing entry for a name also in [from] is overwritten with [from]'s
    value (the two would already agree if [to_] had never independently
    minted for that name, since nothing mints against [to_] until this
    returns). *)
val rv_carry_over_generations : from:t -> to_:t -> unit

(** #752: whether an OCaml row hook fires before the row write it observes
    (able to veto — see [Db.register_row_hook]) or only after (observe-only). *)
type row_hook_timing =
  [ `Before
  | `After
  ]

(** #752: the DML event an OCaml row hook fires for. *)
type row_hook_event =
  [ `Insert
  | `Update
  | `Delete
  ]

(** #752: one row-mutation delivered to a registered row-hook callback,
    modeled on the [NEW]/[OLD] pair a SQL trigger body sees. INSERT:
    [old_row = None], [new_row = Some _]. DELETE: the reverse. UPDATE: both
    [Some _]. *)
type row_mutation =
  { table : string
  ; new_row : Granary_encoding.Row.t option
  ; old_row : Granary_encoding.Row.t option
  }

(** #752: id for one registered row hook, minted from a counter shared by
    every handle over this store (mirrors {!rv_next_generation}), so a handle
    presented an id minted by a sibling matches nothing rather than removing
    an unrelated hook. Transparent (like {!tree_id}) rather than abstract —
    nothing about its representation is meant to be hidden, only its
    provenance (this counter, not any other) is what gives it meaning. *)
type row_hook_id = int

(** #752 (review round 3): the table-keyed registry every [Db.t] handle's
    [register_row_hook] / [unregister_row_hook] and every DROP-TABLE /
    RENAME-TABLE execution path reads and writes — the ONE hardened
    primitive this module gives every caller for "a table-keyed registry
    entry that survives/migrates/purges correctly across RENAME, DROP and
    ROLLBACK, from any execution path, visible across every sibling handle
    sharing this store."

    It lives on {!t} rather than on a catalog or a [Db.t] for the same reason
    {!rowid_counters} and {!rv_generations} do: a hook registered through one
    handle must be visible to, and fire from, every SIBLING handle's write
    path over the same store ({!Db.create_worker_handle}, #589/#633), and a
    DROP/RENAME executed by ANY handle must purge/migrate it for all of them
    — a per-[Db.t] copy could only give that by remembering to synchronise
    copies, which is exactly the design {!row_hooks} replaces. It is opaque
    (unlike {!rowid_counters}/{!rv_generations}'s transparent [Hashtbl]
    aliases) because its correct manipulation is more than a bare table
    lookup — composite keys, insert-order-preserving retrieval, and
    snapshot-based undo — so every caller goes through the functions below
    instead of each re-deriving that logic against a raw [Hashtbl]. *)
type row_hooks

(** This store's row-hook registry. Every [Db.t] opened over the same store
    gets this same table — mirrors {!rowid_counters}/{!rv_generations}. *)
val row_hooks : t -> row_hooks

(** Register [fn] to fire on every [event] mutation of [table] at [timing].
    Returns the fresh id. O(1): entries are held newest-first per
    (table, timing, event) key; {!row_hook_fire_list} reverses at the firing
    site to restore registration order — the same shape [Db]'s #746 view-
    callback registry uses for the same reason. *)
val row_hook_register
  :  row_hooks
  -> table:string
  -> timing:row_hook_timing
  -> event:row_hook_event
  -> (row_mutation -> (unit, string) result Lwt.t)
  -> row_hook_id

(** Detach the hook [id], and return a closure that reverses exactly that
    detachment — the same "perform, return the undo" shape as
    {!row_hooks_purge_table}/{!row_hooks_migrate_table}, for the same reason
    (#752 review round 3, item 3): [Db.unregister_row_hook] needs it so
    [BEGIN; unregister_row_hook h; ROLLBACK] does not leave [h] permanently
    detached, and a caller with no transaction to protect against (an
    unregister that is itself undoing a registration, say) simply discards
    it.

    {b Resolved by [id] alone (#752 review round 4), not by a caller-supplied
    (table, timing, event).} A [Db.row_hook] handle's table name is captured
    at registration time; {!row_hooks_migrate_table} can silently move the
    entry to a new key afterwards (a RENAME), and a caller unregistering by
    the handle's now-stale name would find nothing to remove — an
    undetachable hook, which for a [`Before] hook is an undetachable veto.
    Looking [id] up through the registry's own reverse index instead means a
    rename can never desynchronise a handle from the entry it names.

    Idempotent: a second call to either the outer function or its returned
    closure, or an [id] not currently registered anywhere, is a no-op. Drops
    the (table, timing, event) key from the registry entirely once its last
    hook is removed (rather than leaving it mapped to [[]]), so
    {!row_hooks_is_empty} correctly returns to [true]. *)
val row_hook_unregister : row_hooks -> row_hook_id -> unit -> unit

(** The hooks registered on (table, timing, event), in registration order.
    [[]] if none are registered. *)
val row_hook_fire_list
  :  row_hooks
  -> table:string
  -> timing:row_hook_timing
  -> event:row_hook_event
  -> (row_hook_id * (row_mutation -> (unit, string) result Lwt.t)) list

(** [true] iff no row hook is registered anywhere in this store, for any
    (table, timing, event) — the fast path a write-path caller checks before
    ever computing a lookup key, so a store nobody has registered a hook on
    pays for one length check. *)
val row_hooks_is_empty : row_hooks -> bool

(** Remove every hook registered on [name], for every (timing, event), and
    return a closure that reverses exactly that removal. Calling the closure
    twice is a no-op the second time (it replays a fixed snapshot taken
    before the removal), so it is safe to hand to a schema-undo log that may
    replay it more than once (e.g. a [ROLLBACK TO] nested inside a wider
    [ROLLBACK]'s replay). Used by every DROP-TABLE execution path — see
    [Sql.Exec.execute_drop_table] — to keep the registry consistent with
    [table_exists] across both directions of that statement's outcome. *)
val row_hooks_purge_table : row_hooks -> string -> unit -> unit

(** Move every hook registered on [old_name] to [new_name], for every
    (timing, event), and return a closure that reverses exactly that move.
    A no-op (returning a no-op closure) if the two names are equal.

    {b The reverse move is restricted to exactly the entries the forward
    move actually moved, merged into whatever the destination key holds at
    undo time — never a blind replace (#752 review round 6, item 1).}
    [ALTER TABLE ... RENAME] refuses a target name that already names a
    table, so nothing can already be registered at [new_name] when the
    FORWARD move runs — but a sibling handle can still see [new_name] as
    live before this transaction commits (the documented DDL-visibility
    leak, #589/#633) and register a hook directly against it while the
    rename's transaction is still open. A ROLLBACK then replays this
    function's returned closure with the names swapped; grabbing "whatever
    is currently at [new_name]'s key" at that point — as an earlier
    revision did — would sweep the sibling's brand-new, unrelated
    registration back onto [old_name] along with the entries that
    genuinely moved, misfiling it under a table name it never named (full
    veto power included, for a [`Before] hook). Restricting the reverse to
    the ids the forward call actually moved, and merging rather than
    replacing at the destination, is the same fix round 5 applied to
    {!row_hook_unregister} and {!row_hooks_purge_table} for the identical
    reason — the reverse move's idempotence still follows from the same
    fact {!row_hook_unregister} relies on: replaying it a second time finds
    none of the originally-moved ids left at the source key. Used by every
    RENAME-TABLE execution path — see [Sql.Exec.execute_alter_table]. *)
val row_hooks_migrate_table
  :  row_hooks
  -> old_name:string
  -> new_name:string
  -> unit
  -> unit

(** #752: carry every registered hook from [from] into [to_] — the VACUUM
    case, mirroring {!rv_carry_over_generations} exactly: VACUUM builds a
    wholly new {!t}, so without this call every row hook registered before it
    would silently vanish across the rebuild, which for a [`Before] hook with
    veto power is a correctness change, not a cosmetic loss. Call from
    {!Db.vacuum}'s one call site, before the handle swaps onto the new store —
    the same point {!rv_carry_over_generations} is called from.

    #774: [to_] also joins [from]'s registry LINEAGE, which is what keeps
    {!row_hook_effective_depth} attributing a pre-VACUUM hook's deferred
    [Lwt.async] continuation against the depth it actually nests from once
    that continuation resumes and writes through the post-VACUUM store. The
    one thing NOT carried is {!row_hook_depth} itself: it counts invocations
    that are currently firing, each of whose matching {!row_hook_depth_decr}
    is closed over [from]'s record, so a copied non-zero count would be a
    claim on [to_] that nothing ever releases. See the implementation comment
    for why copying it would also not have fixed #774. *)
val row_hooks_carry_over : from:t -> to_:t -> unit

(** #752 (review round 4): current nested row-hook-firing depth, shared by
    every [Db.t] over this store. [Db]'s recursion guard (mirroring its SQL
    trigger recursion guard) reads this — not a per-handle counter — so that
    a hook whose nested DML re-enters the hook path through a DIFFERENT
    handle sharing this store ({!Db.create_worker_handle}) still counts
    against the SAME budget: the recursion is one logical chain regardless of
    which handle each frame happens to run through. *)
val row_hook_depth : row_hooks -> int

(** Increment {!row_hook_depth} by one — call before entering a row hook's
    body. *)
val row_hook_depth_incr : row_hooks -> unit

(** Decrement {!row_hook_depth} by one — call (under [Lwt.finalize], so it
    runs even if the body raised) after a row hook's body returns. *)
val row_hook_depth_decr : row_hooks -> unit

(** Run [f] tagged, for its whole dynamic extent (every synchronous and
    asynchronous continuation it creates, via [Lwt.with_value]), as
    "executing inside a row hook callback fired for [t]" (#752 review round
    6, item 2). {!rw_begin} consults this tag to refuse — immediately, with a
    clear error — a hook's own nested attempt to open a SECOND write
    transaction on the SAME store while the transaction that fired it is
    still open, which would otherwise deadlock: the writer lock
    (non-reentrant, #740) is released only by that outer transaction's
    commit/rollback, and the outer transaction cannot reach its
    commit/rollback until this nested call returns.

    Deliberately a dynamic-extent tag, not a plain counter like
    {!row_hook_depth}: two logically independent statements can be
    interleaved by the Lwt scheduler on the same store (sibling handles,
    #589), and a bare "is any hook firing anywhere on this store" flag
    cannot tell an ordinary, unrelated writer legitimately queued behind the
    writer lock from the one case that is a genuine self-deadlock. [Db]'s
    [fire_ocaml_row_hook] is the sole caller, wrapping its call to the
    hook's own [fn].

    [~depth] is the recursion depth this particular invocation is running
    at (#752 review round 8, item 2) — see {!row_hook_effective_depth},
    which is how a later, causally-descended firing recovers it.

    [~register_undo] (#752 review round 9, finding 2) pushes a #269
    schema-undo closure onto the #269 undo log owned by the [Cat.t] of the
    [Db.t] statement that is ACTUALLY firing this hook — [Db]'s sole caller,
    {!Db.fire_ocaml_row_hook}, passes [Cat.register_schema_undo] partially
    applied to its own handle's catalog. A plain function rather than a
    [Cat.t] field, since [Store] sits below [Catalog] in the dependency
    graph. See {!row_hook_ambient_undo_target}, which is how
    [Db.register_row_hook]/[Db.unregister_row_hook] recover it instead of
    reaching for their own (possibly wrong, cross-handle) catalog. *)
val run_in_row_hook_scope
  :  t
  -> depth:int
  -> register_undo:((unit -> unit) -> unit)
  -> (unit -> 'a Lwt.t)
  -> 'a Lwt.t

(** #752 (review round 8, item 2): the recursion depth to attribute a NEW row
    hook firing for [t] against, given [t]'s [row_hooks]. [Db]'s
    [fire_ocaml_row_hook] consults this instead of {!row_hook_depth} directly
    for its recursion-limit check.

    {!row_hook_depth} is a store-wide counter that is decremented the
    instant a hook's own synchronous extent ends — including when that
    extent ends because the hook merely SCHEDULED its recursive next step
    via [Lwt.async] and returned immediately, well before the scheduled step
    actually runs. A chain of such hooks — each firing, scheduling its own
    reinvocation, and returning — would therefore never appear to nest by
    {!row_hook_depth}'s reading alone, letting {!max_row_hook_depth}'s bound
    in [Db] go unenforced indefinitely.

    This function instead prefers the depth captured on the row-hook scope
    (see {!run_in_row_hook_scope}) of the currently-running continuation, if
    it is a causal descendant of one for THIS store — which remains valid
    even after that scope's own synchronous extent has ended, exactly
    covering the deferred-[Lwt.async] case above — and falls back to the
    plain {!row_hook_depth} counter only when there is no such ambient
    scope (a fresh top-level statement, or a genuinely synchronous nested
    chain reached through a sibling {!Db.t} with no causal Lwt link to the
    firing hook — {!Db.create_worker_handle}, #589 — the case round 4's
    shared-counter test exercises).

    #774: "for THIS store" means "for a store in the same row-hook registry
    lineage", not "for this exact store object". {!Db.vacuum} swaps a wholly
    new {!t} in under the handle and moves the registry across with
    {!row_hooks_carry_over}; a scope captured before that swap names the old
    object, and testing physical identity would drop back to the fresh
    store's zeroed counter — handing a chain that had already nested deep a
    full [max_row_hook_depth] budget again. *)
val row_hook_effective_depth : t -> row_hooks -> int

(** [true] iff the currently-running continuation is a causal descendant of a
    row hook callback firing for [t] AND still within that hook invocation's
    genuine dynamic extent — i.e. it has not yet returned (synchronously), nor
    settled via a deferred [Lwt.async] continuation constructed while it was
    still running. See {!row_hook_scope} and {!run_in_row_hook_scope}'s doc
    comments for the full mechanism ({!rw_begin} is the original consumer).

    #752 (review round 8, item 1): [Db.register_row_hook]/
    [Db.unregister_row_hook] consult this to decide whether a registry
    mutation needs a #269 schema-undo entry — a mutation made from a hook's
    OWN body (this predicate [true]) is reversible by whatever write is
    currently in flight (an explicit transaction's ROLLBACK, or — when there
    is none — the autocommit DML statement that fired the hook, via
    {!Sql.Exec.execute_insert}/[execute_update]/[execute_delete]'s own
    [owned]-mode rollback), so it must always be tracked. A mutation made from
    ordinary top-level application code (this predicate [false], no ambient
    hook firing) has no such statement or transaction to be undone by unless
    ONE happens to be explicitly open — {!Db.explicit_txn}'s pre-existing,
    unchanged check — and must NOT be tracked otherwise: nothing would ever
    consume (commit or roll back) an undo pushed there, and it would linger on
    the log to be wrongly replayed by whatever unrelated rollback happens
    next. *)
val in_row_hook_for : t -> bool

(** The schema-undo target to push a row-hook registry mutation onto, when
    the currently-running continuation is a causal descendant of a row hook
    callback firing for [t] and still within that hook invocation's genuine
    dynamic extent — the same condition {!in_row_hook_for} tests. [None]
    exactly when {!in_row_hook_for} would answer [false].

    #752 (review round 9, finding 2): [Db.register_row_hook]/
    [Db.unregister_row_hook] consult this INSTEAD OF their own handle's
    [Cat.t] whenever it returns [Some] — the round-8 gate
    ([Option.is_some t.explicit_txn || in_row_hook_for t.store]) pushed onto
    the CALLING handle's catalog unconditionally, which is wrong when a hook
    body running as part of a DIFFERENT sibling [Db.t]'s statement (multiple
    handles can share one [Store.t] via {!Db.create_worker_handle},
    #589/#633/#632) calls [register_row_hook]/[unregister_row_hook] on this
    handle: the undo must land on the FIRING statement's own catalog, not the
    target handle's, or neither handle's rollback/commit will ever resolve
    it. *)
val row_hook_ambient_undo_target : t -> ((unit -> unit) -> unit) option

(** #772: whether the backing device can make a write durable.

    [Mirage_block.S] has exactly four operations — [get_info], [read], [write],
    [disconnect] — and no flush or barrier, so an adapter over it reports
    [`Unavailable] carrying a reason that names the backend and the missing
    capability.  {!open_block} and {!open_block_wal} then refuse every
    durability level that would promise a barrier they cannot issue, instead of
    acking commits as durable while the bytes sit in a volatile cache.

    A structural polymorphic variant rather than a nominal type on purpose:
    [granary.mirage_block] produces this value (see
    [Granary_mirage_block.Mirage_backend.Make.durability_barrier]) without
    depending on [granary.store]. *)
type barrier =
  [ `Available
  | `Unavailable of string
  ]

(** #298: per-deployment durability mode (analogue of SQLite [synchronous]).
    [Full] fsyncs the WAL on every group-commit before acking (the default,
    unchanged behaviour).  [Batched] acks immediately and defers the fsync
    until [commits] un-synced commits accumulate OR [interval_ms] have elapsed
    since the last sync (whichever first; the time bound needs a clock — see
    {!set_clock} — otherwise only the commit count triggers).  [Off] never
    fsyncs on commit.  Checkpoint and {!close} are always full-sync anchors,
    so [Batched]/[Off] data is made durable there.  The setting is
    DATABASE-WIDE (the commit queue is shared across connections), not
    per-connection.  No-op on the in-memory backend.

    {b Crash safety:} an app-process crash is safe in every mode — unsynced
    WAL frames live in the OS page cache, which survives process death, and
    recovery replays them.  An OS or power crash with [Batched]/[Off] loses
    acked commits in the un-synced window; recovery converges to a prefix of
    acked commits (never torn state), but those commits may be gone. *)
type durability =
  | Full
  | Batched of
      { commits : int
      ; interval_ms : int
      }
  | Off

(** Errors from the persistent (B+-tree) backend.  The in-memory backend
    never returns errors. *)
type error =
  | Block_error of string
  | Corruption of string
  | Key_too_large of int
  | Value_too_large of int
  | Header_error of string
  | Encryption_key_required (** DB is encrypted but no key was supplied *)
  | Encryption_key_mismatch (** supplied key fails the header canary *)
  | Not_encrypted (** a key was supplied for a plaintext DB *)
  | Encryption_rng_unseeded
  (** a key was supplied but {!Mirage_crypto_rng} is not seeded, so no per-page
      nonce can be generated — the application must seed the RNG at boot
      (e.g. [Mirage_crypto_rng_unix.use_default ()] or a Mirage entropy source) *)
  | History_unavailable (** as-of API used on a store opened without the feature *)
  | History_pruned (** as-of target is older than the retained floor *)
  | History_misconfigured (** [as_of_history:true] but no history sink supplied *)
  | Durability_unavailable of string
  (** (#772) the requested durability level needs a write barrier the backend
      cannot issue.  Carries the whole refusal text — the level asked for, the
      backend's own reason, and the way out — so {!pp_error} prints it
      verbatim. *)

(** Pretty-print an {!error}. *)
val pp_error : Format.formatter -> error -> unit

(** Open a fresh in-memory store with no trees. *)
val create : unit -> t

(** Open a B+-tree backed store from any block device, given as I/O callbacks.
    Probes pages 0 and 1 for valid headers; if both are corrupt, treats the
    device as fresh and initialises it.  Pass [~n_pages:0L] for Mirage adapters
    (which bound-check internally against device capacity).
    [~close] is called by [Store.close].

    [init_if_corrupt] controls the both-headers-corrupt case: when [true] the
    device is treated as fresh and initialised (correct for zeroed block
    devices); when [false] it returns [Header_error] instead, so an
    existing-but-corrupt file is not silently clobbered.

    [geom] (#95, default {!Granary_storage.Geometry.default}) is the geometry
    used when CREATING a fresh device.  For an existing device the geometry is
    discovered by peeking page 0, and [geom] is ignored.  The block backend's
    own page size must already match (see [Unix_file.set_page_size]).

    [key] (#84, opt-in): when supplied (a 32-byte AES-256 key) the database is
    opened — or, if fresh, created — encrypted; pages >= 2 are stored as
    ciphertext while the pager and B+-tree only ever see plaintext.  Absent ⇒
    plaintext, the default.  May return [Encryption_key_required] (encrypted DB,
    no key), [Encryption_key_mismatch] (wrong key), [Not_encrypted] (key
    supplied for a plaintext DB) or [Encryption_rng_unseeded] (a key was given
    but the RNG was never seeded).

    (#772) [barrier] declares whether the backing device can make a write
    durable; it defaults to [`Available], so every existing caller (and every
    real file backend, which has [fsync]) is unchanged.  When it is
    [`Unavailable], the only durability level that can be honoured is [Off], and
    any other — including the [Full] default — is refused with
    [Durability_unavailable] before a single device operation is issued.  The
    escape hatch is therefore the honest [~durability:Off], not a flag that
    would let [Full] keep lying; see {!barrier}.

    (#772) [durability] sets the durability level at open (default [Full]),
    ahead of the [barrier] check.  Equivalent to a {!set_durability} straight
    after the open, except that this is the only spelling the [barrier] check
    can consult.

    (#266) When [as_of_history] is [true], a [history] sink MUST be supplied
    (else [History_misconfigured]); each commit is recorded for as-of reads.
    [now] supplies the wall-clock (ms since epoch) stamped onto each record.
    Default [false] — feature off, zero overhead. *)
val open_block
  :  ?as_of_history:bool
  -> ?history:History.sink
  -> ?now:(unit -> int64)
  -> ?key:string
  -> ?geom:Granary_storage.Geometry.t
  -> ?barrier:barrier
  -> ?durability:durability
  -> init_if_corrupt:bool
  -> read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> resize:(n_pages:int64 -> (unit, string) result Lwt.t)
  -> n_pages:int64
  -> close:(unit -> unit Lwt.t)
  -> unit
  -> (t, error) result Lwt.t

(** Open a B+-tree backed store in WAL mode. Commits append dirty pages
    to the WAL device; reads route through the WAL first and fall back
    to the main DB. Crash recovery is performed automatically when the
    WAL is opened.  [geom] behaves as in {!open_block} (#95).

    [key] (#84, opt-in) behaves as in {!open_block}: a 32-byte AES-256 key
    opens (or creates) the database encrypted — both the main-DB pages >= 2 and
    the WAL frame payloads — while the pager and B+-tree see only plaintext.
    Absent ⇒ plaintext, the default.  May return [Encryption_key_required],
    [Encryption_key_mismatch], [Not_encrypted] or [Encryption_rng_unseeded].

    (#266) When [as_of_history] is [true], a [history] sink MUST be supplied
    (else [History_misconfigured]); each commit is recorded for as-of reads.
    [now] supplies the wall-clock (ms since epoch) stamped onto each record.
    Default [false] — feature off, zero overhead.

    [barrier] and [durability] (#772) behave exactly as in {!open_block}: with
    [`Unavailable] every level but [Off] is refused up front, because a WAL
    group-commit's whole durability claim rests on the barrier. *)
val open_block_wal
  :  ?as_of_history:bool
  -> ?history:History.sink
  -> ?now:(unit -> int64)
  -> ?key:string
  -> ?geom:Granary_storage.Geometry.t
  -> ?barrier:barrier
  -> ?durability:durability
  -> read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> resize:(n_pages:int64 -> (unit, string) result Lwt.t)
  -> n_pages:int64
  -> wal_read_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> wal_write_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> wal_sync:(unit -> (unit, string) result Lwt.t)
  -> wal_size_bytes:int64
  -> ?wal_resize:(int64 -> (unit, string) result Lwt.t)
       (** (#612) Shrink the WAL device to a byte length.  Supplied, a
           checkpoint physically truncates the WAL back to its 24-byte header
           instead of leaving the file at its all-time high-water mark;
           omitted, the space is reused but never returned.  Optional because
           not every backing device can shrink. *)
  -> close:(unit -> unit Lwt.t)
  -> wal_close:(unit -> unit Lwt.t)
  -> unit
  -> (t, error) result Lwt.t

(** Close the store: drain in-flight background work, fsync any unsynced WAL
    frames (batched/off modes), then release the WAL and backing fds.  Raises if
    the final fsync fails (close is a durability anchor — an EIO/ENOSPC is
    surfaced, not swallowed).

    Quiesce contract (#338).  [close] does NOT acquire the write lock — an
    abandoned write transaction holds it until commit/rollback, and close must
    not hang on that.  Instead:
    - Callers MUST stop issuing new transactions before calling [close]; a write
      begun after close starts is rejected ({!rw_begin} fails once closing).
    - An in-flight autocheckpoint or replication sink ship that is actively
      touching the fds is drained first, so teardown never pulls the WAL/pager
      out from under it.  Since #719 a checkpoint's page migration is one such
      region (it no longer runs under the write lock), so [close] waits for the
      migration pass in flight and no further pass starts.
    - An autocheckpoint merely parked (on the replication floor, or on the write
      lock behind an open txn) is abandoned cleanly without performing further
      I/O; the WAL is left un-truncated and replays on next open (no data
      loss).

    After [close] returns, any further use of the store or its txns is
    undefined. *)
val close : t -> unit Lwt.t

(** [set_tree_tag t tid tag] registers the per-tree page-header stamp (#174):
    the low 32 bits of [tid]'s schema fingerprint.  Branch/Leaf pages written
    for [tid] thereafter carry [tag] in their reserved header bytes (12–15), so
    an orphaned page self-identifies its schema during recovery.  No-op on the
    in-memory backend. *)
val set_tree_tag : t -> tree_id -> int32 -> unit

(** Begin a read-only transaction. Multiple RO txns may run concurrently.

    Fails when the store is closing, and — on a FOLLOWER only — when the
    snapshot's frame horizon would sit below content a checkpoint has already
    copied into the main file (#739).  A snapshot resolves any page whose every
    WAL frame is at or above its horizon from the main file, so such a snapshot
    would observe data past this follower's last-applied commit; the horizon of
    a non-follower is [Wal.committed_frames] and is at or above that boundary by
    construction, so this can never fire there.  {!checkpoint} refuses to start
    in follower mode, so the only way to reach the refusal is switching follower
    mode on while a checkpoint is already migrating; it clears when that
    checkpoint completes. *)
val ro_begin : t -> ro txn Lwt.t

(** [history_pin t ~txn_id] (#266) sets the retention floor: pages reachable from
    commits [>= txn_id] are never reused, so they stay queryable via
    {!ro_begin_as_of}.  No-op on the in-memory backend or when as-of is off.

    The floor is {b not retroactive}: pin {b before} the writes you want to retain
    across.  Pinning after superseded pages have already been recycled cannot
    recover them — an as-of read of such a target returns [History_pruned]. *)
val history_pin : t -> txn_id:int64 -> unit

(** The current retention floor, or [None] when unset. *)
val history_floor : t -> int64 option

(** Clear the retention floor; superseded pages become reclaimable again. *)
val history_release : t -> unit

(** All retained commit-log records in ascending txn order ([] when as-of is off). *)
val history_log : t -> History.record list Lwt.t

(** [history_enabled t] (#266/#412) reports whether the store was opened with an
    as-of commit-log sink ([~as_of_history:true] with a sink supplied).  [false]
    on the in-memory backend or when as-of history is disabled.  Used by ATTACH
    to inherit the top handle's as-of setting. *)
val history_enabled : t -> bool

(** [ro_begin_as_of t target] opens a read-only snapshot against the retained
    root with the largest txn/timestamp [<=] [target].  Raises {!History_error}
    [History_unavailable] when as-of is off and [History_pruned] when the target
    predates the retained floor.  End it with {!ro_end} like any RO txn.

    As-of reads serve {b only} what an active retention floor protects: with no
    floor set (via {!history_pin}) every target resolves to [History_pruned],
    because uncapped commits recycle superseded pages and a snapshot would read
    garbage.  The floor is not retroactive — see {!history_pin}.

    Subject to {!ro_begin}'s #739 refusal too: a retained historical root is
    still resolved through the snapshot's frame horizon. *)
val ro_begin_as_of : t -> History.target -> ro txn Lwt.t

(** Raised by {!ro_begin_as_of} to carry an as-of {!error}. *)
exception History_error of error

(** Begin a read-write transaction. Only one RW txn may be active at a
    time; this call blocks until the previous one commits or rolls back.

    {b Refuses immediately, rather than blocking forever, when called from
    inside a row hook callback that is itself still running as part of an
    outer, not-yet-committed write transaction on THIS store (#752 review
    round 6, item 2).} See {!run_in_row_hook_scope}'s doc comment for why —
    in short, the outer transaction's own writer-lock release is
    unreachable until this nested call returns, so blocking on the lock
    here would never resolve. *)
val rw_begin : t -> rw txn Lwt.t

(** Commit a read-write transaction, making its mutations durable. *)
val commit : rw txn -> unit Lwt.t

(** Roll back a read-write transaction. Phase 0: mutations cannot be
    rolled back (they were applied immediately); this just releases the
    writer lock. Phase 3 introduces true rollback. *)
val rollback : rw txn -> unit Lwt.t

(** Push a named savepoint, snapshotting the current shadow state (#178).
    Implemented on {b both} backends: the Mem arm snapshots the shadow tree map,
    the B-tree arm snapshots the meta root, every tree root, the freelist,
    [n_pages], the pager's dirty set and its txn-owned pool. Names form a stack
    and are matched newest-first, so identical names nest LIFO.

    The B-tree snapshot is proportional to the dirty set, so this is not free —
    see #631's use of it as a statement-level undo point, which is gated to a
    narrow case for that reason. *)
val savepoint_begin : rw txn -> string -> unit Lwt.t

(** Release the named savepoint and all newer ones.
    Writes accumulated since the savepoint remain in the outer transaction.
    Unknown name: no-op. *)
val savepoint_release : rw txn -> string -> unit Lwt.t

(** Restore to the named savepoint, dropping all newer savepoints.
    The named savepoint is kept so ROLLBACK TO can be repeated.
    Unknown name: no-op.

    This restores {b store} state only. Callers that also hold catalog state
    derived from those trees — cached rowid counters, columnar stores, the
    schema-undo log — must pair it with {!Granary_catalog.Catalog.savepoint_rollback_schema}
    (#280/#303), as the db layer's [ROLLBACK TO] handler does. *)
val savepoint_rollback : rw txn -> string -> unit Lwt.t

(** End a read-only transaction. *)
val ro_end : ro txn -> unit Lwt.t

(** [with_ro t f] runs [f] over a fresh RO snapshot and ends the snapshot
    when [f] finishes — including if [f] raises. Use this instead of a bare
    [ro_begin]/[ro_end] pair so an error mid-read cannot leak the read lock,
    the active-reader refcount, or the snapshot's pinned pages (#164). *)
val with_ro : t -> (ro txn -> 'a Lwt.t) -> 'a Lwt.t

(** Look up a key in a tree. Works in both RO and RW transactions. *)
val get : _ txn -> tree_id -> bytes -> bytes option Lwt.t

(** Insert or update a key in a tree. Only available in RW transactions. *)
val put : rw txn -> tree_id -> bytes -> bytes -> unit Lwt.t

(** [put_x tx tid key value] inserts [value] at [key] if absent.
    Returns [None] on success (key was new, value written).
    Returns [Some Bytes.empty] (conflict sentinel) when key already existed
    (NOT overwritten).  Callers that need the old bytes must fetch via {!get}. *)
val put_x : rw txn -> tree_id -> bytes -> bytes -> bytes option Lwt.t

(** Delete a key from a tree. No-op if the key does not exist. Only
    available in RW transactions. *)
val del : rw txn -> tree_id -> bytes -> unit Lwt.t

(** A cursor for iterating over an ordered tree snapshot. *)
type cursor

(** Open a cursor over a tree. The cursor sees a snapshot of the tree as
    of the moment it was opened. Works in both RO and RW transactions. *)
val cursor_open : _ txn -> tree_id -> cursor Lwt.t

(** Close a cursor, releasing its resources. *)
val cursor_close : cursor -> unit

(** Result of a seek or first operation. *)
type seek_result =
  | Found of bytes (** Cursor is positioned at the exact key. *)
  | Not_found of [ `Greater of bytes | `End ]
  (** No exact match. [`Greater k] means cursor is at the next key [k].
        [`End] means there is no key >= the sought key. *)

(** Seek to the smallest key >= the given key.
    Returns [Found k] if [k] matches exactly, or [Not_found] otherwise.
    After this call, [cursor_next] returns the entry at or after the
    sought position. *)
val cursor_seek : cursor -> bytes -> seek_result

(** Position the cursor at the first key in the tree.
    Returns [Found k] if the tree is non-empty, [Not_found `End] if empty.
    After this call, [cursor_next] returns the first entry. *)
val cursor_first : cursor -> seek_result

(** Advance the cursor and return the current (key, value) pair, or [None]
    if the cursor is exhausted.
    On the first call after [cursor_open], [cursor_first], or
    [cursor_seek], returns the positioned entry (not the next one).
    On subsequent calls, advances and returns the next entry. *)
val cursor_next : cursor -> (bytes * bytes) option

(** Return the value at the current cursor position without advancing,
    or [None] if the cursor is not positioned (exhausted or before first). *)
val cursor_value : cursor -> bytes option

(** #716: the tree's greatest key, or [None] when the tree is empty, in
    O(log n) — a rightmost descent rather than a scan, degrading only by the
    number of empty pages it skips (a table whose rows were all deleted and
    committed leaves a branch over N empty leaves, and this still costs
    O(pages) rather than O(1)).  Sees the same snapshot as {!get} /
    {!cursor_open} on the same transaction.

    Use this instead of draining a cursor to find a maximum: {!cursor_open}
    materialises every key and value in the tree before returning.

    Raises {!Max_key_error} on a B+-tree backend error (never on the
    in-memory backend, which cannot fail here).  That covers BOTH failure
    sites, which matters because they read different trees: the descent
    itself, and resolving [tree_id]'s root, which reads the META tree and so
    fails on damage the data tree does not have.  Root resolution used to
    raise a stringified [Failure] instead, escaping every caller matching on
    this exception (#716 round-5 review finding 2). *)
val max_key : _ txn -> tree_id -> bytes option Lwt.t

(** Raised by {!max_key} to carry the underlying B+-tree {!error} typed,
    rather than flattened into a string (#716 round-4 review finding 1).
    [Corruption] is the class a caller may choose to tolerate — a damaged
    page or a [Btree.Tree_corrupt] guard firing; every other constructor,
    in particular [Block_error] (a transient I/O failure), should propagate:
    tolerating corruption is not the same as tolerating a device that failed
    to answer a healthy read. *)
exception Max_key_error of error

(** A lazy, streaming forward cursor positioned by {!seek_ge}.  Unlike
    {!cursor}, it does NOT materialise the whole tree: it descends the
    B+-tree in O(log n) and reads only the entries the caller consumes.
    Use for point/prefix probes (index lookups, UNIQUE/FK pre-checks)
    where draining the full tree would be O(n) per probe (#228, #229). *)
type seek_cursor

(** Open a streaming cursor positioned at the first entry whose key is
    [>= key], in O(log n).  Sees the same snapshot as {!get}/{!cursor_open}
    on the same transaction (committed state + this txn's own writes for an
    RW txn; the captured snapshot for an RO txn). *)
val seek_ge : _ txn -> tree_id -> bytes -> seek_cursor Lwt.t

(** Return the next [(key, value)] in ascending key order, or [None] when
    exhausted.  The first call after {!seek_ge} returns the positioned entry
    (the first with key [>=] the seek key), not the one after it. *)
val seek_next : seek_cursor -> (bytes * bytes) option Lwt.t

(** #481: {!seek_next} without the key.  Same traversal, same values, same
    stack-bounding pause schedule (the two share one call counter, so they may
    be mixed on one cursor); the B+-tree backend simply never copies the entry
    key out of the leaf page.  Use it wherever the key is discarded — the
    sequential table scan and every aggregate over it.

    Like {!seek_next}, at most ONE call may be in flight per cursor — mixing the
    two still means one of either.  Since #481 the B+-tree backend describes the
    current entry in cursor-level mutable state across a possible yield, so
    overlapping pulls on one cursor can return a value from the wrong entry
    (before #481 they could only reorder or skip). Give each fiber its own
    cursor. *)
val seek_next_value : seek_cursor -> bytes option Lwt.t

(** Release any resources held by a {!seek_cursor}. *)
val seek_close : seek_cursor -> unit

(** True if the store is operating in WAL mode (opened via
    [open_block_wal]). *)
val wal_mode : t -> bool

(** Migrate every page in the WAL index to the main DB, sync, then reset the
    WAL.  No-op outside WAL mode.

    {b #719: this no longer holds the writer lock for the whole migration.}  The
    page copying and its fsync run with the lock free — a half-migrated main
    file is invisible, because every read resolves through the WAL overlay until
    [Wal.reset] retires it — and the lock is taken once, at the end, to catch up
    on whatever was committed meanwhile and truncate the WAL.  So this
    serialises with commits only for that final step, and a commit issued while
    a checkpoint is copying pages no longer waits for it.

    Checkpoints still serialise against {e each other} (including the background
    autocheckpoint) for the whole of their duration.

    {b #739: rejected while the store is in follower mode.}  A follower's WAL
    belongs to the replication apply loop, and {!ro_begin} caps every snapshot at
    {!follower_ack_position} so a reader never observes a frame past the last
    applied commit.  A checkpoint copies frames into the main file and then
    retires the overlay, where that cap cannot reach them — so it would make the
    guarantee permanently unenforceable rather than merely racy.  The
    autocheckpoint path was already unreachable there ({!rw_begin} refuses writes,
    so no commit dispatches one).  [Standby] is unaffected: it migrates through
    [Replication.checkpoint_wal_to_main], not this function. *)
val checkpoint : t -> unit Lwt.t

(** #638: the checkpoint-failure signal.  [last_error] is the message of the
    most recent failure and [consecutive_failures] the number of failures since
    the last checkpoint that completed — both cleared by a completing
    checkpoint, so a nonzero [consecutive_failures] means the WAL is growing
    right now.  [total_failures] counts every failure since open and is never
    cleared by success. *)
type checkpoint_health =
  { last_error : string option
  ; total_failures : int
  ; consecutive_failures : int
  }

(** Current checkpoint-failure state (#638).  An {i auto}checkpoint failure is
    not raised to any caller — the commit that triggered it already succeeded
    and its WAL frames are still valid — so this (together with
    {!Store_event.Checkpoint_failed}) is how a failing checkpoint becomes
    observable at all.  Returns the all-clear on the in-memory backend, which
    has no WAL. *)
val checkpoint_health : t -> checkpoint_health

(** #637: what recovery's WAL walk observed about generation boundaries at
    open, lifted from {!Granary_storage.Wal.replay_check}.

    {b What it detects.}  Before #636, [Wal.reset] cleared only in-memory state:
    every frame of the checkpointed generation stayed on disk under an unchanged
    [(salt, seed)] marker and therefore still verified, so the next open replayed
    it — over newer data when the successor generation was shorter.  Each commit
    writes exactly one header page with [txn_id = previous + 1], so a header
    [txn_id] that fails to increase as recovery walks forward means the walk has
    left the newest generation and entered the remains of an older one.  That is
    the signature, and it is present in both of #636's outcomes.

    {b What it does NOT detect, and why the status has three values rather than
    two.}
    - Damage that a PREVIOUS open already replayed into the main file.  The
      row-loss variant leaves a structurally valid database, so no integrity
      check finds it either, and once the WAL has been rotated by a post-#636
      checkpoint the evidence is gone.  This is a report on {i this open's} WAL,
      not a verdict on the file.
    - A stale remainder that is a fragment of a single old commit batch carrying
      no header-page frame.  Recovery can consume such a fragment, and if it
      contains a commit-flagged frame the fragment is applied — undetected.  Any
      stale remainder spanning a whole old commit does contain a header frame.
    - A stale remainder that recovery walked but never APPLIED, because no
      commit frame followed it.  That is deliberately not reported: it is the
      ordinary residue of a crash-torn write, harms nothing, and flagging it
      would make the detector cry wolf on correct databases.
    - A database that will not open at all (#636's other outcome) never reaches
      this, but it is loud by construction.

    So [Wal_replay_no_evidence] means "walked, compared, and nothing regressed",
    never "verified clean"; and a walk with fewer than two header frames to
    compare reports {!Wal_replay_not_examined} rather than pretending to the
    former.  [frames_walked] and [header_frames] are reported so a reader can
    see how much material the test had. *)
type wal_replay_status =
  | Wal_replay_not_examined
  (** No WAL, or fewer than two header frames were walked: the check had
        nothing to compare.  Not a claim either way. *)
  | Wal_replay_no_evidence
  (** The frames recovery walked at this open showed no generation
        regression.  Not a clean bill of health for the database. *)
  | Wal_replay_stale_generation of
      { frame_idx : int
      ; previous_txn_id : int64
      ; frame_txn_id : int64
      }
  (** Recovery walked into frames belonging to an older generation: at
        [frame_idx] a header page carried [frame_txn_id], no greater than the
        [previous_txn_id] already seen.  A database written by a pre-#636 binary
        has replayed stale data over newer data. *)

(** #637: {!wal_replay_status} plus how much material the check had. *)
type wal_replay_check =
  { status : wal_replay_status
  ; frames_walked : int
  ; header_frames : int
  }

(** The stale-generation report for this store (#637).  Reports
    {!Wal_replay_not_examined} on the in-memory backend and outside WAL mode.
    Surfaced to SQL as [PRAGMA wal_replay_check]. *)
val wal_replay_check : t -> wal_replay_check

(** Clear the sticky checkpoint-failure signal ([last_error] and
    [consecutive_failures]); [total_failures] is left alone.  For an operator
    who has acknowledged the condition.  No-op on the in-memory backend. *)
val clear_checkpoint_error : t -> unit

(** The durability mode type is declared earlier in this interface (just after
    {!barrier}) because {!open_block} takes it; see there for the full
    contract. *)

(** (#772) The backing device's durability capability, as declared at open.
    [`Available] on the in-memory backend and on every file backend.  When
    [`Unavailable], the store is pinned to [Off] — {!open_block} refused
    anything else and {!set_durability} keeps refusing — and no barrier is
    issued at commit, checkpoint or {!close}, because there is none to issue and
    no durability claim outstanding for one to satisfy. *)
val barrier : t -> barrier

(** Current durability mode. Returns [Full] on the in-memory backend. *)
val durability : t -> durability

(** Set the durability mode. [Batched] params are remembered across switches
    to [Full]/[Off] (so a later [PRAGMA synchronous=batched] restores them).
    No-op on the in-memory backend.  Negative [Batched] params are clamped to 0.
    When the mode actually changes, the batched durability counters
    ([unsynced_commits] and the T window) are reset, so a long [Off] period
    does not carry a stale count into [Batched]; durability is unaffected
    because checkpoint/close remain the anchors.

    Contract while a replication commit-sink is active (#336): a request to
    relax below [Full] ([Batched]/[Off]) is SILENTLY IGNORED at this Store-API
    layer — the mode stays [Full], though any [Batched] N/T params supplied are
    still recorded so they take effect once the sink is removed (see
    {!set_commit_callback}, {!commit_callback_active}).  This deliberately
    differs from the SQL layer, where [PRAGMA synchronous] raises on the same
    request: an embedder driving the store directly opts into the "configure
    now, apply on sink removal" ergonomics, whereas an interactive SQL user
    expects an explicit error.  Use {!commit_callback_active} to check before
    calling if you need a signal.

    Contract on a barrier-less backend (#772): raises [Failure] when [d] is
    [Full] or [Batched] and {!barrier} is [`Unavailable].  Deliberately louder
    than the replication-sink arm above — that one defers a legal setting until
    the sink goes away, whereas this is a level the device can never reach, so
    there is nothing to defer and silence would reinstate the false durability
    ack this guard exists to remove. *)
val set_durability : t -> durability -> unit

(** Batched commit-count threshold N (default 256). Independent of the active
    mode; only takes effect while the mode is [Batched].
    Returns the default (256) on the in-memory backend. *)
val sync_batch_commits : t -> int

(** Set the batched commit-count threshold N (clamped to >= 0). A value of 0
    DISABLES the commit-count trigger (durability then relies on the time
    trigger, if any, plus checkpoint/close). No-op on the in-memory backend. *)
val set_sync_batch_commits : t -> int -> unit

(** Batched time threshold T in milliseconds (default 100).
    Returns the default (100) on the in-memory backend. *)
val sync_batch_interval_ms : t -> int

(** Set the batched time threshold T in milliseconds (clamped to >= 0). A value
    of 0 DISABLES the time trigger (durability then relies on the commit-count
    trigger, if any, plus checkpoint/close). No-op on the in-memory backend. *)
val set_sync_batch_interval_ms : t -> int -> unit

(** Force any committed-but-unsynced WAL frames to disk now (batched/off modes).
    A no-op in [Full] mode, on the in-memory backend, or when nothing is pending.
    Used when tightening durability (e.g. PRAGMA synchronous=full) so already-acked
    commits become durable immediately rather than only on the next commit. *)
val flush_unsynced : t -> unit Lwt.t

(** Parse a durability mode name (case-insensitive "full"|"batched"|"off").
    Returns [None] for anything else.  [Batched] uses the current default
    params; callers that need to preserve N/T should construct [Batched] from
    {!sync_batch_commits}/{!sync_batch_interval_ms} themselves. *)
val durability_of_string : string -> durability option

(** Canonical lowercase name of a durability mode ("full"|"batched"|"off"). *)
val string_of_durability : durability -> string

(** Install the wall-clock source ([unit -> float], Unix-epoch seconds) used by
    [Batched] mode's time threshold and by the #718 writer-lock accounting.
    Without one, the default [fun () -> 0.] disables the time trigger (only the
    commit count fires) and leaves every duration in {!lock_stats} at [0.].

    The [Batched] half is a no-op on the in-memory backend, which has no
    durability knob; the {!lock_stats} half is not, because both backends
    serialise writers through the same lock. *)
val set_clock : t -> (unit -> float) -> unit

(** [lock_stats t] snapshots the writer lock's wait/hold accounting (#718),
    attributed by acquisition site.

    This is the measurement that separates {e holding} the engine's one global
    critical section from merely spending time inside a statement: service-time
    profiling cannot see the split, because [commit] releases the lock before it
    fsyncs and a [BEGIN] that finds the lock held is waiting rather than
    working.  {!Granary_store.Lock_stats} documents the attribution rule and the
    one approximation it makes.

    Durations are [0.] until {!set_clock} has been called — the returned
    report's [clock_installed] field says which case a run of zeroes is. *)
val lock_stats : t -> Lock_stats.report

(** [reset_lock_stats t] discards every observation {!lock_stats} would report,
    so a benchmark can exclude its warm-up window.  A hold that is outstanding
    when this is called survives, with its start re-stamped to now; see
    {!Granary_store.Lock_stats.reset} for why both halves of that are
    deliberate. *)
val reset_lock_stats : t -> unit

(** Get the per-connection auto-checkpoint threshold (in WAL frames).
    A value of 0 means auto-checkpoint is disabled. Returns 0 on the
    in-memory backend. *)
val wal_autocheckpoint : t -> int

(** Set the per-connection auto-checkpoint threshold (in WAL frames).
    When [n > 0] and the WAL reaches [n] committed frames, the next
    writer's commit will inline a checkpoint before releasing the
    write lock. [n = 0] disables auto-checkpoint (negative values are
    clamped to 0). No-op on the in-memory backend. *)
val set_wal_autocheckpoint : t -> int -> unit

(** Number of fsyncs the WAL has performed since open.  Returns 0 for
    non-WAL backends.  Exposed for #77 group-commit testing: a
    well-behaved coordinator collapses N concurrent autocommit commits
    into far fewer than N fsyncs. *)
val wal_sync_count : t -> int

(** Total active RO-snapshot refcount across all snapshot txn_ids. Returns 0
    when no reader is live. Diagnostic/testing only (#164). *)
val active_reader_count : t -> int

(** Number of distinct pages currently pinned by live RO snapshots (#159).
    Diagnostic/testing only (#164). *)
val pinned_page_count : t -> int

(** Number of currently-held read locks on the store. Diagnostic/testing
    only (#164). *)
val live_read_locks : t -> int

(** Number of entries in the in-memory freelist (diagnostics / testing). *)
val freelist_size : t -> int

(** Raw freelist entries for testing — (page_id, freed_at_txn_id) pairs. *)
val freelist_entries : t -> (int32 * int64) list

(** Current total file page count (diagnostics / testing). *)
val n_pages : t -> int64

(** Enumerate every [tree_id] currently registered in the meta tree.
    The list is unordered.  On the in-memory backend, returns the keys
    of the per-tree hashtable.  Used by VACUUM (phase 37). *)
val list_tree_ids : t -> tree_id list Lwt.t

(** A page sink: receives [(page_id, page_bytes)] pairs.  The page
    buffer is a fresh copy — the callee owns it and may mutate or
    retain it freely beyond the returned Lwt.  No defensive copy is
    needed. *)
type page_sink = page_id:int64 -> page:Cstruct.t -> unit Lwt.t

(** One-shot, consistent, point-in-time full copy of an open database
    to a new destination via [sink].  Opens an RO snapshot that pins
    the view for the copy's lifetime; every page (including headers 0
    and 1) is resolved through the snapshot's WAL overlay before falling
    back to the main DB.  The destination receives a self-contained,
    fully-materialised DB with empty/absent WAL state — it opens
    standalone with a plain [open_file]/[open_block].

    Each page buffer passed to [sink] is a fresh copy; the sink owns it.

    The copy is a physical page copy: all features (FTS, secondary
    indexes, schema, freelist) come along as pages.  The iteration
    is bounded by a snapshot-time page count, so growth during the
    copy does not pull in pages outside the snapshot.

    When the source is encrypted (#84), data pages (>= 2) are re-encrypted
    under the source's key before reaching the sink, so the destination is a
    faithful, self-contained encrypted DB (open it with the same key) and no
    user-data plaintext transits the sink; pages 0 and 1 are the plaintext
    headers (enc marker + canary) and are copied verbatim.

    On the in-memory backend this is a no-op (there are no pages to
    copy). *)
val copy_to : t -> page_sink -> unit Lwt.t

(** [rekey_to t ~new_key sink] offline-rotates the encryption key (#215).  [t]
    must have been opened with the OLD key.  Reads every page as plaintext,
    re-encrypts data pages (>= 2) under a fresh cipher built from [new_key], and
    rewrites the header canary under the new key, sinking a self-contained
    encrypted page image (no WAL).  Returns [Not_encrypted] if [t] is not an
    encrypted store, or a [Block_error] if [new_key] is not 32 bytes. *)
val rekey_to : t -> new_key:string -> page_sink -> (unit, error) result Lwt.t

(** -------------------------------------------------------------------- *)

(** Replication consumer integration (#92)                                   *)

(** -------------------------------------------------------------------- *)

(** Register the replication consumer's shipped position so checkpoint
    truncation waits for frames to be shipped before recycling them.
    [~shipped] is the highest acknowledged frame index.  When set to
    [max_int] (the default), the replication consumer is effectively
    disabled and does not gate checkpoint.

    Broadcasts [reader_done_cond] so that any checkpoint currently
    parked in [wait_for_readers_past] is immediately woken. *)
val update_replication_position : t -> shipped:int -> unit

(** Get (epoch, committed_frames) for the active WAL; [None] if no WAL
    is in effect or on the in-memory backend. *)
val replication_state : t -> (int64 * int) option

(** Bounded-yield "timeout" the checkpoint gate spends waiting for the
    replication floor (a standby's acked position) to reach the checkpoint
    target before proceeding anyway (#207).  Returns [max_int] (unbounded,
    the default) on the B+-tree backend; [0] on the in-memory backend. *)
val replication_gate_max_yields : t -> int

(** Set the checkpoint gate's bounded-yield budget for the replication
    floor.  A dead or slow standby must not wedge the master's WAL forever:
    once the budget is spent the checkpoint proceeds and the now-stranded
    standby must re-base (#208).

    Pure-Mirage has no ambient clock, so this "timeout" is a count of
    cooperative [Lwt.pause] yields, not wall-clock time.  [max_int] (the
    default) means unbounded — wait indefinitely on
    {!update_replication_position}, exactly as before this knob existed.
    Negative inputs clamp to [0].  Local RO readers are never abandoned by
    this budget — only the replication floor.  No-op on the in-memory
    backend. *)
val set_replication_gate_max_yields : t -> int -> unit

(** -------------------------------------------------------------------- *)

(** Incremental backup (#265)                                                *)

(** -------------------------------------------------------------------- *)

(** A captured WAL frame for incremental backup.  Contains the full frame
    metadata and page payload needed to reconstruct the database.

    The {!checksum} field covers the decrypted page payload (transport
    integrity for the backup frame), matching the same scheme used by
    {!Granary_replication.replicated_frame}.  For unencrypted WALs the
    plaintext equals the on-disk page; for encrypted WALs the checksum
    guards against corruption of the decrypted content during transport
    or storage, not the on-disk ciphertext. *)
type backup_frame =
  { epoch : int64
  ; frame_idx : int
  ; page_id : int64
  ; is_commit : bool
  ; page : Cstruct.t
  ; checksum : int64
  ; source_salt : int64
  ; source_seed : int64
  }

(** Register the backup consumer's captured position so checkpoint
    truncation waits for frames to be backed up before recycling them.
    Analogous to {!update_replication_position} but for the incremental
    backup watermark (#265).  When set to [max_int] (the default), the
    backup consumer is effectively disabled and does not gate checkpoint.

    Broadcasts [reader_done_cond] so that any checkpoint currently
    parked in [wait_for_readers_past] is immediately woken. *)
val update_backup_position : t -> shipped:int -> unit

(** Get (epoch, committed_frames) for the active WAL; [None] if no WAL
    is in effect or on the in-memory backend. *)
val backup_state : t -> (int64 * int) option

(** Bounded-yield "timeout" the checkpoint gate spends waiting for the
    backup floor to reach the checkpoint target before proceeding anyway
    (#265).  Returns [max_int] (unbounded, the default) on the B+-tree
    backend; [0] on the in-memory backend. *)
val backup_gate_max_yields : t -> int

(** Set the checkpoint gate's bounded-yield budget for the backup floor.
    A slow or unreachable backup consumer must not wedge the master's WAL
    forever: once the budget is spent the checkpoint proceeds and
    un-captured frames are recycled (the backup must re-base).  Same
    semantics as {!set_replication_gate_max_yields}.

    Pure-Mirage has no ambient clock, so this "timeout" is a count of
    cooperative [Lwt.pause] yields, not wall-clock time.  [max_int] (the
    default) means unbounded — wait indefinitely.  A finite value bounds
    the stall: once the budget plus the replication budget is spent, the
    checkpoint proceeds even if the backup floor is behind.

    {b Caution (review #4):} [max_int] (the default) combined with a
    [capture_frames_since] returning [None] triggers a re-base cycle.  The
    backup consumer must copy the entire database before it can advance
    the floor via {!update_backup_position}, and during that re-base every
    write txn that triggers autocheckpoint is blocked on the backup floor.
    If re-base time exceeds your acceptable write-stall window, set a
    finite budget here so the checkpoint eventually proceeds and the
    re-base is allowed to complete as a fresh incremental chain.

    Negative inputs clamp to [0].  Local RO readers are never abandoned by
    this budget — only the backup floor.  No-op on the in-memory backend. *)
val set_backup_gate_max_yields : t -> int -> unit

(** Capture the committed WAL frames since a given watermark position,
    returning them as a list of {!backup_frame}.

    [~since_epoch] and [~since_idx] identify the watermark: frames with
    indices strictly greater than [since_idx] in the current epoch are
    returned.  If the WAL's epoch has advanced past [since_epoch], no
    frames can be captured (the caller must take a fresh base snapshot
    via {!copy_to}).

    Returns [None] when the WAL's epoch has changed (the watermark is
    stale — re-base needed).  Returns [Some []] when the watermark is
    current but no new frames have been committed.  Returns [Some (Error _)]
    on I/O or corruption errors. *)
val capture_frames_since
  :  t
  -> since_epoch:int64
  -> since_idx:int
  -> (backup_frame list, [> `Capture_error of string ]) result option Lwt.t

(** Install an asynchronous callback invoked after each WAL commit batch.
    The callback receives [~epoch], [~base_idx] (starting WAL frame index
    of this batch), and [~count] (number of frames committed).  Fired
    via [Lwt.async] so the commit path is never blocked by replication
    I/O.

    When a callback is registered, the replication shipped-position
    floor is initialised to the current [committed_frames] so that
    an autocheckpoint cannot recycle frames before the async sink
    reads and ships its first batch.  The consumer must still call
    {!update_replication_position} to advance the floor as frames
    are shipped.  Pass [None] to unregister (resets the floor to
    [max_int], disabling gating).

    A registered replication commit-sink pins durability to [Full];
    [Batched]/[Off] are rejected while a sink is active, because the checkpoint
    replica-floor gate requires every committed frame to be shipped, which only
    holds when every commit fsyncs.

    Registering a sink first {!flush_unsynced}es any committed-but-unsynced
    frames (#336): a store opened [off]/[batched] may have acked commits still
    in the OS page cache, and registration pins [Full] going forward but ships
    only NEW frames — so those historical frames are fsynced now rather than
    left crash-exposed.  This is why registration returns an [Lwt.t].

    (#772) Because registration pins [Full], registering a sink on a backend
    whose {!barrier} is [`Unavailable] fails with [Failure] rather than letting
    the pin re-establish a durability claim the device cannot meet.
    Unregistering ([None]) is always allowed. *)
val set_commit_callback
  :  t
  -> (epoch:int64 -> base_idx:int -> count:int -> unit Lwt.t) option
  -> unit Lwt.t

(** True iff a replication commit-sink is currently registered (see
    {!set_commit_callback}). While active, durability is pinned to [Full]. *)
val commit_callback_active : t -> bool

(** Re-export of the internal-events type (#382). *)
module Event = Store_event

(** Register (or clear with [None]) a synchronous, fire-and-forget observer for
    internal engine events — the internals monitor.  No-op on the in-memory
    backend (it has no storage seams).  The callback must not raise; any
    exception it throws is swallowed so it cannot break a transaction.  The
    callback should also be cheap and non-blocking: some events are emitted
    while the engine holds the write lock (transaction begin, savepoints,
    checkpoint start), so a slow observer can stall writers.  The intended use
    is an O(1) buffer push. *)
val set_event_callback : t -> (Event.t -> unit) option -> unit

(** -------------------------------------------------------------------- *)

(** Standby-follower integration (#172)                                      *)

(** -------------------------------------------------------------------- *)

(** Enable or disable follower mode.  When [true], {!rw_begin} rejects
    write transactions on the B+-tree backend with an exception, keeping
    the standby's WAL from diverging from the master's stream while the
    follower loop is applying incoming frames, and {!checkpoint} is rejected
    too (#739).  No-op on the in-memory backend (Mem stores have no standby
    semantics).

    Switching it on while a checkpoint is already in flight does not stop that
    checkpoint — it is past the refusal — so until it completes {!ro_begin} may
    refuse a snapshot whose horizon sits below what that checkpoint has already
    migrated, rather than serving one that would read the migrated pages. *)
val set_follower : t -> bool -> unit

(** True iff follower mode is active (write transactions are rejected). *)
val is_follower : t -> bool

(** Record a local [Wal.committed_frames] count as the follower's last-applied
    commit boundary.  [ro_begin] will cap RO snapshots to this position so
    readers never observe WAL frames past what has been applied on this node
    (#263).  The caller should supply the count from the WAL handle it applied
    into, so the value lives in local committed-frame count space (no coordinate
    mismatch vs. master epoch indices) without depending on WAL instance identity
    between the caller and the store.  No-op on the in-memory backend. *)
val set_follower_ack_position : t -> frames:int -> unit

(** Get the recorded follower ack position (a local [Wal.committed_frames]
    count), or [None] if not following or no position has been recorded yet. *)
val follower_ack_position : t -> int option

(** Wait for in-flight RO snapshots whose [snap_frames] is below [target] to
    complete.  Reuses the same reader-pin gating as the inline checkpoint
    ([ckpt_install]): local RO readers are waited on unconditionally;
    the replication and backup floors are subject to the store's configured
    bounded-yield budgets (#207, #265).  No-op on the in-memory backend.

    Used by the standby's epoch-transition checkpoint to ensure no live RO
    snapshot references WAL frame indices about to be recycled by
    [Wal.reset] (#263). *)
val wait_for_readers_past : t -> target:int -> unit Lwt.t
