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
    Default [false] — feature off, zero overhead. *)
val open_block_wal
  :  ?as_of_history:bool
  -> ?history:History.sink
  -> ?now:(unit -> int64)
  -> ?key:string
  -> ?geom:Granary_storage.Geometry.t
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
    time; this call blocks until the previous one commits or rolls back. *)
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
    calling if you need a signal. *)
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
    left crash-exposed.  This is why registration returns an [Lwt.t]. *)
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
