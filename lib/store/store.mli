(** Ordered byte-keyed store.
    Phase 0: in-memory [Bytes_map] per [tree_id].
    Phase 1: CoW B+-tree on BLOCK behind the same interface.

    Two backends are available:
    - [create ()] — in-memory [Bytes_map] (no size limits on keys/values).
      This is the legacy Phase 0 backend, retained for tests and
      ephemeral use cases that exceed B+-tree leaf-cell size limits.
    - [open_block ~...] — CoW B+-tree over any BLOCK device, given as I/O
      callbacks. Persistent across reopen. Keys ≤ 512 bytes, values ≤ 1024
      bytes. Unix-file convenience constructors live in the [sqlocaml.unix]
      driver library so this core stays platform-agnostic (#170). *)

type t

(** Page geometry (#95), re-exported so callers can name {!Geometry.t} without
    depending on [sqlocaml.storage] directly. *)
module Geometry = Sqlocaml_storage.Geometry

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

    [geom] (#95, default {!Sqlocaml_storage.Geometry.default}) is the geometry
    used when CREATING a fresh device.  For an existing device the geometry is
    discovered by peeking page 0, and [geom] is ignored.  The block backend's
    own page size must already match (see [Unix_file.set_page_size]).

    [key] (#84, opt-in): when supplied (a 32-byte AES-256 key) the database is
    opened — or, if fresh, created — encrypted; pages >= 2 are stored as
    ciphertext while the pager and B+-tree only ever see plaintext.  Absent ⇒
    plaintext, the default.  May return [Encryption_key_required] (encrypted DB,
    no key), [Encryption_key_mismatch] (wrong key), [Not_encrypted] (key
    supplied for a plaintext DB) or [Encryption_rng_unseeded] (a key was given
    but the RNG was never seeded). *)
val open_block
  :  ?key:string
  -> ?geom:Sqlocaml_storage.Geometry.t
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
    [Encryption_key_mismatch], [Not_encrypted] or [Encryption_rng_unseeded]. *)
val open_block_wal
  :  ?key:string
  -> ?geom:Sqlocaml_storage.Geometry.t
  -> read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> resize:(n_pages:int64 -> (unit, string) result Lwt.t)
  -> n_pages:int64
  -> wal_read_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> wal_write_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> wal_sync:(unit -> (unit, string) result Lwt.t)
  -> wal_size_bytes:int64
  -> close:(unit -> unit Lwt.t)
  -> wal_close:(unit -> unit Lwt.t)
  -> unit
  -> (t, error) result Lwt.t

(** Close the store. After this, any use of the store or its txns is
    undefined. *)
val close : t -> unit Lwt.t

(** [set_tree_tag t tid tag] registers the per-tree page-header stamp (#174):
    the low 32 bits of [tid]'s schema fingerprint.  Branch/Leaf pages written
    for [tid] thereafter carry [tag] in their reserved header bytes (12–15), so
    an orphaned page self-identifies its schema during recovery.  No-op on the
    in-memory backend. *)
val set_tree_tag : t -> tree_id -> int32 -> unit

(** Begin a read-only transaction. Multiple RO txns may run concurrently. *)
val ro_begin : t -> ro txn Lwt.t

(** Begin a read-write transaction. Only one RW txn may be active at a
    time; this call blocks until the previous one commits or rolls back. *)
val rw_begin : t -> rw txn Lwt.t

(** Commit a read-write transaction, making its mutations durable. *)
val commit : rw txn -> unit Lwt.t

(** Roll back a read-write transaction. Phase 0: mutations cannot be
    rolled back (they were applied immediately); this just releases the
    writer lock. Phase 3 introduces true rollback. *)
val rollback : rw txn -> unit Lwt.t

(** Push a named savepoint by snapshotting current Mem tree state.
    No-op on the B-tree backend (deferred). *)
val savepoint_begin : rw txn -> string -> unit Lwt.t

(** Release the named savepoint and all newer ones.
    Writes accumulated since the savepoint remain in the outer transaction.
    No-op on the B-tree backend. *)
val savepoint_release : rw txn -> string -> unit Lwt.t

(** Restore to the named savepoint, dropping all newer savepoints.
    The named savepoint is kept so ROLLBACK TO can be repeated.
    No-op on the B-tree backend. *)
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

(** True if the store is operating in WAL mode (opened via
    [open_block_wal]). *)
val wal_mode : t -> bool

(** Migrate every page in the WAL index to the main DB, sync, then
    reset the WAL. No-op outside WAL mode. Acquires the RW mutex
    internally so it serialises with commits. *)
val checkpoint : t -> unit Lwt.t

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
    [max_int], disabling gating). *)
val set_commit_callback
  :  t
  -> (epoch:int64 -> base_idx:int -> count:int -> unit Lwt.t) option
  -> unit

(** -------------------------------------------------------------------- *)

(** Standby-follower integration (#172)                                      *)

(** -------------------------------------------------------------------- *)

(** Enable or disable follower mode.  When [true], {!rw_begin} rejects
    write transactions on the B+-tree backend with an exception, keeping
    the standby's WAL from diverging from the master's stream while the
    follower loop is applying incoming frames.  No-op on the in-memory
    backend (Mem stores have no standby semantics). *)
val set_follower : t -> bool -> unit

(** True iff follower mode is active (write transactions are rejected). *)
val is_follower : t -> bool
