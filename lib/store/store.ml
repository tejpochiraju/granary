(* Phase 1 store.  Two backends share the same interface:

   - [Mem] — pure in-memory [Bytes_map]-per-tree (the Phase 0 backend).
     Used by [create ()].  No I/O, no size limits, no errors.

   - [Btree] — CoW B+-tree over a Pager over a BLOCK device, given as
     I/O callbacks via [open_block]/[open_block_wal].  Persists across
     reopen.  Inherits the B+-tree leaf-cell size limits (512-byte keys,
     1024-byte values).  Unix-file convenience constructors live in the
     [sqlocaml.unix] driver library, not here, so the core stays
     platform-agnostic (#170).

   The two are wrapped in a sum type so callers see one [Store.t]. *)

open Lwt.Syntax
module Btree = Sqlocaml_storage.Btree
module Pager = Sqlocaml_storage.Pager
module Header = Sqlocaml_storage.Header
module Freelist = Sqlocaml_storage.Freelist
module Page = Sqlocaml_storage.Page
module Geometry = Sqlocaml_storage.Geometry
module Crypto = Sqlocaml_storage.Crypto
module Varint = Sqlocaml_encoding.Varint
module Bytes_map = Map.Make (Bytes)

type ro
type rw
type tree_id = int

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
      nonce can be generated — the application must seed the RNG at boot *)

let pp_error fmt = function
  | Block_error s -> Format.fprintf fmt "Block_error(%s)" s
  | Corruption s -> Format.fprintf fmt "Corruption(%s)" s
  | Key_too_large n -> Format.fprintf fmt "Key_too_large(%d)" n
  | Value_too_large n -> Format.fprintf fmt "Value_too_large(%d)" n
  | Header_error s -> Format.fprintf fmt "Header_error(%s)" s
  | Encryption_key_required -> Format.pp_print_string fmt "Encryption_key_required"
  | Encryption_key_mismatch -> Format.pp_print_string fmt "Encryption_key_mismatch"
  | Not_encrypted -> Format.pp_print_string fmt "Not_encrypted"
  | Encryption_rng_unseeded -> Format.pp_print_string fmt "Encryption_rng_unseeded"
;;

(* ------------------------------------------------------------------ *)
(* Btree-backend internal state                                         *)
(* ------------------------------------------------------------------ *)

(* The meta-tree is stored separately from user trees (it doesn't live
   in the [trees] hashtable).  It tracks the root_page of every tree_id
   created via [get/put/del]; its OWN root_page is what we commit into
   the header. *)

type bt_savepoint =
  { sp_name : string
  ; sp_meta_root : int64
  ; sp_tree_roots : (tree_id * int64) list
  ; sp_freelist : Freelist.t
  ; sp_n_pages : int64
  ; sp_dirty : Pager.dirty_snapshot
  ; sp_txn_pool : int64 list
    (** #297: txn-owned page pool snapshot, restored on savepoint rollback. *)
  }

(* Per-store commit queue for WAL-mode group commit (#77, #151).  After
   a writer has staged its WAL frames it releases [lock] and joins
   this queue to await one shared fsync.  [drainer] is set to [true] by
   the first arriving writer (acting as coordinator); subsequent writers
   register a resolver on [waiters] and block until the drainer wakes
   them with the sync result.  [pending] tracks the number of waiters so
   the drainer can yield additional ticks while new arrivals keep
   registering, widening the batch.  Cooperative Lwt scheduling makes
   the [drainer]/[pending]/[waiters] transitions atomic (no implicit
   yield between read and write).

   The per-batch resolvers carry [(unit, exn) result] so an fsync
   failure in the drainer propagates to every joiner in the same batch
   instead of being silently dropped by a unit-broadcast (#151). *)
type commit_queue =
  { mutable drainer : bool
  ; mutable pending : int
  ; mutable waiters : (unit, exn) result Lwt.u list
  }

let create_commit_queue () = { drainer = false; pending = 0; waiters = [] }

type bt_state =
  { close_fn : unit -> unit Lwt.t
  ; pager : Pager.t
  ; cipher : Crypto.t option
    (** #84: the page cipher when the DB is encrypted, else [None].  Mirrors
        the cipher captured by the read/write callback closures; retained here
        so [copy_to]/[rekey_to] can re-encrypt the snapshot page image. *)
  ; mutable meta : Btree.t
  ; trees : (tree_id, Btree.t) Hashtbl.t
  ; tree_tags : (tree_id, int32) Hashtbl.t
    (** #174: per-tree page-header stamp (low 32 bits of the schema
        fingerprint), set by the catalog via {!set_tree_tag}.  Pages written
        for a tree carry its tag in the reserved header bytes; untagged trees
        (default) carry 0. *)
  ; mutable current_header : Header.t
  ; schema_version : int64
  ; mutable txn_freelist_snapshot : Freelist.t option
  ; (* Snapshot of freelist taken at rw_begin; restored on rollback. None when no RW txn is active. *)
    active_readers : (int64, int) Hashtbl.t
  ; (* Maps snap_txn_id -> reference count of active RO txns at that snapshot *)
    mutable bt_savepoints : bt_savepoint list
  ; (* Stack of named savepoints; newest at front. Cleared on commit/rollback. *)
    bt_append : (tree_id, Btree.append_cursor) Hashtbl.t
  ; (* #356: per-tree append cursor for O(1) bulk sequential inserts.  Set by
     [put_x] after an append, consumed by the next append.  Invalidated on
     commit/rollback/savepoint-rollback and on any non-append mutation of the
     tree.  Re-validated against the live page on every use, so a stale entry
     can only force the slow path, never corrupt the tree. *)
    wal : Sqlocaml_storage.Wal.t option
  ; (* When set, commits append to this WAL instead of writing to the main
     DB; reads route through it via the Pager hook. *)
    wal_close : (unit -> unit Lwt.t) option
  ; mutable wal_autocheckpoint_threshold : int
  ; (* When > 0 and committed WAL frames reach this number, the next
     commit triggers an inline checkpoint (still under [lock]) so
     the WAL stays bounded. 0 disables auto-checkpoint. Per-connection,
     not persisted. *)
    commit_queue : commit_queue
  ; (* WAL-mode group commit (#77).  Used only when [wal] is [Some];
     allocated unconditionally to keep [bt_state] uniform. *)
    active_reader_frames : (int, int) Hashtbl.t
  ; (* WAL committed_frames snapshot value -> refcount of RO snapshots
     captured at that value.  Lets [min_active_ro_reader_frames] compute
     the lowest snapshot bound currently in flight in O(distinct
     snapshots) which is bounded by the number of concurrent readers. *)
    reader_done_cond : unit Lwt_condition.t
  ; (* Broadcast on every [ro_end] so a waiting checkpoint can re-check
     [min_active_ro_reader_frames] without busy-waiting. *)
    mutable autockpt_in_flight : bool
    (* True iff a background autocheckpoint fiber is currently running.
     Used to coalesce: if a commit crosses the threshold while a
     checkpoint is already running, we skip rescheduling. *)
  ; mutable replication_shipped_frames : int
    (* WAL frame index up to which the replication consumer (if any) has
       acknowledged shipment.  Initialized to [max_int] so that when no
       consumer is active it does not gate checkpoint truncation.  When a
       consumer registers it sets this to its shipped position; [checkpoint]
       then waits (subject to [replication_gate_max_yields]) for this to
       reach [committed_frames] via [wait_for_readers_past]. *)
  ; mutable replication_gate_max_yields : int
    (* Bounded-yield "timeout" for the checkpoint gate's wait on the
       replication floor (#207).  When the floor (a standby's acked
       position, plumbed in by the app via [update_replication_position])
       is below the checkpoint target, the gate yields up to this many
       times before proceeding anyway — a dead or slow standby must not
       wedge the master's WAL forever.  Pure-Mirage has no ambient clock,
       so the "timeout" is a bounded count of cooperative [Lwt.pause]
       yields (the project's [wait_for] idiom), not wall-clock time.
       [max_int] (the default) means unbounded: wait indefinitely on the
       broadcast condition, exactly as before this knob existed.  Local
       RO readers are NEVER abandoned by this budget — only the
       replication floor.  On timeout the standby falls outside the live
        un-checkpointed window and must re-base (see #208). *)
  ; mutable backup_shipped_frames : int
    (* WAL frame index up to which the backup consumer (if any) has
       captured frames.  Analogous to [replication_shipped_frames] but for
       incremental backup (#265).  Initialized to [max_int] so that when no
       backup consumer is active it does not gate checkpoint truncation.
       The backup consumer calls {!update_backup_position} to advance this
       as frames are captured and stored. *)
  ; mutable backup_gate_max_yields : int
    (* Bounded-yield "timeout" for the checkpoint gate's wait on the
       backup floor (#265).  Same semantics as
       [replication_gate_max_yields]: when the backup consumer has not yet
       captured frames up to the checkpoint target, the gate yields up to
       this many times before proceeding anyway.  [max_int] (the default)
       means unbounded — wait indefinitely.  A finite budget bounds the
       wait: once spent, the checkpoint proceeds and un-captured frames are
       recycled (the backup must re-base).  Negative inputs clamp to [0]. *)
  ; mutable on_committed_frames :
      (epoch:int64 -> base_idx:int -> count:int -> unit Lwt.t) option
    (* Optional callback invoked asynchronously after each WAL commit batch.
       Receives ~epoch, ~base_idx (starting WAL frame index of the batch),
       ~count (number of frames in the batch).  The application reads the
       individual frames via [Wal.read_frame] and ships them to the object
       store.  Fired via [Lwt.async] so it never blocks the commit path.
       [None] when no sink is registered. *)
  ; mutable follower : bool
    (* When true, [rw_begin] rejects with an error.  Set by the standby
       consumer while following the master's WAL stream; cleared on
       promotion or when the follower loop exits.  The in-memory backend
       ignores this flag (Mem stores have no standby semantics). *)
  ; mutable follower_ack_position : int option
    (* [Wal.committed_frames] at the time the last apply batch completed on
       this follower, or [None] when no position has been recorded yet.
       When [Some n], [ro_begin] caps the RO snapshot's visible WAL frames
       to [min committed_frames n] so readers never observe frames past the
       follower's last-applied commit (#263).  Stored in local committed-frame
       count space so it compares correctly against [Wal.committed_frames]
       and survives local epoch resets. *)
  ; mutable sync_mode : [ `Full | `Batched | `Off ]
    (* #298: durability mode. [`Full] = fsync every group-commit (default).
       [`Batched] = defer fsync until [batch_commits] or [batch_interval_ms].
       [`Off] = never fsync on commit. Only consulted in WAL mode. *)
  ; mutable batch_commits : int (* #298: batched N threshold (default 256) *)
  ; mutable batch_interval_ms : int (* #298: batched T threshold ms (default 100) *)
  ; mutable unsynced_commits : int
    (* #298: committed-but-unsynced batches since last fsync. *)
  ; mutable last_sync_time : float
    (* #298: clock () at last commit fsync; for the T trigger. *)
  ; mutable clock : unit -> float (* #298: wall-clock source; default returns 0. *)
  ; mutable sink_shipped_frames : int
    (* #298/#1: per-epoch count of WAL frames already shipped to the
       replication sink ([on_committed_frames]).  The sink is fired ONLY for
       frames that have been fsynced, so a standby can never lead a
       crash-recovered master.  Reset to 0 on checkpoint (new epoch). *)
  ; mutable closing : bool
    (* #338: set by [close] to signal teardown.  [maybe_autockpt_after_commit]
       then dispatches no fresh checkpoint, and an in-flight/parked one unwinds
       without touching the pager/WAL fds ([checkpoint_unlocked] and
       [wait_for_readers_past] bail on it).  Lets [close] drain checkpoints
       without acquiring [t.lock] — which an abandoned write txn holds until
       commit/rollback, so taking it would hang close. *)
  ; mutable ckpt_io_in_flight : int
    (* #338 (review r2): count of checkpoints that have passed the gate and are
       actively performing pager/WAL fd I/O ([checkpoint_unlocked], both the auto
       and manual paths).  Incremented AFTER lock acquisition + the [closing]
       check, so a checkpoint merely parked on [acquire_write] (e.g. behind an
       abandoned write txn) is NOT counted — [close] therefore never waits on it
       (it aborts on [closing] if it ever acquires the lock).  [close] drains
       this to 0 (together with [sink_ships_in_flight]) before fd teardown.
       Distinct from [autockpt_in_flight], which is dispatch-intent (coalescing)
       only. *)
  ; mutable sink_ships_in_flight : int
    (* #337: count of async sink ships dispatched but not yet completed.  The
       ship callback reads WAL frame payloads LAZILY ([Wal.read_frame]); a
       checkpoint's [Wal.reset] would recycle/zero those frames and bump the
       epoch out from under an in-flight reader ([Corrupt_frame] / stale
       epoch).  Incremented synchronously at dispatch (before the [Lwt.async]);
       decremented in the callback's finalize.  [checkpoint_unlocked] waits for
       this to reach 0 before [Wal.reset]. *)
  }

let default_wal_autocheckpoint_threshold = 1000
let default_batch_commits = 256
let default_batch_interval_ms = 100

(** #298: per-deployment durability mode. *)
type durability =
  | Full
  | Batched of
      { commits : int
      ; interval_ms : int
      }
  | Off

type backend =
  | Mem of (tree_id, Bytes.t Bytes_map.t ref) Hashtbl.t
  | Btree of bt_state

type t =
  { backend : backend
  ; lock : Rwlock.t
  ; (* Shadow copies of Mem backend tree contents for the active RW txn.
       Writes during the txn go to the shadow — the live tree is NEVER
       modified until commit.  This prevents readers (both RO snapshots
       and subsequent RW txns) from ever seeing uncommitted state.
       None when no RW transaction is active.
       #178: without shadow writes, concurrent RO reads could observe
       uncommitted mutations because Rwlock's acquire_read never blocks. *)
    mutable mem_rw_shadow : (tree_id * Bytes.t Bytes_map.t) list option
  ; (* Savepoint stack for the Mem backend; newest entry at front.
       Each entry is (savepoint_name, snapshot_of_shadow). *)
    mutable mem_savepoints : (string * (tree_id * Bytes.t Bytes_map.t) list) list
  }

let pp fmt t =
  Format.fprintf
    fmt
    "Store.t { backend = %s }"
    (match t.backend with
     | Mem _ -> "Mem"
     | Btree _ -> "Btree")
;;

(* #95/#176: the page geometry this store is backed by.  The in-memory backend
   has no on-disk geometry, so it reports {!Geometry.default}; VACUUM uses this
   to rebuild the temp file at the source's page_size/reserved. *)
let geometry t =
  match t.backend with
  | Mem _ -> Geometry.default
  | Btree st -> Pager.geom st.pager
;;

type ro_snapshot =
  { rs_store : t
  ; rs_snap_txn_id : int64
  ; rs_snap_meta_root : int64
  ; rs_snap_trees : (tree_id, Btree.t) Hashtbl.t
  ; rs_snap_frames : int
  ; (* WAL committed_frames at ro_begin; 0 when no WAL is in effect. *)
    rs_pinned : (int64, unit) Hashtbl.t
    (* Page ids this snapshot has pinned in the Pager cache (#159).  Every
     snapshot read records the pages it materialises here; [ro_end]
     releases them via [Pager.unpin_all].  Unused for the Mem backend. *)
  ; rs_mem_snap : (tree_id * Bytes.t Bytes_map.t) list option
    (** #178: for the in-memory backend, a deep copy of every tree's
        contents taken at [ro_begin] so RO reads never see uncommitted
        writes from a concurrent (but rollback-destined) writer.
        [None] for Btree backend. *)
  }

type 'a txn =
  | Ro : ro_snapshot -> ro txn
  | Rw : t -> rw txn

type seek_result =
  | Found of bytes
  | Not_found of [ `Greater of bytes | `End ]

(* Cursor over either backend.
   For the in-memory backend, the cursor holds an immutable snapshot of
   the bindings as a list (matches Phase 0 semantics).

   For the B+-tree backend we similarly materialise a snapshot (list of
   (k,v) pairs) at cursor_open time.  This is acceptable for Phase 1 and
   makes seek/next semantics identical to the in-memory implementation;
   true streaming cursors come later.

   In both cases [ready] and [remaining] together implement the
   "pre-positioned" semantics from [store.mli]: the first [cursor_next]
   after positioning returns the positioned entry without advancing. *)
type cursor =
  { all : (bytes * bytes) list
  ; mutable remaining : (bytes * bytes) list
  ; mutable ready : bool
  }

(* ------------------------------------------------------------------ *)
(* Backend helpers — Mem                                                *)
(* ------------------------------------------------------------------ *)

let mem_tree trees tid =
  match Hashtbl.find_opt trees tid with
  | Some r -> r
  | None ->
    let r = ref Bytes_map.empty in
    Hashtbl.add trees tid r;
    r
;;

(* #178: look up a tree in the snapshot taken at [ro_begin] for the
   in-memory backend.  Returns [Bytes_map.empty] when the tree didn't
   exist at snapshot time — an RO reader should see an empty tree, not
   the live (possibly uncommitted) contents. *)
let mem_tree_snap (snap : (tree_id * Bytes.t Bytes_map.t) list) (tid : tree_id) =
  match List.assoc_opt tid snap with
  | Some map -> map
  | None -> Bytes_map.empty
;;

(* Shadow helpers for the in-memory backend (#178).
   During a RW transaction, all writes go to a per-txn shadow.
   The live tree is never mutated until commit, so RO txn
   snapshots always capture committed-only state. *)

(* Get a tree's content from the shadow, falling back to the live
   tree when the tree hasn't been touched by this txn yet. *)
let shadow_get
      (shadow : (tree_id * Bytes.t Bytes_map.t) list)
      (trees : (tree_id, Bytes.t Bytes_map.t ref) Hashtbl.t)
      (tid : tree_id)
  =
  match List.assoc_opt tid shadow with
  | Some map -> map
  | None -> !(mem_tree trees tid)
;;

(* Update a tree in the shadow.  The tree is lazy-copied from the live
   tree on first access (via [shadow_get]). *)
let shadow_update
      (shadow : (tree_id * Bytes.t Bytes_map.t) list)
      (trees : (tree_id, Bytes.t Bytes_map.t ref) Hashtbl.t)
      (tid : tree_id)
      (f : Bytes.t Bytes_map.t -> Bytes.t Bytes_map.t)
  =
  let map = shadow_get shadow trees tid in
  (tid, f map) :: List.remove_assoc tid shadow
;;

(* ------------------------------------------------------------------ *)
(* Backend helpers — Btree                                              *)
(* ------------------------------------------------------------------ *)

(* tree_id <-> bytes encoding via varint (zigzag, since negative ids are
   reserved for internal use; we don't actually persist negative ids but
   using signed encoding lets us round-trip safely). *)
let encode_tree_id (tid : tree_id) : bytes =
  let buf = Buffer.create 8 in
  Varint.encode_int64 buf (Int64.of_int tid);
  Buffer.to_bytes buf
;;

let encode_root_page (pid : int64) : bytes =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf pid;
  Buffer.to_bytes buf
;;

let decode_root_page (b : bytes) : int64 =
  let v, _ = Varint.decode_uint64 b 0 in
  v
;;

let map_btree_err : Btree.error -> error = function
  | Btree.Pager_error (Pager.Block_error s) -> Block_error s
  | Btree.Pager_error (Pager.Corruption s) -> Corruption s
  | Btree.Key_too_large n -> Key_too_large n
  | Btree.Value_too_large n -> Value_too_large n
  | Btree.Tree_corrupt s -> Corruption s
;;

(* The B+-tree treats Bytes by [Bytes.compare]; cursor_seek consumes the
   raw bytes; everything is byte-clean. *)

(* Lookup-or-build the Btree handle for a tree_id.  Looks up the
   tree_id's root page in the meta-tree; if absent (new tree), creates a
   fresh empty Btree (root_page = 0L). *)
let bt_get_tree st (tid : tree_id) : (Btree.t, error) result Lwt.t =
  match Hashtbl.find_opt st.trees tid with
  | Some bt -> Lwt.return_ok bt
  | None ->
    let key = encode_tree_id tid in
    let* r = Btree.get st.meta key in
    (match r with
     | Error e -> Lwt.return_error (map_btree_err e)
     | Ok None ->
       let bt = Btree.create st.pager ~root_page:0L in
       Hashtbl.replace st.trees tid bt;
       Lwt.return_ok bt
     | Ok (Some v) ->
       let root_page = decode_root_page v in
       let bt = Btree.create st.pager ~root_page in
       Hashtbl.replace st.trees tid bt;
       Lwt.return_ok bt)
;;

(* #174: the page-header stamp for [tid] (0 when untagged). *)
let tree_tag st (tid : tree_id) : int32 =
  Option.value ~default:0l (Hashtbl.find_opt st.tree_tags tid)
;;

(* Convert a result with [error] payload to an Lwt-failing version.  The
   public [get/put/del/cursor_open] signatures don't return [result], so
   B+-tree errors are surfaced as Lwt exceptions. *)
let unwrap_error r =
  match r with
  | Ok v -> Lwt.return v
  | Error e -> Lwt.fail_with (Format.asprintf "Store: %a" pp_error e)
;;

let min_active_reader_txn st =
  Hashtbl.fold
    (fun txn_id _ acc ->
       match acc with
       | None -> Some txn_id
       | Some m -> Some (Int64.min m txn_id))
    st.active_readers
    None
;;

(* Lowest WAL frame index pinned by an in-flight RO snapshot, ignoring the
   replication floor.  A checkpoint must NEVER recycle past this (the
   snapshot would observe a broken WAL), so this gate is honored
   unconditionally — unlike the replication floor, which the #207 timeout
   may abandon. *)
let min_active_ro_reader_frames (st : bt_state) : int option =
  Hashtbl.fold
    (fun k _ acc ->
       match acc with
       | None -> Some k
       | Some m -> Some (min m k))
    st.active_reader_frames
    None
;;

(* True iff an in-flight RO snapshot still needs WAL frames below [target].
   This gate is honored unconditionally by the checkpoint wait. *)
let ro_readers_below (st : bt_state) ~target =
  match min_active_ro_reader_frames st with
  | Some m -> m < target
  | None -> false
;;

(* True iff a replication consumer is active and its acked floor is below
    [target].  This gate is subject to the #207 bounded-yield timeout. *)
let replication_floor_below (st : bt_state) ~target =
  st.replication_shipped_frames <> max_int && st.replication_shipped_frames < target
;;

(* True iff a backup consumer is active and its captured floor is below
   [target].  Analogous to [replication_floor_below] but for the
   incremental backup watermark (#265).  Subject to a bounded-yield
   timeout like the replication floor. *)
let backup_floor_below (st : bt_state) ~target =
  st.backup_shipped_frames <> max_int && st.backup_shipped_frames < target
;;

(* Lookup-or-build the Btree handle for a tree_id using a snapshot's
   pinned meta root page rather than the live meta tree. *)
let bt_get_tree_ro (snap : ro_snapshot) (st : bt_state) (tid : tree_id)
  : (Btree.t, error) result Lwt.t
  =
  match Hashtbl.find_opt snap.rs_snap_trees tid with
  | Some bt -> Lwt.return_ok bt
  | None ->
    let snap_frames =
      if snap.rs_snap_frames = 0 then None else Some snap.rs_snap_frames
    in
    let snap_meta =
      Btree.create
        ?snapshot_frames:snap_frames
        ~pin_set:snap.rs_pinned
        st.pager
        ~root_page:snap.rs_snap_meta_root
    in
    let key = encode_tree_id tid in
    let* r = Btree.get snap_meta key in
    (match r with
     | Error e -> Lwt.return_error (map_btree_err e)
     | Ok None ->
       let bt =
         Btree.create
           ?snapshot_frames:snap_frames
           ~pin_set:snap.rs_pinned
           st.pager
           ~root_page:0L
       in
       Hashtbl.replace snap.rs_snap_trees tid bt;
       Lwt.return_ok bt
     | Ok (Some v) ->
       let root_page = decode_root_page v in
       let bt =
         Btree.create
           ?snapshot_frames:snap_frames
           ~pin_set:snap.rs_pinned
           st.pager
           ~root_page
       in
       Hashtbl.replace snap.rs_snap_trees tid bt;
       Lwt.return_ok bt)
;;

(* ------------------------------------------------------------------ *)
(* Freelist page I/O helpers (forward-declared here; used by open_block *)
(* and commit below)                                                    *)
(* ------------------------------------------------------------------ *)

(* Walk the freelist page chain starting at [first_page], collect all
   entries, and return a reconstructed [Freelist.t]. *)
let read_freelist_pages pager ~first_page : Freelist.t Lwt.t =
  if Int64.equal first_page 0L
  then Lwt.return Freelist.empty
  else (
    let rec loop pid acc =
      if Int64.equal pid 0L
      then Lwt.return (Freelist.of_list (List.rev acc))
      else
        let* r = Pager.read pager pid in
        match r with
        | Error _ -> Lwt.return (Freelist.of_list (List.rev acc))
        | Ok buf ->
          let common = Page.read_common buf in
          let n = min common.Page.n_keys (Pager.max_freelist_entries_per_page pager) in
          let next_pid =
            Int64.logand 0xFFFFFFFFL (Int64.of_int32 common.Page.right_page)
          in
          let entries =
            List.init n (fun i ->
              let e = Page.freelist_entry_at buf ~index:i in
              e.Page.page_id, e.Page.freed_at_txn_id)
          in
          loop next_pid (List.rev_append entries acc)
    in
    loop first_page [])
;;

(* ------------------------------------------------------------------ *)
(* create / open_block / close                                          *)
(* ------------------------------------------------------------------ *)

let create () : t =
  { backend = Mem (Hashtbl.create 16)
  ; lock = Rwlock.create ()
  ; mem_rw_shadow = None
  ; mem_savepoints = []
  }
;;

let map_header_err (e : Header.error) : error =
  match e with
  | Header.Io s -> Header_error s
  | Header.Both_headers_corrupt -> Header_error "both header pages corrupt"
  | Header.Unsupported_format v ->
    Header_error (Printf.sprintf "unsupported on-disk format_version %ld" v)
;;

(* Build a fully-initialised [t] wrapping a B-tree-backed [bt_state] from the
   given pager/meta/header.  [wal]/[wal_close] default to None (plain opens);
   WAL opens pass [Some _]. *)
let make_btree_store
      ?(wal = None)
      ?(wal_close = None)
      ?(cipher = None)
      ~close_fn
      ~pager
      ~meta
      ~(h : Header.t)
      ()
  =
  let st =
    { close_fn
    ; pager
    ; cipher
    ; meta
    ; trees = Hashtbl.create 16
    ; tree_tags = Hashtbl.create 16
    ; current_header = h
    ; schema_version = h.schema_version
    ; txn_freelist_snapshot = None
    ; active_readers = Hashtbl.create 4
    ; bt_savepoints = []
    ; bt_append = Hashtbl.create 8
    ; wal
    ; wal_close
    ; wal_autocheckpoint_threshold = default_wal_autocheckpoint_threshold
    ; commit_queue = create_commit_queue ()
    ; active_reader_frames = Hashtbl.create 4
    ; reader_done_cond = Lwt_condition.create ()
    ; autockpt_in_flight = false
    ; replication_shipped_frames = max_int
    ; replication_gate_max_yields = max_int
    ; backup_shipped_frames = max_int
    ; backup_gate_max_yields = max_int
    ; on_committed_frames = None
    ; follower = false
    ; follower_ack_position = None
    ; sync_mode = `Full
    ; batch_commits = default_batch_commits
    ; batch_interval_ms = default_batch_interval_ms
    ; unsynced_commits = 0
    ; last_sync_time = 0.
    ; clock = (fun () -> 0.)
    ; sink_shipped_frames = 0
    ; sink_ships_in_flight = 0
    ; closing = false
    ; ckpt_io_in_flight = 0
    }
  in
  { backend = Btree st
  ; lock = Rwlock.create ()
  ; mem_rw_shadow = None
  ; mem_savepoints = []
  }
;;

(* #338 (review r2): event-driven wait until [pred] holds, parking on
   [reader_done_cond] (broadcast whenever an in-flight counter changes).  Shared
   by [close] (drain checkpoint fd-I/O + sink ships) and [checkpoint_unlocked]
   (drain sink ships before [Wal.reset]).  Cooperative Lwt: the pred check and
   the [Lwt_condition.wait] register with no yield between, so no wakeup is
   lost. *)
let rec wait_until (st : bt_state) (pred : unit -> bool) : unit Lwt.t =
  if pred ()
  then Lwt.return_unit
  else
    let* () = Lwt_condition.wait st.reader_done_cond in
    wait_until st pred
;;

let close (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    (* #338: an async checkpoint or sink ship touches the pager/WAL fds; tearing
       them down underneath one corrupts it (a checkpoint's error is swallowed
       and relies on WAL-replay self-healing; a ship loses tail frames the
       standby then misses).  Rather than take [t.lock] for teardown — which an
       abandoned write txn holds until commit/rollback, so [close] would hang on
       it (review r2 #3) — signal teardown via [st.closing]:

       - [maybe_autockpt_after_commit] dispatches no fresh checkpoint once set;
       - a checkpoint parked on the gate or [acquire_write] unwinds without fd
         I/O ([wait_for_readers_past]/[checkpoint_unlocked] bail on [closing]);
       - the broadcast wakes a checkpoint parked on the replication floor.

       We then drain — event-driven — the work that is ACTUALLY mid-fd-I/O:
       checkpoints past the gate ([ckpt_io_in_flight], covers the auto AND manual
       paths — review r2 #3) and async sink ships ([sink_ships_in_flight], whose
       lazy [Wal.read_frame] would hit a closed fd — review r2 #2).  A checkpoint
       merely parked on [acquire_write] is invisible here (it never incremented),
       so an abandoned txn cannot wedge close (review r2 #1).  Callers must still
       quiesce their own writers before [close] (see store.mli). *)
    st.closing <- true;
    Lwt_condition.broadcast st.reader_done_cond ();
    let* () =
      wait_until st (fun () -> st.ckpt_io_in_flight = 0 && st.sink_ships_in_flight = 0)
    in
    (* #298: in batched/off mode the last acked commits may never have been
       fsynced. Decide on the WAL's actual committed-frame state rather than the
       in-memory unsynced counter. A redundant fsync here (frames already
       durable) is cheap and safe; skipping a needed one is not. Full mode syncs
       every commit. *)
    let needs_final_sync =
      st.sync_mode <> `Full
      &&
      match st.wal with
      | Some w -> Sqlocaml_storage.Wal.committed_frames w > 0
      | None -> false
    in
    (* #298/#2: a failed final fsync still releases the fds (wal_close/close_fn)
       but THEN raises — close is a durability anchor, so silently reporting
       success on EIO/ENOSPC is wrong (matches the commit/checkpoint convention
       of surfacing sync errors). *)
    let* sync_err =
      if needs_final_sync
      then
        let* r = Pager.wal_sync st.pager in
        match r with
        | Ok () ->
          st.unsynced_commits <- 0;
          Lwt.return_none
        | Error e -> Lwt.return_some e
      else Lwt.return_none
    in
    let* () =
      match st.wal_close with
      | None -> Lwt.return_unit
      | Some f -> f ()
    in
    let* () = st.close_fn () in
    (match sync_err with
     | None -> Lwt.return_unit
     | Some e ->
       Lwt.fail_with (Format.asprintf "Store.close: final wal_sync: %a" Pager.pp_error e))
;;

(* #95: discover the file's geometry by reading page 0's leading bytes through
   the raw block callback (page 0 is always at offset 0, so this works whatever
   the backend's addressing page size, and it bypasses the pager cache).  Falls
   back to [fallback] for a fresh/empty/zeroed device, which a subsequent
   [Header.init] then stamps. *)
let peek_geometry ~read_page ~fallback =
  let buf = Cstruct.create Geometry.default.page_size in
  let%lwt r = read_page ~page_id:0L buf in
  match r with
  | Ok () -> Lwt.return (Option.value (Header.peek_geometry buf) ~default:fallback)
  | Error _ -> Lwt.return fallback
;;

(* ------------------------------------------------------------------ *)
(* Opt-in page encryption (#84).                                        *)
(*                                                                      *)
(* The pager and B+-tree only ever see PLAINTEXT.  A supplied key       *)
(* builds a cipher; we then (a) force the fresh-creation geometry to    *)
(* carve [Crypto.overhead] reserved bytes off each page's tail, (b)     *)
(* wrap the raw read/write callbacks so pages >= 2 are                  *)
(* decrypted/encrypted (pages 0,1 are the headers and pass through      *)
(* plaintext), and (c) stamp / verify a key-check canary in the header. *)
(* ------------------------------------------------------------------ *)

let build_cipher = function
  | None -> Ok None
  | Some k ->
    (match Crypto.create ~key:k with
     | Ok c -> Ok (Some c)
     | Error `Bad_key_length -> Error (Block_error "encryption key must be 32 bytes"))
;;

(* When a key is in play we draw a fresh nonce on every encrypted write (and one
   for the header canary at creation).  [Mirage_crypto_rng.generate] raises if
   the application never seeded the RNG ([lib/] is Mirage-clean and never seeds):
   [No_default_generator] when no generator was installed at all (the common
   forgot-to-seed case) and [Unseeded_generator] when one was installed but not
   seeded.  Either would otherwise surface as a raw exception on the first write
   rather than a [Store.error].  Probe once at open time so the foot-gun is
   caught at the entry point the caller controls; the RNG is process-global, so
   a seed present here is present for later writes. *)
let ensure_rng_seeded = function
  | None -> Ok ()
  | Some _ ->
    (try
       ignore (Mirage_crypto_rng.generate 1 : string);
       Ok ()
     with
     | Mirage_crypto_rng.Unseeded_generator | Mirage_crypto_rng.No_default_generator ->
       Error Encryption_rng_unseeded)
;;

(* Force a fresh-creation geometry to carry the crypto overhead in its
   reserved tail.  If bumping [reserved] to [Crypto.overhead] is rejected we
   surface a clean error rather than silently proceeding with a geometry whose
   reserved tail is too small — encryption would then write nonce+tag into bytes
   the B+-tree believes are usable, corrupting the page.  (Unreachable with the
   4096-multiple page sizes [Geometry.create] permits, since they always leave
   >= 480 payload after reserving 32 bytes; kept as defense-in-depth.) *)
let geom_for_cipher cipher (g : Geometry.t) =
  match cipher with
  | None -> Ok g
  | Some _ ->
    if g.reserved_bytes_per_page >= Crypto.overhead
    then Ok g
    else (
      match
        Geometry.create
          ~page_size:g.page_size
          ~reserved_bytes_per_page:(max g.reserved_bytes_per_page Crypto.overhead)
      with
      | Ok g' -> Ok g'
      | Error e ->
        Error
          (Block_error
             (Format.asprintf
                "encryption needs %d reserved bytes/page, but the geometry rejects it: %a"
                Crypto.overhead
                Geometry.pp_error
                e)))
;;

let wrap_callbacks cipher ~read_page ~write_page =
  match cipher with
  | None -> read_page, write_page
  | Some c ->
    let rd ~page_id buf =
      let* r = read_page ~page_id buf in
      match r with
      | Error _ as e -> Lwt.return e
      | Ok () ->
        if Int64.compare page_id 2L < 0
        then Lwt.return_ok ()
        else (
          match Crypto.decrypt_page c ~page_id buf with
          | Ok () -> Lwt.return_ok ()
          | Error `Tag_mismatch -> Lwt.return_error "decrypt: tag mismatch")
    in
    let wr ~page_id buf =
      if Int64.compare page_id 2L < 0
      then write_page ~page_id buf
      else (
        let tmp = Cstruct.create (Cstruct.length buf) in
        Cstruct.blit buf 0 tmp 0 (Cstruct.length buf);
        Crypto.encrypt_page c ~page_id tmp;
        write_page ~page_id tmp)
    in
    rd, wr
;;

let make_enc_info = function
  | None -> None
  | Some c ->
    let nonce = Mirage_crypto_rng.generate Crypto.nonce_len in
    let tag = Crypto.make_canary c ~nonce in
    Some { Header.canary_nonce = nonce; canary_tag = tag }
;;

let check_key (h : Header.t) cipher =
  match h.Header.enc, cipher with
  | None, None -> Ok ()
  | Some _, None -> Error Encryption_key_required
  | None, Some _ -> Error Not_encrypted
  | Some e, Some c ->
    if Crypto.check_canary c ~nonce:e.Header.canary_nonce ~tag:e.Header.canary_tag
    then Ok ()
    else Error Encryption_key_mismatch
;;

let open_block
      ?(key : string option)
      ?(geom = Geometry.default)
      ~(init_if_corrupt : bool)
      ~(read_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
      ~(write_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
      ~(sync : unit -> (unit, string) result Lwt.t)
      ~(resize : n_pages:int64 -> (unit, string) result Lwt.t)
      ~(n_pages : int64)
      ~(close : unit -> unit Lwt.t)
      ()
  : (t, error) result Lwt.t
  =
  match
    let ( let* ) = Result.bind in
    let* cipher = build_cipher key in
    let* () = ensure_rng_seeded cipher in
    let* geom = geom_for_cipher cipher geom in
    Ok (cipher, geom)
  with
  | Error e -> Lwt.return_error e
  | Ok (cipher, geom) ->
    let read_page, write_page = wrap_callbacks cipher ~read_page ~write_page in
    let pager =
      Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist:Freelist.empty
    in
    (* Adopt the file's real geometry (peeked for an existing file, [geom] for a
       fresh one) before any header read so buffers are sized correctly (#95). *)
    let%lwt eff_geom = peek_geometry ~read_page ~fallback:geom in
    Pager.set_geom pager eff_geom;
    let%lwt hr = Header.read_live pager in
    (match hr with
     | Error Header.Both_headers_corrupt when init_if_corrupt ->
       (* Fresh device — initialise headers.  Disabled via [~init_if_corrupt:false]
          so an existing-but-corrupt device surfaces [Header_error] instead of
          being silently re-initialised (a Unix-file open must not clobber). *)
       let%lwt ir = Header.init ~enc:(make_enc_info cipher) pager in
       (match ir with
        | Error e -> Lwt.return_error (map_header_err e)
        | Ok () ->
          Pager.set_n_pages pager 2L;
          let%lwt hr2 = Header.read_live pager in
          (match hr2 with
           | Error e -> Lwt.return_error (map_header_err e)
           | Ok h ->
             let meta = Btree.create pager ~root_page:0L in
             Lwt.return_ok (make_btree_store ~cipher ~close_fn:close ~pager ~meta ~h ())))
     | Error e -> Lwt.return_error (map_header_err e)
     | Ok h ->
       (match check_key h cipher with
        | Error e -> Lwt.return_error e
        | Ok () ->
          Pager.set_n_pages pager h.n_pages_total;
          let%lwt fl = read_freelist_pages pager ~first_page:h.freelist_page in
          Pager.set_freelist pager fl;
          let meta = Btree.create pager ~root_page:h.root_page in
          Lwt.return_ok (make_btree_store ~cipher ~close_fn:close ~pager ~meta ~h ())))
;;

(* ------------------------------------------------------------------ *)
(* WAL-mode opens                                                       *)
(* ------------------------------------------------------------------ *)

module Wal = Sqlocaml_storage.Wal

let install_wal_hook (pager : Pager.t) (wal : Wal.t) =
  let cb : Pager.wal_callbacks =
    { wal_find_page = (fun pid -> Wal.find_page wal pid)
    ; wal_find_page_at = (fun pid ~max_frame -> Wal.find_page_at wal pid ~max_frame)
    ; wal_read_frame =
        (fun idx ->
          let* r = Wal.read_frame wal idx in
          match r with
          | Ok page -> Lwt.return_ok page
          | Error e -> Lwt.return_error (Format.asprintf "%a" Wal.pp_error e))
    ; wal_append_commit =
        (fun pages ->
          let* r = Wal.append_commit wal pages in
          match r with
          | Ok () -> Lwt.return_ok ()
          | Error e -> Lwt.return_error (Format.asprintf "%a" Wal.pp_error e))
    ; wal_append_commit_no_sync =
        (fun pages ->
          let* r = Wal.append_commit_no_sync wal pages in
          match r with
          | Ok () -> Lwt.return_ok ()
          | Error e -> Lwt.return_error (Format.asprintf "%a" Wal.pp_error e))
    ; wal_sync =
        (fun () ->
          let* r = Wal.flush_sync wal in
          match r with
          | Ok () -> Lwt.return_ok ()
          | Error e -> Lwt.return_error (Format.asprintf "%a" Wal.pp_error e))
    }
  in
  Pager.set_wal pager (Some cb)
;;

(* After the WAL hook is installed, re-read the (now WAL-aware) header,
   reconcile [n_pages] for a freshly-initialised DB, load the freelist, and
   build the WAL-backed store. *)
let finish_wal_open ~cipher ~close ~wal_close ~pager ~wal ~was_fresh =
  let%lwt hr2 = Header.read_live pager in
  match hr2 with
  | Error e -> Lwt.return_error (map_header_err e)
  | Ok h ->
    (match check_key h cipher with
     | Error e -> Lwt.return_error e
     | Ok () ->
       (* If the header n_pages_total is below the pager's current allocation,
          prefer the pager's value (freshly-init'd headers carry
          n_pages_total = 0). *)
       let chosen_n_pages =
         if was_fresh
         then Int64.max h.n_pages_total (Pager.n_pages pager)
         else h.n_pages_total
       in
       Pager.set_n_pages pager chosen_n_pages;
       let%lwt fl = read_freelist_pages pager ~first_page:h.freelist_page in
       Pager.set_freelist pager fl;
       let meta = Btree.create pager ~root_page:h.root_page in
       Lwt.return_ok
         (make_btree_store
            ~cipher
            ~wal:(Some wal)
            ~wal_close:(Some wal_close)
            ~close_fn:close
            ~pager
            ~meta
            ~h
            ()))
;;

let open_block_wal
      ?(key : string option)
      ?(geom = Geometry.default)
      ~(read_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
      ~(write_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
      ~(sync : unit -> (unit, string) result Lwt.t)
      ~(resize : n_pages:int64 -> (unit, string) result Lwt.t)
      ~(n_pages : int64)
      ~(wal_read_at : offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
      ~(wal_write_at : offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
      ~(wal_sync : unit -> (unit, string) result Lwt.t)
      ~(wal_size_bytes : int64)
      ~(close : unit -> unit Lwt.t)
      ~(wal_close : unit -> unit Lwt.t)
      ()
  : (t, error) result Lwt.t
  =
  match
    let ( let* ) = Result.bind in
    let* cipher = build_cipher key in
    let* () = ensure_rng_seeded cipher in
    let* geom = geom_for_cipher cipher geom in
    Ok (cipher, geom)
  with
  | Error e -> Lwt.return_error e
  | Ok (cipher, geom) ->
    let read_page, write_page = wrap_callbacks cipher ~read_page ~write_page in
    let pager =
      Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist:Freelist.empty
    in
    (* Adopt the file's real geometry before any header read or WAL open so the
       main-DB buffers and the WAL frame size both match it (#95). *)
    let%lwt eff_geom = peek_geometry ~read_page ~fallback:geom in
    Pager.set_geom pager eff_geom;
    (* Step 1: read the main-DB header (or initialise if fresh). The WAL
       hook is NOT installed yet, so writes go directly to the main DB.
       [was_fresh] flag preserves the post-init n_pages override below. *)
    let%lwt hr = Header.read_live pager in
    let%lwt init_result =
      match hr with
      | Error Header.Both_headers_corrupt ->
        let%lwt ir = Header.init ~enc:(make_enc_info cipher) pager in
        (match ir with
         | Error e -> Lwt.return_error (map_header_err e)
         | Ok () ->
           Pager.set_n_pages pager 2L;
           Lwt.return_ok true)
      | Error e -> Lwt.return_error (map_header_err e)
      | Ok _ -> Lwt.return_ok false
    in
    (match init_result with
     | Error e -> Lwt.return_error e
     | Ok was_fresh ->
       (* Step 2: open the WAL and recover its index. *)
       let%lwt wr =
         Wal.open_
           ~cipher
           ~page_size:(Pager.page_size pager)
           ~read_at:wal_read_at
           ~write_at:wal_write_at
           ~sync:wal_sync
           ~size_bytes:wal_size_bytes
           ()
       in
       (match wr with
        | Error e ->
          Lwt.return_error (Block_error (Format.asprintf "wal open: %a" Wal.pp_error e))
        | Ok wal ->
          (* Step 3: install the hook so subsequent reads consult the WAL. *)
          install_wal_hook pager wal;
          (* Step 4: re-read the header (now WAL-aware) and build the store. *)
          finish_wal_open ~cipher ~close ~wal_close ~pager ~wal ~was_fresh))
;;

(* ------------------------------------------------------------------ *)
(* Transactions                                                         *)
(* ------------------------------------------------------------------ *)

let ro_begin t =
  (* [Rwlock.acquire_read] is a counter bump, not an exclusion: under
     snapshot isolation readers and writers don't conflict, so the call
     never blocks regardless of writer state.  It only matters for the
     checkpoint coordinator that wants to know "are any RO snapshots
     still in flight?" *)
  let* () = Rwlock.acquire_read t.lock in
  let is_closing =
    match t.backend with
    | Btree st -> st.closing
    | Mem _ -> false
  in
  if is_closing
  then (
    (* #338 (review r3): fail fast on a snapshot begun after [close] signalled
       teardown — matches [rw_begin], avoiding an obscure pager EBADF later. *)
    Rwlock.release_read t.lock;
    Lwt.fail_with "Store.ro_begin: store is closing — read transactions are rejected")
  else (
    match t.backend with
    | Mem trees ->
      (* #178: snapshot every tree so RO reads never observe uncommitted
       writes from a concurrent writer that later rolls back.  The
       Btree backend gets snapshot isolation from the pager/WAL layer;
       the mem backend must provide it here. *)
      let snap = Hashtbl.fold (fun tid r acc -> (tid, !r) :: acc) trees [] in
      Lwt.return
        (Ro
           { rs_store = t
           ; rs_snap_txn_id = 0L
           ; rs_snap_meta_root = 0L
           ; rs_snap_trees = Hashtbl.create 1
           ; rs_snap_frames = 0
           ; rs_pinned = Hashtbl.create 1
           ; rs_mem_snap = Some snap
           })
    | Btree st ->
      let snap_txn_id = st.current_header.txn_id in
      let snap_meta_root = st.current_header.root_page in
      let committed_frames =
        match st.wal with
        | None -> 0
        | Some w -> Wal.committed_frames w
      in
      let snap_frames =
        if st.follower
        then (
          match st.follower_ack_position with
          | Some n ->
            (* On a following replica, cap the RO snapshot to the follower's
               last-applied commit boundary so the reader never observes WAL
               frames that haven't been applied on this node yet (#263).
               Both [committed_frames] and [n] are local committed-frame
               counts (not master-epoch indices), so [min] is safe and
               naturally handles a post-reset WAL where committed_frames
               has dropped below the recorded ack position. *)
            min committed_frames n
          | None -> committed_frames)
        else committed_frames
      in
      let count =
        Option.value ~default:0 (Hashtbl.find_opt st.active_readers snap_txn_id)
      in
      Hashtbl.replace st.active_readers snap_txn_id (count + 1);
      let frame_count =
        Option.value ~default:0 (Hashtbl.find_opt st.active_reader_frames snap_frames)
      in
      Hashtbl.replace st.active_reader_frames snap_frames (frame_count + 1);
      Lwt.return
        (Ro
           { rs_store = t
           ; rs_snap_txn_id = snap_txn_id
           ; rs_snap_meta_root = snap_meta_root
           ; rs_snap_trees = Hashtbl.create 4
           ; rs_snap_frames = snap_frames
           ; rs_pinned = Hashtbl.create 64
           ; rs_mem_snap = None
           }))
;;

let rw_begin t =
  let* () = Rwlock.acquire_write t.lock in
  let is_follower =
    match t.backend with
    | Btree st -> st.follower
    | Mem _ -> false
  in
  let is_closing =
    match t.backend with
    | Btree st -> st.closing
    | Mem _ -> false
  in
  if is_closing
  then (
    (* #338 (review r2 #4): fail fast on a write begun after [close] signalled
       teardown, rather than letting the commit surface an obscure EBADF from a
       torn-down fd.  [close] does not take [t.lock], so a write can still race
       in here; this is best-effort, paired with the quiesce-before-close
       contract documented on [close]. *)
    Rwlock.release_write t.lock;
    Lwt.fail_with "Store.rw_begin: store is closing — write transactions are rejected")
  else if is_follower
  then (
    Rwlock.release_write t.lock;
    Lwt.fail_with
      "Store.rw_begin: store is in follower mode — write transactions are rejected while \
       following")
  else (
    (match t.backend with
     | Mem trees ->
       let snap = Hashtbl.fold (fun tid r acc -> (tid, !r) :: acc) trees [] in
       t.mem_rw_shadow <- Some snap;
       t.mem_savepoints <- []
     | Btree st ->
       let current_rw_txn_id = Int64.add st.current_header.txn_id 1L in
       Pager.set_txn_id st.pager current_rw_txn_id;
       let min_safe =
         match min_active_reader_txn st with
         | None -> current_rw_txn_id
         | Some m -> Int64.min current_rw_txn_id m
       in
       Pager.set_alloc_min_safe st.pager min_safe;
       (* #297: same-txn page reuse happens via the txn_owned_pool (pages
          allocated above n_pages_at_rw_begin), NOT the main freelist, so
          alloc_min_safe is unchanged from the pre-#297 baseline.  The
          None branch (no readers) and Some m branch (reader exists) both
          keep the original guard — committed-tree pages freed at
          current_rw_txn_id are never eligible for same-txn reuse via the
          main freelist regardless of reader state.  The txn_owned_pool,
          checked before the main freelist by Pager.alloc, provides
          same-txn reuse independently of the freelist guard. *)
       Pager.set_n_pages_at_rw_begin st.pager (Pager.n_pages st.pager);
       (* #297: defensive reset — any leftover from the previous txn is stale. *)
       Pager.txn_owned_pool_set st.pager [];
       st.txn_freelist_snapshot <- Some (Pager.freelist st.pager));
    Lwt.return (Rw t))
;;

let ro_end (Ro snap : ro txn) =
  (match snap.rs_store.backend with
   | Mem _ -> ()
   | Btree st ->
     let tid = snap.rs_snap_txn_id in
     (match Hashtbl.find_opt st.active_readers tid with
      | None | Some 1 -> Hashtbl.remove st.active_readers tid
      | Some n -> Hashtbl.replace st.active_readers tid (n - 1));
     (match Hashtbl.find_opt st.active_reader_frames snap.rs_snap_frames with
      | None | Some 1 -> Hashtbl.remove st.active_reader_frames snap.rs_snap_frames
      | Some n -> Hashtbl.replace st.active_reader_frames snap.rs_snap_frames (n - 1));
     (* Release the pages this snapshot pinned (#159) so they become
        evictable again. *)
     Pager.unpin_all st.pager snap.rs_pinned;
     Lwt_condition.broadcast st.reader_done_cond ());
  Rwlock.release_read snap.rs_store.lock;
  Lwt.return_unit
;;

let with_ro t f =
  let* tx = ro_begin t in
  Lwt.finalize (fun () -> f tx) (fun () -> ro_end tx)
;;

(* Free the previous freelist page chain back into the pager's in-memory
   freelist (stamped with the current txn_id). *)
let free_old_freelist_pages pager ~first_page =
  let rec loop pid =
    if Int64.equal pid 0L
    then Lwt.return_unit
    else
      let* r = Pager.read pager pid in
      let next_pid =
        match r with
        | Error _ -> 0L
        | Ok buf ->
          let c = Page.read_common buf in
          Int64.logand 0xFFFFFFFFL (Int64.of_int32 c.Page.right_page)
      in
      (* Note: if read fails mid-chain, remaining pages beyond this point are
         orphaned (leaked). This is acceptable only because a corrupt freelist
         page implies a deeper storage invariant violation. *)
      Pager.free pager ~page_id:pid ~freed_at_txn_id:(Pager.get_txn_id pager);
      loop next_pid
  in
  loop first_page
;;

(* Serialize the current pager freelist to a new page chain.
   Returns the first page id (0L if the freelist is empty). *)
(* Build and write a single freelist page holding [chunk] (possibly empty),
   chaining to [next]. *)
let write_one_freelist_page pager ~pid ~next ~chunk =
  let buf = Cstruct.create (Pager.page_size pager) in
  Cstruct.memset buf 0;
  Page.write_common
    buf
    { Page.kind = Page.Freelist
    ; flags = 0
    ; n_keys = List.length chunk
    ; right_page = Int64.to_int32 next
    ; crc32 = 0l
    };
  List.iteri
    (fun j (page_id, freed_at_txn_id) ->
       Page.freelist_set_entry buf ~index:j ~page_id ~freed_at_txn_id)
    chunk;
  Pager.write pager pid buf
;;

let write_freelist_pages pager : int64 Lwt.t =
  let entries_before = Freelist.to_list (Pager.freelist pager) in
  let n_entries = List.length entries_before in
  let max_per = Pager.max_freelist_entries_per_page pager in
  let n_fl_pages = (n_entries + max_per - 1) / max_per in
  if n_fl_pages = 0
  then Lwt.return 0L
  else
    (* Allocate all needed pages *)
    let* page_ids =
      Lwt_list.map_s
        (fun () ->
           let* r = Pager.alloc pager in
           match r with
           | Ok pid -> Lwt.return pid
           | Error e ->
             Lwt.fail_with (Format.asprintf "write_freelist_pages: %a" Pager.pp_error e))
        (List.init n_fl_pages (fun _ -> ()))
    in
    (* Get FINAL freelist state after allocations *)
    let final_entries = Freelist.to_list (Pager.freelist pager) in
    (* Split into chunks of max_per *)
    let rec chunkify = function
      | [] -> []
      | lst ->
        let chunk = List.filteri (fun i _ -> i < max_per) lst in
        let rest = List.filteri (fun i _ -> i >= max_per) lst in
        chunk :: chunkify rest
    in
    let chunks = chunkify final_entries in
    let n_chunks = List.length chunks in
    let pid_arr = Array.of_list page_ids in
    let next_of i = if i + 1 < Array.length pid_arr then pid_arr.(i + 1) else 0L in
    (* Write each chunk to a freelist page *)
    List.iteri
      (fun i chunk ->
         write_one_freelist_page pager ~pid:pid_arr.(i) ~next:(next_of i) ~chunk)
      chunks;
    (* Any extra allocated pages (n_fl_pages > n_chunks) get empty freelist pages *)
    for i = n_chunks to n_fl_pages - 1 do
      write_one_freelist_page pager ~pid:pid_arr.(i) ~next:(next_of i) ~chunk:[]
    done;
    Lwt.return pid_arr.(0)
;;

(* Block until it is safe to recycle WAL frames below [target].  Used by
   [checkpoint_unlocked] before [Wal.reset] truncates the index — otherwise
   an in-flight reader's [find_page_at] would resolve to a recycled frame
   index after the next writer's append.

   Two distinct gates, with different urgency (#207):

   - RO snapshots ([ro_readers_below]): a local reader still needs frames
     below [target].  Recycling past it corrupts its snapshot, so this gate
     is honored UNCONDITIONALLY — we wait on the broadcast, which a local
     reader always eventually fires via [ro_end].

   - Replication floor ([replication_floor_below]): a standby's acked
     position, plumbed in by the app.  A dead or slow standby must not wedge
     the master's WAL forever, so when this is the SOLE remaining blocker we
     honor a bounded-yield budget [max_floor_yields] and then proceed anyway
     (the standby falls outside the live window and must re-base — #208).

   [max_floor_yields = max_int] means unbounded: we wait on the broadcast
   ([update_replication_position] fires it when the floor advances), exactly
   as before this knob existed — no busy-poll.  A finite budget polls via
   [Lwt.pause] (the project's [wait_for] idiom) because a dead standby
   produces no broadcast to wake on.

   No [~mutex] is passed to [Lwt_condition.wait]: under cooperative Lwt the
    gate check + wait register atomically (no yield between them), so the
    standard POSIX condvar mutex pairing isn't needed.  Would need revisiting
    under a preemptive or effect-based multicore runtime. *)
(** Body of [checkpoint] without mutex management. Caller MUST already
     hold [t.lock] (e.g. during [commit]). Defined here so [commit]
     can invoke it via [maybe_autocheckpoint] below. *)
let rec wait_for_readers_past
          (st : bt_state)
          ~target
          ~replication_max_yields
          ~backup_max_yields
  =
  if st.closing
  then
    (* #338: close is tearing down — stop gating so a parked checkpoint unwinds;
       [checkpoint_unlocked] then aborts before any fd I/O. *)
    Lwt.return_unit
  else if ro_readers_below st ~target
  then
    (* A local reader blocks: wait unconditionally on the broadcast. *)
    let* () = Lwt_condition.wait st.reader_done_cond in
    wait_for_readers_past st ~target ~replication_max_yields ~backup_max_yields
  else if replication_floor_below st ~target && replication_max_yields > 0
  then
    (* Replication floor is behind and budget remains.  Guard with > 0 so
       an exhausted budget falls through to the backup floor check below
       (review #2); the <= 0 sub-branch is therefore never entered. *)
    if replication_max_yields = max_int
    then
      (* Unbounded: efficient event-driven wait, no busy-poll. *)
      let* () = Lwt_condition.wait st.reader_done_cond in
      wait_for_readers_past st ~target ~replication_max_yields ~backup_max_yields
    else
      let* () = Lwt.pause () in
      wait_for_readers_past
        st
        ~target
        ~replication_max_yields:(replication_max_yields - 1)
        ~backup_max_yields
  else if backup_floor_below st ~target
  then
    if backup_max_yields = max_int
    then
      let* () = Lwt_condition.wait st.reader_done_cond in
      wait_for_readers_past st ~target ~replication_max_yields ~backup_max_yields
    else if backup_max_yields <= 0
    then Lwt.return_unit
    else
      let* () = Lwt.pause () in
      wait_for_readers_past
        st
        ~target
        ~replication_max_yields
        ~backup_max_yields:(backup_max_yields - 1)
  else Lwt.return_unit
;;

let checkpoint_unlocked (st : bt_state) (wal : Wal.t) : unit Lwt.t =
  let target = Wal.committed_frames wal in
  let* () =
    wait_for_readers_past
      st
      ~target
      ~replication_max_yields:st.replication_gate_max_yields
      ~backup_max_yields:st.backup_gate_max_yields
  in
  if st.closing
  then
    (* #338: [close] signalled teardown while we were gated — abort before any
       pager/WAL fd I/O.  [close] fsyncs the WAL itself; the un-migrated frames
       replay on next open.  No data loss. *)
    Lwt.return_unit
  else (
    (* #338 (review r2): past the gate and about to touch fds — register as
       in-flight so [close] drains us before teardown (covers BOTH the auto path
       and the manual [checkpoint] path, which share this function).  A
       checkpoint still parked above on [acquire_write]/the gate is NOT yet
       counted, so it cannot wedge close. *)
    st.ckpt_io_in_flight <- st.ckpt_io_in_flight + 1;
    Lwt.finalize
      (fun () ->
         let pairs = ref [] in
         Wal.iter_index wal (fun pid idx -> pairs := (pid, idx) :: !pairs);
         let rec write_each = function
           | [] -> Lwt.return_unit
           | (pid, idx) :: rest ->
             let* r = Wal.read_frame wal idx in
             (match r with
              | Error e ->
                Lwt.fail_with (Format.asprintf "checkpoint read: %a" Wal.pp_error e)
              | Ok page ->
                let* wr = Pager.flush_one_to_main st.pager ~page_id:pid ~buf:page in
                (match wr with
                 | Error e ->
                   Lwt.fail_with (Format.asprintf "checkpoint write: %a" Pager.pp_error e)
                 | Ok () -> write_each rest))
         in
         let* () = write_each !pairs in
         let* sr = Pager.flush_sync_main st.pager in
         match sr with
         | Error e ->
           Lwt.fail_with (Format.asprintf "checkpoint sync: %a" Pager.pp_error e)
         | Ok () ->
           (* #337: an async sink ship dispatched from [commit_wal] reads its
            frame payloads LAZILY.  [Wal.reset] below recycles/zeroes those
            frames and bumps the epoch, so wait for any in-flight ship to finish
            reading first.  The ship runs without [t.lock] (it only reads
            frames), so it makes progress while this fiber holds the lock and
            parks here.  The check-then-reset is yield-free, so no ship
            dispatched after the count reaches 0 can slip in before reset. *)
           let* () = wait_until st (fun () -> st.sink_ships_in_flight = 0) in
           Wal.reset wal;
           (* #298/#1: checkpoint is a full-sync durability anchor — everything
            is now durable and the WAL starts a fresh epoch at frame 0.  Reset
            the sink ship counter (new epoch) and the batched durability counters
            so a long unsynced window doesn't carry stale state across the
            anchor. *)
           st.sink_shipped_frames <- 0;
           st.unsynced_commits <- 0;
           st.last_sync_time <- st.clock ();
           (* Re-pin the replication floor for the new epoch.  [Wal.reset] zeroes
             committed_frames, but [replication_shipped_frames] still refers to
             the old epoch's absolute count.  Without re-pinning, the next
             checkpoint would see a stale floor that appears to be past the new
             target, silently allowing frame recycling before the sink ships
             them. *)
           if st.on_committed_frames <> None
           then st.replication_shipped_frames <- Wal.committed_frames wal;
           (* Re-pin the backup floor for the new epoch (#265).  Same
             reasoning: without re-pinning, the next checkpoint would see a
             stale backup floor from the old epoch and recycle frames before
             the backup consumer has captured them. *)
           if st.backup_shipped_frames <> max_int
           then st.backup_shipped_frames <- Wal.committed_frames wal;
           Lwt.return_unit)
      (fun () ->
         st.ckpt_io_in_flight <- st.ckpt_io_in_flight - 1;
         Lwt_condition.broadcast st.reader_done_cond ();
         Lwt.return_unit))
;;

(** Called from [commit] while [lock] is still held (exclusive). If the WAL has
    grown past the per-connection threshold, migrate it inline so
    subsequent commits start fresh. Best-effort: a checkpoint failure
    is swallowed (the commit itself already succeeded). *)
let maybe_autocheckpoint (st : bt_state) : unit Lwt.t =
  match st.wal with
  | None -> Lwt.return_unit
  | Some wal ->
    let thr = st.wal_autocheckpoint_threshold in
    if thr <= 0
    then Lwt.return_unit
    else if Wal.committed_frames wal < thr
    then Lwt.return_unit
    else Lwt.catch (fun () -> checkpoint_unlocked st wal) (fun _ -> Lwt.return_unit)
;;

(* Group-commit coordinator (#77, #151).  One fiber per [commit_queue]
   runs the actual fsync via [sync_fn]; concurrent writers register a
   per-batch resolver and block until the drainer wakes them with the
   sync result.  Returns the role this fiber played so the caller can
   attach drainer-only side work (e.g. autocheckpoint).

   Each joiner pushes a [(unit, exn) result Lwt.u] resolver onto
   [waiters] and increments [pending] so the drainer can detect
   concurrent arrivals.  The drainer yields via [Lwt.pause] once to let
   any ready-to-write fibers reach the queue, then loops pausing while
   [pending] keeps growing.  This widens the batch from "2 commits per
   fsync" (one Lwt.pause yields one continuation) to "N concurrent
   writers per fsync" while adding only one tick of latency to a lone
   writer.

   Failure semantics (#151): if [sync_fn] raises, the drainer wakes
   every joiner's resolver with [Error exn] (so each joiner re-raises
   the same exception via [Lwt.fail]) and then re-raises to its own
   caller.  All N writers in the current batch observe the failure;
   none see a spurious [Ok].

   Late joiners that arrive while [sync_fn] is in flight register on
   the same [waiters] list (since [q.drainer] is still [true]) and so
   ride along with the current sync's result — matching the pre-#151
   broadcast behaviour.  Whether those late frames are physically
   flushed by the in-flight fsync is timing-dependent at the kernel
   level (POSIX only guarantees flushing of writes queued before the
   fsync syscall); this is a pre-existing concern, not introduced by
   the error-channel rework. *)
let group_commit_sync (q : commit_queue) (sync_fn : unit -> unit Lwt.t)
  : [ `Drainer | `Joiner ] Lwt.t
  =
  if q.drainer
  then (
    let p, u = Lwt.wait () in
    q.waiters <- u :: q.waiters;
    q.pending <- q.pending + 1;
    let* r = p in
    match r with
    | Ok () -> Lwt.return `Joiner
    | Error exn -> Lwt.fail exn)
  else (
    q.drainer <- true;
    (* Gather: one initial pause to let the next-in-line writer reach
       the queue; then keep pausing while [pending] keeps growing.
       Stops as soon as a pause completes without seeing any new
       arrival — keeping per-commit overhead bounded for solo
       writers. *)
    let* () = Lwt.pause () in
    let rec gather last_seen =
      let now_seen = q.pending in
      if now_seen > last_seen
      then
        let* () = Lwt.pause () in
        gather now_seen
      else Lwt.return_unit
    in
    let* () = gather 0 in
    (* Run sync first, capturing success or failure; then atomically
       snapshot the (possibly grown) waiter list, clear queue state for
       the next batch, and wake each joiner with the same result.
       Cooperative scheduling guarantees no yield between try_bind's
       handler and the iter, so no joiner can register after the
       snapshot. *)
    Lwt.try_bind
      sync_fn
      (fun () ->
         let waiters = q.waiters in
         q.waiters <- [];
         q.pending <- 0;
         q.drainer <- false;
         List.iter (fun u -> Lwt.wakeup_later u (Ok ())) waiters;
         Lwt.return `Drainer)
      (fun exn ->
         let waiters = q.waiters in
         q.waiters <- [];
         q.pending <- 0;
         q.drainer <- false;
         List.iter (fun u -> Lwt.wakeup_later u (Error exn)) waiters;
         Lwt.fail exn))
;;

(* Prepare phase of [commit] for the Btree backend.  Pushes every tree's
   latest root_page through the meta-tree, writes the freelist pages,
   and invokes [~header_commit] (either {!Header.commit} for the inline-
   sync path or {!Header.commit_no_sync} for group commit).  On success
   advances [st.current_header] and resets per-txn state.  On failure
   raises via [Lwt.fail_with] without touching the mutex. *)
let commit_prepare_btree
      ~(header_commit :
         Pager.t
         -> prev_header:Header.t
         -> new_state:Header.t
         -> (unit, Header.error) result Lwt.t)
      (st : bt_state)
  : unit Lwt.t
  =
  let* () =
    free_old_freelist_pages st.pager ~first_page:st.current_header.freelist_page
  in
  let bindings = Hashtbl.fold (fun tid bt acc -> (tid, bt) :: acc) st.trees [] in
  (* #174: meta-tree pages are system pages — never stamped with a tree tag. *)
  Pager.set_write_tag st.pager 0l;
  let* () =
    Lwt_list.iter_s
      (fun (tid, bt) ->
         let key = encode_tree_id tid in
         let v = encode_root_page (Btree.root_page bt) in
         let* r = Btree.put st.meta key v in
         match r with
         | Ok meta' ->
           st.meta <- meta';
           Lwt.return_unit
         | Error e ->
           Lwt.fail_with (Format.asprintf "Store.commit: %a" pp_error (map_btree_err e)))
      bindings
  in
  let* freelist_first_page = write_freelist_pages st.pager in
  let new_state : Header.t =
    { txn_id = 0L
    ; (* overwritten by header_commit *)
      root_page = Btree.root_page st.meta
    ; freelist_page = freelist_first_page
    ; n_pages_total = Pager.n_pages st.pager
    ; schema_version = st.schema_version
    ; (* Preserve the on-disk format version this db was opened with (#174);
         never silently upgrade or downgrade it here. *)
      format_version = st.current_header.format_version
    ; (* Preserve the file's page geometry (#95); fixed at creation. *)
      geom = st.current_header.geom
    ; (* Preserve the encryption marker/canary across commits (#84). *)
      enc = st.current_header.enc
    }
  in
  let* r = header_commit st.pager ~prev_header:st.current_header ~new_state in
  match r with
  | Error e ->
    Lwt.fail_with (Format.asprintf "Store.commit: %a" pp_error (map_header_err e))
  | Ok () ->
    st.current_header <- { new_state with txn_id = Int64.add st.current_header.txn_id 1L };
    st.txn_freelist_snapshot <- None;
    st.bt_savepoints <- [];
    (* #297: discard the txn-owned pool — these pages are now part
       of the committed tree (freed file-extension pages reuse
       within the txn; any not reused by commit are orphans). *)
    Pager.txn_owned_pool_set st.pager [];
    Lwt.return_unit
;;

(* commit:
   - Mem backend: no I/O, just release the writer lock.
   - Btree backend without WAL: flush all currently-open trees'
     root_pages into the meta-tree, then write a new header pointing at
     the new meta root, sync inline, autocheckpoint, release.
   - Btree backend with WAL: same prepare phase but using
     [Header.commit_no_sync] so the writer can release [lock]
     before the fsync.  Writers then converge on a per-store
     [commit_queue]; one drainer fsyncs and resolves all waiters.  Only
     the drainer attempts the autocheckpoint (single check per fsync
     covers the whole batch).

   Note: the Btree.create/put/del API returns a NEW Btree.t after every
   mutation (root_page may have changed).  We update [st.trees] each
   time; here we additionally persist the latest root_page for each
   touched tree into the meta-tree (whose own root we then commit via
   the header alternating-pages protocol). *)
(* After a WAL group-commit, the drainer kicks off an async autocheckpoint if
   the WAL has grown past the threshold and none is already in flight. *)
let maybe_autockpt_after_commit t st =
  if
    st.closing
    (* #338: no fresh checkpoint once close has signalled teardown. *)
    || st.wal_autocheckpoint_threshold <= 0
    || Wal.committed_frames
         (match st.wal with
          | Some w -> w
          | None -> assert false)
       < st.wal_autocheckpoint_threshold
    || st.autockpt_in_flight
  then Lwt.return_unit
  else (
    st.autockpt_in_flight <- true;
    Lwt.async (fun () ->
      Lwt.finalize
        (fun () ->
           Lwt.catch
             (fun () ->
                let* () = Rwlock.acquire_write t.lock in
                Lwt.finalize
                  (fun () ->
                     match st.wal with
                     | None -> Lwt.return_unit
                     | Some wal -> checkpoint_unlocked st wal)
                  (fun () ->
                     Rwlock.release_write t.lock;
                     Lwt.return_unit))
             (fun _ -> Lwt.return_unit))
        (fun () ->
           st.autockpt_in_flight <- false;
           (* #338: wake a [close] awaiting the in-flight checkpoint to drain. *)
           Lwt_condition.broadcast st.reader_done_cond ();
           Lwt.return_unit));
    Lwt.return_unit)
;;

(* WAL-mode commit: prepare the btree (no sync), release the write lock early,
   then group-commit-sync the WAL.  The elected drainer may autocheckpoint. *)
let commit_wal t st =
  let unlocked = ref false in
  let unlock_once () =
    if not !unlocked
    then (
      unlocked := true;
      Rwlock.release_write t.lock)
  in
  let wal =
    match st.wal with
    | Some w -> w
    | None -> assert false
  in
  Lwt.catch
    (fun () ->
       let* () = commit_prepare_btree ~header_commit:Header.commit_no_sync st in
       (* #298: decide the sync policy under the write lock so the counter is
          race-free across concurrent writers, then release the lock. *)
       let do_sync =
         match st.sync_mode with
         | `Full -> true (* always sync; unsynced_commits is not tracked in Full mode *)
         | `Off ->
           st.unsynced_commits <- st.unsynced_commits + 1;
           false
         | `Batched ->
           st.unsynced_commits <- st.unsynced_commits + 1;
           (* #298/#8: a non-positive threshold DISABLES that trigger (the
              codebase's [0 = disabled] convention), rather than firing every
              commit.  If BOTH are 0, batched never syncs on commit. *)
           let n_trig = st.batch_commits > 0 && st.unsynced_commits >= st.batch_commits in
           let t_trig =
             st.batch_interval_ms > 0
             && (st.clock () -. st.last_sync_time) *. 1000.
                >= float_of_int st.batch_interval_ms
           in
           n_trig || t_trig
       in
       (* #338/#2: reset the batched loss-window counters HERE, under the write
          lock, at the moment we decide to sync — not after [unlock_once] post
          fsync.  The old unlocked post-fsync write raced both the locked
          checkpoint reset and a concurrent committer's increment, drifting the
          batched-N trigger by one.  The frames become durable when the fsync
          below completes; a failed fsync raises (the counter is then moot). *)
       if do_sync
       then (
         st.unsynced_commits <- 0;
         st.last_sync_time <- st.clock ());
       unlock_once ();
       let* () =
         if do_sync
         then (
           let* role =
             group_commit_sync st.commit_queue (fun () ->
               let* r = Pager.wal_sync st.pager in
               match r with
               | Ok () -> Lwt.return_unit
               | Error e ->
                 Lwt.fail_with
                   (Format.asprintf "Store.commit: wal_sync: %a" Pager.pp_error e))
           in
           (* #298/#1: ship synced frames to the replication sink.  The sink
              must NEVER see a frame that has not been fsynced, so this fires
              ONLY here (in the sync success branch), shipping the whole synced
              range since the last ship.  In Full mode this fires every commit
              (one batch each); in Batched it fires at each sync (the whole
              accumulated batch).  In Off it never fires on commit — only
              checkpoint/close make frames durable. *)
           (match st.on_committed_frames with
            | None -> ()
            | Some cb ->
              let synced = Wal.committed_frames wal in
              (* #338 (review r3): do NOT dispatch a fresh ship once [close] has
                 signalled teardown — its lazy [Wal.read_frame] would race
                 [wal_close] (a commit mid-fsync when close starts can reach here
                 AFTER close's drain saw [sink_ships_in_flight = 0]).  The frames
                 are already fsynced; the standby re-syncs from the WAL on
                 reconnect, same recovery story as an aborted checkpoint.  The
                 check is yield-free up to [Lwt.async], so close cannot set
                 [closing] between this check and the dispatch. *)
              if (not st.closing) && synced > st.sink_shipped_frames
              then (
                let base = st.sink_shipped_frames in
                let count = synced - base in
                st.sink_shipped_frames <- synced;
                let epoch = Wal.epoch wal in
                (* #337: register the ship as in-flight SYNCHRONOUSLY (before the
                   [Lwt.async] yields) so a checkpoint dispatched right after
                   sees the count and waits in [checkpoint_unlocked] before
                   [Wal.reset]; decrement + wake the gate when the lazy reader
                   completes. *)
                st.sink_ships_in_flight <- st.sink_ships_in_flight + 1;
                Lwt.async (fun () ->
                  Lwt.finalize
                    (fun () -> cb ~epoch ~base_idx:base ~count)
                    (fun () ->
                       st.sink_ships_in_flight <- st.sink_ships_in_flight - 1;
                       Lwt_condition.broadcast st.reader_done_cond ();
                       Lwt.return_unit))));
           match role with
           | `Joiner -> Lwt.return_unit
           | `Drainer -> maybe_autockpt_after_commit t st)
         else
           (* No fsync this commit: still bound the WAL via autocheckpoint
              (checkpoint is a full-sync durability anchor). *)
           maybe_autockpt_after_commit t st
       in
       Lwt.return_unit)
    (fun exn ->
       unlock_once ();
       Lwt.fail exn)
;;

let commit (Rw t : rw txn) : unit Lwt.t =
  match t.backend with
  | Mem trees ->
    (* #178: merge the shadow back into the live tree.  The live tree was
       never mutated during the txn — only the shadow was touched — so
       commit is the first and only time the live tree sees the txn's
       writes. *)
    (match t.mem_rw_shadow with
     | None -> ()
     | Some shadow ->
       List.iter
         (fun (tid, map) ->
            let r = mem_tree trees tid in
            r := map)
         shadow);
    t.mem_rw_shadow <- None;
    t.mem_savepoints <- [];
    Rwlock.release_write t.lock;
    Lwt.return_unit
  | Btree st ->
    (* #356: the append cursor is only valid within a txn (its leaf is dirty);
       commit flushes dirty pages, so drop it. *)
    Hashtbl.clear st.bt_append;
    (match st.wal with
     | None ->
       Lwt.finalize
         (fun () ->
            let* () = commit_prepare_btree ~header_commit:Header.commit st in
            maybe_autocheckpoint st)
         (fun () ->
            Rwlock.release_write t.lock;
            Lwt.return_unit)
     | Some _ -> commit_wal t st)
;;

(* rollback:
   - Mem: discard the per-txn shadow.  With shadow writes (#178) the live
     tree is never mutated during a RW txn, so rollback does not need to
     restore anything — it just drops the uncommitted shadow.
   - Btree: drop cached tree handles so subsequent reads pick up
     last-committed roots from the meta-tree, then restore the freelist
     snapshot taken at rw_begin and clear dirty pages.
     Discard dirty pages from the aborted txn: clear_dirty removes them from
     both the dirty set and the read cache, so subsequent reads see committed
     data from disk. The freelist snapshot ensures no aborted CoW frees
     corrupt future allocations. *)
let rollback (Rw t : rw txn) : unit Lwt.t =
  (match t.backend with
   | Mem _ ->
     (* #178: just discard the shadow — the live tree was never touched. *)
     t.mem_rw_shadow <- None;
     t.mem_savepoints <- []
   | Btree st ->
     (* Drop the per-tree cache so subsequent reads pick up the
        last-committed roots from the meta-tree.  Note: the meta-tree
        itself may have been mutated during this txn (uncommitted puts
        to it); we revert it to the last-committed root from the
        header. *)
     Hashtbl.clear st.trees;
     Hashtbl.clear st.bt_append (* #356: dirty pages discarded below. *);
     st.meta <- Btree.create st.pager ~root_page:st.current_header.root_page;
     (match st.txn_freelist_snapshot with
      | Some fl ->
        Pager.set_freelist st.pager fl;
        Pager.clear_dirty st.pager;
        st.txn_freelist_snapshot <- None
      | None -> ());
     st.bt_savepoints <- []);
  Rwlock.release_write t.lock;
  Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* WAL checkpoint                                                       *)
(* ------------------------------------------------------------------ *)

(** Migrate every page currently in the WAL index to the main DB, sync
    the main DB, then reset the WAL. Holds the RW mutex so no
    concurrent commit can append fresh frames while we read the index.
    On Mem stores or non-WAL Btree stores this is a no-op. *)
let checkpoint (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    (match st.wal with
     | None -> Lwt.return_unit
     | Some wal ->
       let* () = Rwlock.acquire_write t.lock in
       Lwt.finalize
         (fun () -> checkpoint_unlocked st wal)
         (fun () ->
            Rwlock.release_write t.lock;
            Lwt.return_unit))
;;

let wal_autocheckpoint (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> st.wal_autocheckpoint_threshold
;;

let set_wal_autocheckpoint (t : t) (n : int) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.wal_autocheckpoint_threshold <- max 0 n
;;

let durability (t : t) : durability =
  match t.backend with
  | Mem _ -> Full
  | Btree st ->
    (match st.sync_mode with
     | `Full -> Full
     | `Off -> Off
     | `Batched ->
       Batched { commits = st.batch_commits; interval_ms = st.batch_interval_ms })
;;

(* #298/#9: durability <-> string helpers for the PRAGMA layer (Group B). *)
let durability_of_string (s : string) : durability option =
  match String.lowercase_ascii s with
  | "full" -> Some Full
  | "off" -> Some Off
  | "batched" ->
    Some
      (Batched
         { commits = default_batch_commits; interval_ms = default_batch_interval_ms })
  | _ -> None
;;

let string_of_durability (d : durability) : string =
  match d with
  | Full -> "full"
  | Batched _ -> "batched"
  | Off -> "off"
;;

let set_durability (t : t) (d : durability) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st ->
    let requested_non_full =
      match d with
      | Full -> false
      | _ -> true
    in
    if requested_non_full && st.on_committed_frames <> None
    then (
      (* #298: a sink mandates Full — ignore the relax request but still record
         any Batched params for when the sink is later removed.  The checkpoint
         replica-floor gate requires every committed frame to be shipped, which
         only holds under Full. *)
      match d with
      | Batched { commits; interval_ms } ->
        st.batch_commits <- max 0 commits;
        st.batch_interval_ms <- max 0 interval_ms
      | _ -> ())
    else (
      let new_mode =
        match d with
        | Full -> `Full
        | Off -> `Off
        | Batched _ -> `Batched
      in
      (* #298/#4: when the mode actually changes, reset the batched durability
         counters so a long [Off] period doesn't carry a huge stale
         [unsynced_commits] into [Batched] (which would immediately fsync), and
         the T window restarts at mode entry.  Safe because [close]/[checkpoint]
         (frame-state based) remain the durability anchors; the counter is only a
         trigger heuristic. *)
      if st.sync_mode <> new_mode
      then (
        st.unsynced_commits <- 0;
        st.last_sync_time <- st.clock ());
      match d with
      | Full -> st.sync_mode <- `Full
      | Off -> st.sync_mode <- `Off
      | Batched { commits; interval_ms } ->
        st.sync_mode <- `Batched;
        st.batch_commits <- max 0 commits;
        st.batch_interval_ms <- max 0 interval_ms)
;;

(* #298: True iff a replication commit-sink is currently registered.  While
   active, durability is pinned to [Full]. *)
let commit_callback_active (t : t) : bool =
  match t.backend with
  | Mem _ -> false
  | Btree st -> st.on_committed_frames <> None
;;

(* #298/#3: force any committed-but-unsynced WAL frames to disk now.  No-op in
   [Full] mode, on the in-memory backend, or when nothing is pending.  Group B
   calls this when tightening durability so already-acked commits become durable
   immediately rather than only on the next commit. *)
let flush_unsynced (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    let pending =
      st.sync_mode <> `Full
      &&
      match st.wal with
      | Some w -> Wal.committed_frames w > 0
      | None -> false
    in
    if not pending
    then Lwt.return_unit
    else
      (* #298/#5: route the fsync through the group-commit serializer rather
         than calling [Pager.wal_sync] unlocked, which raced [commit_wal]'s
         post-unlock fsync + cursor.  A sink now forces Full, so [flush_unsynced]
         only runs when NO sink is active — the previous sink-ship block here is
         dead and has been removed. *)
      let* (_ : [ `Drainer | `Joiner ]) =
        group_commit_sync st.commit_queue (fun () ->
          let* r = Pager.wal_sync st.pager in
          match r with
          | Ok () -> Lwt.return_unit
          | Error e ->
            Lwt.fail_with
              (Format.asprintf "Store.flush_unsynced: wal_sync: %a" Pager.pp_error e))
      in
      st.unsynced_commits <- 0;
      st.last_sync_time <- st.clock ();
      Lwt.return_unit
;;

let sync_batch_commits (t : t) : int =
  match t.backend with
  | Mem _ -> default_batch_commits
  | Btree st -> st.batch_commits
;;

let set_sync_batch_commits (t : t) (n : int) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.batch_commits <- max 0 n
;;

let sync_batch_interval_ms (t : t) : int =
  match t.backend with
  | Mem _ -> default_batch_interval_ms
  | Btree st -> st.batch_interval_ms
;;

let set_sync_batch_interval_ms (t : t) (n : int) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.batch_interval_ms <- max 0 n
;;

let set_clock (t : t) (c : unit -> float) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st ->
    st.clock <- c;
    st.last_sync_time <- c ()
;;

(* Number of fsyncs the WAL has performed since open.  Exposed for #77
   group-commit testing: lets the test assert that N concurrent
   autocommit fibers issue ≪ N fsyncs (proof of coalescing). *)
let wal_sync_count (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st ->
    (match st.wal with
     | None -> 0
     | Some w -> Sqlocaml_storage.Wal.sync_count w)
;;

(* Diagnostic/testing accessors for #164: observe that ending a snapshot
   releases its bookkeeping even when its reader closure raised. *)
let active_reader_count (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> Hashtbl.fold (fun _ c acc -> acc + c) st.active_readers 0
;;

let pinned_page_count (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> Pager.pinned_count st.pager
;;

let live_read_locks (t : t) : int = Rwlock.readers t.lock

(* ------------------------------------------------------------------ *)
(* Savepoints (Mem backend only; B-tree deferred)                      *)
(* ------------------------------------------------------------------ *)

(** Push a named savepoint: snapshot the current shadow state (#178). *)
let savepoint_begin (Rw t : rw txn) name =
  match t.backend with
  | Mem trees ->
    let snap =
      match t.mem_rw_shadow with
      | None -> Hashtbl.fold (fun tid r acc -> (tid, !r) :: acc) trees []
      | Some shadow -> shadow
    in
    t.mem_savepoints <- (name, snap) :: t.mem_savepoints;
    Lwt.return_unit
  | Btree st ->
    let tree_roots =
      Hashtbl.fold (fun tid bt acc -> (tid, Btree.root_page bt) :: acc) st.trees []
    in
    let sp =
      { sp_name = name
      ; sp_meta_root = Btree.root_page st.meta
      ; sp_tree_roots = tree_roots
      ; sp_freelist = Pager.freelist st.pager
      ; sp_n_pages = Pager.n_pages st.pager
      ; sp_dirty = Pager.dirty_clone st.pager
      ; sp_txn_pool = Pager.txn_owned_pool_get st.pager
      }
    in
    st.bt_savepoints <- sp :: st.bt_savepoints;
    Lwt.return_unit
;;

(** Release the named savepoint and all newer ones (writes are kept). *)
let savepoint_release (Rw t : rw txn) name =
  match t.backend with
  | Mem _ ->
    let rec drop = function
      | [] -> []
      | (n, _) :: rest when String.equal n name -> rest
      | _ :: rest -> drop rest
    in
    t.mem_savepoints <- drop t.mem_savepoints;
    Lwt.return_unit
  | Btree st ->
    let rec drop = function
      | [] -> []
      | sp :: rest when String.equal sp.sp_name name -> rest
      | _ :: rest -> drop rest
    in
    st.bt_savepoints <- drop st.bt_savepoints;
    Lwt.return_unit
;;

(** Rollback to the named savepoint: restore snapshot, drop newer savepoints,
    keep the named savepoint so it can be rolled back to again. *)
let savepoint_rollback (Rw t : rw txn) name =
  match t.backend with
  | Mem _ ->
    (* #178: restore the shadow to the savepoint snapshot.  The live tree
       was never mutated, so we just replace the shadow. *)
    let rec find = function
      | [] -> () (* savepoint not found — no-op *)
      | (n, snap) :: rest when String.equal n name ->
        t.mem_rw_shadow <- Some snap;
        t.mem_savepoints <- (name, snap) :: rest
      | _ :: rest -> find rest
    in
    find t.mem_savepoints;
    Lwt.return_unit
  | Btree st ->
    let rec find = function
      | [] -> ()
      | sp :: rest when String.equal sp.sp_name name ->
        (* Restore meta-tree root *)
        st.meta <- Btree.create st.pager ~root_page:sp.sp_meta_root;
        (* Restore per-tree roots: drop the cache, re-populate from snapshot. *)
        Hashtbl.clear st.trees;
        List.iter
          (fun (tid, root) ->
             let bt = Btree.create st.pager ~root_page:root in
             Hashtbl.replace st.trees tid bt)
          sp.sp_tree_roots;
        (* Restore freelist + n_pages + dirty set. Pages above sp.sp_n_pages
           that were freshly allocated in the rolled-back range become
           orphans in the file but are not in any tree, freelist, or
           dirty set — harmless storage leak. *)
        Pager.set_freelist st.pager sp.sp_freelist;
        Pager.set_n_pages st.pager sp.sp_n_pages;
        Pager.dirty_restore st.pager sp.sp_dirty;
        Pager.txn_owned_pool_set st.pager sp.sp_txn_pool;
        (* #356: the restored dirty pages may be an earlier version of the
           cached rightmost leaf; drop the append cursor so it re-primes. *)
        Hashtbl.clear st.bt_append;
        (* Keep the named savepoint at the top so it can be re-used. *)
        st.bt_savepoints <- sp :: rest
      | _ :: rest -> find rest
    in
    find st.bt_savepoints;
    Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* get / put / del                                                      *)
(* ------------------------------------------------------------------ *)

let txn_store : type a. a txn -> t = function
  | Ro snap -> snap.rs_store
  | Rw s -> s
;;

let get : type a. a txn -> tree_id -> bytes -> bytes option Lwt.t =
  fun tx tid key ->
  match tx with
  | Ro snap ->
    (match snap.rs_store.backend with
     | Mem _ ->
       (* #178: read from the snapshot captured at ro_begin so this
          reader never sees uncommitted writes from a concurrent writer
          that may later roll back. *)
       let map =
         match snap.rs_mem_snap with
         | Some snap -> mem_tree_snap snap tid
         | None -> Bytes_map.empty
       in
       Lwt.return (Bytes_map.find_opt key map)
     | Btree st ->
       let* r = bt_get_tree_ro snap st tid in
       let* bt = unwrap_error r in
       let* g = Btree.get bt key in
       (match g with
        | Ok v -> Lwt.return v
        | Error e ->
          Lwt.fail_with (Format.asprintf "Store.get(ro): %a" pp_error (map_btree_err e))))
  | Rw t ->
    (match t.backend with
     | Mem trees ->
       (* #178: read from the active RW shadow, not the live tree.
          The shadow contains the txn's own writes layered on top of the
          pre-txn committed state; the live tree is never mutated until
          commit. *)
       let map =
         match t.mem_rw_shadow with
         | None -> !(mem_tree trees tid)
         | Some shadow -> shadow_get shadow trees tid
       in
       Lwt.return (Bytes_map.find_opt key map)
     | Btree st ->
       let* r = bt_get_tree st tid in
       let* bt = unwrap_error r in
       let* g = Btree.get bt key in
       (match g with
        | Ok v -> Lwt.return v
        | Error e ->
          Lwt.fail_with (Format.asprintf "Store.get(rw): %a" pp_error (map_btree_err e))))
;;

let put (Rw t : rw txn) tid key value : unit Lwt.t =
  match t.backend with
  | Mem trees ->
    (* #178: write to the shadow, not the live tree. *)
    (match t.mem_rw_shadow with
     | None ->
       let r = mem_tree trees tid in
       r := Bytes_map.add key value !r
     | Some shadow ->
       t.mem_rw_shadow <- Some (shadow_update shadow trees tid (Bytes_map.add key value)));
    Lwt.return_unit
  | Btree st ->
    let* r = bt_get_tree st tid in
    let* bt = unwrap_error r in
    Pager.set_write_tag st.pager (tree_tag st tid);
    (* #356: [put] may replace or split anywhere; invalidate the append cursor
       for this tree so a subsequent append re-primes from the live rightmost. *)
    Hashtbl.remove st.bt_append tid;
    let* p = Btree.put bt key value in
    (match p with
     | Ok bt' ->
       Hashtbl.replace st.trees tid bt';
       Lwt.return_unit
     | Error e ->
       Lwt.fail_with (Format.asprintf "Store.put: %a" pp_error (map_btree_err e)))
;;

let put_x (Rw t : rw txn) tid key value : bytes option Lwt.t =
  match t.backend with
  | Mem trees ->
    let map =
      match t.mem_rw_shadow with
      | None -> !(mem_tree trees tid)
      | Some shadow -> shadow_get shadow trees tid
    in
    (match Bytes_map.find_opt key map with
     | Some _ -> Lwt.return (Some Bytes.empty)
     | None ->
       (match t.mem_rw_shadow with
        | None ->
          let r = mem_tree trees tid in
          r := Bytes_map.add key value !r
        | Some shadow ->
          t.mem_rw_shadow
          <- Some (shadow_update shadow trees tid (Bytes_map.add key value)));
       Lwt.return None)
  | Btree st ->
    let* r = bt_get_tree st tid in
    let* bt = unwrap_error r in
    Pager.set_write_tag st.pager (tree_tag st tid);
    (* #356 append fast path: O(1) in-place append to the cached rightmost leaf
       when [key] is strictly greater than the cursor's max. *)
    let* fast =
      match Hashtbl.find_opt st.bt_append tid with
      | Some ac when Bytes.compare key (Btree.append_cursor_max_key ac) > 0 ->
        let* outcome = Btree.try_inplace_append bt ac ~key ~value in
        (match outcome with
         | Btree.Appended ac' ->
           Hashtbl.replace st.bt_append tid ac';
           Lwt.return (Some None)
         | Btree.Not_applicable -> Lwt.return None
         | Btree.Append_failed e ->
           Lwt.fail_with (Format.asprintf "Store.put_x: %a" pp_error (map_btree_err e)))
      | _ -> Lwt.return None
    in
    (match fast with
     | Some old_opt -> Lwt.return old_opt
     | None ->
       (* General path.  Remember the prior cursor max to decide whether this
          insert was an append worth re-priming the cursor for. *)
       let prev_max =
         Option.map Btree.append_cursor_max_key (Hashtbl.find_opt st.bt_append tid)
       in
       let* p = Btree.put_x bt key value in
       (match p with
        | Error e ->
          Lwt.fail_with (Format.asprintf "Store.put_x: %a" pp_error (map_btree_err e))
        | Ok (bt', old_opt) ->
          Hashtbl.replace st.trees tid bt';
          (match old_opt with
           | Some _ ->
             (* Conflict: nothing inserted; leave the cursor as-is. *)
             Lwt.return old_opt
           | None ->
             (* Inserted.  If [key] extends the tree to the right (an append),
                re-prime the cursor from the new rightmost leaf; otherwise it was
                a middle insert and the cursor is invalidated. *)
             let append_like =
               match prev_max with
               | None -> true (* cold start: probe whether it was an append *)
               | Some m -> Bytes.compare key m > 0
             in
             if not append_like
             then (
               Hashtbl.remove st.bt_append tid;
               Lwt.return None)
             else
               let* rc = Btree.rightmost_append_cursor bt' in
               (match rc with
                | Ok (Some ac) when Bytes.equal (Btree.append_cursor_max_key ac) key ->
                  Hashtbl.replace st.bt_append tid ac;
                  Lwt.return None
                | Ok _ ->
                  Hashtbl.remove st.bt_append tid;
                  Lwt.return None
                | Error e ->
                  Lwt.fail_with
                    (Format.asprintf "Store.put_x: %a" pp_error (map_btree_err e))))))
;;

let del (Rw t : rw txn) tid key : unit Lwt.t =
  match t.backend with
  | Mem trees ->
    (* #178: remove from the shadow, not the live tree. *)
    (match t.mem_rw_shadow with
     | None ->
       let r = mem_tree trees tid in
       r := Bytes_map.remove key !r
     | Some shadow ->
       t.mem_rw_shadow <- Some (shadow_update shadow trees tid (Bytes_map.remove key)));
    Lwt.return_unit
  | Btree st ->
    let* r = bt_get_tree st tid in
    let* bt = unwrap_error r in
    Pager.set_write_tag st.pager (tree_tag st tid);
    (* #356: a delete may free or restructure the rightmost leaf; invalidate. *)
    Hashtbl.remove st.bt_append tid;
    let* d = Btree.del bt key in
    (match d with
     | Ok bt' ->
       Hashtbl.replace st.trees tid bt';
       Lwt.return_unit
     | Error e ->
       Lwt.fail_with (Format.asprintf "Store.del: %a" pp_error (map_btree_err e)))
;;

(* #174: register the page-header stamp (low 32 bits of the schema
   fingerprint) for [tid].  Subsequently-written Branch/Leaf pages of that tree
   carry the tag in their reserved header bytes.  No-op on the in-memory
   backend (no pages). *)
let set_tree_tag (t : t) (tid : tree_id) (tag : int32) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> Hashtbl.replace st.tree_tags tid tag
;;

(* ------------------------------------------------------------------ *)
(* Cursors                                                              *)
(* ------------------------------------------------------------------ *)

(* Drain a B+-tree cursor into an in-memory snapshot list.  Phase 1
   cursors are materialised; streaming cursors arrive later. *)
let drain_btree_cursor (c : Btree.cursor) : (bytes * bytes) list Lwt.t =
  let rec loop acc =
    let* r = Btree.cursor_next c in
    match r with
    | Error e ->
      Lwt.fail_with (Format.asprintf "Store.cursor: %a" pp_error (map_btree_err e))
    | Ok None -> Lwt.return (List.rev acc)
    | Ok (Some kv) -> loop (kv :: acc)
  in
  loop []
;;

let cursor_open : type a. a txn -> tree_id -> cursor Lwt.t =
  fun tx tid ->
  match tx with
  | Ro snap ->
    (match snap.rs_store.backend with
     | Mem _ ->
       (* #178: materialise from the snapshot captured at ro_begin.
          Without this, a concurrent writer's uncommitted modifications
          would leak into the cursor — and survive even if the writer
          later rolls back. *)
       let map =
         match snap.rs_mem_snap with
         | Some snap -> mem_tree_snap snap tid
         | None -> Bytes_map.empty
       in
       let entries = Bytes_map.bindings map in
       Lwt.return { all = entries; remaining = []; ready = false }
     | Btree st ->
       let* r = bt_get_tree_ro snap st tid in
       let* bt = unwrap_error r in
       let* co = Btree.cursor_open bt in
       (match co with
        | Error e ->
          Lwt.fail_with
            (Format.asprintf "Store.cursor_open(ro): %a" pp_error (map_btree_err e))
        | Ok c ->
          let* entries = drain_btree_cursor c in
          Btree.cursor_close c;
          Lwt.return { all = entries; remaining = []; ready = false }))
  | Rw _ ->
    let t = txn_store tx in
    (match t.backend with
     | Mem trees ->
       (* #178: cursor materialises from the active RW shadow. *)
       let map =
         match t.mem_rw_shadow with
         | None -> !(mem_tree trees tid)
         | Some shadow -> shadow_get shadow trees tid
       in
       let entries = Bytes_map.bindings map in
       Lwt.return { all = entries; remaining = []; ready = false }
     | Btree st ->
       let* r = bt_get_tree st tid in
       let* bt = unwrap_error r in
       let* co = Btree.cursor_open bt in
       (match co with
        | Error e ->
          Lwt.fail_with
            (Format.asprintf "Store.cursor_open: %a" pp_error (map_btree_err e))
        | Ok c ->
          let* entries = drain_btree_cursor c in
          Btree.cursor_close c;
          Lwt.return { all = entries; remaining = []; ready = false }))
;;

let cursor_close _ = ()

let cursor_first c =
  c.remaining <- c.all;
  match c.all with
  | [] ->
    c.ready <- false;
    Not_found `End
  | (k, _) :: _ ->
    c.ready <- true;
    Found k
;;

let cursor_seek c key =
  let rec find = function
    | [] ->
      c.remaining <- [];
      c.ready <- false;
      Not_found `End
    | (k, _) :: _ as cur ->
      let cmp = Bytes.compare k key in
      if cmp >= 0
      then (
        c.remaining <- cur;
        c.ready <- true;
        if cmp = 0 then Found k else Not_found (`Greater k))
      else find (List.tl cur)
  in
  find c.all
;;

let cursor_next c =
  match c.remaining with
  | [] -> None
  | entry :: rest ->
    if c.ready
    then (
      c.ready <- false;
      Some entry)
    else (
      c.remaining <- rest;
      match rest with
      | [] -> None
      | next :: _ -> Some next)
;;

let cursor_value c =
  match c.remaining with
  | (_, v) :: _ when c.ready -> Some v
  | _ -> None
;;

(* ------------------------------------------------------------------ *)
(* Native streaming seek (#228, #229)                                   *)
(*                                                                       *)
(* The materialised [cursor] above drains the WHOLE tree at open time so *)
(* it can offer a synchronous [cursor_seek]/[cursor_next] API.  For      *)
(* point/prefix probes (index lookups, UNIQUE pre-checks, FK checks)     *)
(* that O(n) drain dominates — it turns an O(log n) seek into a full     *)
(* table scan, and an n-row bulk insert into O(n^2).  [seek_ge] instead  *)
(* descends the B+-tree natively in O(log n) and streams matches lazily, *)
(* never materialising more than the entries the caller actually reads.  *)
(* Semantics match [cursor_open]+[cursor_seek]+[cursor_next]: the first  *)
(* [seek_next] returns the first entry with key >= [key], then ascending.*)
(* ------------------------------------------------------------------ *)
type seek_impl =
  | SC_mem of (bytes * bytes) Seq.t ref
  | SC_bt of Btree.cursor

(* #235: when the backing promise is already determined — the [Mem] backend, or *)
(* a B+-tree page already resident in the pager cache — [Lwt.bind] runs the     *)
(* caller's continuation synchronously, so a recursive [seek_next] consumer     *)
(* (the [gather]/[scan] loops in exec.ml) nests one OCaml frame per call         *)
(* instead of returning to a trampoline.  Streaming a pathologically common      *)
(* term/value (millions of cache-resident postings) would then overflow the      *)
(* stack.  To bound stack growth regardless of consumer shape, [seek_next]       *)
(* splices an [Lwt.pause] every [seek_pause_interval] calls: that defers the     *)
(* continuation to the scheduler, unwinding the stack.  The cost is one          *)
(* cooperative yield per N calls — negligible. *)
type seek_cursor =
  { mutable sc_calls : int
  ; sc_impl : seek_impl
  }

(* Bounds the synchronous recursion depth to <= this many frames; the yield then
   amortises to one [Lwt.pause] per that many reads.  256 trades a tiny, fixed
   per-scan overhead for a shallow stack ceiling. *)
let seek_pause_interval = 256
let mk_seek_cursor sc_impl = { sc_calls = 0; sc_impl }

let seek_ge : type a. a txn -> tree_id -> bytes -> seek_cursor Lwt.t =
  fun tx tid key ->
  match tx with
  | Ro snap ->
    (match snap.rs_store.backend with
     | Mem _ ->
       let map =
         match snap.rs_mem_snap with
         | Some snap -> mem_tree_snap snap tid
         | None -> Bytes_map.empty
       in
       Lwt.return (mk_seek_cursor (SC_mem (ref (Bytes_map.to_seq_from key map))))
     | Btree st ->
       let* r = bt_get_tree_ro snap st tid in
       let* bt = unwrap_error r in
       let* co = Btree.cursor_open bt in
       (match co with
        | Error e ->
          Lwt.fail_with
            (Format.asprintf "Store.seek_ge(ro): %a" pp_error (map_btree_err e))
        | Ok c ->
          let* sr = Btree.cursor_seek c key in
          (match sr with
           | Error e ->
             Lwt.fail_with
               (Format.asprintf "Store.seek_ge(ro): %a" pp_error (map_btree_err e))
           | Ok _ -> Lwt.return (mk_seek_cursor (SC_bt c)))))
  | Rw _ ->
    let t = txn_store tx in
    (match t.backend with
     | Mem trees ->
       let map =
         match t.mem_rw_shadow with
         | None -> !(mem_tree trees tid)
         | Some shadow -> shadow_get shadow trees tid
       in
       Lwt.return (mk_seek_cursor (SC_mem (ref (Bytes_map.to_seq_from key map))))
     | Btree st ->
       let* r = bt_get_tree st tid in
       let* bt = unwrap_error r in
       let* co = Btree.cursor_open bt in
       (match co with
        | Error e ->
          Lwt.fail_with (Format.asprintf "Store.seek_ge: %a" pp_error (map_btree_err e))
        | Ok c ->
          let* sr = Btree.cursor_seek c key in
          (match sr with
           | Error e ->
             Lwt.fail_with
               (Format.asprintf "Store.seek_ge: %a" pp_error (map_btree_err e))
           | Ok _ -> Lwt.return (mk_seek_cursor (SC_bt c)))))
;;

(* Return the next (key, value) >= the seek key in ascending order, or [None]
   when exhausted.  The first call returns the positioned entry. *)
let seek_next : seek_cursor -> (bytes * bytes) option Lwt.t =
  fun sc ->
  let result =
    match sc.sc_impl with
    | SC_mem r ->
      (match !r () with
       | Seq.Nil -> Lwt.return_none
       | Seq.Cons (kv, rest) ->
         r := rest;
         Lwt.return_some kv)
    | SC_bt c ->
      let* r = Btree.cursor_next c in
      (match r with
       | Ok kv -> Lwt.return kv
       | Error e ->
         Lwt.fail_with (Format.asprintf "Store.seek_next: %a" pp_error (map_btree_err e)))
  in
  (* The cursor is advanced eagerly above; [result] already holds this call's
     entry (or [None]), so splicing a pause here only defers the *return*, never
     reordering or dropping a match (#235).  We count calls, not matches: every
     recursive consumer call nests a frame whether or not it yields a row, so the
     terminal [None] call is counted too.

     Yielding mid-stream is safe under the current concurrency model: a paused RO
     seek streams from an immutable snapshot, and a paused RW seek holds the
     single-writer lock — so no other fiber can mutate the tree under the cursor
     between pause and resume.  If that invariant is ever relaxed (concurrent
     writers), revisit this yield point. *)
  sc.sc_calls <- sc.sc_calls + 1;
  if sc.sc_calls mod seek_pause_interval = 0
  then Lwt.bind (Lwt.pause ()) (fun () -> result)
  else result
;;

let seek_close : seek_cursor -> unit =
  fun sc ->
  match sc.sc_impl with
  | SC_mem _ -> ()
  | SC_bt c -> Btree.cursor_close c
;;

let wal_mode t =
  match t.backend with
  | Mem _ -> false
  | Btree st -> st.wal <> None
;;

let freelist_size t =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> Freelist.size (Pager.freelist st.pager)
;;

let freelist_entries t =
  match t.backend with
  | Mem _ -> []
  | Btree st -> Freelist.to_list (Pager.freelist st.pager)
;;

let n_pages t =
  match t.backend with
  | Mem _ -> 0L
  | Btree st -> Pager.n_pages st.pager
;;

(* Enumerate all tree_ids known to the meta tree.  For VACUUM. *)
let list_tree_ids t : tree_id list Lwt.t =
  match t.backend with
  | Mem trees -> Lwt.return (Hashtbl.fold (fun tid _ acc -> tid :: acc) trees [])
  | Btree st ->
    let* r = Btree.cursor_open st.meta in
    (match r with
     | Error e ->
       Lwt.fail_with
         (Format.asprintf "Store.list_tree_ids: %a" pp_error (map_btree_err e))
     | Ok cur ->
       let rec loop acc =
         let* r = Btree.cursor_next cur in
         match r with
         | Error e ->
           Lwt.fail_with
             (Format.asprintf "Store.list_tree_ids: %a" pp_error (map_btree_err e))
         | Ok None -> Lwt.return (List.rev acc)
         | Ok (Some (k, _v)) ->
           let tid, _ = Varint.decode_int64 k 0 in
           loop (Int64.to_int tid :: acc)
       in
       let* result = loop [] in
       Btree.cursor_close cur;
       Lwt.return result)
;;

type page_sink = page_id:int64 -> page:Cstruct.t -> unit Lwt.t

(* Shared page-image iteration for [copy_to]/[rekey_to].  Under an RO snapshot,
   yields each PLAINTEXT page (page_id, buf) for page_id in [0, n), resolving the
   WAL overlay (bounded to the committed_frames horizon captured at ro_begin)
   before the main DB.  The iteration is bounded by the snapshot-time page count
   so growth during the copy does not pull pages outside the snapshot.

   The buffer handed to [f] may be owned by the pager cache — a caller that
   mutates it MUST copy first. *)
let iter_snapshot_pages st (Ro snap) ~(f : page_id:int64 -> page:Cstruct.t -> unit Lwt.t)
  : unit Lwt.t
  =
  (* Read the page count INSIDE the snapshot (after ro_begin) so that the loop
     bound n is consistent with the snapshot's WAL horizon.  If a writer commits
     between ro_begin and reading n_pages, the snapshot's WAL horizon already
     includes those new pages; reading n_pages after ro_begin ensures we copy
     them too. *)
  let n = Pager.n_pages st.pager in
  let horizon = snap.rs_snap_frames in
  let rec loop (page_id : int64) =
    if Int64.compare page_id n >= 0
    then Lwt.return_unit
    else
      let* page_buf =
        match st.wal with
        | None ->
          (* Non-WAL path: pass ~snapshot_frames:0 so the read path skips the
             dirty set entirely (pager.ml:274-276), avoiding any
             concurrent-writer uncommitted data.  With no WAL the
             resolve_wal_page returns Ok None, falling through to
             load_main_page which reads the committed on-disk state.  Also pin
             the page so the writer's eviction pressure doesn't drop our copy. *)
          let* r =
            Pager.read ~snapshot_frames:0 ~pin_set:snap.rs_pinned st.pager page_id
          in
          (match r with
           | Ok buf -> Lwt.return buf
           | Error e ->
             Lwt.fail_with
               (Format.asprintf
                  "Store.iter_snapshot_pages(pg=%Ld): %a"
                  page_id
                  Pager.pp_error
                  e))
        | Some wal ->
          (* WAL-mode path: snapshot overlay (WAL first, then main DB). *)
          (match Wal.find_page_at wal page_id ~max_frame:horizon with
           | Some idx ->
             let* r = Wal.read_frame wal idx in
             (match r with
              | Ok buf -> Lwt.return buf
              | Error e ->
                Lwt.fail_with
                  (Format.asprintf
                     "Store.iter_snapshot_pages(pg=%Ld,frame=%d): %a"
                     page_id
                     idx
                     Wal.pp_error
                     e))
           | None ->
             let* r =
               Pager.read
                 ~snapshot_frames:horizon
                 ~pin_set:snap.rs_pinned
                 st.pager
                 page_id
             in
             (match r with
              | Ok buf -> Lwt.return buf
              | Error e ->
                Lwt.fail_with
                  (Format.asprintf
                     "Store.iter_snapshot_pages(pg=%Ld): %a"
                     page_id
                     Pager.pp_error
                     e)))
      in
      let* () = f ~page_id ~page:page_buf in
      loop (Int64.add page_id 1L)
  in
  loop 0L
;;

(* One-shot consistent full copy via an RO snapshot + page sink (#93).

   When the source is encrypted (#84), data pages (>= 2) are re-encrypted under
   the source's own key before reaching the sink, so the destination is a
   faithful, self-contained encrypted DB (open it with the same key) and no
   user-data plaintext transits the sink.  Pages 0 and 1 are plaintext headers
   (carrying the enc marker + canary) and are copied verbatim.

   On the Mem backend this is a no-op (there are no pages to copy). *)
let copy_to (t : t) (sink : page_sink) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    with_ro t (fun ro ->
      iter_snapshot_pages st ro ~f:(fun ~page_id ~page ->
        match st.cipher with
        | Some c when Int64.compare page_id 2L >= 0 ->
          (* Copy first — [page] may be the pager's cached buffer. *)
          let tmp = Cstruct.create (Cstruct.length page) in
          Cstruct.blit page 0 tmp 0 (Cstruct.length page);
          Crypto.encrypt_page c ~page_id tmp;
          sink ~page_id ~page:tmp
        | _ -> sink ~page_id ~page))
;;

(* #215: offline key rotation.  [t] must have been opened WITH THE OLD KEY so
   reads decrypt to plaintext; every data page (>= 2) is re-encrypted under a
   fresh cipher built from [new_key], and each header page (0, 1) has its canary
   rewritten under the new key (txn parity and all other fields preserved) and
   its CRC resealed.  The sunk page image is a self-contained encrypted DB under
   [new_key] with no WAL.  Rejects a plaintext source ([Not_encrypted]) and a
   wrong-length key ([Block_error]). *)
let rekey_to (t : t) ~(new_key : string) (sink : page_sink) : (unit, error) result Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_ok ()
  | Btree st ->
    (match st.cipher with
     | None -> Lwt.return_error Not_encrypted
     | Some _old ->
       (match Crypto.create ~key:new_key with
        | Error `Bad_key_length ->
          Lwt.return_error (Block_error "encryption key must be 32 bytes")
        | Ok c' ->
          let nonce = Mirage_crypto_rng.generate Crypto.nonce_len in
          let canary_tag = Crypto.make_canary c' ~nonce in
          let* () =
            with_ro t (fun ro ->
              iter_snapshot_pages st ro ~f:(fun ~page_id ~page ->
                let len = Cstruct.length page in
                let tmp = Cstruct.create len in
                Cstruct.blit page 0 tmp 0 len;
                if Int64.compare page_id 2L < 0
                then (
                  (* header page: rewrite the canary under the new key, leaving
                     enc_magic + every structural field intact, then reseal CRC. *)
                  let f = Page.read_header_fields tmp in
                  Page.write_header_fields
                    tmp
                    { f with Page.canary_nonce = nonce; canary_tag };
                  Page.seal tmp;
                  sink ~page_id ~page:tmp)
                else (
                  Crypto.encrypt_page c' ~page_id tmp;
                  sink ~page_id ~page:tmp)))
          in
          Lwt.return_ok ()))
;;

(* ------------------------------------------------------------------ *)
(* Replication consumer integration (#92)                                *)
(* ------------------------------------------------------------------ *)

(** Register the replication consumer's shipped position so checkpoint
    truncation waits for frames to be shipped before recycling them. *)
let update_replication_position (t : t) ~shipped =
  match t.backend with
  | Mem _ -> ()
  | Btree st ->
    st.replication_shipped_frames <- shipped;
    Lwt_condition.broadcast st.reader_done_cond ()
;;

(** Bounded-yield "timeout" for the checkpoint gate's wait on the
    replication floor (#207).  Returns [max_int] (unbounded) by default.
    [0] on the in-memory backend (no checkpoint gating). *)
let replication_gate_max_yields (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> st.replication_gate_max_yields
;;

(** Set the bounded-yield budget the checkpoint gate will spend waiting for
    the replication floor (a standby's acked position) to reach the
    checkpoint target before proceeding anyway.  See {!update_replication_position}.

    Pure-Mirage has no ambient clock, so this "timeout" is a count of
    cooperative [Lwt.pause] yields rather than wall-clock time.  [max_int]
    (the default) means wait indefinitely — a dead standby wedges the WAL,
    matching the behavior before this knob existed.  A finite value bounds
    the wait: once spent, the checkpoint proceeds and the now-stranded
    standby must re-base (#208).  Negative inputs clamp to [0] (proceed
    immediately if the floor is behind).

    Local RO readers are never abandoned by this budget — only the
    replication floor.  No-op on the in-memory backend. *)
let set_replication_gate_max_yields (t : t) (n : int) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.replication_gate_max_yields <- max 0 n
;;

(** Get (epoch, committed_frames) for the active WAL; [None] if no WAL. *)
let replication_state (t : t) =
  match t.backend with
  | Mem _ -> None
  | Btree st ->
    (match st.wal with
     | None -> None
     | Some wal -> Some (Wal.epoch wal, Wal.committed_frames wal))
;;

(* ------------------------------------------------------------------ *)
(* Incremental backup (#265)                                            *)
(* ------------------------------------------------------------------ *)

(** Register the backup consumer's captured position so checkpoint
    truncation waits for frames to be backed up before recycling them.
    Analogous to {!update_replication_position} but for the incremental
    backup watermark. *)
let update_backup_position (t : t) ~shipped =
  match t.backend with
  | Mem _ -> ()
  | Btree st ->
    st.backup_shipped_frames <- shipped;
    Lwt_condition.broadcast st.reader_done_cond ()
;;

(** Get (epoch, committed_frames) for the active WAL; [None] if no WAL.
    Review #8: delegates to {!replication_state} — the two functions share
    the same body because both track the same WAL position. *)
let backup_state (t : t) = replication_state t

(** Return the backup floor's bounded-yield budget for the checkpoint
    gate.  Defaults to [max_int] (unbounded) on the B+-tree backend,
    [0] on the in-memory backend (no checkpoint gating). *)
let backup_gate_max_yields (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> st.backup_gate_max_yields
;;

(** Set the bounded-yield budget the checkpoint gate will spend waiting
    for the backup floor to reach the checkpoint target before proceeding
    anyway (#265).  Same semantics as {!set_replication_gate_max_yields}.
    Negative inputs clamp to [0].  No-op on the in-memory backend. *)
let set_backup_gate_max_yields (t : t) (n : int) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.backup_gate_max_yields <- max 0 n
;;

(** A captured WAL frame for incremental backup (#265).  Contains the
    full frame metadata and page payload needed to reconstruct the
    database at a later point.

    The {!checksum} field covers the decrypted page payload (transport
    integrity for the backup frame), matching the same scheme used by
    {!Sqlocaml_replication.replicated_frame}.  For unencrypted WALs the
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

(* NOTE (review #8): backup_frame and Sqlocaml_replication.replicated_frame
   are structurally identical.  A future consolidation could merge them
   into a shared frame type, but the two modules have no common dependency
   today and the duplication is small enough to live with. *)

(** Capture the committed WAL frames since a given watermark position,
    returning them as a list of {!backup_frame}.

    [~since_epoch] and [~since_idx] identify the watermark: frames with
    indices strictly greater than [since_idx] in the current epoch are
    returned.  If the WAL's epoch has advanced past [since_epoch], no
    frames can be captured (the caller must take a fresh base snapshot).

    Returns [None] when the WAL's epoch has changed (meaning the caller's
    watermark is stale and a re-base is needed).  Returns [Some []] when
    the watermark is current but no new frames have been committed. *)
let capture_frames_since (t : t) ~since_epoch ~since_idx
  : (backup_frame list, [> `Capture_error of string ]) result option Lwt.t
  =
  match t.backend with
  | Mem _ -> Some (Ok []) |> Lwt.return
  | Btree st ->
    (match st.wal with
     | None -> Lwt.return (Some (Error (`Capture_error "no WAL active")))
     | Some wal ->
       let current_epoch = Wal.epoch wal in
       if not (Int64.equal current_epoch since_epoch)
       then
         (* Epoch changed: the watermark is stale and the caller must
            re-base (take a fresh full snapshot). *)
         Lwt.return None
       else (
         let committed = Wal.committed_frames wal in
         (* max_int = "no floor" sentinel; increment would overflow to min_int *)
         let start = if since_idx = max_int then max_int else since_idx + 1 in
         if start >= committed
         then Lwt.return (Some (Ok []))
         else (
           let salt = Wal.salt wal in
           let seed = Wal.seed wal in
           let rec loop idx acc =
             if idx >= committed
             then Lwt.return (Some (Ok (List.rev acc)))
             else (
               (* A concurrent checkpoint can bump the epoch while we yield
                 on I/O.  If the epoch changed, the watermark is stale —
                 signal through [None] so the caller re-bases cleanly
                 instead of getting an I/O error. *)
               let current_epoch = Wal.epoch wal in
               if not (Int64.equal current_epoch since_epoch)
               then Lwt.return None
               else
                 let* r = Wal.read_committed_frame wal idx in
                 match r with
                 | Error _ when not (Int64.equal (Wal.epoch wal) since_epoch) ->
                   Lwt.return None
                 | Error e ->
                   Lwt.return
                     (Some
                        (Error
                           (`Capture_error
                               (Format.asprintf "read frame %d: %a" idx Wal.pp_error e))))
                 | Ok f ->
                   if not (Int64.equal (Wal.epoch wal) since_epoch)
                   then Lwt.return None
                   else (
                     let flags = if f.is_commit then 1L else 0L in
                     let checksum =
                       Wal.frame_checksum
                         ~salt
                         ~seed
                         ~page_id:f.page_id
                         ~flags
                         ~page:f.page
                     in
                     let bf : backup_frame =
                       { epoch = current_epoch
                       ; frame_idx = idx
                       ; page_id = f.page_id
                       ; is_commit = f.is_commit
                       ; page = f.page
                       ; checksum
                       ; source_salt = salt
                       ; source_seed = seed
                       }
                     in
                     loop (idx + 1) (bf :: acc)))
           in
           (* loop already returns the exact type of this branch —
             None for epoch-changed, Some (Ok frames) for success,
             Some (Error _) for I/O failure.  Direct return. *)
           loop start [])))
;;

(** Install an asynchronous callback invoked after each WAL commit batch.
    The callback receives ~epoch, ~base_idx (starting WAL frame index),
    and ~count (number of committed frames).  Fired via [Lwt.async] so
    the commit path is never blocked by replication I/O.

    When a callback is registered, the replication shipped-position
    floor is initialised to the WAL's current [committed_frames] so
    that checkpoint cannot recycle already-acknowledged frames before
    the async sink ships its first batch.  The consumer must still
    call {!update_replication_position} to advance the floor as
    frames are shipped.

    Pass [None] to unregister (resets the floor to [max_int]). *)
let set_commit_callback
      (t : t)
      (cb : (epoch:int64 -> base_idx:int -> count:int -> unit Lwt.t) option)
  =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    (match cb with
     | None ->
       st.on_committed_frames <- None;
       st.replication_shipped_frames <- max_int;
       Lwt.return_unit
     | Some _ ->
       (* #336/1 + review #1: do the flush AND the pin atomically under the
          write lock.  [flush_unsynced] yields (group-commit drain); without the
          lock a concurrent commit could interleave in the OLD mode (no fsync,
          no ship — cb not yet set), and the subsequent
          [sink_shipped_frames <- committed_frames] pin would then bury those
          frames below the ship cursor forever (silent standby divergence). The
          lock blocks new commits ([rw_begin]) for the brief flush+pin so the
          cursor pins exactly the pre-registration frontier. *)
       let* () = Rwlock.acquire_write t.lock in
       Lwt.finalize
         (fun () ->
            (* Flush while still in the old mode — [flush_unsynced] is a no-op
               once we pin [Full] below.  A store opened [off]/[batched] may have
               acked commits in the OS page cache; the sink ships only NEW
               frames, so these historical frames would otherwise linger
               crash-exposed until the next commit/checkpoint/close despite the
               sink implying synchronous=full. *)
            let* () = flush_unsynced t in
            st.on_committed_frames <- cb;
            (* #298: a replication commit-sink requires Full durability — the
               checkpoint replica-floor gate assumes every committed frame is
               shipped, which only holds when every commit fsyncs. Force Full on
               registration; relaxing durability is rejected while a sink is
               active (see set_durability / the PRAGMA handler). *)
            st.sync_mode <- `Full;
            (match st.wal with
             | None -> ()
             | Some wal ->
               st.replication_shipped_frames <- Wal.committed_frames wal;
               (* #298/#1: a sink registered mid-life ships only NEW synced
                  frames, not history — start the ship cursor at the current
                  count. *)
               st.sink_shipped_frames <- Wal.committed_frames wal);
            Lwt.return_unit)
         (fun () ->
            Rwlock.release_write t.lock;
            Lwt.return_unit))
;;

(* ------------------------------------------------------------------ *)
(* Follower mode (#172)                                                 *)
(* ------------------------------------------------------------------ *)

(** Enable or disable follower mode on the store.  When [true],
    [rw_begin] rejects write transactions so the standby's WAL does not
    diverge from the master's stream.  No-op on the in-memory backend. *)
let set_follower (t : t) (on : bool) =
  match t.backend with
  | Mem _ -> ()
  | Btree st ->
    st.follower <- on;
    if not on then st.follower_ack_position <- None
;;

(** True iff follower mode is active (writes are rejected). *)
let is_follower (t : t) =
  match t.backend with
  | Mem _ -> false
  | Btree st -> st.follower
;;

(** Record the current [Wal.committed_frames] as the follower's last-applied
    commit boundary.  [ro_begin] will cap RO snapshots to this position so
    readers never observe WAL frames past what has been applied on this node
    (#263).  The value is captured from the store's own WAL so it lives in
    local committed-frame count space — no coordinate mismatch vs. master
    epoch indices.  No-op on the in-memory backend. *)
let set_follower_ack_position (t : t) =
  match t.backend with
  | Mem _ -> ()
  | Btree st ->
    let n =
      match st.wal with
      | None -> 0
      | Some w -> Wal.committed_frames w
    in
    st.follower_ack_position <- Some n
;;

(** Get the recorded follower ack position (a local [Wal.committed_frames]
    count), or [None] if not following or no position has been recorded yet. *)
let follower_ack_position (t : t) =
  match t.backend with
  | Mem _ -> None
  | Btree st -> st.follower_ack_position
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
