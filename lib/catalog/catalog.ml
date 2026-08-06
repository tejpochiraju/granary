module S = Granary_store.Store
module Row = Granary_encoding.Row
module Varint = Granary_encoding.Varint
module Schema_fingerprint = Granary_encoding.Schema_fingerprint
module Rowid = Granary_encoding.Rowid

type fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default

(* System tree IDs *)
let sys_tables_tid : S.tree_id = 0
let sys_columns_tid : S.tree_id = 1
let sys_indexes_tid : S.tree_id = 2
let sys_meta_tid : S.tree_id = 3
let sys_fts_tid : S.tree_id = 4
let sys_views_tid : S.tree_id = 5
let sys_triggers_tid : S.tree_id = 6

(* #427: reactive-view definitions.  Keyed by view name; value is the full
   [CREATE REACTIVE VIEW …] SQL text, re-parsed on open (as views/triggers). *)
let sys_reactive_views_tid : S.tree_id = 8

(* #174: redundant catalog mirror.  A second, self-describing copy of every
   table's schema keyed by tree_id, so a single damaged primary-catalog page
   does not lose the schema for every table.  Also the canonical
   reference-fingerprint store used for drift detection on open. *)
let sys_mirror_tid : S.tree_id = 7

(* Rowid counter key suffix for FTS tables: name ++ "\x00rowid" *)
let sys_fts_rowid_suffix = Bytes.of_string "\x00rowid"
let next_user_tid_key = Bytes.of_string "next_user_tid"
let next_user_tid_init = 16

(* Counter for monotonically-increasing index IDs, stored in sys_meta. *)
let next_index_id_key = Bytes.of_string "next_index_id"
let user_version_key = Bytes.of_string "\x00user_version"

(** Read user_version from an already-open RW or RO transaction.
    Returns 0 if never set. *)
let read_user_version_tx tx : int64 Lwt.t =
  let%lwt v = S.get tx sys_meta_tid user_version_key in
  Lwt.return
    (match v with
     | None -> 0L
     | Some b -> Bytes.get_int64_be b 0)
;;

(** Write user_version inside an already-open RW transaction.
    Caller is responsible for commit. *)
let write_user_version_tx tx (v : int64) : unit Lwt.t =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 v;
  S.put tx sys_meta_tid user_version_key b
;;

type fk_constraint =
  { fk_local_cols : string list
  ; fk_parent_table : string
  ; fk_parent_cols : string list
  ; fk_on_delete : fk_action
  ; fk_on_update : fk_action
  ; fk_deferrable : bool (** false = IMMEDIATE (default), true = INITIALLY DEFERRED *)
  }

type pending_fk_kind =
  [ `Insert
  | `Update
  | `Delete
  ]

type pending_fk_recheck = { recheck : 'm. 'm S.txn -> bool Lwt.t }

type pending_fk_check =
  { pfk_kind : pending_fk_kind
  ; pfk_table : string
  ; pfk_rowid : int64
  ; pfk_message : string
  ; pfk_recheck : pending_fk_recheck
  }

type storage =
  | Row of
      { tree_id : S.tree_id
      ; next_rowid : int64
      ; without_rowid : bool
      ; autoincrement : bool
      }
  | Columnar of Granary_columnar.Col_store.t * S.tree_id

type table_meta =
  { name : string
  ; storage : storage
  ; columns : Row.column list
  ; fk_constraints : fk_constraint list
  }

(** #250: sentinel [next_rowid] for an alias table with no rowid seeded yet.  A
    fresh/empty INTEGER PRIMARY KEY table starts here so the first row — an
    explicit id (even <= 0) OR an auto NULL — seeds the counter from the real
    value (explicit: id+1; NULL: 1), matching SQLite ([max(existing)+1], empty
    -> 1) and [recover_next_rowid].  The live counter is always
    [max(rowid)+1 >= Int64.min_int + 1], so it can never collide with this
    sentinel (allocation guards the [max_int] overflow that would wrap to it). *)
let empty_next_rowid = Int64.min_int

let row_storage (m : table_meta) =
  match m.storage with
  | Row { tree_id; next_rowid; without_rowid; autoincrement } ->
    tree_id, next_rowid, without_rowid, autoincrement
  | Columnar _ -> failwith (Printf.sprintf "table '%s' is a columnar table" m.name)
;;

let is_columnar m =
  match m.storage with
  | Columnar _ -> true
  | Row _ -> false
;;

let tid_of_storage = function
  | Row { tree_id; _ } -> tree_id
  | Columnar (_, tid) -> tid
;;

type idx_origin =
  [ `Implicit_pk
  | `Implicit_unique
  | `User
  ]

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list (* col names for plain; expr SQL for expression indexes *)
  ; idx_unique : bool
  ; idx_tree_id : S.tree_id
  ; idx_expr_flags : bool list (* true = expression index column, false = plain column *)
  ; idx_where_sql : string option
  ; idx_origin : idx_origin
  }

type fts_table_meta =
  { fts_name : string
  ; fts_content_tree : S.tree_id
  ; fts_index_tree : S.tree_id
  ; fts_columns : string list
  }

(* #283: the in-memory schema cache and its rollback ledger, sealed behind a
   signature so the ONLY way to mutate the three catalog hashtables is through a
   mutator that registers its own reversal.  "Mutate the cache without recording
   how to undo it" is therefore unrepresentable outside this module.

   Two reversal strategies, both explicit (no raw, unprotected write exists):
   - undo-tracked mutators ([put_*]/[remove_*]) capture the prior binding and push
     the synthesized inverse onto [undo]; replayed by [rollback]/[savepoint_rollback],
     discarded by [commit].  Used for DDL run THROUGH [Exec.with_ddl_txn] (the undo is
     discarded on COMMIT in either Auto or explicit mode, replayed on ROLLBACK / a
     mid-statement failure — see exec.ml).
   - durable mutators ([*_durable]) apply with NO undo, for a catalog function's own
     autocommit path that self-commits its own writer txn (so the write is already
     durable and a [?txn=None] branch can never be inside an ambient writer txn — a
     nested rw_begin would deadlock).  Also used for ephemeral CTE sentinels.

   The rowid counter keeps the #293 recompute-on-rollback strategy: [bump_rowid]
   records the table in the dirty set instead of pushing a closure, and the db layer
   re-derives max(rowid)+1 from the rolled-back tree for exactly those tables. *)
module Schema_cache : sig
  type t

  (** [stamp] re-stamps the #174 tree-tag for a [table_meta]; wired to
      [register_tag store].  Every [table_meta] entering the cache is stamped so the
      page-stamp stays consistent automatically, and an undo re-stamps the prior.
      [rowid_counters] is the STORE's rowid allocator state (#589/#633) — it is
      mandatory, and there is exactly one right value for it: [S.rowid_counters]
      of the store this cache describes.  See the implementation note below for
      why it exists and why the key is a tree id rather than a table name. *)
  val create : rowid_counters:S.rowid_counters -> stamp:(table_meta -> unit) -> unit -> t

  (* reads — never touch the undo log *)
  val find_table : t -> string -> table_meta option
  val mem_table : t -> string -> bool
  val find_index : t -> string -> index_info option
  val mem_index : t -> string -> bool
  val find_fts : t -> string -> fts_table_meta option
  val fold_tables : (string -> table_meta -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_fts : (string -> fts_table_meta -> 'a -> 'a) -> t -> 'a -> 'a
  val indexes_for_table : t -> table:string -> index_info list
  val count_tables : t -> int
  val count_indexes : t -> int
  val count_fts : t -> int

  (* undo-tracked mutators (DDL under with_ddl_txn) *)
  val put_table : t -> name:string -> table_meta -> unit
  val remove_table : t -> name:string -> unit
  val put_index : t -> name:string -> index_info -> unit
  val remove_index : t -> name:string -> unit
  val put_fts : t -> name:string -> fts_table_meta -> unit

  (* durable mutators (catalog-internal autocommit / ephemeral — no undo) *)
  val put_table_durable : t -> name:string -> table_meta -> unit

  (** #589: open-time seeding ONLY.  Identical to [put_table_durable] except that
      it does not overwrite a shared rowid counter that is already live — a
      worker handle re-reads the catalog off disk, and disk is by definition no
      fresher than the counter the sharing handles are already using.  The rule
      is absolute — an existing entry is never overwritten, whatever its value;
      see [publish_if_absent] for why the [empty_next_rowid] exception that PR
      #650 briefly carried had to go. *)
  val seed_table : t -> name:string -> table_meta -> unit

  val remove_table_durable : t -> name:string -> unit
  val put_index_durable : t -> name:string -> index_info -> unit
  val put_fts_durable : t -> name:string -> fts_table_meta -> unit

  (* rowid counter: in-txn bump (dirty-set tracked) and post-rollback/autocommit
     durable set; [take_rowid_bumped] returns the dirty names and clears the set. *)
  val bump_rowid : t -> name:string -> table_meta -> unit
  val set_rowid_durable : t -> name:string -> table_meta -> unit
  val take_rowid_bumped : t -> string list

  (** Append an arbitrary reversal to the undo log.  The ONLY way an external
      owner (the db layer's view/trigger caches, #269) can enroll a rollback in
      this ledger so it replays in order with the catalog's own DDL undos at
      ROLLBACK / ROLLBACK TO SAVEPOINT.  Note this appends to the undo LOG only —
      it cannot reach the sealed cache hashtables, so the structural guarantee is
      preserved.  The closure MUST be idempotent (re-run as a no-op): #280's
      [savepoint_rollback] can leave an already-run closure queued for the outer
      ROLLBACK. *)
  val register_undo : t -> (unit -> unit) -> unit

  (* lifecycle — drive by the db layer at txn / savepoint boundaries *)
  val commit : t -> unit
  val rollback : t -> unit
  val savepoint_begin : t -> string -> unit
  val savepoint_rollback : t -> string -> unit
  val savepoint_release : t -> string -> unit
  val mark_poisoned : t -> unit
  val is_poisoned : t -> bool
end = struct
  (* #280/#293/#303: one frame per open SAVEPOINT.  [sp_undo] is the [undo] list
     as it stood when the savepoint opened (a physical suffix — see [push_undo]),
     so ROLLBACK TO can run+drop exactly the DDL undos registered since.
     [sp_poison] restores the #295 poison flag.  [sp_rowids] snapshots every
     table's cached [next_rowid] at SAVEPOINT so ROLLBACK TO can restore the
     in-memory counter (#303): the full-ROLLBACK recompute-from-tree path
     ([recompute_rowid_counters_after_rollback]) is unusable mid-transaction —
     the RW txn is still open, so a fresh RO snapshot reads the last-committed
     tree, not the savepoint state — so we snapshot/restore in memory instead.
     [sp_columnar] snapshots the encoded state (plus dirty flag) of every dirty
     columnar store at SAVEPOINT, for the same reason: Persist.save only runs at
     commit time, so the B-tree never holds in-txn columnar data. *)
  type savepoint =
    { sp_name : string
    ; sp_undo : (unit -> unit) list
    ; sp_poison : bool
    ; sp_rowids : (string * int64) list
    ; sp_columnar : (string * bytes * bool) list
    }

  (* #589: the live rowid allocator state, lifted OUT of [tables] and into a
     table that two caches over one [Store.t] can share.

     [Db.create_worker_handle] is [of_store] over the same store, and [of_store]
     builds a fresh catalog.  Each catalog used to own its table's [next_rowid]
     outright, inside its cached [table_meta].  Two handles then held two
     counters over one data tree and neither invalidated the other, so an
     engine-assigned rowid was handed out twice and the second write silently
     overwrote the first: one row where there should be two, and for a
     [TEXT PRIMARY KEY] table an index entry left pointing at the wrong row.
     Symmetric (the parent went stale in exactly the same way once the worker
     wrote), unbounded, and durable.

     So [tables] no longer holds the counter's truth — this table does.  Every
     read of a cached [table_meta] is patched from here on the way out, and every
     write publishes here on the way in, which keeps [table_meta] the one type
     the rest of the engine has to know about.

     Keyed by TREE ID, not by table name: a tree id is the identity of the data
     tree the counter counts for, and it survives [ALTER TABLE ... RENAME].
     Keying by name would let a DROP+CREATE inherit the dead table's counter.

     BUT A TREE ID IS NOT UNIQUE FOR ALL TIME, and the difference matters:

     - Never reused after a COMMITTED DROP — [next_user_tid] only moves forward
       and nothing hands the dropped id back.  (Measured: drop tid 16, next
       CREATE gets 18.)
     - REUSED after a ROLLED-BACK CREATE.  [next_user_tid_tx] writes the bumped
       counter INSIDE the transaction, so [S.rollback] reverts it and the next
       CREATE TABLE gets the same id the doomed table had.  (Measured: doomed
       tid 17, rolled back, next CREATE also gets 17.)

     So the invariant that keeps a recreated table from inheriting a dead
     counter is NOT "tree ids are unique".  It is that the entry is cleared or
     overwritten before the reused id is allocated from, and TWO REDUNDANT
     MECHANISMS do that — verified by mutation, each one alone is sufficient:

     1. [put_table]'s undo runs [del_meta] -> [unpublish], dropping the doomed
        table's entry when the CREATE rolls back.
     2. The replacement CREATE goes through [put_table] -> [set_meta] ->
        [publish], and a fresh table's meta carries [empty_next_rowid], so it
        OVERWRITES whatever sat under that tree id.

     Removing either one alone still passes; removing BOTH loses the row.  That
     is exactly what [test_tid_reuse_after_rolled_back_create] and
     [test_tid_reuse_worker] pin — they are a guard on the pair, not on either
     mechanism, so do not read a green suite as proof that the one you are
     editing is unused.  Keep DDL on the [set_meta]/[del_meta] chokepoints and
     both stay true for free.

     Negative tree ids are skipped entirely: they are the ephemeral/sentinel
     metas, which name no data tree, allocate no rowid, and would otherwise all
     collide on one entry.  There are four — [-1] for a CTE and for a decoded
     columnar table whose stored tid is 0, [-2] for [sqlite_master], [-3] for
     [sqlite_sequence] (see planner.ml's sentinel block and
     [decode_table_storage]) — and the [tree_id >= 0] guard covers all of
     them.

     Sharing is safe under the store's single-writer lock, which is what makes
     the two handles serialize.  The direction of staleness that costs rows is a
     counter too LOW — that is the one that collides — and what keeps it from
     being observed is that every allocator both ALLOCATES AND PUBLISHES with the
     writer lock held: [next_rowid_in_txn] and [bump_next_rowid_in_txn] by
     construction (they are handed a txn), [next_rowid] since #632.

     Read that as the precise claim it is.  It is NOT "the allocator holds the
     lock", which would be satisfied by publishing in a continuation of
     [S.commit] — and would be false, because [S.commit] releases the lock before
     its promise resolves (store.ml:2071 [unlock_once ()] ahead of the fsync
     await; store.ml:2184 for the non-WAL arm).  The publish has to happen at a
     point where the lock is still held, which for [next_rowid] means before
     [S.commit] is called at all.  Anything that moves a publish past a commit
     re-opens #589 by way of #632.

     A ROLLBACK lowers the counter again through [set_rowid_durable] (#293's
     recompute) and [restore_rowids] (#303's savepoint restore), by which point
     no other handle can hold the lock either.

     #633: the table itself is owned by [S.t] ([S.rowid_counters]), not by this
     cache and not by a caller.  It used to be threaded through
     [Cat.open_ ?rowid_counters] / [Db.of_store ?rowid_counters], which meant
     every future caller reaching [of_store] over an ALREADY-OPEN store owed it
     the argument by hand — and the penalty for forgetting was #589 verbatim
     (silent row loss plus index corruption, durable and surviving reopen).  A
     tree id is only an identity within one store, so the store is where the
     table belongs; sharing is now a consequence of naming the same store rather
     than of remembering an argument.  It also makes ATTACH right by type: an
     attached schema is a different [S.t] and therefore, necessarily, a
     different set of counters. *)
  type t =
    { tables : (string, table_meta) Hashtbl.t
    ; indexes : (string, index_info) Hashtbl.t
    ; indexes_by_table : (string, index_info list) Hashtbl.t
    ; fts : (string, fts_table_meta) Hashtbl.t
    ; stamp : table_meta -> unit
    ; mutable undo : (unit -> unit) list
    ; mutable savepoints : savepoint list
    ; mutable poisoned : bool
    ; rowid_bumped : (string, unit) Hashtbl.t
    ; counters : S.rowid_counters
    }

  let create ~rowid_counters ~stamp () =
    { tables = Hashtbl.create 16
    ; indexes = Hashtbl.create 16
    ; indexes_by_table = Hashtbl.create 16
    ; fts = Hashtbl.create 8
    ; stamp
    ; undo = []
    ; savepoints = []
    ; poisoned = false
    ; rowid_bumped = Hashtbl.create 8
    ; counters = rowid_counters
    }
  ;;

  (* Patch a cached [table_meta] with the shared counter on the way out. *)
  let patch t (m : table_meta) =
    match m.storage with
    | Row ({ tree_id; next_rowid; _ } as r) when tree_id >= 0 ->
      (match Hashtbl.find_opt t.counters tree_id with
       | Some n when not (Int64.equal n next_rowid) ->
         { m with storage = Row { r with next_rowid = n } }
       | _ -> m)
    | Row _ | Columnar _ -> m
  ;;

  (* Publish a [table_meta]'s counter to the shared table on the way in. *)
  let publish t (m : table_meta) =
    match m.storage with
    | Row { tree_id; next_rowid; _ } when tree_id >= 0 ->
      Hashtbl.replace t.counters tree_id next_rowid
    | Row _ | Columnar _ -> ()
  ;;

  (* Open-time seeding: never overwrite a counter another cache is already
     using — disk is no fresher than the live allocator.

     THE RULE IS ABSOLUTE: an entry that exists is never overwritten, whatever
     its value.  PR #650 briefly carved out an exception for [empty_next_rowid]
     ("a sentinel has never allocated, so seeding over it can only move the
     counter up"), and it was wrong twice over.  The sentinel is ALSO written on
     purpose: the sqlite_sequence reset paths ([reset_next_rowid_in_txn] for
     [DELETE FROM sqlite_sequence WHERE name = 't'], [reset_all_next_rowid_in_txn]
     for the bare DELETE) set it INSIDE an open transaction, so an [open_] landing
     on the same store mid-transaction would read the committed pre-reset
     high-water off disk and clobber the reset — AUTOINCREMENT then resumes from
     the old value once the reset commits.  And the obvious guard against that
     (skip tables marked dirty in [rowid_bumped]) does not work, because
     [rowid_bumped] is PER-CACHE: the resetting transaction's dirty flag lives on
     its own cache, and the cache doing the seeding is a brand-new one whose set
     is empty.  There is no cheap store-wide discriminator, so there is no
     exception.

     The exception existed only to let a test simulate a process restart by
     re-opening a catalog over a still-live store.  Since #633 that is not a
     restart — it is a worker handle, and sharing is the correct answer.  A test
     that wants a genuine restart uses a file-backed store and closes it (see
     [test_mirror_recovers_next_rowid] in test_catalog.ml).

     KNOWN RESIDUAL, accepted deliberately (PR #650 review r2).  Dropping the
     exception means mirror recovery is invisible to a SECOND catalog opened over
     a LIVE store when the shared counter is still at the sentinel: the recovered
     [max(rowid) + 1] loses to the sentinel already published by the original
     CREATE, and the second catalog allocates from 1.  Reaching it needs the data
     tree to hold rows the allocator never issued AND the table's [_sys_tables]
     row to be lost, on a store that is still open — i.e. corruption on a live
     store, not a restart, because ordinary inserts advance the counter through
     [bump_rowid]/[bump_next_rowid_in_txn] and a restart drops the whole table
     with its [S.t].  The alternative was an exception that silently reverses a
     sqlite_sequence reset, which is reachable without corruption; this is the
     better trade, but it is a trade. *)
  let publish_if_absent t (m : table_meta) =
    match m.storage with
    | Row { tree_id; next_rowid; _ } when tree_id >= 0 ->
      if not (Hashtbl.mem t.counters tree_id)
      then Hashtbl.replace t.counters tree_id next_rowid
    | Row _ | Columnar _ -> ()
  ;;

  let unpublish t name =
    match Hashtbl.find_opt t.tables name with
    | Some { storage = Row { tree_id; _ }; _ } when tree_id >= 0 ->
      Hashtbl.remove t.counters tree_id
    | _ -> ()
  ;;

  (* THE two chokepoints: no other code in this module may touch [t.tables]. *)
  let set_meta t name m =
    Hashtbl.replace t.tables name m;
    publish t m
  ;;

  let del_meta t name =
    unpublish t name;
    Hashtbl.remove t.tables name
  ;;

  (* The undo log only ever grows by prepending, so a saved suffix stays
     physically identical (==) — the invariant [savepoint_rollback] relies on. *)
  let push_undo t f = t.undo <- f :: t.undo
  let register_undo = push_undo
  let find_table t name = Option.map (patch t) (Hashtbl.find_opt t.tables name)
  let mem_table t name = Hashtbl.mem t.tables name
  let find_index t name = Hashtbl.find_opt t.indexes name
  let mem_index t name = Hashtbl.mem t.indexes name
  let find_fts t name = Hashtbl.find_opt t.fts name
  let fold_tables f t acc = Hashtbl.fold (fun k m acc -> f k (patch t m) acc) t.tables acc
  let fold_fts f t acc = Hashtbl.fold f t.fts acc

  let indexes_for_table t ~table =
    Option.value (Hashtbl.find_opt t.indexes_by_table table) ~default:[]
  ;;

  let count_tables t = Hashtbl.length t.tables
  let count_indexes t = Hashtbl.length t.indexes
  let count_fts t = Hashtbl.length t.fts

  let put_table t ~name meta =
    let prior = Option.map (patch t) (Hashtbl.find_opt t.tables name) in
    set_meta t name meta;
    t.stamp meta;
    push_undo t (fun () ->
      match prior with
      | Some m ->
        set_meta t name m;
        t.stamp m
      | None -> del_meta t name)
  ;;

  let remove_table t ~name =
    let prior = Option.map (patch t) (Hashtbl.find_opt t.tables name) in
    del_meta t name;
    push_undo t (fun () ->
      match prior with
      | Some m ->
        set_meta t name m;
        t.stamp m
      | None -> ())
  ;;

  let by_table_add t idx_table info =
    let lst = Option.value (Hashtbl.find_opt t.indexes_by_table idx_table) ~default:[] in
    Hashtbl.replace t.indexes_by_table idx_table (info :: lst)
  ;;

  let by_table_remove t idx_table idx_name =
    match Hashtbl.find_opt t.indexes_by_table idx_table with
    | None -> ()
    | Some lst ->
      (match List.filter (fun i -> not (String.equal i.idx_name idx_name)) lst with
       | [] -> Hashtbl.remove t.indexes_by_table idx_table
       | lst' -> Hashtbl.replace t.indexes_by_table idx_table lst')
  ;;

  let put_index t ~name info =
    let prior = Hashtbl.find_opt t.indexes name in
    (match prior with
     | Some p -> by_table_remove t p.idx_table name
     | None -> ());
    Hashtbl.replace t.indexes name info;
    by_table_add t info.idx_table info;
    push_undo t (fun () ->
      by_table_remove t info.idx_table name;
      match prior with
      | Some i ->
        Hashtbl.replace t.indexes name i;
        by_table_add t i.idx_table i
      | None -> Hashtbl.remove t.indexes name)
  ;;

  let remove_index t ~name =
    let prior = Hashtbl.find_opt t.indexes name in
    Hashtbl.remove t.indexes name;
    (match prior with
     | Some p -> by_table_remove t p.idx_table name
     | None -> ());
    push_undo t (fun () ->
      match prior with
      | Some i ->
        Hashtbl.replace t.indexes name i;
        by_table_add t i.idx_table i
      | None -> ())
  ;;

  let put_fts t ~name meta =
    let prior = Hashtbl.find_opt t.fts name in
    Hashtbl.replace t.fts name meta;
    push_undo t (fun () ->
      match prior with
      | Some m -> Hashtbl.replace t.fts name m
      | None -> Hashtbl.remove t.fts name)
  ;;

  let put_table_durable t ~name meta =
    set_meta t name meta;
    t.stamp meta
  ;;

  let seed_table t ~name meta =
    Hashtbl.replace t.tables name meta;
    publish_if_absent t meta;
    t.stamp meta
  ;;

  let remove_table_durable t ~name = del_meta t name

  let put_index_durable t ~name info =
    (match Hashtbl.find_opt t.indexes name with
     | Some p -> by_table_remove t p.idx_table name
     | None -> ());
    Hashtbl.replace t.indexes name info;
    by_table_add t info.idx_table info
  ;;

  let put_fts_durable t ~name meta = Hashtbl.replace t.fts name meta

  let bump_rowid t ~name meta =
    set_meta t name meta;
    Hashtbl.replace t.rowid_bumped name ()
  ;;

  let set_rowid_durable t ~name meta = set_meta t name meta

  let take_rowid_bumped t =
    let names = Hashtbl.fold (fun k _ acc -> k :: acc) t.rowid_bumped [] in
    Hashtbl.reset t.rowid_bumped;
    names
  ;;

  let commit t =
    t.undo <- [];
    t.savepoints <- [];
    t.poisoned <- false;
    (* #293: COMMIT keeps the bumped next_rowid counter but clears the dirty set so
       a later unrelated ROLLBACK won't recompute a table not bumped in that txn. *)
    Hashtbl.reset t.rowid_bumped
  ;;

  let rollback t =
    List.iter (fun f -> f ()) t.undo;
    t.undo <- [];
    t.savepoints <- [];
    t.poisoned <- false
  ;;

  (* #293: [rowid_bumped] is intentionally NOT cleared here — the db layer calls
       the recompute step right after, which reads it via [take_rowid_bumped]. *)

  (* #303: snapshot every table's cached [next_rowid] so ROLLBACK TO can restore
     the in-memory counter to its value at SAVEPOINT.  Tables created after the
     savepoint are absent here and their CREATE is undone by [sp_undo]; tables
     dropped/altered after it are restored by [sp_undo] first, then their counter
     is corrected to this snapshot value. *)
  let snapshot_rowids t =
    Hashtbl.fold
      (fun name (m : table_meta) acc ->
         match (patch t m).storage with
         | Row { next_rowid; _ } -> (name, next_rowid) :: acc
         | Columnar _ -> acc)
      t.tables
      []
  ;;

  (* Snapshot every columnar store's encoded state so ROLLBACK TO can restore
     it.  Persist.save only runs at commit time, so the B-tree never holds
     in-txn columnar data and cannot be used as a rollback source.  We snapshot
     ALL columnar stores (not just dirty), because a clean store may be mutated
     after the savepoint and then need restoration. *)
  let snapshot_columnar t =
    Hashtbl.fold
      (fun name (m : table_meta) acc ->
         match m.storage with
         | Columnar (cs, _) ->
           let encoded = Granary_columnar.Col_store.encode cs in
           let was_dirty = Granary_columnar.Col_store.dirty cs in
           (name, encoded, was_dirty) :: acc
         | _ -> acc)
      t.tables
      []
  ;;

  let savepoint_begin t name =
    t.savepoints
    <- { sp_name = name
       ; sp_undo = t.undo
       ; sp_poison = t.poisoned
       ; sp_rowids = snapshot_rowids t
       ; sp_columnar = snapshot_columnar t
       }
       :: t.savepoints
  ;;

  (* #303: restore each snapshotted counter onto the (already DDL-undone) cached
     meta.  Skip names no longer cached (their CREATE was rolled back). *)
  let restore_rowids t rowids =
    List.iter
      (fun (name, next_rowid) ->
         match Hashtbl.find_opt t.tables name with
         | Some ({ storage = Row r; _ } as m) ->
           set_meta t name { m with storage = Row { r with next_rowid } }
         | _ -> ())
      rowids
  ;;

  (* Restore each columnar store to its savepoint snapshot.  The store is
     marked dirty if it was dirty at snapshot time so unpersisted pre-savepoint
     rows are still persisted on the next COMMIT. *)
  let restore_columnar t snapshots =
    List.iter
      (fun (name, encoded, was_dirty) ->
         match Hashtbl.find_opt t.tables name with
         | Some ({ storage = Columnar (_, tid); columns; _ } as m) ->
           let cs = Granary_columnar.Col_store.decode columns encoded in
           if was_dirty then Granary_columnar.Col_store.mark_dirty cs;
           set_meta t name { m with storage = Columnar (cs, tid) }
         | _ -> ())
      snapshots
  ;;

  let savepoint_rollback t name =
    let rec find = function
      | [] -> None
      | ({ sp_name; _ } as sp) :: older when String.equal sp_name name -> Some (sp, older)
      | _ :: rest -> find rest
    in
    match find t.savepoints with
    | None -> ()
    | Some (sp, older) ->
      let rec run lst =
        if lst == sp.sp_undo
        then ()
        else (
          match lst with
          | [] -> ()
          | f :: tl ->
            f ();
            run tl)
      in
      run t.undo;
      t.undo <- sp.sp_undo;
      t.poisoned <- sp.sp_poison;
      (* After the DDL undos above, correct the cached rowid counters to their
          savepoint values (#303). *)
      restore_rowids t sp.sp_rowids;
      (* Restore columnar stores to their savepoint snapshots. *)
      restore_columnar t sp.sp_columnar;
      t.savepoints <- sp :: older
  ;;

  let savepoint_release t name =
    let rec drop = function
      | [] -> []
      | { sp_name; _ } :: older when String.equal sp_name name -> older
      | _ :: rest -> drop rest
    in
    t.savepoints <- drop t.savepoints
  ;;

  let mark_poisoned t = t.poisoned <- true
  let is_poisoned t = t.poisoned
end

type t =
  { store : S.t
  ; sc : Schema_cache.t
    (** #283: the sealed in-memory schema cache (tables/indexes/fts) and its
        rollback ledger.  The only path to a cache mutation, so a write that does
        not record its reversal is unrepresentable. *)
  ; mutable fk_enforcement : bool
  ; mutable recursive_triggers : bool
  ; mutable defer_fks_pragma : bool
    (** PRAGMA defer_foreign_keys — when ON, every FK enforcement site treats
        the violation as deferred regardless of constraint definition.
        Reset to false at every txn boundary by the db layer. *)
  ; mutable pending_fk_checks : pending_fk_check list
    (** Queued deferred FK violations; drained at commit. The list is in
        reverse insertion order; drain reverses again before returning. *)
  ; mutable last_inserted_rowid : int64
    (** #243 (T1): rowid of the most recently INSERTed row, set by the executor.
        Read by the db layer for [last_insert_rowid()].  Required because with
        INTEGER PRIMARY KEY rowid aliases an explicit id need not equal
        [next_rowid - 1] (e.g. inserting id=5 after id=100). *)
  }

let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>Catalog.t { tables = %d;@ indexes = %d;@ fts = %d;@ fk_enforcement = %b }@]"
    (Schema_cache.count_tables t.sc)
    (Schema_cache.count_indexes t.sc)
    (Schema_cache.count_fts t.sc)
    t.fk_enforcement
;;

(* #243 (T1): SQLite's "INTEGER PRIMARY KEY is an alias for the rowid".  When a
   rowid table has exactly one PRIMARY KEY column whose type is INTEGER, that
   column IS the rowid: the table tree is keyed by its value, no separate __pk
   index exists, and uniqueness is enforced by the table tree itself.  Returns
   the column index of that alias column, or None for every other shape
   (WITHOUT ROWID, composite PK, non-INTEGER PK, no PK).

   Note: this port's AST collapses every integer type spelling (INT, INTEGER,
   BIGINT, ...) to a single [Row.Integer], so unlike SQLite — which aliases only
   the exact spelling "INTEGER" — any integer-typed single-column PK qualifies.
   The distinction is not representable here and was already absent. *)
let compute_rowid_alias_col (columns : Row.column list) ~without_rowid : int option =
  if without_rowid
  then None
  else (
    let indexed = List.mapi (fun i (c : Row.column) -> i, c) columns in
    let pks = List.filter (fun (_, (c : Row.column)) -> c.primary_key) indexed in
    match pks with
    (* #312: an INTEGER PRIMARY KEY DESC is NOT a rowid alias in SQLite — the
       column gets a hidden auto rowid plus a real unique index, exactly like a
       non-INTEGER PK.  So [pk_desc] disqualifies the alias. *)
    | [ (i, (c : Row.column)) ] when c.ty = Row.Integer && not c.pk_desc -> Some i
    | _ -> None)
;;

(* Convenience: the rowid-alias column of a loaded table, if any. *)
let rowid_alias_col (m : table_meta) : int option =
  match m.storage with
  | Columnar _ -> None
  | Row { without_rowid; _ } -> compute_rowid_alias_col m.columns ~without_rowid
;;

(* ------------------------------------------------------------------ *)
(* Encoding helpers                                                     *)
(* ------------------------------------------------------------------ *)

let encode_table_value m =
  let buf = Buffer.create 16 in
  (match m.storage with
   | Row { tree_id; next_rowid; without_rowid; autoincrement } ->
     Varint.encode_uint64 buf (Int64.of_int tree_id);
     Varint.encode_int64 buf next_rowid;
     Varint.encode_uint64 buf (if without_rowid then 1L else 0L);
     Varint.encode_uint64 buf (if autoincrement then 1L else 0L);
     Varint.encode_uint64 buf 0L (* storage_kind = Row *)
   | Columnar (_, tree_id) ->
     Varint.encode_uint64 buf (Int64.of_int tree_id);
     (* tree_id *)
     Varint.encode_int64 buf empty_next_rowid;
     Varint.encode_uint64 buf 0L;
     (* without_rowid = false *)
     Varint.encode_uint64 buf 0L;
     (* autoincrement = false *)
     Varint.encode_uint64 buf 1L (* storage_kind = Columnar *));
  Buffer.to_bytes buf
;;

let decode_table_storage bytes columns =
  let tid, off = Varint.decode_uint64 bytes 0 in
  let next, off' = Varint.decode_int64 bytes off in
  let without_rowid, off'' =
    if off' >= Bytes.length bytes
    then false, off'
    else (
      let v, o = Varint.decode_uint64 bytes off' in
      Int64.to_int v <> 0, o)
  in
  let autoincrement, off''' =
    if off'' >= Bytes.length bytes
    then false, off''
    else (
      let v, o = Varint.decode_uint64 bytes off'' in
      Int64.to_int v <> 0, o)
  in
  let storage_kind =
    if off''' >= Bytes.length bytes
    then 0
    else (
      let v, _ = Varint.decode_uint64 bytes off''' in
      Int64.to_int v)
  in
  match storage_kind with
  | 1 ->
    let tid = Int64.to_int tid in
    let tid = if tid = 0 then -1 else tid in
    Columnar (Granary_columnar.Col_store.create columns, tid)
  | _ ->
    Row { tree_id = Int64.to_int tid; next_rowid = next; without_rowid; autoincrement }
;;

(* Column key: table_name ++ NUL ++ ordinal_be8 *)
let column_key table_name ordinal =
  let tn = Bytes.of_string table_name in
  let ord = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set_uint8 ord i ((ordinal lsr ((7 - i) * 8)) land 0xFF)
  done;
  Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord
;;

let column_prefix table_name =
  Bytes.cat (Bytes.of_string table_name) (Bytes.of_string "\x00")
;;

let type_tag = function
  | Row.Integer -> 1
  | Row.Text -> 2
  | Row.Real -> 3
  | Row.Blob -> 4
;;

let type_of_tag = function
  | 1 -> Row.Integer
  | 2 -> Row.Text
  | 3 -> Row.Real
  | 4 -> Row.Blob
  | n -> failwith (Printf.sprintf "unknown column type tag %d" n)
;;

let default_value_tag : Row.default_value -> int = function
  | Row.DV_null -> 0
  | Row.DV_int _ -> 1
  | Row.DV_real _ -> 2
  | Row.DV_text _ -> 3
  | Row.DV_blob _ -> 4
  | Row.DV_current_timestamp -> 5
  | Row.DV_current_date -> 6
  | Row.DV_current_time -> 7
;;

let encode_default_value buf (dv : Row.default_value) =
  Varint.encode_uint64 buf (Int64.of_int (default_value_tag dv));
  match dv with
  | Row.DV_null -> ()
  | Row.DV_int n ->
    (* 8-byte LE int64 *)
    let tmp = Bytes.create 8 in
    for k = 0 to 7 do
      Bytes.set_uint8
        tmp
        k
        (Int64.to_int (Int64.logand (Int64.shift_right_logical n (k * 8)) 0xFFL))
    done;
    Buffer.add_bytes buf tmp
  | Row.DV_real f ->
    let bits = Int64.bits_of_float f in
    let tmp = Bytes.create 8 in
    for k = 0 to 7 do
      Bytes.set_uint8
        tmp
        k
        (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (k * 8)) 0xFFL))
    done;
    Buffer.add_bytes buf tmp
  | Row.DV_text s ->
    Varint.encode_uint64 buf (Int64.of_int (String.length s));
    Buffer.add_string buf s
  | Row.DV_blob b ->
    Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
    Buffer.add_bytes buf b
  | Row.DV_current_timestamp | Row.DV_current_date | Row.DV_current_time ->
    () (* tag alone is sufficient — no payload *)
;;

let decode_default_value bytes off =
  let tag, off = Varint.decode_uint64 bytes off in
  match Int64.to_int tag with
  | 0 -> Row.DV_null, off
  | 1 ->
    let n = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 bytes (off + k)) in
      n := Int64.logor !n (Int64.shift_left byte (k * 8))
    done;
    Row.DV_int !n, off + 8
  | 2 ->
    let bits = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 bytes (off + k)) in
      bits := Int64.logor !bits (Int64.shift_left byte (k * 8))
    done;
    Row.DV_real (Int64.float_of_bits !bits), off + 8
  | 3 ->
    let len, off = Varint.decode_uint64 bytes off in
    let len = Int64.to_int len in
    let s = Bytes.sub_string bytes off len in
    Row.DV_text s, off + len
  | 4 ->
    let len, off = Varint.decode_uint64 bytes off in
    let len = Int64.to_int len in
    let b = Bytes.sub bytes off len in
    Row.DV_blob b, off + len
  | 5 -> Row.DV_current_timestamp, off
  | 6 -> Row.DV_current_date, off
  | 7 -> Row.DV_current_time, off
  | n -> failwith (Printf.sprintf "unknown default value tag %d" n)
;;

let encode_column (col : Row.column) =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int (type_tag col.ty));
  Varint.encode_uint64 buf (Int64.of_int (String.length col.name));
  Buffer.add_string buf col.name;
  Varint.encode_uint64 buf (if col.not_null then 1L else 0L);
  Varint.encode_uint64 buf (if col.primary_key then 1L else 0L);
  (match col.default with
   | None -> Varint.encode_uint64 buf 0L
   | Some dv ->
     Varint.encode_uint64 buf 1L;
     encode_default_value buf dv);
  (* Phase 9: check_sql field — appended at end for backward compat *)
  (match col.check_sql with
   | None -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  (* Phase 25: generated_as field — appended for backward compat *)
  (match col.generated_as with
   | None -> Varint.encode_uint64 buf 0L
   | Some (sql, is_stored) ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (if is_stored then 1L else 0L);
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  (* #312: trailing pk_desc flag — appended for backward compat (absent ⇒ false). *)
  Varint.encode_uint64 buf (if col.pk_desc then 1L else 0L);
  Buffer.to_bytes buf
;;

let decode_check_sql bytes off =
  if Bytes.length bytes - off <= 0
  then None, off
  else (
    let has_check, off2 = Varint.decode_uint64 bytes off in
    if Int64.to_int has_check = 0
    then None, off2
    else (
      let sql_len, off3 = Varint.decode_uint64 bytes off2 in
      let sql = Bytes.sub_string bytes off3 (Int64.to_int sql_len) in
      Some sql, off3 + Int64.to_int sql_len))
;;

(* Returns the decoded [generated_as] AND the offset just past it, so the
   caller can continue decoding trailing fields (#312 pk_desc). *)
let decode_generated_as bytes off =
  if Bytes.length bytes - off <= 0
  then None, off
  else (
    let has_gen, off2 = Varint.decode_uint64 bytes off in
    if Int64.to_int has_gen = 0
    then None, off2
    else (
      let is_stored, off3 = Varint.decode_uint64 bytes off2 in
      let sql_len, off4 = Varint.decode_uint64 bytes off3 in
      let sql = Bytes.sub_string bytes off4 (Int64.to_int sql_len) in
      Some (sql, Int64.to_int is_stored = 1), off4 + Int64.to_int sql_len))
;;

(* #312: optional trailing pk_desc flag.  Absent (old encodings) ⇒ false. *)
let decode_pk_desc bytes off =
  if off >= Bytes.length bytes
  then false
  else (
    let flag, _ = Varint.decode_uint64 bytes off in
    Int64.to_int flag <> 0)
;;

let decode_column bytes =
  let tag, off = Varint.decode_uint64 bytes 0 in
  let len, off = Varint.decode_uint64 bytes off in
  let name = Bytes.sub_string bytes off (Int64.to_int len) in
  let off = off + Int64.to_int len in
  (* not_null and primary_key — present only in the new format.
     If there are no more bytes, default to false (backward compat). *)
  let bytes_left = Bytes.length bytes - off in
  if bytes_left = 0
  then
    Row.
      { name
      ; ty = type_of_tag (Int64.to_int tag)
      ; not_null = false
      ; primary_key = false
      ; pk_desc = false
      ; default = None
      ; check_sql = None
      ; generated_as = None
      }
  else (
    let nn, off = Varint.decode_uint64 bytes off in
    let pk, off = Varint.decode_uint64 bytes off in
    let has_def, off = Varint.decode_uint64 bytes off in
    let default, off =
      if Int64.to_int has_def = 0
      then None, off
      else (
        let dv, off' = decode_default_value bytes off in
        Some dv, off')
    in
    let check_sql, final_off = decode_check_sql bytes off in
    let generated_as, off = decode_generated_as bytes final_off in
    let pk_desc = decode_pk_desc bytes off in
    Row.
      { name
      ; ty = type_of_tag (Int64.to_int tag)
      ; not_null = Int64.to_int nn <> 0
      ; primary_key = Int64.to_int pk <> 0
      ; pk_desc
      ; default
      ; check_sql
      ; generated_as
      })
;;

let byte_of_idx_origin : idx_origin -> char = function
  | `Implicit_pk -> '\x00'
  | `Implicit_unique -> '\x01'
  | `User -> '\x02'
;;

let idx_origin_of_byte = function
  | 0 -> `Implicit_pk
  | 1 -> `Implicit_unique
  | _ -> `User
;;

(* Index value encoding:
   varint(name_len) ++ name ++ varint(table_len) ++ table
   ++ varint(n_cols) ++ (varint(col_len) ++ col)*n_cols
   ++ [unique: 1 byte] ++ varint(tree_id) *)
let encode_index_value (idx : index_info) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_name));
  Buffer.add_string buf idx.idx_name;
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_table));
  Buffer.add_string buf idx.idx_table;
  Varint.encode_uint64 buf (Int64.of_int (List.length idx.idx_columns));
  List.iter
    (fun col ->
       Varint.encode_uint64 buf (Int64.of_int (String.length col));
       Buffer.add_string buf col)
    idx.idx_columns;
  Buffer.add_char buf (if idx.idx_unique then '\x01' else '\x00');
  Varint.encode_uint64 buf (Int64.of_int idx.idx_tree_id);
  (* Extended fields version 3: origin byte + expr flags + optional WHERE *)
  Varint.encode_uint64 buf 3L;
  Buffer.add_char buf (byte_of_idx_origin idx.idx_origin);
  (* One varint per column: 0 = plain column, 1 = expression column *)
  List.iter
    (fun is_expr -> Varint.encode_uint64 buf (if is_expr then 1L else 0L))
    idx.idx_expr_flags;
  (* WHERE clause SQL *)
  (match idx.idx_where_sql with
   | None -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  Buffer.to_bytes buf
;;

(* Returns [(expr_flags, where_sql, origin)].  The current encoder always writes
   version 3 (with an explicit origin); the pre-v3 branches default [origin] to
   [`User] — the dump-safe "emit it" choice — since the format is pre-release and
   no v<3 data exists.  Decode of expr flags + WHERE is shared by versions 2/3. *)
let decode_index_ext_fields bytes off2 cols =
  let decode_flags_and_where off_start =
    let off_ref = ref off_start in
    let expr_flags =
      List.map
        (fun _ ->
           let flag, next = Varint.decode_uint64 bytes !off_ref in
           off_ref := next;
           Int64.to_int flag = 1)
        cols
    in
    let has_where, off4 = Varint.decode_uint64 bytes !off_ref in
    let where_sql =
      if Int64.to_int has_where = 0
      then None
      else (
        let sql_len, off5 = Varint.decode_uint64 bytes off4 in
        Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
    in
    expr_flags, where_sql
  in
  if off2 >= Bytes.length bytes
  then List.map (fun _ -> false) cols, None, `User (* old format: no extended fields *)
  else (
    let version, off3 = Varint.decode_uint64 bytes off2 in
    match Int64.to_int version with
    | 1 ->
      (* Version 1 (Task 1): only WHERE clause, no expr flags *)
      let has_where, off4 = Varint.decode_uint64 bytes off3 in
      let where_sql =
        if Int64.to_int has_where = 0
        then None
        else (
          let sql_len, off5 = Varint.decode_uint64 bytes off4 in
          Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
      in
      List.map (fun _ -> false) cols, where_sql, `User
    | 2 ->
      (* Version 2 (Task 2): n_cols expr flags, then WHERE clause *)
      let expr_flags, where_sql = decode_flags_and_where off3 in
      expr_flags, where_sql, `User
    | 3 ->
      (* Version 3 (#273): origin byte, then expr flags, then WHERE clause *)
      let origin = idx_origin_of_byte (Bytes.get_uint8 bytes off3) in
      let expr_flags, where_sql = decode_flags_and_where (off3 + 1) in
      expr_flags, where_sql, origin
    | _ -> List.map (fun _ -> false) cols, None, `User)
;;

let decode_index_value bytes =
  let name_len, off = Varint.decode_uint64 bytes 0 in
  let name_len = Int64.to_int name_len in
  let name = Bytes.sub_string bytes off name_len in
  let off = off + name_len in
  let tbl_len, off = Varint.decode_uint64 bytes off in
  let tbl_len = Int64.to_int tbl_len in
  let tbl = Bytes.sub_string bytes off tbl_len in
  let off = off + tbl_len in
  let n_cols, off = Varint.decode_uint64 bytes off in
  let n_cols = Int64.to_int n_cols in
  let off = ref off in
  let cols =
    List.init n_cols (fun _ ->
      let col_len, next_off = Varint.decode_uint64 bytes !off in
      let col = Bytes.sub_string bytes next_off (Int64.to_int col_len) in
      off := next_off + Int64.to_int col_len;
      col)
  in
  let unique_byte = Bytes.get_uint8 bytes !off in
  let tree_id, off2 = Varint.decode_uint64 bytes (!off + 1) in
  let idx_expr_flags, idx_where_sql, idx_origin =
    decode_index_ext_fields bytes off2 cols
  in
  { idx_name = name
  ; idx_table = tbl
  ; idx_columns = cols
  ; idx_unique = unique_byte <> 0
  ; idx_tree_id = Int64.to_int tree_id
  ; idx_expr_flags
  ; idx_where_sql
  ; idx_origin
  }
;;

(* FTS value encoding:
   varint(content_tree) ++ varint(index_tree) ++ varint(n_cols)
   ++ (varint(col_len) ++ col_bytes)* *)
let encode_fts_value (m : fts_table_meta) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int m.fts_content_tree);
  Varint.encode_uint64 buf (Int64.of_int m.fts_index_tree);
  Varint.encode_uint64 buf (Int64.of_int (List.length m.fts_columns));
  List.iter
    (fun col ->
       let b = Bytes.of_string col in
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
       Buffer.add_bytes buf b)
    m.fts_columns;
  Buffer.to_bytes buf
;;

let decode_fts_value fts_name bytes =
  let ct, off0 = Varint.decode_uint64 bytes 0 in
  let it, off1 = Varint.decode_uint64 bytes off0 in
  let nc, off2 = Varint.decode_uint64 bytes off1 in
  let n = Int64.to_int nc in
  let cols = ref [] in
  let pos = ref off2 in
  for _ = 1 to n do
    let len, off = Varint.decode_uint64 bytes !pos in
    let col = Bytes.sub_string bytes off (Int64.to_int len) in
    cols := col :: !cols;
    pos := off + Int64.to_int len
  done;
  { fts_name
  ; fts_content_tree = Int64.to_int ct
  ; fts_index_tree = Int64.to_int it
  ; fts_columns = List.rev !cols
  }
;;

(* ------------------------------------------------------------------ *)
(* next_user_tid / next_index_id management                              *)
(* ------------------------------------------------------------------ *)

(* #269: the tx-threaded forms are the primitives — they read/write the counter
   through a supplied [tx], so allocation is read-your-own-writes (two CREATEs in
   one transaction never alloc the SAME tree-ID) and rolls back with the txn.
   The store-level forms below wrap them in their own RO snapshot / RW txn for
   the autocommit path; those must NOT be used while an explicit writer txn is
   held (the nested [rw_begin] would self-deadlock — that was the #269 bug). *)
let read_uint64_key_tx tx key default =
  let%lwt v = S.get tx sys_meta_tid key in
  match v with
  | Some b ->
    let n, _ = Varint.decode_uint64 b 0 in
    Lwt.return (Int64.to_int n)
  | None -> Lwt.return default
;;

let write_uint64_key_tx tx key n =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int n);
  S.put tx sys_meta_tid key (Buffer.to_bytes buf)
;;

let read_uint64_key store key default =
  S.with_ro store @@ fun tx -> read_uint64_key_tx tx key default
;;

let write_uint64_key store key n =
  let%lwt tx = S.rw_begin store in
  let%lwt () = write_uint64_key_tx tx key n in
  S.commit tx
;;

let read_next_user_tid store = read_uint64_key store next_user_tid_key next_user_tid_init
let write_next_user_tid store tid = write_uint64_key store next_user_tid_key tid
let read_next_index_id store = read_uint64_key store next_index_id_key 0
let write_next_index_id store id = write_uint64_key store next_index_id_key id
let read_next_user_tid_tx tx = read_uint64_key_tx tx next_user_tid_key next_user_tid_init
let write_next_user_tid_tx tx tid = write_uint64_key_tx tx next_user_tid_key tid
let read_next_index_id_tx tx = read_uint64_key_tx tx next_index_id_key 0
let write_next_index_id_tx tx id = write_uint64_key_tx tx next_index_id_key id

(* Allocate the next user tree-ID through [tx] (atomic with the caller's txn). *)
let next_user_tid_tx tx =
  let%lwt tid = read_next_user_tid_tx tx in
  let%lwt () = write_next_user_tid_tx tx (tid + 1) in
  Lwt.return tid
;;

(* Encode an int as a varint key for the _sys_indexes tree. *)
let index_key id =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int id);
  Buffer.to_bytes buf
;;

(* ------------------------------------------------------------------ *)
(* Load all metadata from the store                                     *)
(* ------------------------------------------------------------------ *)

let load_columns tx table_name =
  let prefix = column_prefix table_name in
  let%lwt cur = S.cursor_open tx sys_columns_tid in
  let _sr = S.cursor_seek cur prefix in
  let cols = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (ck, cv) ->
      let plen = Bytes.length prefix in
      if Bytes.length ck >= plen && Bytes.equal (Bytes.sub ck 0 plen) prefix
      then (
        cols := decode_column cv :: !cols;
        walk ())
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !cols)
;;

let load_all_tables store =
  let tbl = Hashtbl.create 16 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_tables_tid in
  let _sr = S.cursor_first cur in
  let rec walk_tables () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (k, v) ->
      let%lwt () =
        Lwt.catch
          (fun () ->
             let name = Bytes.to_string k in
             let%lwt cols = load_columns tx name in
             let storage = decode_table_storage v cols in
             Hashtbl.replace
               tbl
               name
               { name; storage; columns = cols; fk_constraints = [] };
             Lwt.return_unit)
          (fun _exn ->
             (* Corrupt primary catalog row/columns (#174): skip it here; the
                table is reconstructed from the redundant mirror in [open_]. *)
             Lwt.return_unit)
      in
      walk_tables ()
  in
  let%lwt () = walk_tables () in
  S.cursor_close cur;
  Lwt.return tbl
;;

let load_all_indexes store =
  let tbl = Hashtbl.create 8 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_indexes_tid in
  let _sr = S.cursor_first cur in
  let rec walk () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (_k, v) ->
      let info = decode_index_value v in
      Hashtbl.replace tbl info.idx_name info;
      walk ()
  in
  let%lwt () = walk () in
  S.cursor_close cur;
  Lwt.return tbl
;;

let is_fts_rowid_key k =
  let slen = Bytes.length sys_fts_rowid_suffix in
  Bytes.length k >= slen
  && Bytes.equal (Bytes.sub k (Bytes.length k - slen) slen) sys_fts_rowid_suffix
;;

let load_all_fts store =
  let tbl = Hashtbl.create 4 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_fts_tid in
  let _sr = S.cursor_first cur in
  let rec walk () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (k, v) ->
      (* Skip rowid counter keys: they end with "\x00rowid" *)
      if is_fts_rowid_key k
      then walk ()
      else (
        (try
           let name = Bytes.to_string k in
           let meta = decode_fts_value name v in
           Hashtbl.replace tbl name meta
         with
         | Invalid_argument msg ->
           (* Corrupt FTS catalog entry for key; skip and continue.
             A corrupt entry will simply be absent from the cache;
             queries against that table will fail with "table not found". *)
           Printf.eprintf "warning: skipping corrupt FTS catalog entry (%s)\n%!" msg);
        walk ())
  in
  let%lwt () = walk () in
  S.cursor_close cur;
  Lwt.return tbl
;;

(* ------------------------------------------------------------------ *)
(* View persistence                                                     *)
(* ------------------------------------------------------------------ *)

let load_all_pairs_in_tx tx tid =
  let%lwt cur = S.cursor_open tx tid in
  let _sr = S.cursor_first cur in
  let pairs = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      pairs := (Bytes.to_string k, Bytes.to_string v) :: !pairs;
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !pairs)
;;

(* #322/#323: read view DDL through a caller-supplied snapshot so [Db.dump] can
   thread its single shared RO snapshot here, making view DDL point-in-time with
   the row data; a concurrent CREATE/DROP VIEW commit then cannot tear the dump's
   schema section relative to its rows. *)
let load_all_views_in_tx (tx : _ S.txn) = load_all_pairs_in_tx tx sys_views_tid
let load_all_views store = S.with_ro store load_all_views_in_tx

(* #269: run [f tx] through the ambient explicit transaction ([?txn = Some tx],
   left uncommitted — the db layer owns its lifecycle) or, in autocommit, a fresh
   writer txn committed here.  Shared by the view/trigger persistence and the FK
   save so the borrow-or-autocommit plumbing lives in one place. *)
let borrow_or_autocommit ?txn store f =
  match txn with
  | Some tx -> f tx
  | None ->
    let%lwt tx = S.rw_begin store in
    let%lwt () = f tx in
    S.commit tx
;;

(* [?txn] (#269): persist/remove through the ambient explicit transaction when
   one is active, else autocommit. *)
let persist_view ?txn store ~name ~sql =
  borrow_or_autocommit ?txn store (fun tx ->
    S.put tx sys_views_tid (Bytes.of_string name) (Bytes.of_string sql))
;;

let remove_view ?txn store ~name =
  borrow_or_autocommit ?txn store (fun tx ->
    S.del tx sys_views_tid (Bytes.of_string name))
;;

(* #427: reactive-view persistence — same shape as views (name -> SQL text). *)
let load_all_reactive_views_in_tx (tx : _ S.txn) =
  load_all_pairs_in_tx tx sys_reactive_views_tid
;;

let load_all_reactive_views store = S.with_ro store load_all_reactive_views_in_tx

let persist_reactive_view ?txn store ~name ~sql =
  borrow_or_autocommit ?txn store (fun tx ->
    S.put tx sys_reactive_views_tid (Bytes.of_string name) (Bytes.of_string sql))
;;

let remove_reactive_view ?txn store ~name =
  borrow_or_autocommit ?txn store (fun tx ->
    S.del tx sys_reactive_views_tid (Bytes.of_string name))
;;

(* ------------------------------------------------------------------ *)
(* Trigger persistence                                                  *)
(* ------------------------------------------------------------------ *)

(* #322/#323: trigger DDL counterpart to [load_all_views_in_tx]. *)
let load_all_triggers_in_tx (tx : _ S.txn) = load_all_pairs_in_tx tx sys_triggers_tid
let load_all_triggers store = S.with_ro store load_all_triggers_in_tx

(* [?txn] (#269): persist/remove through the ambient explicit transaction when
   one is active, else autocommit. *)
let persist_trigger ?txn store ~name ~sql =
  borrow_or_autocommit ?txn store (fun tx ->
    S.put tx sys_triggers_tid (Bytes.of_string name) (Bytes.of_string sql))
;;

let remove_trigger ?txn store ~name =
  borrow_or_autocommit ?txn store (fun tx ->
    S.del tx sys_triggers_tid (Bytes.of_string name))
;;

(* ------------------------------------------------------------------ *)
(* FK constraint persistence                                            *)
(* ------------------------------------------------------------------ *)

let fk_meta_key table_name = Bytes.of_string ("fk:" ^ table_name)

let fk_action_to_string = function
  | FA_no_action -> "no_action"
  | FA_restrict -> "restrict"
  | FA_cascade -> "cascade"
  | FA_set_null -> "set_null"
  | FA_set_default -> "set_default"
;;

let fk_action_of_string = function
  | "no_action" -> FA_no_action
  | "restrict" -> FA_restrict
  | "cascade" -> FA_cascade
  | "set_null" -> FA_set_null
  | "set_default" -> FA_set_default
  | s -> failwith ("catalog: unknown fk_action: " ^ s)
;;

let encode_fks fks =
  let lines =
    List.map
      (fun fk ->
         String.concat
           "\t"
           [ String.concat "," fk.fk_local_cols
           ; fk.fk_parent_table
           ; String.concat "," fk.fk_parent_cols
           ; fk_action_to_string fk.fk_on_delete
           ; fk_action_to_string fk.fk_on_update
           ; (if fk.fk_deferrable then "1" else "0")
           ])
      fks
  in
  Bytes.of_string (String.concat "\n" lines)
;;

let decode_fks bytes =
  let s = Bytes.to_string bytes in
  if s = ""
  then []
  else
    List.filter_map
      (fun line ->
         match String.split_on_char '\t' line with
         | [ lc; pt; pc ] ->
           (* Legacy 3-field form (very old). *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = FA_restrict
             ; fk_on_update = FA_restrict
             ; fk_deferrable = false
             }
         | [ lc; pt; pc; od; ou ] ->
           (* Pre-phase-35 5-field form: deferrable defaults false. *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = fk_action_of_string od
             ; fk_on_update = fk_action_of_string ou
             ; fk_deferrable = false
             }
         | [ lc; pt; pc; od; ou; def ] ->
           (* Phase 35 6-field form. *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = fk_action_of_string od
             ; fk_on_update = fk_action_of_string ou
             ; fk_deferrable = def = "1"
             }
         | _ -> None)
      (String.split_on_char '\n' s)
;;

(* ------------------------------------------------------------------ *)
(* Schema fingerprint + redundant catalog mirror helpers (#174)         *)
(* Defined here, ahead of all DDL and [open_], which use them.          *)
(* ------------------------------------------------------------------ *)

(* Schema fingerprint: a stable hash of a table's shape (columns +
   without_rowid).  Computed from the in-memory cache, so it always reflects
   the current schema after any DDL. *)
let fingerprint_of_meta (m : table_meta) =
  let without_rowid =
    match m.storage with
    | Row r -> r.without_rowid
    | Columnar _ -> false
  in
  Schema_fingerprint.compute ~columns:m.columns ~without_rowid
;;

(* #174: register the table's page-header stamp (low 32 bits of its
   fingerprint) with the store, so its B+-tree pages self-identify their
   schema.  Skips the ephemeral CTE sentinel (tree_id = -1). *)
let register_tag store (m : table_meta) =
  match m.storage with
  | Columnar (_, tid) when tid >= 0 ->
    S.set_tree_tag store tid (Schema_fingerprint.low32 (fingerprint_of_meta m))
  | Row { tree_id; _ } when tree_id >= 0 ->
    S.set_tree_tag store tree_id (Schema_fingerprint.low32 (fingerprint_of_meta m))
  | _ -> ()
;;

(* The mirror is keyed by tree_id (fixed 8-byte BE) and stores a fully
   self-describing schema blob — name, tree_id, WITHOUT ROWID, fingerprint,
   every column (via the same [encode_column] used by the primary), and FK
   constraints.  It carries no volatile state (no [next_rowid]) so it only
   changes on DDL, not on every insert. *)
(* v2 (#299): appends a trailing autoincrement byte after the column/FK blocks.
   v1 entries lack it; the decoder treats their absence as [false]. *)
(* v3 (#314): for an AUTOINCREMENT table ONLY, appends a presence byte + int64
   carrying the volatile [next_rowid] high-water, so mirror reconstruction can
   restore the sticky counter instead of recomputing max(rowid)+1 (which would
   make a committed-DELETE high-water reusable).  Non-AUTOINCREMENT tables write
   a presence byte of 0 and incur no per-insert mirror write. *)
let mirror_version = 3

let mirror_key (tid : S.tree_id) =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int tid);
  b
;;

let encode_mirror_entry (m : table_meta) =
  let tree_id, without_rowid, autoincrement, next_rowid =
    match m.storage with
    | Row { tree_id; without_rowid; autoincrement; next_rowid } ->
      tree_id, without_rowid, autoincrement, next_rowid
    | Columnar _ -> -1, false, false, empty_next_rowid
  in
  let buf = Buffer.create 128 in
  Varint.encode_uint64 buf (Int64.of_int mirror_version);
  Varint.encode_uint64 buf (Int64.of_int (String.length m.name));
  Buffer.add_string buf m.name;
  Varint.encode_uint64 buf (Int64.of_int tree_id);
  Buffer.add_uint8 buf (if without_rowid then 1 else 0);
  let fpb = Bytes.create 8 in
  Bytes.set_int64_be fpb 0 (fingerprint_of_meta m);
  Buffer.add_bytes buf fpb;
  Varint.encode_uint64 buf (Int64.of_int (List.length m.columns));
  List.iter
    (fun col ->
       let cb = encode_column col in
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length cb));
       Buffer.add_bytes buf cb)
    m.columns;
  let fkb = encode_fks m.fk_constraints in
  Varint.encode_uint64 buf (Int64.of_int (Bytes.length fkb));
  Buffer.add_bytes buf fkb;
  (* #299 (mirror v2): trailing autoincrement byte. *)
  Buffer.add_uint8 buf (if autoincrement then 1 else 0);
  (* #314 (mirror v3): for an AUTOINCREMENT table, persist the volatile rowid
     high-water so mirror reconstruction restores it instead of recomputing
     max(rowid)+1.  Non-AUTOINCREMENT tables omit it (no per-insert mirror
     write).  Encoded as a presence byte + int64. *)
  if autoincrement
  then (
    Buffer.add_uint8 buf 1;
    Varint.encode_int64 buf next_rowid)
  else Buffer.add_uint8 buf 0;
  Buffer.to_bytes buf
;;

(* Decode a mirror entry into a [table_meta] (with [next_rowid =
   empty_next_rowid]; the mirror does not persist the rowid counter — it is
   recovered at open-time by scanning the data tree, see [recover_next_rowid])
   and the stored fingerprint. *)
let decode_mirror_entry bytes : table_meta * int64 =
  let ver, off = Varint.decode_uint64 bytes 0 in
  let ver = Int64.to_int ver in
  let nlen, off = Varint.decode_uint64 bytes off in
  let nlen = Int64.to_int nlen in
  let name = Bytes.sub_string bytes off nlen in
  let off = off + nlen in
  let tid, off = Varint.decode_uint64 bytes off in
  let without_rowid = Bytes.get_uint8 bytes off <> 0 in
  let off = off + 1 in
  let fp = Bytes.get_int64_be bytes off in
  let off = ref (off + 8) in
  let ncols, o = Varint.decode_uint64 bytes !off in
  off := o;
  let columns =
    List.init (Int64.to_int ncols) (fun _ ->
      let clen, o = Varint.decode_uint64 bytes !off in
      let clen = Int64.to_int clen in
      let col = decode_column (Bytes.sub bytes o clen) in
      off := o + clen;
      col)
  in
  let fklen, o = Varint.decode_uint64 bytes !off in
  let fkb = Bytes.sub bytes o (Int64.to_int fklen) in
  let fk_constraints = decode_fks fkb in
  off := o + Int64.to_int fklen;
  (* #299 (mirror v2): trailing autoincrement byte; absent in v1. *)
  let ai_present = ver >= 2 && !off < Bytes.length bytes in
  let autoincrement = ai_present && Bytes.get_uint8 bytes !off <> 0 in
  if ai_present then incr off;
  (* #314 (mirror v3): for an AUTOINCREMENT table, a presence byte followed (when
     nonzero) by the persisted [next_rowid] high-water.  v1/v2 blobs lack this;
     a v3 non-AUTOINCREMENT blob has presence byte 0.  Either way [next_rowid]
     defaults to [empty_next_rowid], so [recover_next_rowid] still recomputes
     max(rowid)+1 for those. *)
  let next_rowid =
    if ver >= 3 && !off < Bytes.length bytes && Bytes.get_uint8 bytes !off <> 0
    then (
      incr off;
      let v, o = Varint.decode_int64 bytes !off in
      off := o;
      v)
    else empty_next_rowid
  in
  ( { name
    ; storage =
        Row { tree_id = Int64.to_int tid; next_rowid; without_rowid; autoincrement }
    ; columns
    ; fk_constraints
    }
  , fp )
;;

(* Write/replace a table's mirror entry inside an already-open RW txn. *)
let put_mirror_tx tx (m : table_meta) =
  match m.storage with
  | Columnar _ -> Lwt.return_unit
  | Row { tree_id; _ } ->
    S.put tx sys_mirror_tid (mirror_key tree_id) (encode_mirror_entry m)
;;

(* Persist [m]'s rowid counter to the primary [_sys_tables] row, and — for
   AUTOINCREMENT tables (#314) — keep the redundant mirror's high-water in step
   so a later mirror reconstruction restores the sticky counter.  Non-
   AUTOINCREMENT tables skip the mirror write (no per-insert mirror amplification).
   Single home for the "mirror tracks primary" invariant shared by every
   counter-mutation path. *)
let put_table_counter_tx tx (m : table_meta) =
  let%lwt () = S.put tx sys_tables_tid (Bytes.of_string m.name) (encode_table_value m) in
  match m.storage with
  | Row { autoincrement = true; _ } -> put_mirror_tx tx m
  | _ -> Lwt.return_unit
;;

(* Remove a table's mirror entry inside an already-open RW txn. *)
let del_mirror_tx tx (m : table_meta) =
  match m.storage with
  | Columnar _ -> Lwt.return_unit
  | Row { tree_id; _ } -> S.del tx sys_mirror_tid (mirror_key tree_id)
;;

(* Decode every mirror entry into a [table_meta]; skip corrupt entries. *)
let load_mirror_entries store =
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_mirror_tid in
  let _sr = S.cursor_first cur in
  let acc = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (_k, v) ->
      (try
         let m, _fp = decode_mirror_entry v in
         acc := m :: !acc
       with
       | Invalid_argument _ | Failure _ -> ());
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !acc)
;;

(* #175: recover next_rowid for tables reconstructed from the mirror.
   Scan the table's data tree for the maximum integer rowid key (the
   tree is keyed by [Rowid.encode], whose offset-binary encoding sorts
   negatives correctly, so the last key in byte-sorted order is the maximum
   rowid).  Return [next_rowid = max + 1], or [empty_next_rowid] for an empty or
   unreadable tree (#250: so a subsequent NULL insert seeds at 1 and an explicit
   below-counter id seeds from its own value, exactly as in-session).  WITHOUT
   ROWID tables are skipped — they don't use rowid keys. *)
let recover_next_rowid store (m : table_meta) : table_meta Lwt.t =
  let without_rowid, autoincrement, next_rowid, tree_id =
    match m.storage with
    | Row { without_rowid; autoincrement; next_rowid; tree_id } ->
      without_rowid, autoincrement, next_rowid, tree_id
    | Columnar _ -> false, false, empty_next_rowid, -1
  in
  if without_rowid
  then Lwt.return m
  else if autoincrement && not (Int64.equal next_rowid empty_next_rowid)
  then
    (* #314: the mirror (v3) carries the sticky high-water for AUTOINCREMENT
       tables; trust it instead of recomputing max(rowid)+1, which would make a
       committed-DELETE high-water reusable.  Only a v3 AUTOINCREMENT mirror
       entry decodes to a non-empty [next_rowid], so the rollback-recompute
       caller (which passes live-cache, non-AUTOINCREMENT metas) never trips
       this branch. *)
    Lwt.return m
  else
    S.with_ro store
    @@ fun tx ->
    let%lwt cur = S.cursor_open tx tree_id in
    let _sr = S.cursor_first cur in
    let max_key = ref None in
    let rec walk () =
      match S.cursor_next cur with
      | None -> ()
      | Some (k, _) ->
        max_key := Some k;
        walk ()
    in
    walk ();
    S.cursor_close cur;
    let recovered =
      match !max_key with
      | None -> empty_next_rowid
      | Some k -> Int64.add (Rowid.decode k) 1L
    in
    Lwt.return
      { m with
        storage = Row { tree_id; next_rowid = recovered; without_rowid; autoincrement }
      }
;;

(* #299: read a table's LAST-COMMITTED [next_rowid] straight from its
   [_sys_tables] row.  Used by the AUTOINCREMENT rollback path: after
   [S.rollback] the store row has reverted to the committed value (the sticky
   high-water that a committed DELETE never lowers), so restoring the cached
   counter from it — rather than recomputing [max(rowid)+1] from data — keeps
   the counter sticky across committed deletes while still reverting a
   rolled-back allocation to the committed mark.  Returns [empty_next_rowid] if
   the row is absent (e.g. the table was created in the rolled-back txn — the
   caller skips uncached names anyway). *)
let read_committed_next_rowid store ~name : int64 Lwt.t =
  S.with_ro store
  @@ fun tx ->
  let%lwt v = S.get tx sys_tables_tid (Bytes.of_string name) in
  match v with
  | None -> Lwt.return empty_next_rowid
  | Some bytes ->
    let storage = decode_table_storage bytes [] in
    let next =
      match storage with
      | Row { next_rowid; _ } -> next_rowid
      | Columnar _ -> empty_next_rowid
    in
    Lwt.return next
;;

let load_fk_constraints_raw store table_name =
  let key = fk_meta_key table_name in
  S.with_ro store
  @@ fun tx ->
  let%lwt v = S.get tx sys_meta_tid key in
  Lwt.return
    (match v with
     | None -> []
     | Some b -> decode_fks b)
;;

(* [?txn]: when set (#269) the FK rows are written through the ambient explicit
   transaction; otherwise an autocommit writer txn is used. *)
let save_fk_constraints ?txn t ~table_name ~fks =
  let key = fk_meta_key table_name in
  borrow_or_autocommit ?txn t.store (fun tx ->
    let%lwt () =
      if fks = []
      then S.del tx sys_meta_tid key
      else S.put tx sys_meta_tid key (encode_fks fks)
    in
    (* Keep the mirror's FK list current so a mirror reconstruction restores
       constraints, not just columns. *)
    match Schema_cache.find_table t.sc table_name with
    | Some m -> put_mirror_tx tx { m with fk_constraints = fks }
    | None -> Lwt.return_unit)
;;

(* #269/#282: this is a RAW, undo-free cache update by design — the FK mutation's
   ROLLBACK is handled by the enclosing operation (the txn store-rollback, or
   [add_column]'s schema-cache undo which restores the whole prior [table_meta]),
   never by this call.  Hence [put_table_durable] (no undo), matching the original
   [Hashtbl.replace] semantics exactly. *)
let set_fk_constraints t ~table_name ~fks =
  match Schema_cache.find_table t.sc table_name with
  | None -> ()
  | Some meta ->
    Schema_cache.put_table_durable
      t.sc
      ~name:table_name
      { meta with fk_constraints = fks }
;;

(* ------------------------------------------------------------------ *)
(* Public API                                                           *)
(* ------------------------------------------------------------------ *)

let open_ store =
  let%lwt cache = load_all_tables store in
  let%lwt indexes = load_all_indexes store in
  let%lwt fts = load_all_fts store in
  (* Load FK constraints for each table *)
  let names = Hashtbl.fold (fun k _ acc -> k :: acc) cache [] in
  let%lwt () =
    Lwt_list.iter_s
      (fun name ->
         let%lwt fks = load_fk_constraints_raw store name in
         (match Hashtbl.find_opt cache name with
          | Some meta -> Hashtbl.replace cache name { meta with fk_constraints = fks }
          | None -> ());
         Lwt.return_unit)
      names
  in
  (* #174: reconstruct any table missing from the primary catalog (its
     _sys_tables row or column entries were lost or failed to decode) from the
     redundant mirror.  Tables loaded fine from the primary are left untouched
     here; the mirror only fills gaps.  Reconstructed entries carry the
     mirror's own columns and FK constraints (the primary FK load above ran
     only over primary tables). *)
  let%lwt mirror = load_mirror_entries store in
  let present_tids =
    Hashtbl.fold (fun _ (m : table_meta) acc -> tid_of_storage m.storage :: acc) cache []
  in
  let reconstructed =
    List.filter
      (fun (m : table_meta) ->
         let tid = tid_of_storage m.storage in
         not (List.mem tid present_tids))
      mirror
  in
  List.iter (fun (m : table_meta) -> Hashtbl.replace cache m.name m) reconstructed;
  (* #175: for tables reconstructed from the mirror, recover next_rowid by
     scanning the data tree for the maximum integer rowid key.  WITHOUT ROWID
     tables are skipped.  Best-effort: defaults to 1L for empty trees. *)
  let%lwt () =
    Lwt_list.iter_s
      (fun (m : table_meta) ->
         let%lwt recovered = recover_next_rowid store m in
         Hashtbl.replace cache recovered.name recovered;
         Lwt.return_unit)
      reconstructed
  in
  (* #174: schema-drift check on open.  For tables present in BOTH the primary
     and the mirror, a fingerprint mismatch means one copy is corrupt or drifted
     — warn (but stay openable so recovery tooling can still run).  Reconstructed
     tables match the mirror by construction, so they never trip this. *)
  List.iter
    (fun (m : table_meta) ->
       match Hashtbl.find_opt cache m.name with
       | Some primary
         when (let tid_p = tid_of_storage primary.storage in
               let tid_m = tid_of_storage m.storage in
               tid_p = tid_m)
              && not (Int64.equal (fingerprint_of_meta primary) (fingerprint_of_meta m))
         ->
         Printf.eprintf
           "warning: schema fingerprint mismatch for table %s — primary and redundant \
            catalog disagree (possible corruption, #174)\n\
            %!"
           m.name
       | _ -> ())
    mirror;
  (* #533/#542: re-derive PRIMARY KEY column flags from the implicit PK indexes.

     [primary_key]/[not_null] are stored PER COLUMN, and every database written
     before #530 stored them wrong for a table-level [PRIMARY KEY (...)]: the
     marking ran too late to reach the [not_null = not_null || primary_key]
     derivation, and a composite key marked no column at all.  The oldest column
     encoding is worse still — [decode_column] has no flag bytes to read and
     defaults both to false for EVERY column.

     Leaving that as-read is not just "the old permissive INSERT semantics".  It
     is the exact input [Planner.seek_is_unique_point] tests, so a pre-existing
     TPC-C file would keep failing [all_not_null], drop its composite-PK point
     lookup out of the one-row class and silently revert the #513 StockLevel win
     — the very thing #526's exemption was covering for.

     The `Implicit_pk index names exactly the key's columns and is the same
     thing #530 derives the flags from at CREATE time, so it is a faithful
     re-derivation rather than a guess.  In memory only: nothing is rewritten to
     disk, so an older build still reads the file it wrote.  Marking is additive
     — a column already flagged is left alone and no flag is ever cleared — and
     it runs AFTER the #174 drift check so the comparison still sees both copies
     exactly as stored. *)
  let pk_cols_of_table = Hashtbl.create 8 in
  Hashtbl.iter
    (fun _ (i : index_info) ->
       if i.idx_origin = `Implicit_pk
       then (
         let prev =
           Option.value ~default:[] (Hashtbl.find_opt pk_cols_of_table i.idx_table)
         in
         Hashtbl.replace pk_cols_of_table i.idx_table (i.idx_columns @ prev)))
    indexes;
  let normalized =
    Hashtbl.fold
      (fun name (m : table_meta) acc ->
         match Hashtbl.find_opt pk_cols_of_table name with
         | None -> acc
         | Some pk_cols ->
           let columns =
             List.map
               (fun (c : Row.column) ->
                  if List.mem c.Row.name pk_cols
                  then { c with Row.primary_key = true; not_null = true }
                  else c)
               m.columns
           in
           if columns = m.columns then acc else (name, { m with columns }) :: acc)
      cache
      []
  in
  List.iter (fun (name, m) -> Hashtbl.replace cache name m) normalized;
  (* #283: seed the sealed cache durably (no undo, this is open-time state).
     [put_table_durable] re-stamps each table's #174 page-header tag, replacing
     the old explicit [register_tag] iteration. *)
  (* #633: the allocator belongs to the STORE.  Every catalog over this store —
     [Db.create_worker_handle]'s included — therefore shares it by construction,
     with no argument to pass and none to forget. *)
  let sc =
    Schema_cache.create
      ~rowid_counters:(S.rowid_counters store)
      ~stamp:(fun m -> register_tag store m)
      ()
  in
  (* #589: [seed_table], not [put_table_durable] — when a sibling handle over the
     same store is already using these counters, what it holds is at least as
     fresh as what we just read off disk, and clobbering it with the disk values
     would reintroduce the very collision this fixes (in the opposite direction:
     the PARENT would go stale). *)
  Hashtbl.iter (fun name m -> Schema_cache.seed_table sc ~name m) cache;
  Hashtbl.iter (fun name i -> Schema_cache.put_index_durable sc ~name i) indexes;
  Hashtbl.iter (fun name m -> Schema_cache.put_fts_durable sc ~name m) fts;
  Lwt.return
    { store
    ; sc
    ; fk_enforcement = false
    ; recursive_triggers = true
    ; defer_fks_pragma = false
    ; pending_fk_checks = []
    ; last_inserted_rowid = 0L
    }
;;

(* #243 (T1): last-inserted rowid accessors for [last_insert_rowid()]. *)
let set_last_inserted_rowid t rowid = t.last_inserted_rowid <- rowid
let last_inserted_rowid t = t.last_inserted_rowid

(** Allocate and return the next available user tree ID, atomically incrementing the counter. *)
let next_user_tid t =
  let%lwt tid = read_next_user_tid t.store in
  let%lwt () = write_next_user_tid t.store (tid + 1) in
  Lwt.return tid
;;

(* #269: register an in-memory schema-cache reversal for a change run through an
   explicit transaction.  Delegates to the sealed [Schema_cache] undo log so the
   db layer's view/trigger-cache reversals replay in order with the catalog's own
   DDL undos.  (Catalog DDL self-registers; this entry point is for the db layer's
   own [t.views]/[t.triggers] caches, which live outside the catalog.) *)
let register_schema_undo t f = Schema_cache.register_undo t.sc f

(* #286: mark the ambient explicit transaction uncommittable because an in-txn
   DDL statement failed partway through (partial on-disk effects remain). *)
let mark_schema_txn_poisoned t = Schema_cache.mark_poisoned t.sc
let schema_txn_poisoned t = Schema_cache.is_poisoned t.sc

(* #293: COMMIT keeps the bumped next_rowid counter, so do NOT recompute — but
   clears the dirty set so a later unrelated ROLLBACK won't wrongly recompute a
   table that wasn't bumped in that later txn. *)
let commit_schema_changes t = Schema_cache.commit t.sc

(* #293: the rowid dirty set is intentionally NOT cleared on rollback here — the
   db layer calls [recompute_rowid_counters_after_rollback] right after, which
   reads the set then clears it. *)
let rollback_schema_changes t = Schema_cache.rollback t.sc

(* #293: re-derive the cached [next_rowid] from the (now rolled-back) data tree,
   using the same max(rowid)+1 logic as [recover_next_rowid], for ONLY the tables
   whose counter was bumped during the rolled-back transaction.  An INSERT run
   THROUGH an explicit transaction bumps the in-memory counter via
   [next_rowid_in_txn]/[bump_next_rowid_in_txn] (which also record the table in
   [rowid_bumped_in_txn] and write the bumped value to [_sys_tables] under the
   txn).  On ROLLBACK the store row reverts but the in-memory counter does not,
   so the next allocation would SKIP the rolled-back rowid instead of reusing it.
   SQLite, for a plain (non-AUTOINCREMENT) rowid table, recomputes max(rowid)+1
   from the data after a rollback and so reuses it; recomputing here matches that.

   Option (b) from the issue: rather than snapshot/restore each DML's counter
   delta, we drop straight to the authoritative source (the data tree).  This
   leans on the store having already been rolled back — the db layer calls this
   only AFTER [S.rollback], so the trees show the last-committed state and the
   RW lock is released (so [recover_next_rowid]'s own RO txn cannot deadlock).

   We recompute ONLY the [rowid_bumped_in_txn] set — usually a single table —
   rather than every cached rowid table.  [recover_next_rowid] is an O(n) tree
   walk to find max(rowid); scanning every table would make a rollback cost
   O(total rows across ALL tables), a regression on a perf-sensitive engine
   (cf. #228/#229 driving cursor_open O(n)->O(log n)).  Restricting to the
   bumped set keeps rollback ~O(1) in the common case.  A bumped name that is no
   longer cached (e.g. its CREATE TABLE rolled back in the same txn) or that is
   WITHOUT ROWID is skipped.  The set is CLEARED here so it never leaks into a
   later transaction; commit clears it too (via [commit_schema_changes]), which
   is why a COMMIT keeps the bumped counter yet a subsequent unrelated ROLLBACK
   does not wrongly recompute it.

   #299: AUTOINCREMENT tables take a DIFFERENT branch.  Their counter is a
   sticky high-water that a committed DELETE never lowers, so recomputing
   [max(rowid)+1] from data would wrongly reissue an id below the high-water.
   Instead we restore the cached counter to the LAST-COMMITTED value persisted
   in [_sys_tables] (which [S.rollback] has already reverted to), via
   [read_committed_next_rowid].  That still reverts a rolled-back allocation to
   the committed mark (matching SQLite, whose [sqlite_sequence] is itself
   transactional) while preserving stickiness across committed deletes. *)
let recompute_rowid_counters_after_rollback t =
  let names = Schema_cache.take_rowid_bumped t.sc in
  Lwt_list.iter_s
    (fun name ->
       match Schema_cache.find_table t.sc name with
       | None -> Lwt.return_unit
       | Some m when is_columnar m -> Lwt.return_unit
       | Some m ->
         let tree_id, _next_rowid, without_rowid, autoincrement = row_storage m in
         if without_rowid
         then Lwt.return_unit
         else if autoincrement
         then (
           let%lwt committed = read_committed_next_rowid t.store ~name in
           Schema_cache.set_rowid_durable
             t.sc
             ~name
             { m with
               storage =
                 Row { tree_id; next_rowid = committed; without_rowid; autoincrement }
             };
           Lwt.return_unit)
         else (
           let%lwt recovered = recover_next_rowid t.store m in
           Schema_cache.set_rowid_durable t.sc ~name recovered;
           Lwt.return_unit))
    names
;;

(* #280/#295: open a savepoint over the schema-undo log.  Records the current
   [schema_undo] list so [ROLLBACK TO]/[RELEASE] of this savepoint can find the
   boundary between entries registered before and after it, and the current
   [schema_txn_poisoned] flag (#295) so [ROLLBACK TO] can restore the poison
   state as it was when this savepoint opened. *)
let savepoint_begin_schema t name = Schema_cache.savepoint_begin t.sc name

(* #280/#295: ROLLBACK TO a savepoint.  Run+drop the undo closures registered
   since the savepoint (the prefix of [schema_undo] down to its recorded
   snapshot, most-recent-first), reset [schema_undo] to that snapshot, restore
   [schema_txn_poisoned] to its snapshot (#295), drop newer savepoint markers,
   and keep this savepoint so it can be rolled back to again (mirroring
   [Store.savepoint_rollback]).  Unknown name: no-op.

   #295: the poison restore is what un-poisons a txn whose failed in-txn DDL
   lay AFTER this savepoint (its partial effects are in the unwound range).  A
   failure that PREDATES the savepoint left the poison flag already set when
   this savepoint opened, so the snapshot is [true] and the txn stays poisoned —
   exactly correct, since those partial effects are NOT unwound here. *)
let savepoint_rollback_schema t name = Schema_cache.savepoint_rollback t.sc name

(* #280/#295: RELEASE a savepoint.  The since-savepoint undo entries merge into
   the enclosing scope, so [schema_undo] is untouched — only the marker (and any
   newer markers, including their recorded poison snapshots) is dropped
   (mirroring [Store.savepoint_release]).  The poison flag itself is left as-is:
   a poison raised since the savepoint survives the RELEASE into the enclosing
   scope.  An outer ROLLBACK still unwinds the merged entries.  Unknown name:
   no-op. *)
let savepoint_release_schema t name = Schema_cache.savepoint_release t.sc name

(* Write a table's catalog rows (primary + columns + mirror) through [tx] and
   return its meta.  Shared by the autocommit and in-transaction paths. *)
let put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid =
  let m =
    { name
    ; storage =
        Row { tree_id = tid; next_rowid = empty_next_rowid; without_rowid; autoincrement }
    ; columns
    ; fk_constraints = []
    }
  in
  let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m) in
  let%lwt () =
    Lwt_list.iteri_s
      (fun i col -> S.put tx sys_columns_tid (column_key name i) (encode_column col))
      columns
  in
  let%lwt () = put_mirror_tx tx m in
  Lwt.return m
;;

(* [?txn]: when an explicit transaction is active the executor threads it here
   (#269) so the table's catalog rows AND its tree-ID allocation participate in
   that transaction — no nested [rw_begin] (which would self-deadlock), and a
   [ROLLBACK] discards both the rows and the cache entry.  When absent, the
   autocommit path opens and commits its own writer txn (counter bump first, as
   before). *)
let create_table ?txn t ~name ~columns ~without_rowid ~autoincrement =
  if Schema_cache.mem_table t.sc name
  then failwith (Printf.sprintf "table '%s' already exists" name);
  match txn with
  | Some tx ->
    let%lwt tid = next_user_tid_tx tx in
    let%lwt m = put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid in
    (* [put_table] also stamps the store-global in-memory [tree_tags] for [tid].
       On ROLLBACK we revert only the cache entry, not the tag — but that is
       safe: the txn also rolls back [tid]'s allocation (the user-tid counter is
       restored), leaving [tid] unallocated, so no table_meta references it and
       nothing writes pages to it.  The next CREATE reuses [tid] and overwrites
       the tag.  A lingering stamp for an unreferenced tree therefore cannot
       mis-stamp any page (#174). *)
    Schema_cache.put_table t.sc ~name m;
    Lwt.return tid
  | None ->
    let%lwt tid = next_user_tid t in
    let%lwt tx = S.rw_begin t.store in
    let%lwt m = put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid in
    let%lwt () = S.commit tx in
    Schema_cache.put_table_durable t.sc ~name m;
    Lwt.return tid
;;

let create_columnstore_table ?txn t ~name ~columns =
  if Schema_cache.mem_table t.sc name
  then failwith (Printf.sprintf "table '%s' already exists" name);
  let write_rows col_store tid tx =
    let m = { name; storage = Columnar (col_store, tid); columns; fk_constraints = [] } in
    let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m) in
    let%lwt () =
      Lwt_list.iteri_s
        (fun i col -> S.put tx sys_columns_tid (column_key name i) (encode_column col))
        columns
    in
    Lwt.return m
  in
  match txn with
  | Some tx ->
    let%lwt tid = next_user_tid_tx tx in
    let col_store = Granary_columnar.Col_store.create columns in
    let%lwt m = write_rows col_store tid tx in
    Schema_cache.put_table t.sc ~name m;
    Lwt.return ()
  | None ->
    let%lwt tx = S.rw_begin t.store in
    let%lwt tid = next_user_tid_tx tx in
    let col_store = Granary_columnar.Col_store.create columns in
    let%lwt m = write_rows col_store tid tx in
    let%lwt () = S.commit tx in
    Schema_cache.put_table_durable t.sc ~name m;
    Lwt.return ()
;;

let find_table t ~name = Lwt.return (Schema_cache.find_table t.sc name)
let find_table_cached t ~name = Schema_cache.find_table t.sc name

let table_fingerprint t ~name =
  Option.map fingerprint_of_meta (Schema_cache.find_table t.sc name)
;;

let fingerprints_by_tree_id t =
  Schema_cache.fold_tables
    (fun _ (m : table_meta) acc ->
       (* Skip the ephemeral CTE sentinel (tree_id = -1): no real on-disk tree. *)
       match m.storage with
       | Columnar _ -> acc
       | Row { tree_id; _ } when tree_id >= 0 -> (tree_id, fingerprint_of_meta m) :: acc
       | Row _ -> acc)
    t.sc
    []
;;

(* All [(tree_id, fingerprint)] pairs recorded in the mirror. *)
let mirror_fingerprints t =
  S.with_ro t.store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_mirror_tid in
  let _sr = S.cursor_first cur in
  let acc = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (_k, v) ->
      (try
         let m, fp = decode_mirror_entry v in
         let tid = tid_of_storage m.storage in
         acc := (tid, fp) :: !acc
       with
       | Invalid_argument _ | Failure _ -> ());
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return !acc
;;

type schema_discrepancy =
  | Fingerprint_mismatch of
      { tree_id : S.tree_id
      ; primary : int64
      ; mirror : int64
      }
  | Missing_in_mirror of S.tree_id
  | Missing_in_primary of S.tree_id

let verify_against_mirror t =
  let%lwt mirror = mirror_fingerprints t in
  let primary = fingerprints_by_tree_id t in
  let findings = ref [] in
  List.iter
    (fun (tid, pfp) ->
       match List.assoc_opt tid mirror with
       | None -> findings := Missing_in_mirror tid :: !findings
       | Some mfp ->
         if not (Int64.equal pfp mfp)
         then
           findings
           := Fingerprint_mismatch { tree_id = tid; primary = pfp; mirror = mfp }
              :: !findings)
    primary;
  List.iter
    (fun (tid, _) ->
       if not (List.mem_assoc tid primary)
       then findings := Missing_in_primary tid :: !findings)
    mirror;
  Lwt.return (List.rev !findings)
;;

let register_ephemeral t (meta : table_meta) =
  Schema_cache.put_table_durable t.sc ~name:meta.name meta
;;

let unregister_ephemeral t ~name = Schema_cache.remove_table_durable t.sc ~name
let all_tables t = Schema_cache.fold_tables (fun _ v acc -> v :: acc) t.sc []
let list_tables t = Lwt.return (all_tables t)

let persist_dirty_columnar_stores t (tx : S.rw S.txn) =
  let tables = all_tables t in
  let%lwt saved =
    Lwt_list.filter_map_s
      (fun (m : table_meta) ->
         match m.storage with
         | Columnar (cs, tid) when Granary_columnar.Col_store.dirty cs ->
           let%lwt () = Granary_columnar.Persist.save tx tid cs in
           Lwt.return_some cs
         | _ -> Lwt.return_none)
      tables
  in
  Lwt.return saved
;;

let load_columnar_stores_with_txn t (tx : _ S.txn) =
  let tables = all_tables t in
  Lwt_list.iter_s
    (fun (m : table_meta) ->
       match m.storage with
       | Columnar (_, tid) ->
         let%lwt loaded = Granary_columnar.Persist.load tx tid m.columns in
         let cs =
           match loaded with
           | Some cs -> cs
           | None -> Granary_columnar.Col_store.create m.columns
         in
         let m' = { m with storage = Columnar (cs, tid) } in
         Schema_cache.put_table_durable t.sc ~name:m.name m';
         Lwt.return_unit
       | _ -> Lwt.return_unit)
    tables
;;

let load_columnar_stores t store = S.with_ro store (load_columnar_stores_with_txn t)

(* #250: pick the rowid to auto-allocate for a NULL/omitted id, and the new
   counter.  An unseeded table ([empty_next_rowid]) allocates 1 (SQLite: empty
   table -> rowid 1); a seeded one allocates the running [max+1] counter.  The
   new counter is [id+1], except at the [max_int] ceiling we hold at [max_int]
   rather than wrap to [Int64.min_int] (which is the empty sentinel) — a further
   NULL insert then re-tries [max_int] and collides, instead of silently
   resetting the table to "empty". *)
let alloc_rowid (next_rowid : int64) : int64 * int64 =
  let id = if Int64.equal next_rowid empty_next_rowid then 1L else next_rowid in
  let next = if Int64.equal id Int64.max_int then Int64.max_int else Int64.add id 1L in
  id, next
;;

(* #632: the autocommit allocator.  The read-modify-write of the counter used to
   straddle [rw_begin]: find -> alloc -> [set_rowid_durable] -> rw_begin ->
   put -> commit.  That was defensible while each catalog owned its own counter,
   but since #589 [set_rowid_durable] publishes into the store-wide table, so the
   window between the publish and the commit is a window in which the allocation
   is visible to every other handle and yet has not happened:

   - another handle can take the writer lock while this fiber is parked in
     [rw_begin], ROLLBACK, and LOWER the shared counter through #293's
     post-rollback recompute — discarding an allocation already handed out, so
     the next allocation collides with the row this fiber is about to write
     (#589's symptom by a different route); and
   - if the commit itself fails, the counter stays advanced for a write that
     never landed.

   Same shape as #223 (a WAL counter lost-update found by Jepsen): a read-modify-
   write straddling a lock acquisition.  The fix is to take the lock FIRST and do
   the entire read-modify-write under it.

   PUBLISHING AFTER [S.commit] WOULD NOT BE UNDER THE LOCK, and is the trap this
   function fell into once (PR #650 review).  [S.commit] releases the writer lock
   BEFORE its promise resolves: [commit_wal] calls [unlock_once ()]
   (store.ml:2071) and only then awaits [group_commit_sync]'s fsync, and the
   non-WAL btree arm releases from a [Lwt.finalize] handler (store.ml:2184).  So
   a continuation bound with [let%lwt () = S.commit tx in ...] runs after other
   fibers have had the chance to take the lock and read the counter.  Publishing
   there leaves the shared counter too LOW for the whole duration of the fsync —
   another fiber's [next_rowid_in_txn] reads the stale value and allocates the
   same id, which is #589's symptom.  Too LOW is the dangerous direction; too
   HIGH only skips ids.  (The in-memory backend hides this completely — its
   commit releases and returns an already-resolved promise, store.ml:2164 — so
   an in-memory test cannot tell the two orderings apart.  That is why
   [wal_two_fiber_*] in test_rowid_counter_ownership_632.ml are WAL-mode.)

   So the publish sits immediately after the allocation: still under the lock,
   and with no Lwt yield point at all between reading the counter and writing it
   back, which makes the read-modify-write atomic against other fibers on both
   counts.  The residual is "the commit fails => the counter is too high", the
   safe direction, and exactly what the pre-#632 code already lived with.

   The unknown-table check stays SYNCHRONOUS (it raises before any [Lwt.t] is
   constructed) because that is the documented behaviour and [test_catalog]'s
   [test_rowid_unknown_table] pins it. *)
let next_rowid t ~name =
  if not (Schema_cache.mem_table t.sc name)
  then failwith (Printf.sprintf "no table '%s'" name);
  let%lwt tx = S.rw_begin t.store in
  match Schema_cache.find_table t.sc name with
  | None ->
    (* Unreachable: nothing can drop the table between the check above and here
       (no yield point but [rw_begin], and DDL needs the very lock we hold). *)
    let%lwt () = S.rollback tx in
    Lwt.fail_with (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let tree_id, nrid, without_rowid, autoincrement = row_storage m in
    let id, next = alloc_rowid nrid in
    let m' =
      { m with
        storage = Row { tree_id; next_rowid = next; without_rowid; autoincrement }
      }
    in
    (* #632: publish HERE — under the writer lock, and with no yield between the
       read and the write.  Not after [S.commit]; see the note above. *)
    Schema_cache.set_rowid_durable t.sc ~name m';
    let%lwt () = put_table_counter_tx tx m' in
    let%lwt () = S.commit tx in
    Lwt.return id
;;

(** Like [next_rowid] but uses an already-acquired RW transaction.
    The txn is NOT committed; the caller is responsible for the commit.
    Use this when an explicit transaction is already held to avoid
    deadlocking on the store's RW mutex. *)
let next_rowid_in_txn ?(defer_counter = false) t ~name (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let tree_id, next_rowid, without_rowid, autoincrement = row_storage m in
    if autoincrement && Int64.equal next_rowid Int64.max_int
    then Lwt.fail_with "database or disk is full"
    else (
      let id, next = alloc_rowid next_rowid in
      let m' =
        { m with
          storage = Row { tree_id; next_rowid = next; without_rowid; autoincrement }
        }
      in
      (* #293: [bump_rowid] caches [m'] and marks this table's counter dirty so a
         ROLLBACK recomputes only it.
         #347: when [defer_counter] (explicit txn), skip the per-row B-tree write
         to sys_tables — [flush_dirty_counters_tx] writes it once at COMMIT. *)
      Schema_cache.bump_rowid t.sc ~name m';
      if defer_counter
      then Lwt.return id
      else (
        let%lwt () = put_table_counter_tx tx m' in
        Lwt.return id))
;;

(** #243 (T1): after an INSERT supplies an explicit INTEGER PRIMARY KEY value,
    advance the autoincrement counter so a later NULL/omitted insert receives a
    fresh, non-colliding id (SQLite parity: rowid becomes max(existing)+1).
    [at_least] is the smallest value the next allocation must be (= id + 1).

    #250: an UNSEEDED table ([empty_next_rowid]) seeds directly from [at_least],
    even when that is <= 1 — the explicit id IS the table's max, so a below-1 id
    (e.g. -5) correctly makes the next NULL insert -4 instead of 1.  A seeded
    counter only ever rises and never lowers.  [at_least] is always [id+1] with
    [id < max_int] (the caller guards the ceiling), so it can never be the empty
    sentinel. *)
let bump_next_rowid_in_txn ?(defer_counter = false) t ~name ~at_least (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let tree_id, next_rowid, without_rowid, autoincrement = row_storage m in
    let unseeded = Int64.equal next_rowid empty_next_rowid in
    if (not unseeded) && Int64.compare at_least next_rowid <= 0
    then Lwt.return_unit
    else (
      let m' =
        { m with
          storage = Row { tree_id; next_rowid = at_least; without_rowid; autoincrement }
        }
      in
      (* #293: [bump_rowid] caches [m'] and marks dirty only when the counter
         actually moved (the early-return no-op above leaves the cached counter
         untouched, so nothing to recompute). *)
      Schema_cache.bump_rowid t.sc ~name m';
      (* #314: [put_table_counter_tx] mirrors the high-water for AUTOINCREMENT
         tables.  The no-op early-return path above never reaches here, so a
         non-moving bump leaves both primary and mirror untouched.
         #347: when [defer_counter] (explicit txn), skip per-row write — flushed once at COMMIT. *)
      if defer_counter then Lwt.return_unit else put_table_counter_tx tx m')
;;

(* #312.1: largest stored rowid in a table's data tree, computed within an
   already-open txn.  Used only on the uncommon lower-clamp path of a writable
   [sqlite_sequence] SET/INSERT, to avoid lowering the counter below the live
   max(rowid).  The store has no [cursor_last]/[cursor_prev], so this reuses the
   forward walk from [recover_next_rowid]: [Rowid.encode]'s offset-binary
   encoding sorts integer rowids correctly, so the last key in byte order is the
   maximum.  Returns [None] for an empty tree. *)
let max_rowid_in_txn t ~name (tx : 'a S.txn) : int64 option Lwt.t =
  match Schema_cache.find_table t.sc name with
  (* Private helper; the sole caller ([set_next_rowid_in_txn]) has already
     confirmed the table is present, so this arm is unreachable in practice. *)
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let tree_id =
      match m.storage with
      | Row { tree_id; _ } -> tree_id
      | Columnar _ -> failwith "max_rowid_in_txn on columnar table"
    in
    let%lwt cur = S.cursor_open tx tree_id in
    let _sr = S.cursor_first cur in
    let max_key = ref None in
    let rec walk () =
      match S.cursor_next cur with
      | None -> ()
      | Some (k, _) ->
        max_key := Some k;
        walk ()
    in
    walk ();
    S.cursor_close cur;
    Lwt.return
      (match !max_key with
       | None -> None
       | Some k -> Some (Rowid.decode k))
;;

(* #409: SQLite keeps no persisted counter for a plain (non-AUTOINCREMENT)
   rowid table — it derives the next rowid as [max(rowid) + 1] over the LIVE
   rows, so a committed DELETE of the current maximum makes that rowid reusable.
   Our engine caches a high-water [next_rowid] that [bump_rowid] only ever
   raises, so without this hook the deleted high-water survives and the rowid is
   never reused (diverging from SQLite in both the autocommit and explicit-txn
   paths).

   When a plain rowid table's CURRENT high-water row ([next_rowid - 1]) is
   deleted, recompute the counter from the live tree — within the caller's [tx],
   so [max_rowid_in_txn] sees the just-applied delete (read-your-own-writes)
   whether the surrounding statement autocommits or runs inside an explicit
   transaction — and persist it (primary row via [put_table_counter_tx]; marked
   dirty via [bump_rowid] so a ROLLBACK reverts it like any other counter move).

   AUTOINCREMENT is intentionally skipped: its high-water is sticky across
   committed deletes (#299/#314).  The cheap [rowid = next_rowid - 1] guard means
   this only fires on the rare delete-of-max, so ordinary deletes and the O(1)
   cached INSERT fast path are untouched. *)
let note_rowid_deleted t ~name ~rowid (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> Lwt.return_unit
  | Some m ->
    (match m.storage with
     | Row { tree_id; next_rowid; without_rowid = false; autoincrement = false }
       when (not (Int64.equal next_rowid empty_next_rowid))
            && Int64.equal rowid (Int64.sub next_rowid 1L) ->
       let%lwt mx = max_rowid_in_txn t ~name tx in
       let recovered =
         match mx with
         | None -> empty_next_rowid
         | Some k -> Int64.add k 1L
       in
       if Int64.equal recovered next_rowid
       then Lwt.return_unit
       else (
         let m' =
           { m with
             storage =
               Row
                 { tree_id
                 ; next_rowid = recovered
                 ; without_rowid = false
                 ; autoincrement = false
                 }
           }
         in
         Schema_cache.bump_rowid t.sc ~name m';
         put_table_counter_tx tx m')
     | _ -> Lwt.return_unit)
;;

(* #312.1: writable [sqlite_sequence] SET/INSERT for table [name] with the
   requested seq value [requested].  Faithful to SQLite's effective rule
   [next = max(requested, max(rowid)) + 1]: the new counter is
   [max(requested + 1, max(rowid) + 1)].
   - RAISE path (the common case): when [requested + 1 >= next_rowid] and the
     counter is already seeded, [next_rowid] already equals [max(rowid) + 1], so
     just set [next_rowid := requested + 1] — no tree scan needed.
   - LOWER path: when [requested + 1] would drop below the counter (or the
     counter is unseeded), clamp to [max(rowid) + 1] so the next insert never
     collides with a live row.
   Mutates through [tx] via [put_table_counter_tx] (primary row + #314 mirror)
   and marks the counter dirty ([Schema_cache.bump_rowid]) so a ROLLBACK reverts
   it via [recompute_rowid_counters_after_rollback].  The table must exist and
   be AUTOINCREMENT. *)
let set_next_rowid_in_txn t ~name ~requested (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "sqlite_sequence: no such table '%s'" name)
  | Some m ->
    let tree_id, next_rowid, without_rowid, autoincrement = row_storage m in
    if not autoincrement
    then
      failwith (Printf.sprintf "sqlite_sequence: '%s' is not an AUTOINCREMENT table" name)
    else (
      let want_next =
        if Int64.equal requested Int64.max_int
        then Int64.max_int
        else Int64.add requested 1L
      in
      let%lwt clamped =
        if
          (not (Int64.equal next_rowid empty_next_rowid))
          && Int64.compare want_next next_rowid >= 0
        then Lwt.return want_next
        else (
          let%lwt mx = max_rowid_in_txn t ~name tx in
          let floor =
            match mx with
            | Some k -> Int64.add k 1L
            | None -> 1L
          in
          Lwt.return (if Int64.compare want_next floor > 0 then want_next else floor))
      in
      let m' =
        { m with
          storage = Row { tree_id; next_rowid = clamped; without_rowid; autoincrement }
        }
      in
      Schema_cache.bump_rowid t.sc ~name m';
      put_table_counter_tx tx m')
;;

(* #312.1: writable [sqlite_sequence] DELETE for table [name].  Resets the
   counter to [empty_next_rowid] so the next insert recomputes from data —
   matching SQLite removing the [sqlite_sequence] row.  Mutates through [tx] and
   marks the counter dirty so a ROLLBACK reverts it.  The table must exist and be
   AUTOINCREMENT. *)
let reset_next_rowid_in_txn t ~name (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "sqlite_sequence: no such table '%s'" name)
  | Some m ->
    let tree_id, _nrid, without_rowid, autoincrement = row_storage m in
    if not autoincrement
    then
      failwith (Printf.sprintf "sqlite_sequence: '%s' is not an AUTOINCREMENT table" name)
    else (
      let m' =
        { m with
          storage =
            Row { tree_id; next_rowid = empty_next_rowid; without_rowid; autoincrement }
        }
      in
      Schema_cache.bump_rowid t.sc ~name m';
      put_table_counter_tx tx m')
;;

(* #312.1: [DELETE FROM sqlite_sequence] with no WHERE — reset EVERY seeded
   AUTOINCREMENT counter (SQLite parity, and what a real [sqlite3 .dump] emits
   before re-INSERTing).  Non-AUTOINCREMENT and already-unseeded tables are left
   untouched (no spurious mirror writes). *)
let reset_all_next_rowid_in_txn t (tx : S.rw S.txn) =
  let%lwt tables = list_tables t in
  Lwt_list.iter_s
    (fun (m : table_meta) ->
       match m.storage with
       | Columnar _ -> Lwt.return_unit
       | Row { tree_id; next_rowid = nrid; without_rowid; autoincrement }
         when autoincrement && not (Int64.equal nrid empty_next_rowid) ->
         let m' =
           { m with
             storage =
               Row
                 { tree_id; next_rowid = empty_next_rowid; without_rowid; autoincrement }
           }
         in
         Schema_cache.bump_rowid t.sc ~name:m.name m';
         put_table_counter_tx tx m'
       | _ -> Lwt.return_unit)
    tables
;;

(* #347: flush all dirty rowid counters into the B-tree under [tx], called once
   at COMMIT to replace the per-row [put_table_counter_tx] writes that
   [next_rowid_in_txn]/[bump_next_rowid_in_txn] skip when [~defer_counter:true].
   Uses [take_rowid_bumped] which clears the dirty set; [commit_schema_changes]'s
   subsequent reset is a no-op on the already-empty table. *)
let flush_dirty_counters_tx t (tx : S.rw S.txn) =
  let names = Schema_cache.take_rowid_bumped t.sc in
  Lwt_list.iter_s
    (fun name ->
       match Schema_cache.find_table t.sc name with
       | None -> Lwt.return_unit
       | Some m -> put_table_counter_tx tx m)
    names
;;

(* [?txn]: as for [create_table] (#269), an active explicit transaction is
   threaded here so the index's catalog row, tree-ID and index-ID allocation,
   and cache entry all participate in it and roll back together. *)
let create_index ?txn t ~name ~table ~columns ~unique ~expr_flags ~where_sql ~origin =
  if Schema_cache.mem_index t.sc name
  then Lwt.return (Error (Printf.sprintf "index '%s' already exists" name))
  else (
    match Schema_cache.find_table t.sc table with
    | None -> Lwt.return (Error (Printf.sprintf "no table '%s'" table))
    | Some tm ->
      (* Validate: for plain columns, check they exist in the table; skip for expression columns *)
      let col_with_flags = List.combine columns expr_flags in
      let missing =
        List.find_opt
          (fun (col, is_expr) ->
             (not is_expr)
             && not (List.exists (fun (c : Row.column) -> c.name = col) tm.columns))
          col_with_flags
      in
      (match missing with
       | Some (col, _) ->
         Lwt.return (Error (Printf.sprintf "no column '%s' on table '%s'" col table))
       | None ->
         let mk_info tid =
           { idx_name = name
           ; idx_table = table
           ; idx_columns = columns
           ; idx_unique = unique
           ; idx_tree_id = tid
           ; idx_expr_flags = expr_flags
           ; idx_where_sql = where_sql
           ; idx_origin = origin
           }
         in
         (match txn with
          | Some tx ->
            let%lwt tid = next_user_tid_tx tx in
            let%lwt id = read_next_index_id_tx tx in
            let%lwt () = write_next_index_id_tx tx (id + 1) in
            let info = mk_info tid in
            let%lwt () =
              S.put tx sys_indexes_tid (index_key id) (encode_index_value info)
            in
            Schema_cache.put_index t.sc ~name info;
            Lwt.return (Ok info)
          | None ->
            let%lwt tid = next_user_tid t in
            let%lwt id = read_next_index_id t.store in
            let%lwt () = write_next_index_id t.store (id + 1) in
            let info = mk_info tid in
            let%lwt tx = S.rw_begin t.store in
            let%lwt () =
              S.put tx sys_indexes_tid (index_key id) (encode_index_value info)
            in
            let%lwt () = S.commit tx in
            Schema_cache.put_index_durable t.sc ~name info;
            Lwt.return (Ok info))))
;;

(* [?txn] (#282): when an explicit transaction is active the executor threads it
   here so the column's catalog row and mirror refresh participate in it (no
   nested [rw_begin], which would self-deadlock against the writer lock the txn
   already holds).  A schema-cache undo restores the prior [table_meta] AND its
   tree-tag fingerprint on [ROLLBACK] — unlike a fresh CREATE the tree_id stays
   allocated, so the #174 page-stamp must revert to the old schema too. *)
let add_column ?txn t ~table_name ~(column : Row.column) =
  match Schema_cache.find_table t.sc table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let exists =
      List.exists (fun c -> String.equal c.Row.name column.Row.name) meta.columns
    in
    if exists
    then Lwt.return (Error (Printf.sprintf "column already exists: %s" column.Row.name))
    else (
      let new_cols = meta.columns @ [ column ] in
      let new_meta = { meta with columns = new_cols } in
      let ordinal = List.length meta.columns in
      let col_k = column_key table_name ordinal in
      let col_v = encode_column column in
      let%lwt () =
        borrow_or_autocommit ?txn t.store (fun tx ->
          let%lwt () = S.put tx sys_columns_tid col_k col_v in
          put_mirror_tx tx new_meta)
      in
      (match txn with
       | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
       | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta);
      Lwt.return (Ok ()))
;;

let indexes_for_table t ~table = Schema_cache.indexes_for_table t.sc ~table
let find_index t ~name = Schema_cache.find_index t.sc name

let find_index_covering_cols t ~table_name ~col_idxs =
  match Schema_cache.find_table t.sc table_name with
  | None -> None
  | Some meta ->
    let n_target = List.length col_idxs in
    if n_target = 0
    then None
    else (
      (* Resolve col_idxs to column names; bail out if any idx is out of range. *)
      let cols_arr = Array.of_list meta.columns in
      let n_cols = Array.length cols_arr in
      let target_names_opt =
        try
          Some
            (List.map
               (fun i ->
                  if i < 0 || i >= n_cols then raise Exit else cols_arr.(i).Row.name)
               col_idxs)
        with
        | Exit -> None
      in
      match target_names_opt with
      | None -> None
      | Some target_names ->
        let candidates = indexes_for_table t ~table:table_name in
        List.find_opt
          (fun (i : index_info) ->
             (* Skip partial indexes — a row absent from the index may still
             satisfy the FK predicate (the WHERE clause masks rows). *)
             if i.idx_where_sql <> None
             then false
             else (
               (* Skip indexes that contain any expression column in the leading
               prefix we'd be scanning — we cannot match a raw value list
               against an expression key. *)
               let n_idx = List.length i.idx_columns in
               if n_idx < n_target
               then false
               else (
                 let prefix_names =
                   List.filteri (fun k _ -> k < n_target) i.idx_columns
                 in
                 let prefix_flags =
                   let len_flags = List.length i.idx_expr_flags in
                   if len_flags = 0
                   then List.init n_target (fun _ -> false)
                   else List.filteri (fun k _ -> k < n_target) i.idx_expr_flags
                 in
                 let no_expr_in_prefix = not (List.exists Fun.id prefix_flags) in
                 no_expr_in_prefix
                 &&
                 try List.for_all2 String.equal prefix_names target_names with
                 | Invalid_argument _ -> false)))
          candidates)
;;

let table_exists t ~name = Schema_cache.mem_table t.sc name
let index_exists t ~name = Schema_cache.mem_index t.sc name

(** Scan _sys_indexes (using the given txn) to find the key for [name].
    Returns [None] if not found.

    [cursor_first] positions at the first entry (id=0 when it exists).
    We inspect that entry immediately via [cursor_next] — which on a
    pre-positioned cursor returns the current entry without advancing —
    so no entry is ever skipped, including the very first one. *)
let find_index_key_in_txn tx name =
  let%lwt cur = S.cursor_open tx sys_indexes_tid in
  (* Position at the first entry; returns Not_found `End if the tree is
     empty, in which case cursor_next will immediately return None. *)
  let _sr = S.cursor_first cur in
  let result = ref None in
  (* cursor_next after cursor_first returns the positioned (first) entry on
     its initial call, then advances on each subsequent call. *)
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      let info = decode_index_value v in
      if info.idx_name = name then result := Some k else walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return !result
;;

let drop_index t tx ~name =
  (* Remove from _sys_indexes on disk by scanning for the numeric key. *)
  let%lwt key_opt = find_index_key_in_txn tx name in
  let%lwt () =
    match key_opt with
    | None -> Lwt.return_unit
    | Some key -> S.del tx sys_indexes_tid key
  in
  (* Update in-memory cache.  [remove_index] self-registers a ROLLBACK restore. *)
  Schema_cache.remove_index t.sc ~name;
  Lwt.return_unit
;;

let drop_table t tx ~name =
  (* 0. Remove the mirror entry (keyed by tree_id), if we know the tree_id. *)
  let%lwt () =
    match Schema_cache.find_table t.sc name with
    | Some m -> del_mirror_tx tx m
    | None -> Lwt.return_unit
  in
  (* 1. Remove table entry from _sys_tables. *)
  let%lwt () = S.del tx sys_tables_tid (Bytes.of_string name) in
  (* 2. Remove all column entries from _sys_columns. *)
  let n_cols =
    match Schema_cache.find_table t.sc name with
    | None -> 0
    | Some m -> List.length m.columns
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun i -> S.del tx sys_columns_tid (column_key name i))
      (List.init n_cols (fun i -> i))
  in
  (* 3. Remove all associated indexes.  Each [drop_index] self-registers its own
     ROLLBACK restore. *)
  let idx_list = indexes_for_table t ~table:name in
  let%lwt () =
    Lwt_list.iter_s
      (fun (idx : index_info) -> drop_index t tx ~name:idx.idx_name)
      idx_list
  in
  (* 4. Update in-memory cache.  [remove_table] self-registers a ROLLBACK restore. *)
  Schema_cache.remove_table t.sc ~name;
  Lwt.return_unit
;;

(* #282: on a corrupt-catalog Error this no longer rolls [tx] back itself — the
   caller decides.  In autocommit the caller aborts its own writer txn; when the
   txn is borrowed from an ambient explicit transaction, aborting it here would
   tear down the user's whole transaction, so we surface the Error and let the
   db layer's [ROLLBACK] (or [with_ddl_txn] in [Auto]) handle teardown. *)
let rekey_table_columns tx ~old_name ~new_name ~n_cols =
  let rec loop i =
    if i >= n_cols
    then Lwt.return (Ok ())
    else (
      let old_k = column_key old_name i in
      let new_k = column_key new_name i in
      let%lwt bytes_opt = S.get tx sys_columns_tid old_k in
      match bytes_opt with
      | None ->
        Lwt.return
          (Error
             (Printf.sprintf "catalog corrupt: column %d missing for table %s" i old_name))
      | Some bytes ->
        let%lwt () = S.del tx sys_columns_tid old_k in
        let%lwt () = S.put tx sys_columns_tid new_k bytes in
        loop (i + 1))
  in
  loop 0
;;

(* ------------------------------------------------------------------ *)
(* #553: remapping the stored references to a renamed column/table      *)
(* ------------------------------------------------------------------ *)

(* Every sys_indexes entry naming [table], as (storage key, decoded info).

   The scan runs THROUGH [tx] (read-your-own-writes) so an index created earlier
   in the same transaction is seen too, not just pre-txn indexes. *)
let indexes_of_table_tx tx ~table =
  let%lwt cur = S.cursor_open tx sys_indexes_tid in
  let _sr = S.cursor_first cur in
  let acc = ref [] in
  let rec scan () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      let info = decode_index_value v in
      if String.equal info.idx_table table then acc := (k, info) :: !acc;
      scan ()
  in
  scan ();
  S.cursor_close cur;
  Lwt.return (List.rev !acc)
;;

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || Char.code c >= 128
;;

let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9') || c = '$'

(* Copy the string literal starting at [i] (which is its opening quote) into
   [buf] verbatim, doubled quotes included; returns the index just past it. *)
let copy_sql_string sql buf i =
  let n = String.length sql in
  let q = sql.[i] in
  Buffer.add_char buf q;
  let rec go k =
    if k >= n
    then k
    else if sql.[k] <> q
    then (
      Buffer.add_char buf sql.[k];
      go (k + 1))
    else if k + 1 < n && sql.[k + 1] = q
    then (
      Buffer.add_char buf q;
      Buffer.add_char buf q;
      go (k + 2))
    else (
      Buffer.add_char buf q;
      k + 1)
  in
  go (i + 1)
;;

(* Is the identifier ENDING at [j] a column reference, and so renameable?  Only
   the character that follows can say otherwise: '(' makes it a function name,
   '\'' makes it a literal prefix ([x'6a'] is a BLOB, not a column), and '.'
   makes it a qualifier (a table, not a column).

   Shared by the bare and the delimited scanner so the two cannot drift: ["t"."a"]
   must keep its qualifier for the same reason [t.a] does. *)
let is_column_ref_at sql j =
  j >= String.length sql
  ||
  match sql.[j] with
  | '(' | '\'' | '.' -> false
  | _ -> true
;;

(* A delimited identifier ("x" or `x`) starting at [i]: renamed when its body
   matches AND it stands in a column position, re-emitted with the same
   delimiter either way. *)
let copy_quoted_ident sql buf i ~old_name ~new_name =
  let n = String.length sql in
  let q = sql.[i] in
  let body = Buffer.create 16 in
  let rec go k =
    if k >= n
    then None
    else if sql.[k] <> q
    then (
      Buffer.add_char body sql.[k];
      go (k + 1))
    else if k + 1 < n && sql.[k + 1] = q
    then (
      Buffer.add_char body q;
      go (k + 2))
    else Some (k + 1)
  in
  match go (i + 1) with
  | None ->
    (* Unterminated — not something we can safely reinterpret; copy verbatim. *)
    Buffer.add_char buf q;
    i + 1
  | Some stop ->
    let text = Buffer.contents body in
    let text =
      if String.equal text old_name && is_column_ref_at sql stop then new_name else text
    in
    Buffer.add_char buf q;
    String.iter
      (fun c ->
         if c = q then Buffer.add_char buf q;
         Buffer.add_char buf c)
      text;
    Buffer.add_char buf q;
    stop
;;

(* #572 / #577: a name that cannot be written bare must be substituted into a
   bare-identifier position in its DELIMITED spelling — [a -> "my col"] would
   otherwise turn [(a > 0)] into [(my col > 0)], and [a -> "order"] into
   [(order > 0)], neither of which parses.  Substituting the delimited spelling
   is always valid wherever a bare word stood.

   #572 shipped a keyword-blind local copy of the predicate here, because the
   catalog sits below the parser and could not reach the lexer's keyword table;
   a reserved word is a plain word, so it passed through bare and left the
   table permanently un-insertable (#577).  #577 hoisted the whole rule —
   keyword list included — into {!Granary_encoding.Sql_ident}, which both this
   module and {!Granary_sql.Ast} depend on, so there is one implementation
   rather than two that can drift.

   Note this uses the LEXER's notion of a bare word ([[A-Za-z_][A-Za-z0-9_]*]),
   which is narrower than {!is_ident_char} above — that one also admits ['$']
   and bytes ≥ 128 so the scanner treats them as part of one word rather than
   splitting mid-name, but neither can appear in a bare identifier. *)
let bare_position_spelling = Granary_encoding.Sql_ident.quote_ident

(* A bare word starting at [i], renamed when it stands in a column position. *)
let copy_bare_ident sql buf i ~old_name ~new_name =
  let n = String.length sql in
  let rec stop k = if k < n && is_ident_char sql.[k] then stop (k + 1) else k in
  let j = stop i in
  let word = String.sub sql i (j - i) in
  Buffer.add_string
    buf
    (if String.equal word old_name && is_column_ref_at sql j
     then bare_position_spelling new_name
     else word);
  j
;;

(* #553: rename every reference to the identifier [old_name] inside stored SQL
   text — a CHECK or GENERATED expression, a partial index's WHERE clause, an
   expression index's column SQL.

   Lexical rather than parse-and-reprint, for two reasons.  The catalog sits
   BELOW the parser in the dependency graph ([granary.sql] depends on
   [granary.catalog], not the reverse), so no AST is reachable from here; and a
   reprint would rewrite text the user wrote and we have no business touching —
   [Ast.expr_to_sql] raises outright on a BLOB literal.  A token scan preserves
   every byte it does not rename.

   Matching is case-sensitive, as every other column lookup in this module is
   ([rename_column], [drop_column] and [clear_pk_flags] all use [String.equal]).

   The scan itself is [rewrite_ident_in_sql] below; its per-token step is
   [rewrite_step], and the two helpers between here and it belong to it. *)

(* #609 review: the index just past the [--] comment starting at [i], or [None]
   when [i] does not start one.  Only [--] — the lexer ([lexer.mll]) has no
   block-comment rule, so a slash-star block comment is not a comment in this
   dialect and must not be treated as one here.

   This exists because the persisted text is the RAW statement, comments and
   all, and an apostrophe inside one — [-- it's the positive rows] — otherwise
   opens a string literal that never closes.  Both scanners below then swallow
   the rest of the statement: the rewriter stops renaming half way through a
   CHECK expression, and the #609 detector passes a definition it should have
   blocked.  Both are silent.  A comment is copied through verbatim and never
   searched, which is also the right answer on its own terms — a name inside a
   comment is not a reference. *)
let line_comment_end sql i =
  let n = String.length sql in
  if i + 1 < n && sql.[i] = '-' && sql.[i + 1] = '-'
  then (
    let rec eol k = if k < n && sql.[k] <> '\n' then eol (k + 1) else k in
    Some (eol (i + 2)))
  else None
;;

(* One step of the rewrite scan: consume the token at [i], appending its
   (possibly renamed) text to [buf], and return the index just past it.  Lifted
   out of [rewrite_ident_in_sql] so the comment case above can be added without
   pushing the loop past merlint's nesting limit. *)
let rewrite_step sql buf i ~old_name ~new_name =
  match line_comment_end sql i with
  | Some j ->
    Buffer.add_string buf (String.sub sql i (j - i));
    j
  | None ->
    let c = sql.[i] in
    if c = '\''
    then copy_sql_string sql buf i
    else if c = '"' || c = '`'
    then copy_quoted_ident sql buf i ~old_name ~new_name
    else if is_ident_start c
    then copy_bare_ident sql buf i ~old_name ~new_name
    else (
      Buffer.add_char buf c;
      i + 1)
;;

let rewrite_ident_in_sql ~old_name ~new_name sql =
  if String.equal old_name new_name
  then sql
  else (
    let n = String.length sql in
    let buf = Buffer.create (n + 16) in
    let rec go i =
      if i >= n then () else go (rewrite_step sql buf i ~old_name ~new_name)
    in
    go 0;
    Buffer.contents buf)
;;

(* The string literal starting at [i] (its opening quote): the index just past
   it.  [copy_sql_string] without the buffer — the detector below only needs to
   step over a literal, never to reproduce it. *)
let skip_sql_string sql i =
  let n = String.length sql in
  let q = sql.[i] in
  let rec go k =
    if k >= n
    then k
    else if sql.[k] <> q
    then go (k + 1)
    else if k + 1 < n && sql.[k + 1] = q
    then go (k + 2)
    else k + 1
  in
  go (i + 1)
;;

(* The delimited identifier starting at [i]: its undoubled body and the index
   just past the closing delimiter.  An unterminated one yields the rest of the
   text, which cannot equal any identifier we look for and so is simply skipped. *)
let read_quoted_ident sql i =
  let n = String.length sql in
  let q = sql.[i] in
  let body = Buffer.create 16 in
  let rec go k =
    if k >= n
    then Buffer.contents body, k
    else if sql.[k] <> q
    then (
      Buffer.add_char body sql.[k];
      go (k + 1))
    else if k + 1 < n && sql.[k + 1] = q
    then (
      Buffer.add_char body q;
      go (k + 2))
    else Buffer.contents body, k + 1
  in
  go (i + 1)
;;

(* The bracket-delimited identifier starting at [i] ([[my col]]): its undoubled
   body and the index just past the closing bracket.  [lexer.mll:295] accepts
   this as a third identifier delimiter, with []]] as the doubling escape, so a
   detector that ignored it would read [[my col]] as the two bare words [my] and
   [col] and never match the column it names — a false negative, the direction
   this whole scan exists to avoid.  {!rewrite_ident_in_sql} still does not
   handle brackets; it would have to re-emit them, which is a rewriter change
   (#609 review). *)
let read_bracket_ident sql i =
  let n = String.length sql in
  let body = Buffer.create 16 in
  let rec go k =
    if k >= n
    then Buffer.contents body, k
    else if sql.[k] <> ']'
    then (
      Buffer.add_char body sql.[k];
      go (k + 1))
    else if k + 1 < n && sql.[k + 1] = ']'
    then (
      Buffer.add_char body ']';
      go (k + 2))
    else Buffer.contents body, k + 1
  in
  go (i + 1)
;;

(* The bare word starting at [i]: the index just past it. *)
let bare_ident_end sql i =
  let n = String.length sql in
  let rec go k = if k < n && is_ident_char sql.[k] then go (k + 1) else k in
  go i
;;

(* One step of the detection scan: does the token at [i] satisfy [hit], and
   where does it end?  Separate from the loop for the same nesting reason as
   [rewrite_step]. *)
let mentions_step sql i ~hit =
  match line_comment_end sql i with
  | Some j -> false, j
  | None ->
    let c = sql.[i] in
    if c = '\''
    then false, skip_sql_string sql i
    else if c = '"' || c = '`'
    then (
      let text, j = read_quoted_ident sql i in
      hit text, j)
    else if c = '['
    then (
      let text, j = read_bracket_ident sql i in
      hit text, j)
    else if is_ident_start c
    then (
      let j = bare_ident_end sql i in
      hit (String.sub sql i (j - i)), j)
    else false, i + 1
;;

(* #609: does [sql] name [ident] as an identifier token — bare, double-quoted,
   backtick- or bracket-delimited — anywhere outside a string literal or a
   comment?

   This is a DETECTOR guarding a refusal, not a rewriter, and its two failure
   modes are not symmetric: a false positive costs the user a rename they could
   have had and tells them exactly why, a false negative silently leaves a view
   or trigger naming a column that no longer exists.  So it deliberately differs
   from {!rewrite_ident_in_sql} in three ways, all erring towards refusing:

   - **Position-blind.**  [is_column_ref_at] excludes a word followed by ['.'],
     which is precisely where a TABLE name stands ([v0.a]).  A detector wearing
     the rewriter's column-position filter would miss every qualified reference
     — the common spelling inside a view body.
   - **Case-insensitive.**  The rewriter matches case-sensitively because every
     other column lookup in this module does; a detector that did would let
     [SELECT A FROM v0] through.
   - **Bracket-aware.**  See {!read_bracket_ident}.

   It is also ROLE-blind, and that is a real cost, not just a caveat: an
   identifier-shaped token counts wherever it stands, so a function name
   (a [COUNT] call against a column named [count]) or a table ALIAS spelled like the
   renamed table ([FROM other AS t]) is indistinguishable from a genuine
   reference and will refuse a rename that was in fact safe.  The refusal is
   loud and names the object, which is the side this design errs to; making it
   precise needs the parser, i.e. the same work that option (a) needs. *)
let sql_mentions_ident ~ident sql =
  let n = String.length sql in
  let want = String.lowercase_ascii ident in
  let hit s = String.equal (String.lowercase_ascii s) want in
  let rec go i =
    if i >= n
    then false
    else (
      let found, j = mentions_step sql i ~hit in
      found || go j)
  in
  go 0
;;

(* #609: every stored view / reactive-view / trigger definition, as
   [(kind, name, sql)].  Views and triggers are persisted as raw CREATE ... SQL
   TEXT (see [sys_views_tid], [sys_reactive_views_tid], [sys_triggers_tid]) —
   there is no AST here to walk and re-render, and the catalog sits BELOW the
   parser in the dependency graph, so there cannot be one.  A lexical rewrite of
   that text cannot scope a name to a table the way SQLite's does: a view body
   legitimately names other tables' columns, and rewriting one of those turns a
   working view into a wrong one silently.  So a rename that would touch such a
   definition is REFUSED rather than guessed at. *)
let load_definitions_tx tx =
  let scan tid kind =
    let%lwt pairs = load_all_pairs_in_tx tx tid in
    Lwt.return (List.map (fun (name, sql) -> kind, name, sql) pairs)
  in
  let%lwt views = scan sys_views_tid "view" in
  let%lwt rviews = scan sys_reactive_views_tid "reactive view" in
  let%lwt triggers = scan sys_triggers_tid "trigger" in
  Lwt.return (views @ rviews @ triggers)
;;

let describe_definition kind name = Printf.sprintf "%s %s" kind name

(* #609 review: the lowercased names a rename of [root] can be seen through —
   [root] itself, plus every definition whose text names something already in
   the set, to a fixpoint.

   The first cut of this gate required the table name AND the column name in the
   SAME stored text, and claimed that could not under-refuse because "a
   definition that reaches the column only through another view is blocked
   transitively, because that other view names the table itself".  That is
   FALSE when the intervening view projects [*]:

   {v
     CREATE TABLE t (a INTEGER, b INTEGER);
     CREATE VIEW  v AS SELECT * FROM t;   -- names t, never a
     CREATE VIEW  w AS SELECT a FROM v;   -- names a, never t
     ALTER TABLE t RENAME COLUMN a TO z;  -- neither had BOTH: gate passed
     SELECT * FROM w;                     -- unknown column: a
   v}

   [db.ml] persists the raw statement, so [SELECT *] is stored unexpanded, and
   the residue was #609's own symptom: a view left silently dead whose dumped
   DDL restores dead.  Closing over the intermediate names fixes it — [v] names
   [t] so [v] joins the set, and [w] names [v] and [a] so [w] blocks.

   Note [v] itself is correctly NOT blocked: it never spells [a], and a [*]
   projection re-expands on the next bind, so the rename leaves it working. *)
let reachable_names defs ~root =
  let low = String.lowercase_ascii in
  let rec grow reach =
    let extra =
      List.filter_map
        (fun (_kind, name, sql) ->
           if List.mem (low name) reach
           then None
           else if List.exists (fun ident -> sql_mentions_ident ~ident sql) reach
           then Some (low name)
           else None)
        defs
    in
    if extra = [] then reach else grow (List.sort_uniq String.compare (extra @ reach))
  in
  grow [ low root ]
;;

(* #609: definitions that name [table], for a table rename.  No closure is
   needed here — anything that reaches [table] indirectly does so through a
   definition that names it directly, and that one blocks. *)
let table_dependents_tx tx ~table =
  let%lwt defs = load_definitions_tx tx in
  Lwt.return
    (List.filter_map
       (fun (kind, name, sql) ->
          if sql_mentions_ident ~ident:table sql
          then Some (describe_definition kind name)
          else None)
       defs)
;;

(* #609: definitions that name [column] AND name something reachable from
   [table] — see {!reachable_names} for why reachability rather than [table]
   alone.  Requiring [column] too is what keeps an unrelated definition using
   the same column name over a different table from blocking the rename.

   What this DOES guarantee: every stored definition whose text spells the
   column name and can see the table, directly or through a chain of views, is
   refused by name.  What it does NOT: precision (see {!sql_mentions_ident}'s
   role-blindness), and a reference that never spells the column name in the
   stored text — which is exactly the [*] projection that cannot break. *)
let column_dependents_tx tx ~table ~column =
  let%lwt defs = load_definitions_tx tx in
  let reach = reachable_names defs ~root:table in
  Lwt.return
    (List.filter_map
       (fun (kind, name, sql) ->
          if
            sql_mentions_ident ~ident:column sql
            && List.exists (fun ident -> sql_mentions_ident ~ident sql) reach
          then Some (describe_definition kind name)
          else None)
       defs)
;;

(* #609: the refusal message.  Names every dependent object, because the caller's
   only way forward is to drop and recreate them. *)
let dependents_error ~what ~deps =
  Printf.sprintf
    "cannot %s: it is referenced by %s; drop and recreate %s first"
    what
    (String.concat ", " deps)
    (match deps with
     | [ _ ] -> "it"
     | _ -> "them")
;;

(* An index with [old_col] renamed to [new_col]: a plain column matches by name,
   an expression column and the partial WHERE by the lexical rewrite above. *)
let rename_col_in_index ~old_col ~new_col (info : index_info) =
  let rw = rewrite_ident_in_sql ~old_name:old_col ~new_name:new_col in
  let one is_expr col =
    if is_expr then rw col else if String.equal col old_col then new_col else col
  in
  (* [zip], not [List.map2].  The two lists are one-per-column on every path that
     can produce an [index_info] today — [create_index] rejects a mismatch with
     its own [List.combine], and [decode_index_ext_fields] pairs them — but the
     FK index picker above still defends against an empty [idx_expr_flags], and
     the only way to reach a divergence here is a record decoded from a damaged
     file, which no API can construct and so no test can pin.  What must not
     happen then is an [Invalid_argument] escaping past the
     [(unit, string) result] every other failure in this rename is reported
     through.  A missing flag means "plain column" — the pre-expression-index
     shape the flags were added to. *)
  let rec zip cols flags =
    match cols, flags with
    | [], _ -> []
    | col :: cols, [] -> one false col :: zip cols []
    | col :: cols, is_expr :: flags -> one is_expr col :: zip cols flags
  in
  { info with
    idx_columns = zip info.idx_columns info.idx_expr_flags
  ; idx_where_sql = Option.map rw info.idx_where_sql
  }
;;

(* A stored column record with [old_col] renamed: its own name, plus any CHECK
   or GENERATED expression — which may name ANY column of the table, so this
   runs over every column, not only the renamed one. *)
let rename_col_in_column ~old_col ~new_col (c : Row.column) =
  let rw = rewrite_ident_in_sql ~old_name:old_col ~new_name:new_col in
  { c with
    Row.name = (if String.equal c.Row.name old_col then new_col else c.Row.name)
  ; check_sql = Option.map rw c.Row.check_sql
  ; generated_as = Option.map (fun (e, stored) -> rw e, stored) c.Row.generated_as
  }
;;

(* A foreign key with [old_col] renamed on [table]: the local side when the FK
   belongs to [table], the parent side when it points AT [table] (which includes
   a self-reference, where both sides move). *)
let rename_col_in_fk ~owner ~table ~old_col ~new_col (fk : fk_constraint) =
  let sub cols = List.map (fun c -> if String.equal c old_col then new_col else c) cols in
  { fk with
    fk_local_cols =
      (if String.equal owner table then sub fk.fk_local_cols else fk.fk_local_cols)
  ; fk_parent_cols =
      (if String.equal fk.fk_parent_table table
       then sub fk.fk_parent_cols
       else fk.fk_parent_cols)
  }
;;

(* Write [fks] to the primary FK record for [table] inside [tx].  Mirrors
   [save_fk_constraints]'s storage decision (delete when empty) without its
   mirror write, which the callers here fold into their own [put_mirror_tx]. *)
let put_fks_tx tx ~table ~fks =
  let key = fk_meta_key table in
  if fks = []
  then S.del tx sys_meta_tid key
  else S.put tx sys_meta_tid key (encode_fks fks)
;;

(* Every table whose FK constraints mention [table] as a PARENT, excluding
   [table] itself — a self-reference moves with the table's own record, so the
   callers handle it there rather than through this list. *)
let child_tables_of t ~table =
  Schema_cache.fold_tables
    (fun name (m : table_meta) acc ->
       if String.equal name table
       then acc
       else if
         List.exists (fun fk -> String.equal fk.fk_parent_table table) m.fk_constraints
       then m :: acc
       else acc)
    t.sc
    []
;;

(* Re-point every FK whose parent is [old_name] at [new_name].  Applied to the
   renamed table's OWN record as well as to its children: a self-referential
   [REFERENCES e(id)] on table [e] is a parent reference like any other, and it
   is the one shape [child_tables_of] cannot reach. *)
let repoint_fk_parent ~old_name ~new_name (m : table_meta) =
  { m with
    fk_constraints =
      List.map
        (fun fk ->
           if String.equal fk.fk_parent_table old_name
           then { fk with fk_parent_table = new_name }
           else fk)
        m.fk_constraints
  }
;;

(* [~txn] (#282): [Some] when the surrounding rename is borrowing an ambient
   explicit transaction; the entry is committed (and the cache undo registered)
   only on the borrowed path, otherwise the autocommit caller commits its own
   writer txn. *)
let finish_rename t tx ~txn ~old_name ~new_name ~meta =
  (* Re-write sys_indexes entries that reference old_name. *)
  let%lwt idx_updates = indexes_of_table_tx tx ~table:old_name in
  let%lwt () =
    Lwt_list.iter_s
      (fun (k, (info : index_info)) ->
         let new_info = { info with idx_table = new_name } in
         S.put tx sys_indexes_tid k (encode_index_value new_info))
      idx_updates
  in
  (* #553: the primary FK record is keyed by the table name, so it has to move
     with it — otherwise the constraints load as absent on the next open and the
     renamed table silently stops enforcing them.  Every FK pointing AT the old
     name is re-pointed in the same txn: those of other tables, AND the renamed
     table's own self-references, which are not in [child_tables_of] and which
     nothing else here would touch (a self-referential table came out of a
     RENAME TO permanently un-insertable, its FK naming a parent table that no
     longer exists). *)
  let renamed = repoint_fk_parent ~old_name ~new_name { meta with name = new_name } in
  let%lwt () = put_fks_tx tx ~table:old_name ~fks:[] in
  let%lwt () = put_fks_tx tx ~table:new_name ~fks:renamed.fk_constraints in
  let children =
    List.map (repoint_fk_parent ~old_name ~new_name) (child_tables_of t ~table:old_name)
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun (m : table_meta) ->
         let%lwt () = put_fks_tx tx ~table:m.name ~fks:m.fk_constraints in
         put_mirror_tx tx m)
      children
  in
  (* Refresh the mirror entry (keyed by the unchanged tree_id) with the new
     name; the schema shape — hence the fingerprint — is unchanged. *)
  let%lwt () = put_mirror_tx tx renamed in
  let%lwt () =
    match txn with
    | Some _ -> Lwt.return_unit
    | None -> S.commit tx
  in
  (* Update in-memory cache + index back-references.  Each mutator self-registers
     its own ROLLBACK restore (replayed most-recent-first: index back-refs, then
     the new table removed, then the old table restored — reversing the rename
     exactly).  The fingerprint is unchanged by a rename, so [put_table]'s re-stamp
     is a no-op. *)
  let to_update =
    List.map
      (fun (v : index_info) -> v.idx_name, v)
      (Schema_cache.indexes_for_table t.sc ~table:old_name)
  in
  (match txn with
   | Some _ ->
     Schema_cache.remove_table t.sc ~name:old_name;
     Schema_cache.put_table t.sc ~name:new_name renamed;
     List.iter
       (fun (k, v) -> Schema_cache.put_index t.sc ~name:k { v with idx_table = new_name })
       to_update;
     List.iter
       (fun (m : table_meta) -> Schema_cache.put_table t.sc ~name:m.name m)
       children
   | None ->
     Schema_cache.remove_table_durable t.sc ~name:old_name;
     Schema_cache.put_table_durable t.sc ~name:new_name renamed;
     List.iter
       (fun (k, v) ->
          Schema_cache.put_index_durable t.sc ~name:k { v with idx_table = new_name })
       to_update;
     List.iter
       (fun (m : table_meta) -> Schema_cache.put_table_durable t.sc ~name:m.name m)
       children);
  Lwt.return (Ok ())
;;

(* The store half of [rename_table], lifted to the top level (#609) so the
   dependency gate can sit in front of it without deepening the nesting. *)
let rename_table_body t tx ~txn ~old_name ~new_name ~(meta : table_meta) =
  (* Remove old sys_tables entry *)
  let%lwt () = S.del tx sys_tables_tid (Bytes.of_string old_name) in
  (* Insert new sys_tables entry *)
  let%lwt () =
    S.put tx sys_tables_tid (Bytes.of_string new_name) (encode_table_value meta)
  in
  (* Re-key all column entries; return Error if any entry is missing *)
  let%lwt col_result =
    rekey_table_columns tx ~old_name ~new_name ~n_cols:(List.length meta.columns)
  in
  match col_result with
  | Error msg -> Lwt.return (Error msg)
  | Ok () -> finish_rename t tx ~txn ~old_name ~new_name ~meta
;;

let rename_table ?txn t ~old_name ~new_name =
  match Schema_cache.find_table t.sc old_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" old_name))
  | Some meta ->
    if Schema_cache.mem_table t.sc new_name
    then Lwt.return (Error (Printf.sprintf "table already exists: %s" new_name))
    else (
      let body tx =
        (* #609: a view or trigger naming this table stores raw SQL text that
           still says [old_name] after the rename, so the rename is refused
           rather than left to break it silently. *)
        let%lwt deps = table_dependents_tx tx ~table:old_name in
        if deps = []
        then rename_table_body t tx ~txn ~old_name ~new_name ~meta
        else
          Lwt.return
            (Error
               (dependents_error ~what:(Printf.sprintf "rename table %s" old_name) ~deps))
      in
      match txn with
      | Some tx -> body tx
      | None ->
        let%lwt tx = S.rw_begin t.store in
        let%lwt r = body tx in
        (match r with
         | Ok () -> Lwt.return (Ok ()) (* finish_rename committed on the None path *)
         | Error msg ->
           let%lwt () = S.rollback tx in
           Lwt.return (Error msg)))
;;

(* Rewrite every _sys_columns record of [table_name] that the rename touches:
   the renamed column's own name, plus any CHECK or GENERATED expression naming
   it — those live on whichever column DECLARED them, not on the one they
   reference, so all of them are examined.

   Records that come out byte-identical are left alone rather than re-written.
   That is not just an optimisation: a legacy record decodes into defaults its
   stored bytes never carried (#533), so re-encoding an untouched column would
   silently upgrade the on-disk encoding of a file an older build still reads. *)
let rewrite_columns_tx tx ~table_name ~columns ~old_col ~new_col =
  Lwt_list.iteri_s
    (fun j _ ->
       let k = column_key table_name j in
       match%lwt S.get tx sys_columns_tid k with
       | None -> Lwt.return_unit
       | Some b ->
         let c = decode_column b in
         let c' = rename_col_in_column ~old_col ~new_col c in
         if c' = c then Lwt.return_unit else S.put tx sys_columns_tid k (encode_column c'))
    columns
;;

(* Foreign keys of OTHER tables that point at [table_name].[old_col]; only those
   that actually change are returned, each already rewritten. *)
let rewrite_child_fks_tx t tx ~table_name ~old_col ~new_col =
  let children =
    List.filter_map
      (fun (m : table_meta) ->
         let fks =
           List.map
             (rename_col_in_fk ~owner:m.name ~table:table_name ~old_col ~new_col)
             m.fk_constraints
         in
         if fks = m.fk_constraints then None else Some { m with fk_constraints = fks })
      (child_tables_of t ~table:table_name)
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun (m : table_meta) ->
         let%lwt () = put_fks_tx tx ~table:m.name ~fks:m.fk_constraints in
         put_mirror_tx tx m)
      children
  in
  Lwt.return children
;;

(* Indexes of [table_name] that name [old_col], each already rewritten and
   written back through [tx]. *)
let rewrite_indexes_tx tx ~table_name ~old_col ~new_col =
  let%lwt idxs = indexes_of_table_tx tx ~table:table_name in
  let changed =
    List.filter_map
      (fun (k, info) ->
         let info' = rename_col_in_index ~old_col ~new_col info in
         if info' = info then None else Some (k, info'))
      idxs
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun (k, info) -> S.put tx sys_indexes_tid k (encode_index_value info))
      changed
  in
  Lwt.return (List.map snd changed)
;;

(* [?txn] (#282): mirrors [add_column].  Renaming a column changes the schema
   fingerprint (it is computed over column names), so the undo restores both the
   prior [table_meta] and its tree-tag stamp.  The corrupt-catalog error path no
   longer rolls a borrowed txn back — it surfaces an Error and leaves teardown to
   the caller.

   #553: a column name is recorded in five more places than its _sys_columns
   record — an index's [idx_columns], a partial index's [idx_where_sql], a CHECK
   expression, a GENERATED expression, and both sides of a FOREIGN KEY (this
   table's [fk_local_cols], any other table's [fk_parent_cols]).  All of them are
   remapped HERE, in the caller's transaction and under the same schema-cache
   undo, so the rename is atomic in exactly the way the column rewrite already
   was.  Leaving any of them behind leaves the catalog naming a column the table
   does not have; for the implicit PRIMARY KEY index that is a dump which will
   not restore, because since #533 the DDL renderer reads that index as the
   record of the table's key.

   #609: the two stored-SQL trees the remap does NOT reach — [sys_views_tid] and
   [sys_triggers_tid], plus [sys_reactive_views_tid] — hold whole [CREATE ...]
   statements as raw text, and a lexical rewrite of one of those cannot be
   scoped to this table.  A rename that would touch one is therefore REFUSED by
   [rename_column]'s gate below rather than guessed at; this function is the
   store half that runs once the gate passes. *)
let rename_column_body t tx ~table_name ~(meta : table_meta) ~col_k ~old_col ~new_col =
  match%lwt S.get tx sys_columns_tid col_k with
  | None -> Lwt.return (Error "column entry missing from catalog")
  | Some _ ->
    let%lwt () =
      rewrite_columns_tx tx ~table_name ~columns:meta.columns ~old_col ~new_col
    in
    let new_meta =
      { meta with
        columns = List.map (rename_col_in_column ~old_col ~new_col) meta.columns
      ; fk_constraints =
          List.map
            (rename_col_in_fk ~owner:table_name ~table:table_name ~old_col ~new_col)
            meta.fk_constraints
      }
    in
    let%lwt () =
      if new_meta.fk_constraints = meta.fk_constraints
      then Lwt.return_unit
      else put_fks_tx tx ~table:table_name ~fks:new_meta.fk_constraints
    in
    let%lwt () = put_mirror_tx tx new_meta in
    let%lwt children = rewrite_child_fks_tx t tx ~table_name ~old_col ~new_col in
    let%lwt indexes = rewrite_indexes_tx tx ~table_name ~old_col ~new_col in
    Lwt.return (Ok (new_meta, indexes, children))
;;

let rename_column ?txn t ~table_name ~old_col ~new_col =
  match Schema_cache.find_table t.sc table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    (match List.find_index (fun c -> String.equal c.Row.name old_col) meta.columns with
     | None -> Lwt.return (Error (Printf.sprintf "column not found: %s" old_col))
     | Some i ->
       let col_k = column_key table_name i in
       let body tx =
         (* #609: a view or trigger that can see this table and names this column
            stores raw SQL text that still says [old_col] after the rename.  The
            catalog cannot re-render that text safely — see
            [column_dependents_tx] — so the rename is refused instead. *)
         let%lwt deps = column_dependents_tx tx ~table:table_name ~column:old_col in
         if deps = []
         then rename_column_body t tx ~table_name ~meta ~col_k ~old_col ~new_col
         else
           Lwt.return
             (Error
                (dependents_error
                   ~what:(Printf.sprintf "rename column %s.%s" table_name old_col)
                   ~deps))
       in
       let finalize (new_meta, indexes, children) =
         let put_table, put_index =
           match txn with
           | Some _ -> Schema_cache.put_table t.sc, Schema_cache.put_index t.sc
           | None ->
             Schema_cache.put_table_durable t.sc, Schema_cache.put_index_durable t.sc
         in
         put_table ~name:table_name new_meta;
         List.iter (fun (idx : index_info) -> put_index ~name:idx.idx_name idx) indexes;
         List.iter (fun (m : table_meta) -> put_table ~name:m.name m) children
       in
       (match txn with
        | Some tx ->
          (match%lwt body tx with
           | Error msg -> Lwt.return (Error msg)
           | Ok result ->
             finalize result;
             Lwt.return (Ok ()))
        | None ->
          let%lwt tx = S.rw_begin t.store in
          (match%lwt body tx with
           | Error msg ->
             let%lwt () = S.rollback tx in
             Lwt.return (Error msg)
           | Ok result ->
             let%lwt () = S.commit tx in
             finalize result;
             Lwt.return (Ok ()))))
;;

(* #533: clear the [primary_key] flag on [cols] — the inverse of the [open_]
   normalization pass, for when the `Implicit_pk index that justified the flag
   goes away ([ALTER TABLE ... DROP COLUMN] on a member of a composite key).

   Persisted, not just in-memory: [open_] only ever ADDS flags, so an unmarked
   column would be re-marked from the stored record on the next open.

   [not_null] is deliberately NOT cleared.  #530 merged "declared NOT NULL" and
   "implied by PRIMARY KEY" into one stored bit, so the two are no longer
   distinguishable here; keeping it is the conservative half.  Clearing it would
   drop a constraint the live table still enforces (a table that rejected NULL
   before the DROP COLUMN would start accepting it), whereas keeping it leaves
   the live table and its rendered DDL enforcing exactly the same thing — which
   is the property that makes a dump restorable. *)
let clear_pk_flags ?txn t ~table_name ~cols =
  match Schema_cache.find_table t.sc table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let hits =
      List.mapi (fun i (c : Row.column) -> i, c) meta.columns
      |> List.filter (fun (_, (c : Row.column)) ->
        c.Row.primary_key && List.mem c.Row.name cols)
    in
    if hits = []
    then Lwt.return (Ok ())
    else (
      let new_columns =
        List.map
          (fun (c : Row.column) ->
             if c.Row.primary_key && List.mem c.Row.name cols
             then { c with Row.primary_key = false }
             else c)
          meta.columns
      in
      let new_meta = { meta with columns = new_columns } in
      let%lwt () =
        borrow_or_autocommit ?txn t.store (fun tx ->
          let%lwt () =
            Lwt_list.iter_s
              (fun (i, _) ->
                 let k = column_key table_name i in
                 match%lwt S.get tx sys_columns_tid k with
                 | None -> Lwt.return_unit
                 | Some bytes ->
                   let c = decode_column bytes in
                   S.put
                     tx
                     sys_columns_tid
                     k
                     (encode_column { c with Row.primary_key = false }))
              hits
          in
          put_mirror_tx tx new_meta)
      in
      (match txn with
       | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
       | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta);
      Lwt.return (Ok ()))
;;

(* [?txn] (#282): mirrors [add_column].  Dropping a column changes the schema
   fingerprint, so the undo restores the prior [table_meta] and re-stamps its
   tree-tag.  Only the catalog's _sys_columns re-keying happens here; the
   executor ([alter_drop_column]) is responsible for migrating the row data
   through the same txn. *)
let drop_column ?txn t ~table_name ~col_name =
  match Schema_cache.find_table t.sc table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let rec find_idx i = function
      | [] -> None
      | (c : Row.column) :: _ when String.equal c.name col_name -> Some i
      | _ :: rest -> find_idx (i + 1) rest
    in
    (match find_idx 0 meta.columns with
     | None -> Lwt.return (Error (Printf.sprintf "column not found: %s" col_name))
     | Some drop_idx ->
       let n_cols = List.length meta.columns in
       let new_columns = List.filteri (fun i _ -> i <> drop_idx) meta.columns in
       let new_meta = { meta with columns = new_columns } in
       let%lwt () =
         borrow_or_autocommit ?txn t.store (fun tx ->
           (* Delete the dropped column's entry *)
           let%lwt () = S.del tx sys_columns_tid (column_key table_name drop_idx) in
           (* Re-key all columns after drop_idx: shift ordinal down by 1 *)
           let%lwt () =
             let rec shift i =
               if i >= n_cols
               then Lwt.return_unit
               else (
                 let old_k = column_key table_name i in
                 let new_k = column_key table_name (i - 1) in
                 let%lwt bytes_opt = S.get tx sys_columns_tid old_k in
                 match bytes_opt with
                 | None -> shift (i + 1)
                 | Some bytes ->
                   let%lwt () = S.del tx sys_columns_tid old_k in
                   let%lwt () = S.put tx sys_columns_tid new_k bytes in
                   shift (i + 1))
             in
             shift (drop_idx + 1)
           in
           put_mirror_tx tx new_meta)
       in
       (match txn with
        | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
        | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta);
       Lwt.return (Ok ()))
;;

(* ------------------------------------------------------------------ *)
(* FTS public API                                                       *)
(* ------------------------------------------------------------------ *)

let find_fts (t : t) name = Schema_cache.find_fts t.sc name

let list_fts_tables (t : t) =
  Schema_cache.fold_fts (fun _name meta acc -> meta :: acc) t.sc []
;;

(* [?txn]: as for [create_table] (#269), an active explicit transaction is
   threaded here so the FTS metadata write and both tree-ID allocations
   participate in it and roll back together. *)
let create_fts_table ?txn (t : t) ~name ~columns : fts_table_meta Lwt.t =
  (* NOTE (autocommit path only): tree-ID allocation and metadata write span
     multiple transactions.  A crash between the two next_user_tid calls leaks a
     tree-ID slot (non-fatal; the next create will allocate the next available
     slot). A crash after both allocations but before the sys_fts_tid write leaves
     the name unregistered and the two tree IDs permanently unused. Same pattern
     as create_table.  The in-transaction path is atomic. *)
  match txn with
  | Some tx ->
    let%lwt content_tree = next_user_tid_tx tx in
    let%lwt index_tree = next_user_tid_tx tx in
    let meta =
      { fts_name = name
      ; fts_content_tree = content_tree
      ; fts_index_tree = index_tree
      ; fts_columns = columns
      }
    in
    let%lwt () = S.put tx sys_fts_tid (Bytes.of_string name) (encode_fts_value meta) in
    Schema_cache.put_fts t.sc ~name meta;
    Lwt.return meta
  | None ->
    (* Allocate two new tree IDs: one for content, one for the inverted index *)
    let%lwt content_tree = next_user_tid t in
    let%lwt index_tree = next_user_tid t in
    let meta =
      { fts_name = name
      ; fts_content_tree = content_tree
      ; fts_index_tree = index_tree
      ; fts_columns = columns
      }
    in
    (* Write to sys_fts_tid *)
    let%lwt tx = S.rw_begin t.store in
    let key = Bytes.of_string name in
    let value = encode_fts_value meta in
    let%lwt () = S.put tx sys_fts_tid key value in
    let%lwt () = S.commit tx in
    Schema_cache.put_fts_durable t.sc ~name meta;
    Lwt.return meta
;;

(** Rowid counter for FTS tables stored as a separate key in sys_fts_tid.
    Key format: name ++ "\x00rowid" (the \x00 prefix sorts before printable ASCII). *)
let fts_rowid_counter_key name = Bytes.cat (Bytes.of_string name) sys_fts_rowid_suffix

let read_fts_rowid_counter tx rowid_key =
  let%lwt cur_opt = S.get tx sys_fts_tid rowid_key in
  match cur_opt with
  | None -> Lwt.return 1L
  | Some b ->
    let n, _ = Varint.decode_int64 b 0 in
    Lwt.return n
;;

let write_fts_rowid_counter tx rowid_key v =
  let nbuf = Buffer.create 8 in
  Varint.encode_int64 nbuf v;
  S.put tx sys_fts_tid rowid_key (Buffer.to_bytes nbuf)
;;

let next_fts_rowid_in_txn (_t : t) ~name (tx : S.rw S.txn) : int64 Lwt.t =
  let rowid_key = fts_rowid_counter_key name in
  let%lwt cur = read_fts_rowid_counter tx rowid_key in
  let%lwt () = write_fts_rowid_counter tx rowid_key (Int64.add cur 1L) in
  Lwt.return cur
;;

(* #330: after an explicit-rowid insert, advance the high-water so the next
   auto-allocated rowid is past [rowid] (the counter holds the next rowid to
   assign).  No-op when the counter is already beyond [rowid]. *)
let ensure_fts_rowid_above_in_txn (_t : t) ~name (tx : S.rw S.txn) (rowid : int64)
  : unit Lwt.t
  =
  let rowid_key = fts_rowid_counter_key name in
  let%lwt cur = read_fts_rowid_counter tx rowid_key in
  let want = Int64.add rowid 1L in
  if Int64.compare want cur > 0
  then write_fts_rowid_counter tx rowid_key want
  else Lwt.return_unit
;;

let get_fk_enforcement t = t.fk_enforcement
let set_fk_enforcement t v = t.fk_enforcement <- v
let get_recursive_triggers t = t.recursive_triggers
let set_recursive_triggers t v = t.recursive_triggers <- v
let get_defer_fks_pragma t = t.defer_fks_pragma
let set_defer_fks_pragma t v = t.defer_fks_pragma <- v
let queue_pending_fk_check t check = t.pending_fk_checks <- check :: t.pending_fk_checks

let drain_pending_fk_checks t =
  let pending = List.rev t.pending_fk_checks in
  t.pending_fk_checks <- [];
  pending
;;

let clear_pending_fk_checks t = t.pending_fk_checks <- []
let pending_fk_check_count t = List.length t.pending_fk_checks
let store t = t.store

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
