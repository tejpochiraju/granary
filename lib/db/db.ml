open Lwt.Syntax
module S = Granary_store.Store
module Cat = Granary_catalog.Catalog
module Sql = Granary_sql
module Row = Granary_encoding.Row

(* #427: reactive-view runtime state.  A view is either full-refresh (re-run the
   SELECT on each relevant commit) or delta-maintained through the IVM aggregate
   engine.  [rv_query] is the parsed SELECT; [rv_out_cols] the resolved
   materialization column names; [rv_base_tables] the tables whose row changes
   drive the view. *)
type rv_measure =
  | RV_count
  | RV_sum of int (* ordinal of the SUM column in the base row *)

type rv_mode =
  | RV_full
  | RV_delta of
      { group_ord : int (* ordinal of the GROUP BY column in the base row *)
      ; measure : rv_measure
      ; engine : Reactive_view.Agg_engine.state
      }

type rv_entry =
  { rv_name : string
  ; rv_query : Sql.Ast.stmt
  ; rv_base_tables : string list
  ; rv_out_cols : string list
  ; mutable rv_mode : rv_mode
  ; mutable rv_provisional : bool
    (** #427: a full-refresh view materialised while empty gets an all-TEXT
        placeholder schema; on the first non-empty refresh the [_rv_…] table is
        re-created with column types inferred from the data. *)
  ; mutable rv_callbacks : (Sql.Exec.row_change list -> unit Lwt.t) list
  }

(* #634: the shared invalidation generation for every [Db.t] sitting over ONE
   [Store.t].  {!vacuum} closes that store and swaps a freshly opened one into
   the vacuuming handle; every handle produced by {!create_worker_handle} keeps
   pointing at the CLOSED store and at the pre-VACUUM rowid allocator.  Nothing
   detected that, so the failure surfaced later and elsewhere — a closed-store
   error, or (worse) a rowid handed out by a counter table that no longer
   describes the live data.

   The cohort record is shared by reference between a handle and every worker
   derived from it; [handle_generation] on each handle records the generation it
   was created at.  VACUUM bumps the cohort and re-stamps only the handle that
   ran it, so every other handle in the cohort is now [handle_generation <>
   cohort.vacuum_generation] — i.e. stale — and every statement on it is refused
   loudly.  This is option 2 of #634 ("invalidate loudly"); option 3 (re-seat the
   workers through an indirection on [Store.t]) is the clean fix and is left to
   #633's restructuring. *)
type store_cohort = { mutable vacuum_generation : int }

type t =
  { mutable store : S.t
  ; mutable catalog : Cat.t
  ; cohort : store_cohort
    (** #634: shared by reference with every handle over the same [Store.t]. *)
  ; mutable handle_generation : int
    (** #634: the cohort generation this handle was created (or re-seated) at.
        Differs from [cohort.vacuum_generation] exactly when a VACUUM on a
        sibling handle has invalidated this one. *)
  ; clock : (unit -> float) option
  ; mutable explicit_txn : S.rw S.txn option
  ; mutable txn_poisoned : bool
    (** #555: cross-fiber transaction containment.  A [Db.t] has exactly one
        explicit-transaction slot ([explicit_txn] above), and every statement
        resolves its transaction from it.  So when two fibers share one handle
        and interleave [BEGIN … COMMIT], the second [BEGIN] failing is {e not}
        enough: the losing fiber's later statements would run inside the
        winner's transaction and its [COMMIT] would commit the winner's
        half-finished work.  Setting this flag on the losing [BEGIN] turns that
        silent contamination into a loud refusal — while it is set every
        statement on this handle is rejected except [ROLLBACK], which aborts the
        in-flight transaction and clears the flag.

        The window it covers is bounded by that recovery: once [ROLLBACK] clears
        the flag the slot is free again, and the same contamination is reachable
        with the fibers exchanged (#584).  So this NARROWS the hazard, it does
        not close it, and it is not a fix for the one-slot design — see #585
        (scoped transaction combinator) and #555 option 1 (session object).
        {!create_worker_handle} is what an application that wants concurrent
        explicit transactions should use today. *)
  ; views : (string, Sql.Ast.stmt) Hashtbl.t
  ; triggers : (string, Sql.Ast.stmt) Hashtbl.t (* trigger_name -> S_create_trigger AST *)
  ; mutable savepoint_names : string list (* active savepoints, newest first *)
  ; mutable auto_began : bool (* txn started implicitly by SAVEPOINT *)
  ; mutable last_changes : int (** rows affected by the last DML statement *)
  ; mutable last_insert_rowid : int64 (** rowid of the last INSERT row *)
  ; mutable total_changes : int (** total rows affected by DML since connection opened *)
  ; mutable trigger_depth : int (** recursion depth for nested trigger firing *)
  ; file_path : string option
    (** Set to the on-disk path for file-backed handles (opened via the
        [granary.unix] driver or ATTACH).  VACUUM needs this to rebuild the
        file in place; [None] for in-memory / arbitrary block devices. *)
  ; attached : (string, t) Hashtbl.t
    (** Sub-handles registered via [ATTACH DATABASE 'path' AS schema]
        (phase 40 / #64).  Keyed by schema name.  Always empty on
        sub-handles themselves — only the top-level handle holds the map. *)
  ; mutable active_schema : string
    (** Active schema for routing non-routing statements (phase 40 /
        #64).  Defaults to ["main"].  Switched via [PRAGMA
        active_database = name]; the named schema must exist in
        [attached] or equal ["main"]. *)
  ; reactive_views : (string, rv_entry) Hashtbl.t
    (** #427: [CREATE REACTIVE VIEW] definitions, keyed by view name.  Held on
        the top-level handle only. *)
  ; rv_pending : (string, Sql.Exec.row_change list) Hashtbl.t
    (** #427: base-table row changes accumulated since the last reactive-view
        flush, keyed by base-table name (newest-appended, application order).
        Flushed on autocommit / COMMIT, dropped on ROLLBACK. *)
  ; mutable rv_depth : int
    (** #427/#475: re-entrancy nesting DEPTH — positive while the driver writes
        [_rv_…] tables, so those writes do not themselves drive reactive views.

        #475: this was a bare [bool] and the four (now five) driver entry points
        that set it nest: [rv_flush]'s refresh can be entered while [rv_create]
        holds the flag, and whichever finished first cleared it for everybody,
        re-arming reactive driving in the middle of the outer operation's own
        internal writes.  A counter incremented and decremented under
        [Lwt.finalize] ({!rv_guard}) is monotone under nesting, so the guard is
        released exactly once, by the outermost entry. *)
  ; rv_internal_tables : (string, int) Hashtbl.t
    (** #475: counted set of [_rv_<name>] materialisation tables the driver is
        *currently* dropping on its own behalf, so {!execute_control_op}'s
        internal-table [DROP TABLE] guard can let those through while refusing
        every user drop.

        This is deliberately NOT [rv_depth]: the depth stays positive across the
        whole of [rv_flush] / [rv_create] / [rv_load], each of which awaits the
        store repeatedly, and Lwt being cooperative a *user*
        [DROP TABLE _rv_<other>] scheduled into one of those windows used to
        bypass the guard entirely and destroy an unrelated live view's
        materialisation.  Suppression is therefore keyed on the table name the
        driver is actually dropping, not on whether the driver is busy. *)
  ; mutable rv_resync : bool
    (** #427: set when a savepoint rollback made the accumulated deltas
        untrustworthy; the next flush full-resyncs every affected view. *)
  ; mutable txn_scope : int option
    (** #585: the owner token of the {!with_transaction} extent that opened the
        transaction currently sitting in [explicit_txn], or [None] when the
        transaction (if any) was opened by a bare [BEGIN].

        This is the first instance of the thing #555/#584 said the engine did not
        have: a transaction {e extent} an owner token can be attached to.  The
        token is minted when [with_transaction] BEGINs, stored here, and
        simultaneously published into the calling fiber's Lwt storage under
        {!txn_scope_key}.  A fiber is the owner iff the two agree, which is
        exactly what {!in_transaction_scope} tests.

        Today it decides exactly one question — whether a [with_transaction] call
        is a re-entrant call by the owner (refuse cleanly, outer transaction
        intact) or a collision with another fiber (fall through to [BEGIN], which
        poisons as it always has).  Binding {e statements} to their owner needs
        the same token plumbed to [resolve_txn]; that is #555 option 1's work,
        not this field's. *)
  }

(* #585: monotone source of transaction-scope owner tokens.  Process-wide rather
   than per-handle so a token is meaningful even when it travels with a fiber
   across handles — comparing tokens can then never produce a false match. *)
let txn_scope_counter = ref 0

(* #585: the calling fiber's transaction-scope owner token.  Same idiom as
   [Sql.Exec]'s [txn_mode_key] and the #240 dirty-table accumulator: set with
   [Lwt.with_value] for the dynamic extent of the [with_transaction] body, so
   every fiber spawned inside that body inherits it and every fiber outside it
   reads [None]. *)
let txn_scope_key : int Lwt.key = Lwt.new_key ()

let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>Db.t { path = %s;@ schema = %s;@ savepoints = %d;@ total_changes = %d }@]"
    (match t.file_path with
     | Some p -> p
     | None -> ":memory:")
    t.active_schema
    (List.length t.savepoint_names)
    t.total_changes
;;

(* #634: THE staleness predicate — one spelling, used at every gate.  True when
   a VACUUM ran on a SIBLING handle over the same store: this handle's [store] is
   the closed pre-VACUUM one and its catalog's rowid counters describe a file
   that no longer exists.

   Unlike #555's poison there is no recovery: the store is gone, so [ROLLBACK]
   is refused too.  The only legal operation on a stale handle is {!close}, and
   the caller must obtain a fresh handle (a new {!create_worker_handle} off the
   handle that ran the VACUUM). *)
let is_stale t = t.handle_generation <> t.cohort.vacuum_generation

(* #634: the message every statement gets on a handle invalidated by VACUUM.
   Deliberately verbose and deliberately names VACUUM: the whole point of the
   fix is that the caller learns the cause here rather than meeting a
   closed-store failure or a bad rowid somewhere unrelated later. *)
let stale_msg =
  "handle invalidated by VACUUM (#634): another handle over this store ran VACUUM, which \
   closed the store this handle points at and replaced the database file.  This handle \
   is dead - there is no ROLLBACK recovery.  Close it and obtain a fresh one via \
   Db.create_worker_handle on the handle that ran the VACUUM."
;;

type value = Row.value =
  | V_int of int64
  | V_text of string
  | V_null
  | V_real of float
  | V_blob of bytes

type row = Row.t

(** In-memory representation of a trigger, extracted from S_create_trigger AST. *)
type trigger_meta =
  { trig_table : string
  ; trig_timing : [ `Before | `After | `Instead_of ]
  ; trig_event : [ `Insert | `Update | `Delete ]
  ; trig_when : Sql.Ast.expr option
  ; trig_body : Sql.Ast.stmt list
  }

(* #239: re-export the executor's per-query cost/stats record so cache callers
   read [Db.rows_examined] / [Db.used_index] without reaching into the SQL
   internals.  Same type as {!Sql.Exec.query_stats}. *)
type query_stats = Sql.Exec.query_stats =
  { mutable rows_examined : int
  ; mutable rows_returned : int
  ; mutable index_entries : int
  ; mutable used_index : bool
  }

(* #240: user tables whose rows a write statement actually mutated, including
   tables touched indirectly by triggers and FK cascades.  Sorted, deduplicated;
   internal/system tables excluded. *)
type dirty_tables = string list

(* #417 Phase 0: the row-level delta feed.  Same constructors as
   {!Sql.Exec.row_change}, re-exported so consumers stay in [Db]. *)
type row_change = Sql.Exec.row_change =
  | Inserted of
      { rowid : int64
      ; row : Row.t
      }
  | Deleted of
      { rowid : int64
      ; row : Row.t
      }
  | Updated of
      { rowid : int64
      ; old_row : Row.t
      ; new_row : Row.t
      }

type table_changes = (string * row_change list) list

type error =
  | Parse of string
  | Sema of Sql.Sema.error
  | Runtime of string
  | History_unavailable (* #266: as-of query on a db opened without history *)
  | History_pruned (* #266: as-of target predates the retained floor *)

(* #427: forward hooks for the reactive-view driver.  The heavy lifting (reading
   base tables, materialising [_rv_…] tables) re-enters [query]/[execute]/[run],
   which are defined far below; these refs are populated at module-load time by
   the implementations at the bottom of the file. *)
let rv_load_hook : (t -> unit Lwt.t) ref = ref (fun _ -> Lwt.return_unit)

let rv_create_hook
  : (t
     -> sql:string
     -> name:string
     -> Sql.Ast.stmt
     -> Sql.Ast.refresh_mode
     -> (unit, error) result Lwt.t)
      ref
  =
  ref (fun _ ~sql:_ ~name:_ _ _ -> Lwt.return (Ok ()))
;;

let rv_flush_hook : (t -> (unit, error) result Lwt.t) ref =
  ref (fun _ -> Lwt.return (Ok ()))
;;

(* #469: forward hook for [DROP REACTIVE VIEW]; populated at the bottom of the
   file alongside the other reactive-view hooks. *)
let rv_drop_hook : (t -> name:string -> if_exists:bool -> (unit, error) result Lwt.t) ref =
  ref (fun _ ~name:_ ~if_exists:_ -> Lwt.return (Ok ()))
;;

(* Is [tbl] a base table of some registered reactive view? *)
let rv_is_base_table t tbl =
  Hashtbl.fold
    (fun _ e found -> found || List.mem tbl e.rv_base_tables)
    t.reactive_views
    false
;;

let rv_absorb_one t tbl changes =
  let prev =
    match Hashtbl.find_opt t.rv_pending tbl with
    | Some l -> l
    | None -> []
  in
  Hashtbl.replace t.rv_pending tbl (prev @ changes)
;;

(* Accumulate one statement's captured row changes into [t.rv_pending], keeping
   only tables that some reactive view depends on. *)
let rv_absorb_changes t acc =
  if Hashtbl.length t.reactive_views > 0
  then
    List.iter
      (fun (tbl, changes) -> if rv_is_base_table t tbl then rv_absorb_one t tbl changes)
      (Sql.Exec.dirty_changes acc)
;;

(* #737: a statement that returned [Error] may still have left writes behind,
   and its delta log cannot be trusted to describe them — so the views are
   marked for a resync from the base tables instead, which is the same answer
   [rollback_to_savepoint] already gives for the same reason (#427).

   Absorbing the accumulator on [Error] was the other candidate and it is NOT
   sufficient, which is the finding to keep: [execute_insert] records its
   [Inserted] delta only AFTER [execute_insert_write] returns, and the AFTER
   INSERT trigger fires inside it, so a raising AFTER trigger in a borrowed
   transaction leaves the row in the store with no delta recorded anywhere.
   Absorbing would still have left the materialisation missing that row.
   Dropping the deltas and rebuilding is exact whatever the write path did.

   Two conditions, and both are needed:

   - [explicit_txn] — in a BORROWED transaction #631 keeps a raising
     statement's partial effects deliberately, so ANY error may have left rows
     behind, including the no-delta shape above.
   - [dirty_elements] — in AUTOCOMMIT a statement is not one transaction. A
     multi-row [VALUES] list and [INSERT ... SELECT] run one [execute_insert]
     (and one COMMIT) per row, so a failure on row k leaves rows 1..k-1
     durably committed; the accumulator is what says the statement got that
     far. Using the #240 NAME set rather than {!dirty_changes} is deliberate:
     it is the over-approximate half (#666), so it errs towards a superfluous
     rebuild rather than a missed one.

   With neither — the common `try INSERT, catch UNIQUE` in autocommit, where
   the per-row transaction was rolled back and nothing was recorded — no
   resync is scheduled and the error path costs what it always did. *)
let rv_note_failed_statement t acc =
  if
    Option.is_some t.explicit_txn
    || List.exists (fun tbl -> rv_is_base_table t tbl) (Sql.Exec.dirty_elements acc)
  then t.rv_resync <- true
;;

(* Drop all accumulated pending changes (on ROLLBACK). *)
let rv_clear_pending t = Hashtbl.reset t.rv_pending

(* File operations the SQL engine needs for ATTACH and VACUUM, injected by a
   platform driver (e.g. [granary.unix]) through [set_file_provider].  The
   core itself carries no OS/filesystem dependency (#170); when no provider is
   installed, ATTACH and VACUUM fail with a clear error. *)
type file_provider =
  { open_store :
      ?geom:S.Geometry.t
      -> ?as_of_history:bool
      -> path:string
      -> unit
      -> (S.t, S.error) result Lwt.t
  ; remove_file : string -> unit
  ; rename_file : string -> string -> unit
  }

let file_provider_ref : file_provider option ref = ref None
let set_file_provider p = file_provider_ref := Some p

let open_in_memory ?clock () =
  let store = S.create () in
  (match clock with
   | Some c -> S.set_clock store c
   | None -> ());
  let* catalog = Cat.open_ store in
  Lwt.return
    { store
    ; catalog
    ; cohort = { vacuum_generation = 0 }
    ; handle_generation = 0
    ; clock
    ; explicit_txn = None
    ; txn_poisoned = false
    ; views = Hashtbl.create 4
    ; triggers = Hashtbl.create 4
    ; savepoint_names = []
    ; auto_began = false
    ; last_changes = 0
    ; last_insert_rowid = 0L
    ; total_changes = 0
    ; trigger_depth = 0
    ; file_path = None
    ; attached = Hashtbl.create 1
    ; active_schema = "main"
    ; reactive_views = Hashtbl.create 4
    ; rv_pending = Hashtbl.create 4
    ; rv_depth = 0
    ; rv_internal_tables = Hashtbl.create 1
    ; rv_resync = false
    ; txn_scope = None
    }
;;

let load_views_into_hashtbl store views_tbl =
  let* pairs = Cat.load_all_views store in
  List.iter
    (fun (name, sql) ->
       match
         let lexbuf = Lexing.from_string sql in
         Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
       with
       | Sql.Ast.S_create_view { query; _ } -> Hashtbl.replace views_tbl name query
       | _ ->
         Printf.eprintf "warning: skipping non-view SQL in sys_views (name: %s)\n%!" name
       | exception _ -> ())
    pairs;
  Lwt.return_unit
;;

let load_triggers_into_hashtbl store trig_tbl =
  let* pairs = Cat.load_all_triggers store in
  List.iter
    (fun (name, sql) ->
       match
         let lexbuf = Lexing.from_string sql in
         Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
       with
       | Sql.Ast.S_create_trigger _ as ast -> Hashtbl.replace trig_tbl name ast
       | _ ->
         Printf.eprintf
           "warning: skipping non-trigger SQL in sys_triggers (name: %s)\n%!"
           name
       | exception exn ->
         Printf.eprintf
           "warning: failed to parse trigger SQL for '%s': %s\n%!"
           name
           (Printexc.to_string exn))
    pairs;
  Lwt.return_unit
;;

(* Wrap an already-open store as a [Db.t]: load the catalog, views, and
   triggers.  [file_path] is recorded so VACUUM can rebuild the file in place
   (Some for file-backed handles, None for in-memory / arbitrary devices).
   [durability] sets the database-wide durability knob on the store before
   loading the catalog. *)
let of_store ?clock ?durability ?file_path ?cohort store =
  (match clock with
   | Some c -> S.set_clock store c
   | None -> ());
  (match durability with
   | Some d -> S.set_durability store d
   | None -> ());
  let* catalog = Cat.open_ store in
  (* Load persisted columnar data for columnar tables. *)
  let* () = Cat.load_columnar_stores catalog store in
  let views = Hashtbl.create 4 in
  let* () = load_views_into_hashtbl store views in
  let triggers = Hashtbl.create 4 in
  let* () = load_triggers_into_hashtbl store triggers in
  (* #634: a handle opened without [?cohort] starts its own cohort; one opened
     WITH it joins the caller's and is stamped with the generation current at
     creation time, so a handle created after a VACUUM is live, not stale. *)
  let cohort =
    match cohort with
    | Some c -> c
    | None -> { vacuum_generation = 0 }
  in
  let db =
    { store
    ; catalog
    ; cohort
    ; handle_generation = cohort.vacuum_generation
    ; clock
    ; explicit_txn = None
    ; txn_poisoned = false
    ; views
    ; triggers
    ; savepoint_names = []
    ; auto_began = false
    ; last_changes = 0
    ; last_insert_rowid = 0L
    ; total_changes = 0
    ; trigger_depth = 0
    ; file_path
    ; attached = Hashtbl.create 1
    ; active_schema = "main"
    ; reactive_views = Hashtbl.create 4
    ; rv_pending = Hashtbl.create 4
    ; rv_depth = 0
    ; rv_internal_tables = Hashtbl.create 1
    ; rv_resync = false
    ; txn_scope = None
    }
  in
  (* #427: reconstruct reactive-view registry (parse defs, rebuild delta engine
     state from the current base-table contents). *)
  let* () = !rv_load_hook db in
  Lwt.return db
;;

let open_block
      ?geom
      ?clock
      ?durability
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages
      ~close
      ()
  : (t, error) result Lwt.t
  =
  let* result =
    S.open_block
      ?geom
      ~init_if_corrupt:true
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages
      ~close
      ()
  in
  match result with
  | Error e -> Lwt.return (Error (Runtime (Format.asprintf "%a" S.pp_error e)))
  | Ok store ->
    let* db = of_store ?clock ?durability store in
    Lwt.return (Ok db)
;;

let close t =
  let attached_subs = Hashtbl.fold (fun _ sub acc -> sub :: acc) t.attached [] in
  let* () = Lwt_list.iter_s (fun sub -> S.close sub.store) attached_subs in
  Hashtbl.clear t.attached;
  (* #634: closing a handle invalidated by VACUUM must not close its store a
     second time — VACUUM already did, and the teardown touches the (now closed)
     pager/WAL fds.  Releasing the handle is still the right and only thing a
     caller can do with it, so [close] succeeds rather than raising. *)
  if is_stale t then Lwt.return_unit else S.close t.store
;;

(* #589: the worker gets a fresh catalog — that is what makes DDL on one handle
   invisible to the other — but it must NOT get a fresh rowid allocator.  Both
   catalogs sit over one [Store.t] and therefore over one set of data trees, and
   two counters over one tree hand the same rowid out twice: the second write
   silently overwrites the first (and, on a [TEXT PRIMARY KEY] table, leaves the
   index pointing at the wrong row).  Sharing the allocator is the whole fix.

   #633: that sharing is no longer this function's doing — the allocator hangs
   off [Store.t], so naming the same store IS sharing it.  This call site is
   therefore no longer special, and neither is any future one. *)
let create_worker_handle t =
  let* () = Lwt.return_unit in
  (* #634: deriving a worker from a handle a sibling's VACUUM already killed
     would produce a second handle over the same closed store.  Raise (as
     {!vacuum} does) rather than hand one back. *)
  if is_stale t
  then Lwt.fail_with stale_msg
  else
    (* #634: join the parent's cohort so a later VACUUM on either handle
       invalidates the other loudly instead of leaving it over a closed store.
       #633: the rowid allocator is no longer passed here — it hangs off
       [Store.t], so naming the same store already shares it. *)
    of_store ~cohort:t.cohort t.store
;;

let wal_sync_count t = S.wal_sync_count t.store

module Event = S.Event

let set_event_callback t cb = S.set_event_callback t.store cb

let tree_of_table t name =
  match Cat.find_table_cached t.catalog ~name with
  | Some meta when Cat.tid_of_storage meta.Cat.storage >= 0 ->
    Some (Cat.tid_of_storage meta.Cat.storage)
  | _ -> None
;;

(* #433: hand out a read-only projection, never the live [Catalog.t] — see
   schema.mli.  [Schema.of_catalog] is the identity on the handle, so this is a
   live view of the catalog the handle executes against, not a snapshot. *)
let schema t = Schema.of_catalog t.catalog

(* #634: this handle's cohort, to pass to {!of_store} for a second handle over
   the SAME store so a VACUUM on either invalidates the other loudly.
   {!create_worker_handle} does this for you; a caller reaching for [of_store]
   over an already-open store owes it by hand, exactly as it owes
   [~rowid_counters] (#589). *)
let cohort t = t.cohort

(* ------------------------------------------------------------------ *)
(* VACUUM (#120)                                                       *)
(* ------------------------------------------------------------------ *)

(* Copy every tree from [src] to [dst].  We commit every batch_size
   entries so freed CoW pages become reusable in subsequent batches —
   without periodic commits, a long single transaction makes the
   destination file BIGGER than the source because each [put] CoWs
   the leaf and the freed pages can't be reused inside the same txn
   (alloc_min_safe is pinned at rw_begin). *)
let copy_all_trees ~src ~dst ~tids =
  let batch_size = 16 in
  S.with_ro src
  @@ fun tx_ro ->
  let* () =
    Lwt_list.iter_s
      (fun tid ->
         let* cur = S.cursor_open tx_ro tid in
         let _ = S.cursor_first cur in
         let rec drain tx_rw_opt count =
           let* tx_rw =
             match tx_rw_opt with
             | Some t -> Lwt.return t
             | None -> S.rw_begin dst
           in
           match S.cursor_next cur with
           | None ->
             (match tx_rw_opt with
              | Some _ -> S.commit tx_rw
              | None -> S.rollback tx_rw)
           | Some (k, v) ->
             let* () = S.put tx_rw tid k v in
             if count + 1 >= batch_size
             then
               let* () = S.commit tx_rw in
               drain None 0
             else drain (Some tx_rw) (count + 1)
         in
         let* () = drain None 0 in
         S.cursor_close cur;
         Lwt.return_unit)
      tids
  in
  Lwt.return_unit
;;

(* Compact rebuild: copy every tree from [src] into a fresh [dst] file at
   [tmp_path], then atomically rename it over [path].  After this the
   caller MUST swap its [store]/[catalog] references to the freshly opened
   destination. *)
let vacuum t : unit Lwt.t =
  (* #634: a handle a sibling's VACUUM already invalidated cannot run one — its
     store is closed and its [file_path] names a file it no longer owns. *)
  if is_stale t
  then Lwt.fail_with stale_msg
  else (
    match t.file_path with
    | None -> Lwt.fail_with "VACUUM: only supported on file-backed databases"
    | Some path ->
      if t.explicit_txn <> None
      then Lwt.fail_with "VACUUM cannot run inside an explicit transaction"
      else (
        match !file_provider_ref with
        | None ->
          Lwt.fail_with
            "VACUUM requires a file provider; link granary.unix and call \
             Db.set_file_provider"
        | Some prov ->
          let tmp_path = path ^ ".vacuum-tmp" in
          prov.remove_file tmp_path;
          prov.remove_file (tmp_path ^ "-wal");
          (* Rebuild at the source's geometry so a non-default page_size /
           reserved-bytes choice survives the vacuum (#176). *)
          let geom = S.geometry t.store in
          let* dst_r = prov.open_store ~geom ~path:tmp_path () in
          (match dst_r with
           | Error e ->
             let msg = Format.asprintf "VACUUM open tmp: %a" S.pp_error e in
             Lwt.fail_with msg
           | Ok dst ->
             let* tids = S.list_tree_ids t.store in
             let* () = copy_all_trees ~src:t.store ~dst ~tids in
             let* () = S.close dst in
             (* #412: preserve the as-of CAPABILITY across VACUUM.  Compaction
              drops the pre-VACUUM roots, so the existing [<path>.aslog] (which
              records now-invalid root pages) must be discarded — appending to it
              would yield a non-monotonic log whose old targets resolve to garbage
              roots in the rebuilt file.  We capture whether history was on, drop
              the stale log below, and reopen with the sink enabled so recording
              continues fresh.  The rebuild target [dst] deliberately ran WITHOUT
              history (no orphaned [tmp_path.aslog]). *)
             let had_history = S.history_enabled t.store in
             let* () = S.close t.store in
             (* Best-effort cleanup of WAL sidecar — its contents are now stale. *)
             prov.remove_file (path ^ "-wal");
             (* Stale as-of log: its roots predate compaction (see above). *)
             prov.remove_file (path ^ ".aslog");
             prov.rename_file tmp_path path;
             let* new_store_r = prov.open_store ~as_of_history:had_history ~path () in
             (match new_store_r with
              | Error e ->
                let msg = Format.asprintf "VACUUM reopen: %a" S.pp_error e in
                Lwt.fail_with msg
              | Ok new_store ->
                (* #634: the fresh catalog deliberately gets FRESH rowid counters
                 (no [?rowid_counters]).  It must: the counters the old catalog
                 held describe the pre-VACUUM file, and this one is re-seeded
                 from the rebuilt file — for a plain rowid table by rescanning
                 the copied data tree, for an AUTOINCREMENT table from the
                 [_sys_tables] row the copy carried over.  VACUUM preserves tree
                 ids ([copy_all_trees] writes each tid to the same tid), so the
                 "counters are keyed by tree id" invariant (#589) is not
                 disturbed; the counters are replaced wholesale rather than
                 remapped.  What the fresh table does NOT do is reach the
                 sibling handles that still hold the old one — which is exactly
                 why they must be invalidated below rather than left running. *)
                let* new_catalog = Cat.open_ new_store in
                (* Load persisted columnar data into the fresh catalog. *)
                let* () = Cat.load_columnar_stores new_catalog new_store in
                t.store <- new_store;
                t.catalog <- new_catalog;
                (* #634: bump the shared cohort and re-stamp ONLY this handle.
                 Every sibling handle over the store just closed is now stale
                 and every statement on it is refused with [stale_msg].  Done
                 after the swap so a VACUUM that failed part-way leaves the
                 cohort untouched. *)
                t.cohort.vacuum_generation <- t.cohort.vacuum_generation + 1;
                t.handle_generation <- t.cohort.vacuum_generation;
                Hashtbl.clear t.views;
                let* () = load_views_into_hashtbl new_store t.views in
                Hashtbl.clear t.triggers;
                let* () = load_triggers_into_hashtbl new_store t.triggers in
                Lwt.return_unit))))
;;

(* ------------------------------------------------------------------ *)
(* #487: positioned parse errors                                        *)
(* ------------------------------------------------------------------ *)

(* Line (1-based) and column (1-based, counted in bytes) of byte [offset]
   within [src].

   The line is counted here from the source text rather than read off the
   lexbuf: [Sql.Lexer] skips whitespace with a single rule and never calls
   {!Lexing.new_line}, so [lexbuf]'s own [pos_lnum] is 1 for every position in
   every statement.  Counting from [src] is correct whatever the lexer does
   with newlines, which is what keeps this from silently reporting "line 1"
   again the moment someone edits [lexer.mll]. *)
let line_col_of_offset src offset =
  let offset = max 0 (min offset (String.length src)) in
  let line = ref 1 in
  let bol = ref 0 in
  for i = 0 to offset - 1 do
    if Char.equal (String.unsafe_get src i) '\n'
    then (
      incr line;
      bol := i + 1)
  done;
  !line, offset - !bol + 1
;;

(* "line L, column C (byte offset O)" for the token the lexer matched last —
   which, when Menhir's monolithic entry point raises [Error], is the token it
   could not shift, and when the lexer itself fails is the text it choked on. *)
let error_position src lexbuf =
  let offset = Lexing.lexeme_start lexbuf in
  let line, col = line_col_of_offset src offset in
  Printf.sprintf "line %d, column %d (byte offset %d)" line col offset
;;

(* #487: the grammar resolves ~290 shift/reduce conflicts arbitrarily, so the
   automaton state at failure does not correspond to an honest "expected X"
   set.  Report the position and the offending token, which are exact, and
   claim nothing about what would have been accepted. *)
let syntax_error_msg src lexbuf =
  let tok = Lexing.lexeme lexbuf in
  let what =
    if String.length tok = 0
    then "unexpected end of input"
    else Printf.sprintf "unexpected token %S" tok
  in
  Printf.sprintf "syntax error at %s: %s" (error_position src lexbuf) what
;;

(* A [Failure] out of the lexer ("unexpected char", "unterminated string
   literal") or out of a semantic action already says what went wrong; it only
   ever lacked the where. *)
let lex_error_msg src lexbuf msg =
  Printf.sprintf "%s at %s" msg (error_position src lexbuf)
;;

let parse sql =
  let lexbuf = Lexing.from_string sql in
  match Sql.Parser.stmt_eof Sql.Lexer.token lexbuf with
  | stmt -> Ok stmt
  | exception Sql.Parser.Error -> Error (Parse (syntax_error_msg sql lexbuf))
  | exception Failure msg -> Error (Parse (lex_error_msg sql lexbuf msg))
;;

let compile t sql =
  match parse sql with
  | Error e -> Lwt.return (Error e)
  | Ok ast ->
    let* bound = Sql.Sema.bind ~views:t.views t.catalog ast in
    (match bound with
     | Error e -> Lwt.return (Error (Sema e))
     | Ok b -> Lwt.return (Ok (Sql.Planner.plan ~cat:t.catalog b)))
;;

(* ------------------------------------------------------------------ *)
(* Multi-database routing (phase 40 / #64)                              *)
(* ------------------------------------------------------------------ *)

(** Pick the handle [sql]'s plan should execute against, given [top]'s
    active schema.  Routing-affecting statements (ATTACH/DETACH and the
    new database_list/active_database PRAGMAs) always run on [top]
    itself so they can read and mutate the top-level routing state;
    everything else runs on the sub-handle under [top.active_schema]. *)
let resolve_target_ast (top : t) (ast : Sql.Ast.stmt) : t =
  let routes_to_top =
    match ast with
    | Sql.Ast.S_attach _ | Sql.Ast.S_detach _ -> true
    | Sql.Ast.S_pragma Sql.Ast.Pragma_database_list -> true
    | Sql.Ast.S_pragma Sql.Ast.Pragma_active_database -> true
    | Sql.Ast.S_pragma (Sql.Ast.Pragma_active_database_set _) -> true
    | _ -> false
  in
  if routes_to_top || String.equal top.active_schema "main"
  then top
  else (
    match Hashtbl.find_opt top.attached top.active_schema with
    | Some sub -> sub
    | None -> top)
;;

(** Parse, route, bind, plan.  Returns the compiled op alongside the
    handle it was bound against — callers must use that handle for any
    downstream execution / state mutation.

    #474: [?on] PINS the statement to a given handle, bypassing
    {!resolve_target_ast} entirely.  It exists for the engine's own internal SQL
    — the reactive-view driver's writes against [_rv_<name>] — which belongs to
    a specific schema and must not follow [top.active_schema].  It is not a
    routing statement and never mutates routing state, so it is not a second way
    for a caller to move their own routing (#598): the caller cannot reach it,
    and nothing it does is observable as a schema switch. *)
let compile_routed ?on (top : t) (sql : string) : (Sql.Plan.op, error) result Lwt.t * t =
  match parse sql with
  | Error e -> Lwt.return (Error e), Option.value on ~default:top
  | Ok ast ->
    let target =
      match on with
      | Some t -> t
      | None -> resolve_target_ast top ast
    in
    let promise =
      let* bound = Sql.Sema.bind ~views:target.views target.catalog ast in
      match bound with
      | Error e -> Lwt.return (Error (Sema e))
      | Ok b -> Lwt.return (Ok (Sql.Planner.plan ~cat:target.catalog b))
    in
    promise, target
;;

(* Prepared statement: holds a compiled plan for repeated execution. *)
type stmt =
  { db_ref : t
  ; plan : Sql.Plan.op
  ; param_names : (string * int) list
  ; mutable finalized : bool
  }

(* ------------------------------------------------------------------ *)
(* Explicit transaction management                                      *)
(* ------------------------------------------------------------------ *)

(* #555: the message every statement gets while the connection is poisoned.
   Deliberately verbose: the only way out is a ROLLBACK, and whoever reads this
   in a log needs to know that the in-flight transaction is now doomed. *)
let poisoned_msg =
  "connection poisoned: a second BEGIN was issued while an explicit transaction was \
   already active on this handle (#555).  Every statement is rejected until ROLLBACK, \
   which aborts the in-flight transaction and clears the poison.  Do not share one Db.t \
   across fibers that use explicit transactions - use Db.create_worker_handle so each \
   fiber has its own transaction slot."
;;

(* #555: THE poison predicate — one spelling, used at every gate.

   It is applied to the handle whose transaction slot the statement would
   actually use, which under multi-database routing (#64) is the handle
   [compile_routed] / [resolve_target_ast] picked, NOT necessarily the top-level
   handle the caller holds.  Each attached schema is its own [Db.t] with its own
   [explicit_txn], so poisoning is per-handle: a poisoned "aux" does not (and
   must not) stop statements that route to "main", which has its own slot and no
   contamination to contain. *)
let is_poisoned t = t.txn_poisoned

(* #555: the handle that non-routing statements currently route to.  Distinct
   from [resolve_target_ast]'s answer for a ROUTING statement such as
   [PRAGMA active_database = …], which is always [top] — which is exactly why
   the schema switch needs this and cannot reuse the generic gate below. *)
let active_handle (top : t) =
  if String.equal top.active_schema "main"
  then top
  else (
    match Hashtbl.find_opt top.attached top.active_schema with
    | Some sub -> sub
    | None -> top)
;;

(** #555: whether ANY handle reachable from this one — the top-level handle or
    any ATTACHed schema — is poisoned.  Must consider the attached sub-handles:
    [BEGIN] routes to the active schema, so under ATTACH the poison lands on the
    sub and a check of the top-level flag alone would report [false] on a
    genuinely poisoned connection. *)
let transaction_poisoned t =
  is_poisoned t || Hashtbl.fold (fun _ sub acc -> acc || is_poisoned sub) t.attached false
;;

(** #634: whether ANY handle reachable from this one — the top-level handle or
    any ATTACHed schema — has been invalidated by a VACUUM run on a sibling
    handle over the same store.  Mirrors {!transaction_poisoned}'s fold for the
    same reason: each ATTACHed schema is its own [Db.t] with its own store and
    its own cohort, so checking only the top-level handle would answer [false]
    for a connection whose "aux" is dead. *)
let stale_after_vacuum t =
  is_stale t || Hashtbl.fold (fun _ sub acc -> acc || is_stale sub) t.attached false
;;

(** #718: the writer-lock accounting for the store THIS handle sits on.

    Deliberately not a fold over the ATTACHed sub-handles, unlike
    {!transaction_poisoned} and {!stale_after_vacuum}: those answer a yes/no
    question about the connection, where an unchecked sub-handle makes the
    answer wrong.  This one describes a lock, each attached schema is a
    different [Store.t] with a different lock, and summing four independent
    locks' hold times would produce a number that describes nothing.  Read a
    sub-handle's lock through its own handle. *)
let lock_stats t = S.lock_stats t.store

let reset_lock_stats t = S.reset_lock_stats t.store

(* #598: whether ANY schema reachable from this handle has an explicit
   transaction open.  Under ATTACH each schema is its own [Db.t] with its own
   slot, so a connection legitimately holds several at once and the question
   "is a transaction open here" has no single-slot answer. *)
let any_explicit_txn (top : t) =
  Option.is_some top.explicit_txn
  || Hashtbl.fold
       (fun _ sub acc -> acc || Option.is_some sub.explicit_txn)
       top.attached
       false
;;

(* #598: the message for a routing statement refused because a transaction is
   open somewhere on the connection.  Distinct from [poisoned_msg]: nothing is
   poisoned, nothing is doomed, and the caller's transaction is intact — the
   statement simply cannot be honoured while it is open. *)
let routing_blocked_msg verb =
  Printf.sprintf
    "%s refused: an explicit transaction is open on this connection (#598).  Under \
     ATTACH each schema has its own transaction slot while the active schema is shared \
     handle state, so moving the routing now would silently autocommit the caller's next \
     write into a different database and orphan the open transaction.  COMMIT or \
     ROLLBACK first."
    verb
;;

(* #740: [PRAGMA wal_checkpoint] issued inside an explicit transaction used to
   SELF-DEADLOCK.  [begin_txn] -> [Store.rw_begin] takes the store's writer lock
   and holds it for the whole transaction (only COMMIT/ROLLBACK release it);
   [Store.checkpoint] takes the same lock for its install phase, and [Rwlock] is
   deliberately not re-entrant ("a fiber that holds the writer lock must not call
   [acquire_write] again"), so the fiber parked on itself and the connection was
   unusable from that point on.  Pre-existing, and not a #719 regression: before
   the phase split the checkpoint parked on the lock it took first, afterwards it
   parks at [ckpt_finish]'s single [acquire_writer] — with [ckpt_mutex] held as
   well, so every later checkpoint on the store queues behind the wedged one.

   Refusing is the same shape as #473's [DROP REACTIVE VIEW] refusal and #598's
   routing refusals: report the constraint up front rather than hang.  Making
   [Rwlock] re-entrant is the alternative, and it is a much larger change that
   interacts with #555/#585's transaction-ownership work.  sqlite3 also declines
   [PRAGMA wal_checkpoint] mid-transaction (it reports SQLITE_LOCKED rather than
   checkpointing), so this is not a divergence.

   Scoped to the ROUTED handle's own slot, not [any_explicit_txn]: under ATTACH
   each schema is its own [Db.t] over its own [Store.t] with its own writer lock,
   so a transaction open on "aux" cannot deadlock a checkpoint of "main".  A
   transaction held by a DIFFERENT handle over the SAME store (a
   {!create_worker_handle} sibling) is not this bug either — that checkpoint
   blocks and then proceeds, which is ordinary mutual exclusion. *)
let wal_checkpoint_in_txn_msg =
  "PRAGMA wal_checkpoint refused: an explicit transaction is open on this handle \
   (#740).  The transaction holds the store's writer lock for its whole extent and the \
   checkpoint must acquire that same lock, which is not re-entrant - issuing it here \
   would deadlock the connection.  COMMIT or ROLLBACK first."
;;

(* #740: true when [op] is the checkpoint PRAGMA and [t] is the handle whose
   transaction would deadlock it.  Consulted from {!execute_control_op} (the
   one-shot path) and from {!run_core} (the prepared path, which bypasses
   [execute_control_op] entirely). *)
let wal_checkpoint_would_deadlock t op =
  match op with
  | Sql.Plan.Op_pragma_wal_checkpoint -> Option.is_some t.explicit_txn
  | _ -> false
;;

let begin_txn t =
  match t.explicit_txn with
  | _ when is_poisoned t -> Lwt.return (Error (Runtime poisoned_msg))
  | Some _ ->
    (* #555: refusing this BEGIN is not sufficient — without the poison the
       caller's *next* statement would silently join the transaction that won
       the race, and its COMMIT would commit that transaction's work.  Poison
       the handle so the whole sequence fails loudly instead, for as long as the
       poison lasts: it is cleared by the ROLLBACK that recovery prescribes, and
       past that point the same contamination is reachable with the roles
       exchanged (#584). *)
    t.txn_poisoned <- true;
    Lwt.return (Error (Runtime "transaction already active"))
  | None ->
    let* tx = S.rw_begin t.store in
    (* #269: defensive — start with an empty schema-undo log so a stale entry
       from a prior op can never leak into this transaction's rollback. (It is
       already cleared by every commit/rollback path; this just hardens it.) *)
    Cat.commit_schema_changes t.catalog;
    t.explicit_txn <- Some tx;
    (* #585: a transaction opened here has NO owner until someone stamps one.
       [with_transaction] assigns its token immediately after this returns, so
       clearing here is invisible to it — but for a bare BEGIN it is what stops a
       previous scope's token from surviving into a transaction that is not that
       scope's.  Without this the #584 boundary guard false-matches and commits
       the new transaction on the old scope's behalf. *)
    t.txn_scope <- None;
    Lwt.return (Ok ())
;;

(* Roll back the active explicit transaction and reset ALL connection txn state
   in one place: store rollback + #269 schema-undo replay + #293/#301 rowid-
   counter recompute + the [explicit_txn]/[savepoint_names]/[auto_began] resets +
   FK/defer cleanup.  Every rollback path (user-issued, #286's poisoned-COMMIT
   force, the COMMIT-time deferred-FK arm) routes through here so the reset
   sequence can't drift between them — #301 was exactly that drift. *)
let force_rollback_txn t tx =
  let* () = S.rollback tx in
  Cat.rollback_schema_changes t.catalog;
  (* #293: an in-txn INSERT bumped the in-memory next_rowid counter; the store
     row reverted with [S.rollback] above but the cache did not.  Re-derive each
     rowid table's counter from the rolled-back data tree so the next allocation
     reuses a rolled-back rowid (SQLite parity for plain rowid tables).  Done
     after [S.rollback] released the RW lock so the recompute's RO scan is safe
     — #706: the scan runs unlocked, but the recompute re-acquires the writer
     lock around its publish and only takes effect there if nothing else has
     published to the shared counter in the meantime (see the doc comment on
     [Cat.recompute_rowid_counters_after_rollback]). *)
  let* () = Cat.recompute_rowid_counters_after_rollback t.catalog in
  (* Reload all columnar stores from the rolled-back B-tree.  The RO snapshot
     opened by [load_columnar_stores] sees the last committed state, which is
     correct after a full rollback. *)
  let* () = Cat.load_columnar_stores t.catalog t.store in
  t.explicit_txn <- None;
  (* #585: the transaction this token named no longer exists.  Kept adjacent to
     the [explicit_txn] reset on purpose — the token is only meaningful while the
     slot holds the transaction it was minted for, so the two must always be
     cleared together or the boundary guard starts matching a dead token. *)
  t.txn_scope <- None;
  t.savepoint_names <- [];
  t.auto_began <- false;
  Cat.clear_pending_fk_checks t.catalog;
  Cat.set_defer_fks_pragma t.catalog false;
  Lwt.return_unit
;;

(** Drain any pending deferred FK checks queued during the transaction.
    For each entry, run the recheck closure (passing the active write txn so
    it observes uncommitted writes); on the first one still violated,
    rollback the transaction and raise [Failure].  All entries are removed
    from the queue regardless of outcome — including on exception paths
    (wrapped via [Lwt.finalize] to keep the queue state well-defined if a
    recheck raises a non-[Failure] exception). *)
let drain_pending_fks_or_fail t (tx : S.rw S.txn) : unit Lwt.t =
  let pending = Cat.drain_pending_fk_checks t.catalog in
  let rec loop = function
    | [] -> Lwt.return_unit
    | check :: rest ->
      let* still = check.Cat.pfk_recheck.Cat.recheck tx in
      if still
      then
        (* #309: roll back and reset all connection txn state via the shared
           [force_rollback_txn] (store rollback + #269 schema-undo + #293/#301
           rowid recompute + field/FK/defer resets), then surface the violation.
           The extra [clear_pending_fk_checks] it does is idempotent with this
           function's [Lwt.finalize] arm below. *)
        let* () = force_rollback_txn t tx in
        Lwt.fail_with check.Cat.pfk_message
      else loop rest
  in
  Lwt.finalize
    (fun () -> loop pending)
    (fun () ->
       Cat.clear_pending_fk_checks t.catalog;
       Lwt.return_unit)
;;

(** Drain pending FK checks after an auto-commit DML statement.  The active
    write txn was already committed inside [Sql.Exec.execute_with_count]; we
    open a fresh RO snapshot for the recheck (it now sees the just-committed
    writes).  On the first still-violated check, returns
    [Error (Runtime msg)]; otherwise [Ok ()].  The pending queue is cleared
    unconditionally — including on non-[Failure] exceptions from the
    recheck closure — to keep state well-defined for the next statement. *)
let drain_pending_fks_autocommit t : (unit, error) result Lwt.t =
  let pending = Cat.drain_pending_fk_checks t.catalog in
  if pending = []
  then Lwt.return (Ok ())
  else
    Lwt.finalize
      (fun () ->
         let* ro_tx = S.ro_begin t.store in
         Lwt.finalize
           (fun () ->
              let rec loop = function
                | [] -> Lwt.return (Ok ())
                | check :: rest ->
                  let* still = check.Cat.pfk_recheck.Cat.recheck ro_tx in
                  if still
                  then Lwt.return (Error (Runtime check.Cat.pfk_message))
                  else loop rest
              in
              loop pending)
           (fun () -> S.ro_end ro_tx))
      (fun () ->
         Cat.clear_pending_fk_checks t.catalog;
         Lwt.return_unit)
;;

let commit_txn t =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction"))
  | Some tx when Cat.schema_txn_poisoned t.catalog ->
    (* #286: an in-txn DDL statement failed partway through, leaving partial
       on-disk effects under this borrowed txn.  The transaction is
       uncommittable — roll it back (schema-undo log + store rollback unwind
       cleanly) and surface an error rather than persist half-applied DDL. *)
    let* () = force_rollback_txn t tx in
    Lwt.return
      (Error
         (Runtime
            "cannot commit transaction - a DDL statement failed partway through; the \
             transaction was uncommittable and has been rolled back"))
  | Some tx ->
    (* Drain deferred FK checks first; if any still violate, this raises
       and the txn has already been rolled back. *)
    Lwt.catch
      (fun () ->
         let* () = drain_pending_fks_or_fail t tx in
         (* #347: write deferred rowid counters once before committing the txn. *)
         let* () = Cat.flush_dirty_counters_tx t.catalog tx in
         (* Persist any dirty columnar stores before committing. *)
         let* saved = Cat.persist_dirty_columnar_stores t.catalog tx in
         let* () = S.commit tx in
         (* Dirty flags are cleared only after commit succeeds — if commit
             fails (disk-full, fsync error), the B-tree changes are discarded
             but the in-memory Col_store retains the rows, and dirty stays
             true so the next cycle retries. *)
         List.iter Granary_columnar.Col_store.mark_clean saved;
         (* #269: in-txn DDL's cache changes are now durable — drop the undo log. *)
         Cat.commit_schema_changes t.catalog;
         t.explicit_txn <- None;
         (* #585: cleared with the slot — see [force_rollback_txn]. *)
         t.txn_scope <- None;
         t.savepoint_names <- [];
         t.auto_began <- false;
         Cat.set_defer_fks_pragma t.catalog false;
         Lwt.return (Ok ()))
      (function
        | Failure msg -> Lwt.return (Error (Runtime msg))
        | exn -> Lwt.fail exn)
;;

(* #555: ROLLBACK is the sole exit from the poisoned state, and it is
   unconditional — it clears the flag whether or not there was a transaction
   left to abort, so a handle can never be left permanently unusable.  Any fiber
   may issue it; there is no ownership to check, which is precisely the
   limitation #585 (a scoped transaction combinator) would remove.

   That unconditional clear is also the end of the containment window: it frees
   the slot for a fresh BEGIN while the fiber whose transaction was just aborted
   has not been told, which is #584.  Making the clear conditional does not help
   — without an owner token there is nobody to condition it on, and refusing to
   clear would strand the handle instead. *)
let rollback_txn t =
  let was_poisoned = is_poisoned t in
  t.txn_poisoned <- false;
  match t.explicit_txn with
  | None ->
    (* #585: no transaction left, so no owner either.  The [Some] arm below
       clears it through [force_rollback_txn]; this arm covers the degenerate
       poison-with-no-transaction case so no path out of [ROLLBACK] can leave a
       token behind. *)
    t.txn_scope <- None;
    if was_poisoned
    then Lwt.return (Ok ())
    else Lwt.return (Error (Runtime "no active transaction"))
  | Some tx ->
    (* #269: [force_rollback_txn] reverts any in-txn DDL's in-memory cache
       changes (schema-undo log) along with the store. *)
    let* () = force_rollback_txn t tx in
    (* #427: the aborted txn's row changes never happened — drop them. *)
    rv_clear_pending t;
    Lwt.return (Ok ())
;;

let savepoint_txn t name =
  let* tx =
    match t.explicit_txn with
    | Some tx -> Lwt.return tx
    | None ->
      let* tx = S.rw_begin t.store in
      (* #269/#286: harden the auto-begin path the same way [begin_txn] does —
         clear any stale schema-undo log AND poison flag so a fresh auto-began
         savepoint transaction never inherits a prior transaction's state. *)
      Cat.commit_schema_changes t.catalog;
      t.explicit_txn <- Some tx;
      (* #585: an auto-begun transaction has no owner — same reasoning as
         [begin_txn]'s [None] arm.  Only reachable when the slot was empty, so
         it cannot be clearing a live scope's token. *)
      t.txn_scope <- None;
      t.auto_began <- true;
      Lwt.return tx
  in
  let* () = S.savepoint_begin tx name in
  (* #280: mark the schema-undo log so [ROLLBACK TO]/[RELEASE] of this savepoint
     can unwind only the DDL cache-changes registered since here. *)
  Cat.savepoint_begin_schema t.catalog name;
  t.savepoint_names <- name :: t.savepoint_names;
  Lwt.return (Ok ())
;;

let release_savepoint t name =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction for RELEASE"))
  | Some tx ->
    let* () = S.savepoint_release tx name in
    (* #280: merge this savepoint's schema-undo entries into the enclosing scope
       (no undo runs; an outer ROLLBACK still unwinds them). *)
    Cat.savepoint_release_schema t.catalog name;
    if List.mem name t.savepoint_names
    then (
      let rec drop = function
        | [] -> []
        | n :: rest when String.equal n name -> rest
        | _ :: rest -> drop rest
      in
      t.savepoint_names <- drop t.savepoint_names);
    if t.auto_began && t.savepoint_names = []
    then (
      if Cat.schema_txn_poisoned t.catalog
      then
        (* #286: releasing the last savepoint would auto-commit, but a failed
           in-txn DDL poisoned the transaction — roll back instead. *)
        let* () = force_rollback_txn t tx in
        Lwt.return
          (Error
             (Runtime
                "cannot commit transaction - a DDL statement failed partway through; the \
                 transaction was uncommittable and has been rolled back"))
      else
        (* #347: write deferred rowid counters once before committing the txn. *)
        let* () = Cat.flush_dirty_counters_tx t.catalog tx in
        (* Persist any dirty columnar stores before committing. *)
        let* saved = Cat.persist_dirty_columnar_stores t.catalog tx in
        let* () = S.commit tx in
        List.iter Granary_columnar.Col_store.mark_clean saved;
        (* #269: finalize any in-txn DDL's cache changes on this auto-commit. *)
        Cat.commit_schema_changes t.catalog;
        t.explicit_txn <- None;
        (* #585: cleared with the slot — see [force_rollback_txn]. *)
        t.txn_scope <- None;
        t.auto_began <- false;
        Lwt.return (Ok ()))
    else Lwt.return (Ok ())
;;

let rollback_to_savepoint t name =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction for ROLLBACK TO"))
  | Some tx ->
    let* () = S.savepoint_rollback tx name in
    (* #280: revert the in-memory cache mutations of DDL registered since this
       savepoint so the catalog agrees with the store rolled back to [name]. *)
    Cat.savepoint_rollback_schema t.catalog name;
    if List.mem name t.savepoint_names
    then (
      let rec trim = function
        | [] -> []
        | n :: _ as rest when String.equal n name -> rest
        | _ :: rest -> trim rest
      in
      t.savepoint_names <- trim t.savepoint_names);
    (* #427: a partial rollback makes the accumulated deltas untrustworthy;
       resync every affected view from base tables at the enclosing COMMIT. *)
    t.rv_resync <- true;
    Lwt.return (Ok ())
;;

(* ------------------------------------------------------------------ *)
(* Trigger firing helpers                                               *)
(* ------------------------------------------------------------------ *)

let trigger_meta_of_ast = function
  | Sql.Ast.S_create_trigger { timing; event; table; when_; body; _ } ->
    let trig_timing =
      match timing with
      | Sql.Ast.TT_before -> `Before
      | Sql.Ast.TT_after -> `After
      | Sql.Ast.TT_instead_of -> `Instead_of
    in
    let trig_event =
      match event with
      | Sql.Ast.TE_insert -> `Insert
      | Sql.Ast.TE_update -> `Update
      | Sql.Ast.TE_delete -> `Delete
    in
    Some
      { trig_table = table; trig_timing; trig_event; trig_when = when_; trig_body = body }
  | _ -> None
;;

let value_to_literal = function
  | Row.V_int n -> Sql.Ast.L_int n
  | Row.V_text s -> Sql.Ast.L_text s
  | Row.V_real f -> Sql.Ast.L_real f
  | Row.V_blob b -> Sql.Ast.L_blob b
  | Row.V_null -> Sql.Ast.L_null
;;

(** Walk an Ast.expr, applying [f tbl col] to each E_tbl_col.
    If f returns Some lit, substitute E_lit; otherwise keep the original. *)
let rec map_expr f e =
  let go = map_expr f in
  match e with
  | Sql.Ast.E_tbl_col (tbl, col) ->
    (match f tbl col with
     | Some lit -> Sql.Ast.E_lit lit
     | None -> e)
  | Sql.Ast.E_binop (op, a, b) -> Sql.Ast.E_binop (op, go a, go b)
  | Sql.Ast.E_not x -> Sql.Ast.E_not (go x)
  | Sql.Ast.E_is_null x -> Sql.Ast.E_is_null (go x)
  | Sql.Ast.E_is_not_null x -> Sql.Ast.E_is_not_null (go x)
  | Sql.Ast.E_neg x -> Sql.Ast.E_neg (go x)
  | Sql.Ast.E_bitnot x -> Sql.Ast.E_bitnot (go x)
  | Sql.Ast.E_between (x, lo, hi) -> Sql.Ast.E_between (go x, go lo, go hi)
  | Sql.Ast.E_in (x, vs) -> Sql.Ast.E_in (go x, List.map go vs)
  | Sql.Ast.E_func (fn, args) -> Sql.Ast.E_func (fn, List.map go args)
  | Sql.Ast.E_agg (fn, arg) -> Sql.Ast.E_agg (fn, Option.map go arg)
  | Sql.Ast.E_agg_distinct (fn, arg) -> Sql.Ast.E_agg_distinct (fn, go arg)
  | Sql.Ast.E_case { scrutinee; branches; else_ } ->
    Sql.Ast.E_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Sql.Ast.E_cast (x, ty) -> Sql.Ast.E_cast (go x, ty)
  | Sql.Ast.E_collate (x, c) -> Sql.Ast.E_collate (go x, c)
  (* NEW/OLD references inside subquery expressions (E_subquery, E_exists,
     E_in_select, E_window) are not substituted. Trigger bodies should not
     reference NEW/OLD inside subqueries. *)
  | other -> other
;;

let make_subst_fn ~schema ~(new_row : Row.t option) ~(old_row : Row.t option) =
  let col_idx name =
    let rec fi i = function
      | [] -> None
      | (c : Row.column) :: _ when String.equal c.name name -> Some i
      | _ :: rest -> fi (i + 1) rest
    in
    fi 0 schema
  in
  fun tbl col ->
    match String.uppercase_ascii tbl with
    | "NEW" ->
      (match new_row, col_idx col with
       | Some r, Some i -> Some (value_to_literal r.(i))
       | _ -> None)
    | "OLD" ->
      (match old_row, col_idx col with
       | Some r, Some i -> Some (value_to_literal r.(i))
       | _ -> None)
    | _ -> None
;;

(** Substitute NEW.col / OLD.col references in an Ast.stmt with literal values. *)
let subst_new_old ~schema ~new_row ~old_row stmt =
  let f = make_subst_fn ~schema ~new_row ~old_row in
  let ge = map_expr f in
  match stmt with
  | Sql.Ast.S_insert { table; columns; values; on_conflict; returning; upsert_update } ->
    Sql.Ast.S_insert
      { table
      ; columns
      ; values = List.map (List.map ge) values
      ; on_conflict
      ; returning = List.map ge returning
      ; upsert_update =
          Option.map
            (fun u ->
               { u with
                 Sql.Ast.assignments =
                   List.map (fun (c, e) -> c, ge e) u.Sql.Ast.assignments
               })
            upsert_update
      }
  | Sql.Ast.S_update { table; assignments; where; order; limit; offset; returning } ->
    Sql.Ast.S_update
      { table
      ; assignments = List.map (fun (c, e) -> c, ge e) assignments
      ; where = Option.map ge where
      ; order = List.map (fun ok -> { ok with Sql.Ast.expr = ge ok.Sql.Ast.expr }) order
      ; limit
      ; offset
      ; returning = List.map ge returning
      }
  | Sql.Ast.S_delete { table; where; order; limit; offset; returning } ->
    Sql.Ast.S_delete
      { table
      ; where = Option.map ge where
      ; order = List.map (fun ok -> { ok with Sql.Ast.expr = ge ok.Sql.Ast.expr }) order
      ; limit
      ; offset
      ; returning = List.map ge returning
      }
  | Sql.Ast.S_select
      { distinct
      ; proj
      ; table
      ; table_alias
      ; joins
      ; where
      ; group_by
      ; having
      ; order
      ; limit
      ; offset
      } ->
    Sql.Ast.S_select
      { distinct
      ; proj =
          (match proj with
           | `Exprs es -> `Exprs (List.map (fun (e, a) -> ge e, a) es)
           | other -> other)
      ; table
      ; table_alias
      ; joins = List.map (fun j -> { j with Sql.Ast.on = ge j.Sql.Ast.on }) joins
      ; where = Option.map ge where
      ; group_by
      ; having = Option.map ge having
      ; order = List.map (fun ok -> { ok with Sql.Ast.expr = ge ok.Sql.Ast.expr }) order
      ; limit
      ; offset
      }
  | other -> other
;;

(** Maximum trigger recursion depth.  Mirrors SQLite's
    SQLITE_MAX_TRIGGER_DEPTH default. *)
let max_trigger_depth = 32

(** Compile and execute one pre-substituted trigger body statement within the
    db context.  This installs hooks for the nested DML so triggers fired from
    within a trigger body can themselves fire further triggers, up to
    [max_trigger_depth] levels deep.

    [?tx] is the active write tx of the parent DML.  Phase 38: when provided,
    nested trigger DML runs with [In_txn tx] so triggers share the parent's
    transaction (atomic rollback on failure, no nested deadlock).  When [None],
    falls back to [t.explicit_txn] then [Auto]. *)
let rec fire_trigger_stmt ?(tx : S.rw S.txn option = None) t stmt =
  if t.trigger_depth >= max_trigger_depth
  then
    Lwt.fail_with
      (Printf.sprintf "trigger recursion limit (%d) exceeded" max_trigger_depth)
  else if t.trigger_depth > 0 && not (Cat.get_recursive_triggers t.catalog)
  then
    (* Recursion disabled by PRAGMA recursive_triggers = OFF; nested DML
       inside a trigger body does not fire further triggers. The top-level
       trigger (depth = 0 → about to become 1) still fires. *)
    Lwt.return_unit
  else (
    t.trigger_depth <- t.trigger_depth + 1;
    let finally () =
      t.trigger_depth <- t.trigger_depth - 1;
      Lwt.return_unit
    in
    Lwt.finalize
      (fun () ->
         let* bound = Sql.Sema.bind ~views:t.views t.catalog stmt in
         match bound with
         | Error e ->
           Lwt.fail_with (Format.asprintf "trigger sema: %a" Sql.Sema.pp_error e)
         | Ok b -> run_trigger_op t ~tx b)
      finally)

(** Build a DML hook for exec.ml that fires triggers.
    Returns None if no triggers exist for the given table/timing/event (fast path).
    Scans the trigger hashtable exactly once to collect matching triggers.

    Phase 38 (#138/#139): the hook receives [~tx], the parent DML's active
    write txn.  Trigger nested DML runs with [In_txn tx] so triggers share
    the parent transaction.  This delivers atomic rollback when a trigger
    body raises and removes the nested-trigger deadlock that previously
    forced AFTER firing outside the parent txn. *)
and make_trigger_hook t table_meta ~timing ~event =
  if Hashtbl.length t.triggers = 0
  then None
  else (
    let matching =
      Hashtbl.fold
        (fun _name ast acc ->
           match trigger_meta_of_ast ast with
           | Some m
             when String.equal m.trig_table table_meta.Cat.name
                  && m.trig_timing = timing
                  && m.trig_event = event -> m :: acc
           | _ -> acc)
        t.triggers
        []
    in
    if matching = []
    then None
    else
      Some
        (fun ~tx ~new_row ~old_row ->
          let schema = table_meta.Cat.columns in
          Lwt_list.iter_s
            (fun m ->
               let* should_fire =
                 match m.trig_when with
                 | None -> Lwt.return true
                 | Some when_expr ->
                   let subst =
                     map_expr (make_subst_fn ~schema ~new_row ~old_row) when_expr
                   in
                   let* bound =
                     Sql.Sema.bind
                       ~views:t.views
                       t.catalog
                       (Sql.Ast.S_const_select { exprs = [ subst, None ] })
                   in
                   (match bound with
                    | Error e ->
                      Lwt.fail_with
                        (Format.asprintf
                           "trigger WHEN clause binding error: %a"
                           Sql.Sema.pp_error
                           e)
                    | Ok bw ->
                      let op = Sql.Planner.plan ~cat:t.catalog bw in
                      (* WHEN clause query runs inside the parent txn so it sees
                         the in-flight writes (matches SQLite semantics for AFTER
                         WHEN). *)
                      let mode = Sql.Exec.In_txn tx in
                      let* stream =
                        Sql.Exec.query ~mode ~clock:t.clock t.store t.catalog op
                      in
                      let* rows = Lwt_stream.to_list stream in
                      Lwt.return
                        (match rows with
                         | row :: _ when Array.length row > 0 ->
                           (match row.(0) with
                            | Row.V_int 0L | Row.V_null -> false
                            | _ -> true)
                         | _ -> true))
               in
               if not should_fire
               then Lwt.return_unit
               else
                 Lwt_list.iter_s
                   (fun stmt ->
                      let substituted = subst_new_old ~schema ~new_row ~old_row stmt in
                      fire_trigger_stmt ~tx:(Some tx) t substituted)
                   m.trig_body)
            matching))

(** Build the REPLACE-conflict-delete and UPSERT-conflict-update hooks for
    an INSERT-ish op.  These fire when [INSERT OR REPLACE] conflicts on a
    UNIQUE index (DELETE triggers on the displaced row) or when the UPSERT
    [DO UPDATE] branch runs (UPDATE triggers on the updated row).

    Phase 38 (#139): returns BOTH BEFORE and AFTER hooks for both displaced
    cases.  All four fire inside the parent txn.  Returns all-[None] for
    ops that are not insert-like.

    Part of the [fire_trigger_stmt] recursive group so that nested trigger
    bodies that perform INSERT OR REPLACE / UPSERT also fire DELETE/UPDATE
    triggers on conflict-displaced or upsert-updated rows. *)
and insert_replace_upsert_hooks t op =
  let table_meta_opt =
    match op with
    | Sql.Plan.Op_insert { table_meta; _ } | Sql.Plan.Op_insert_select { table_meta; _ }
      -> Some table_meta
    | _ -> None
  in
  match table_meta_opt with
  | None -> None, None, None, None
  | Some tm ->
    let delete_before_hook = make_trigger_hook t tm ~timing:`Before ~event:`Delete in
    let delete_after_hook = make_trigger_hook t tm ~timing:`After ~event:`Delete in
    let update_before_hook = make_trigger_hook t tm ~timing:`Before ~event:`Update in
    let update_after_hook = make_trigger_hook t tm ~timing:`After ~event:`Update in
    let on_replace_delete_before =
      Option.map
        (fun hook -> fun ~tx ~old_row -> hook ~tx ~new_row:None ~old_row:(Some old_row))
        delete_before_hook
    in
    let on_replace_delete =
      Option.map
        (fun hook -> fun ~tx ~old_row -> hook ~tx ~new_row:None ~old_row:(Some old_row))
        delete_after_hook
    in
    let on_upsert_update_before =
      Option.map
        (fun hook ->
           fun ~tx ~old_row ~new_row ->
           hook ~tx ~new_row:(Some new_row) ~old_row:(Some old_row))
        update_before_hook
    in
    let on_upsert_update =
      Option.map
        (fun hook ->
           fun ~tx ~old_row ~new_row ->
           hook ~tx ~new_row:(Some new_row) ~old_row:(Some old_row))
        update_after_hook
    in
    on_replace_delete_before, on_replace_delete, on_upsert_update_before, on_upsert_update

(** Plan and execute a trigger-body statement [b], installing the nested
    trigger + REPLACE/UPSERT hooks so further triggers fire. *)
and run_trigger_op t ~tx b =
  let op = Sql.Planner.plan ~cat:t.catalog b in
  let mode =
    match tx with
    | Some tx -> Sql.Exec.In_txn tx
    | None ->
      (match t.explicit_txn with
       | None -> Sql.Exec.Auto
       | Some etx -> Sql.Exec.In_txn etx)
  in
  let table_meta_opt, event_opt =
    match op with
    | Sql.Plan.Op_insert { table_meta; _ } | Sql.Plan.Op_insert_select { table_meta; _ }
      -> Some table_meta, Some `Insert
    | Sql.Plan.Op_update { table_meta; _ } -> Some table_meta, Some `Update
    | Sql.Plan.Op_delete { table_meta; _ } -> Some table_meta, Some `Delete
    | _ -> None, None
  in
  let before_hook, after_hook =
    match table_meta_opt, event_opt with
    | Some tm, Some ev ->
      ( make_trigger_hook t tm ~timing:`Before ~event:ev
      , make_trigger_hook t tm ~timing:`After ~event:ev )
    | _ -> None, None
  in
  let ( on_replace_delete_before
      , on_replace_delete
      , on_upsert_update_before
      , on_upsert_update )
    =
    match op with
    | Sql.Plan.Op_insert _ | Sql.Plan.Op_insert_select _ ->
      insert_replace_upsert_hooks t op
    | _ -> None, None, None, None
  in
  Sql.Exec.execute
    ~before_hook
    ~after_hook
    ~on_replace_delete_before
    ~on_replace_delete
    ~on_upsert_update_before
    ~on_upsert_update
    ~mode
    ~clock:t.clock
    t.store
    t.catalog
    op
;;

(** Extract column names from a view query's projection, in order.
    Returns [] if the projection cannot be resolved to simple column names. *)
let view_col_names view_query =
  let extract_from_select = function
    | Sql.Ast.S_select { proj = `Cols cols; _ } -> cols
    | Sql.Ast.S_select { proj = `Exprs exprs; _ } ->
      List.filter_map
        (fun (expr, alias) ->
           match alias with
           | Some a -> Some a
           | None ->
             (match expr with
              | Sql.Ast.E_col name -> Some name
              | Sql.Ast.E_tbl_col (_, col) -> Some col
              | _ -> None))
        exprs
    | _ -> []
  in
  match view_query with
  | Sql.Ast.S_select _ -> extract_from_select view_query
  | Sql.Ast.S_compound { left; _ } -> extract_from_select left
  | _ -> []
;;

(** Build a Row.column schema list from a list of column name strings. *)
let make_col_schema names =
  List.map
    (fun col_name ->
       { Row.name = col_name
       ; Row.ty = Row.Text
       ; Row.not_null = false
       ; Row.primary_key = false
       ; Row.pk_desc = false
       ; Row.default = None
       ; Row.check_sql = None
       ; Row.generated_as = None
       })
    names
;;

(** Collect INSTEAD OF triggers on [view_name] matching the given event. *)
let find_instead_of t view_name event =
  Hashtbl.fold
    (fun _name trig_ast acc ->
       match trigger_meta_of_ast trig_ast with
       | Some m
         when String.equal m.trig_table view_name
              && m.trig_timing = `Instead_of
              && m.trig_event = event -> m :: acc
       | _ -> acc)
    t.triggers
    []
;;

(** Evaluate a literal INSERT/UPDATE value expression to a [Row.value];
    non-literal expressions collapse to NULL (matches Phase 32 semantics). *)
let eval_insert_ast_value = function
  | Sql.Ast.E_lit (Sql.Ast.L_int n) -> Row.V_int n
  | Sql.Ast.E_lit (Sql.Ast.L_text s) -> Row.V_text s
  | Sql.Ast.E_lit (Sql.Ast.L_real f) -> Row.V_real f
  | Sql.Ast.E_lit (Sql.Ast.L_blob b) -> Row.V_blob b
  | Sql.Ast.E_lit Sql.Ast.L_null -> Row.V_null
  | Sql.Ast.E_neg (Sql.Ast.E_lit (Sql.Ast.L_int n)) -> Row.V_int (Int64.neg n)
  | _ -> Row.V_null
;;

let instead_of_insert t view_name ~columns ~values =
  let matching = find_instead_of t view_name `Insert in
  if matching = []
  then
    Lwt.return
      (Error
         (Sema
            (Sql.Sema.Unsupported
               (Printf.sprintf
                  "view '%s' is not directly modifiable (no INSTEAD OF INSERT trigger)"
                  view_name))))
  else (
    (* When INSERT has no explicit column list, derive column names from the view's SELECT. *)
    let effective_cols =
      if columns <> []
      then columns
      else (
        match Hashtbl.find_opt t.views view_name with
        | Some view_query -> view_col_names view_query
        | None -> [])
    in
    (* Validate column count matches values arity *)
    let* () =
      match values with
      | [] -> Lwt.return_unit
      | first_row :: _ ->
        let n_vals = List.length first_row in
        let n_cols = List.length effective_cols in
        if effective_cols = []
        then Lwt.return_unit (* no columns = no schema, trigger may not use NEW.col *)
        else if n_cols <> n_vals
        then
          Lwt.fail_with
            (Printf.sprintf
               "INSTEAD OF INSERT on view '%s': cannot derive column names for all \
                values (view has computed expressions without aliases)"
               view_name)
        else Lwt.return_unit
    in
    let* () =
      Lwt_list.iter_s
        (fun value_exprs ->
           let schema = make_col_schema effective_cols in
           let new_vals = List.map eval_insert_ast_value value_exprs in
           let new_row = Some (Array.of_list new_vals) in
           Lwt_list.iter_s
             (fun m ->
                let substituted_body =
                  List.map
                    (fun stmt -> subst_new_old ~schema ~new_row ~old_row:None stmt)
                    m.trig_body
                in
                Lwt_list.iter_s (fire_trigger_stmt t) substituted_body)
             matching)
        values
    in
    Lwt.return (Ok ()))
;;

let instead_of_delete t view_name =
  let matching = find_instead_of t view_name `Delete in
  if matching = []
  then
    Lwt.return
      (Error
         (Sema
            (Sql.Sema.Unsupported
               (Printf.sprintf
                  "view '%s' is not directly modifiable (no INSTEAD OF DELETE trigger)"
                  view_name))))
  else (
    let schema = [] in
    let* () =
      Lwt_list.iter_s
        (fun m ->
           let substituted_body =
             List.map
               (fun stmt -> subst_new_old ~schema ~new_row:None ~old_row:None stmt)
               m.trig_body
           in
           Lwt_list.iter_s (fire_trigger_stmt t) substituted_body)
        matching
    in
    Lwt.return (Ok ()))
;;

(** Resolve the OLD rows for an INSTEAD OF UPDATE by re-selecting from the
    view under [where].  Returns [`Resolved rows] when column names are
    derivable and the SELECT succeeds, else [`Unresolved] (caller fires once
    with OLD=NULL — the Phase 32 fallback). *)
let resolve_instead_of_update_olds t view_name ~where ~old_col_names =
  match Hashtbl.find_opt t.views view_name with
  | None -> Lwt.return `Unresolved
  | Some _ when old_col_names = [] ->
    (* View projects unnamed expressions; OLD.col can't bind. *)
    Lwt.return `Unresolved
  | Some _ ->
    (* Re-serialize the WHERE clause to SQL.  [expr_to_sql] raises Failure
       for subqueries, aggregates, blob literals, etc.; in those cases we
       cannot determine OLD rows, so fall back to firing once with OLD=NULL. *)
    let where_sql_opt =
      match where with
      | None -> Some None
      | Some w ->
        (try Some (Some (Sql.Ast.expr_to_sql w)) with
         | Failure _ -> None)
    in
    (match where_sql_opt with
     | None -> Lwt.return `Unresolved
     | Some where_sql ->
       let select_sql =
         match where_sql with
         | None -> Printf.sprintf "SELECT * FROM %s" view_name
         | Some w_sql -> Printf.sprintf "SELECT * FROM %s WHERE %s" view_name w_sql
       in
       let* op = compile t select_sql in
       (match op with
        | Error _ -> Lwt.return `Unresolved
        | Ok plan_op ->
          let mode =
            match t.explicit_txn with
            | None -> Sql.Exec.Auto
            | Some tx -> Sql.Exec.In_txn tx
          in
          (match Sql.Exec.query ~mode ~clock:t.clock t.store t.catalog plan_op with
           | exception Failure _ -> Lwt.return `Unresolved
           | lwt_stream ->
             Lwt.catch
               (fun () ->
                  let* stream = lwt_stream in
                  let* rows = Lwt_stream.to_list stream in
                  Lwt.return (`Resolved rows))
               (fun _ -> Lwt.return `Unresolved))))
;;

let instead_of_update t view_name ~assignments ~where =
  let matching = find_instead_of t view_name `Update in
  if matching = []
  then
    Lwt.return
      (Error
         (Sema
            (Sql.Sema.Unsupported
               (Printf.sprintf
                  "view '%s' is not directly modifiable (no INSTEAD OF UPDATE trigger)"
                  view_name))))
  else (
    (* Build NEW row from assignment expressions.
       Only literal values are substituted; complex expressions become NULL. *)
    let assign_cols = List.map fst assignments in
    let assign_exprs = List.map snd assignments in
    let new_schema = make_col_schema assign_cols in
    let new_vals = List.map eval_insert_ast_value assign_exprs in
    let new_row = Some (Array.of_list new_vals) in
    let old_col_names =
      match Hashtbl.find_opt t.views view_name with
      | Some vq -> view_col_names vq
      | None -> []
    in
    let old_schema = make_col_schema old_col_names in
    let* outcome = resolve_instead_of_update_olds t view_name ~where ~old_col_names in
    let process_one_old_row old_row_opt =
      Lwt_list.iter_s
        (fun m ->
           let substituted_body =
             List.map
               (fun stmt ->
                  (* Substitute NEW first using the assignment-column schema,
             then OLD using the view's column schema.  Both passes are
             disjoint: each only rewrites references whose alias matches
             its schema. *)
                  let s1 = subst_new_old ~schema:new_schema ~new_row ~old_row:None stmt in
                  subst_new_old ~schema:old_schema ~new_row:None ~old_row:old_row_opt s1)
               m.trig_body
           in
           Lwt_list.iter_s (fire_trigger_stmt t) substituted_body)
        matching
    in
    let* () =
      match outcome with
      | `Unresolved -> process_one_old_row None
      | `Resolved rows -> Lwt_list.iter_s (fun r -> process_one_old_row (Some r)) rows
    in
    Lwt.return (Ok ()))
;;

(** Execute INSTEAD OF triggers for a view write operation. *)
let execute_instead_of t view_name ast =
  match ast with
  | Sql.Ast.S_insert { columns; values; _ } ->
    instead_of_insert t view_name ~columns ~values
  | Sql.Ast.S_delete _ -> instead_of_delete t view_name
  | Sql.Ast.S_update { assignments; where; _ } ->
    instead_of_update t view_name ~assignments ~where
  | _ ->
    Lwt.return
      (Error
         (Sema
            (Sql.Sema.Unsupported
               (Printf.sprintf "view '%s' is not directly modifiable" view_name))))
;;

(* ------------------------------------------------------------------ *)
(* Public execute / query API                                           *)
(* ------------------------------------------------------------------ *)

(* Note: [insert_replace_upsert_hooks] is defined as part of the
   [fire_trigger_stmt] / [make_trigger_hook] recursive group above so that
   nested trigger bodies can install REPLACE/UPSERT secondary hooks. *)

(* #269: apply a view/trigger schema change that lives in a db-owned cache
   ([t.views] / [t.triggers]).  [apply] performs the in-memory mutation; when an
   explicit transaction is active, the matching store write goes THROUGH it (so
   CREATE/DROP VIEW/TRIGGER no longer self-deadlocks inside BEGIN … COMMIT) and
   [undo] is registered to revert the cache on ROLLBACK; otherwise [persist None]
   self-commits, as before. *)
let staged_schema_change t ~apply ~undo ~persist =
  (* Persist to the store FIRST, then mutate the in-memory cache: if the store
     write raises, the cache is left untouched (no orphaned cache entry / undo). *)
  match t.explicit_txn with
  | Some tx ->
    let* () = persist (Some tx) in
    apply ();
    Cat.register_schema_undo t.catalog undo;
    Lwt.return_unit
  | None ->
    let* () = persist None in
    apply ();
    Lwt.return_unit
;;

(* Double-quote an identifier, escaping embedded quotes.  Defined here, well
   above the rest of the [rv_] helpers, because the DROP guards in
   {!execute_control_op} below need it to render the "use DROP REACTIVE VIEW
   <name>" hint for a name that cannot be written bare. *)
let rv_quote id = "\"" ^ String.concat "\"\"" (String.split_on_char '"' id) ^ "\""

(* #469: render [id] the way a caller must type it in SQL: bare when it lexes
   as a plain identifier, double-quoted otherwise.  Keeps the common hint
   readable ([DROP REACTIVE VIEW cnt]) while staying copy-pasteable for a name
   with spaces or punctuation.  Bare keywords are deliberately not quoted: the
   grammar's [any_ident] accepts them in this position anyway. *)
let rv_sql_name id =
  let plain c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
  in
  if id <> "" && (not (id.[0] >= '0' && id.[0] <= '9')) && String.for_all plain id
  then id
  else rv_quote id
;;

(* #469: is [tbl] the materialisation [_rv_<name>] of a *registered* reactive
   view?  A user table that merely starts with [_rv_] is not — the registry, not
   the naming convention, is the authority (same rule as {!is_reactive_view}). *)
let rv_owned_table top tbl =
  String.length tbl > 4
  && String.sub tbl 0 4 = "_rv_"
  && Hashtbl.mem top.reactive_views (String.sub tbl 4 (String.length tbl - 4))
;;

(* #475: bracket the driver's internal work.  [rv_depth] is a nesting COUNTER,
   not a flag: [rv_flush] can be entered while [rv_create] or [rv_load] is still
   running, and a bool cleared by whichever finished first re-armed reactive
   driving in the middle of the outer operation's own [_rv_…] writes.  The
   decrement runs under [Lwt.finalize] so an exception cannot strand the
   guard. *)
let rv_guard top f =
  top.rv_depth <- top.rv_depth + 1;
  Lwt.finalize f (fun () ->
    top.rv_depth <- top.rv_depth - 1;
    Lwt.return_unit)
;;

(* #475: is the driver currently in the middle of a re-entrant write of its own?
   Only consulted by [drive_reactive]'s fast path — the [DROP TABLE] guard uses
   {!rv_internal_drop_permitted}, which is narrower. *)
let rv_in_driver top = top.rv_depth > 0

(* #475: may [tbl] be dropped without the internal-table refusal?  True only
   while the driver itself is executing a [DROP TABLE] for exactly that table
   ({!rv_dropping_internal}).  A COUNTED set rather than a plain one, because
   the same materialisation can be re-typed by a nested refresh. *)
let rv_internal_drop_permitted top tbl = Hashtbl.mem top.rv_internal_tables tbl

(* #475: run [f] with [tbl] marked as an internally-driven drop target.  Held
   for the extent of the driver's own [DROP TABLE] statement and no longer, so a
   user drop of any OTHER [_rv_…] table interleaving into the surrounding
   refresh window is still refused. *)
let rv_dropping_internal top tbl f =
  let bump d =
    match Hashtbl.find_opt top.rv_internal_tables tbl with
    | None -> if d > 0 then Hashtbl.replace top.rv_internal_tables tbl 1
    | Some k ->
      if k + d <= 0
      then Hashtbl.remove top.rv_internal_tables tbl
      else Hashtbl.replace top.rv_internal_tables tbl (k + d)
  in
  bump 1;
  Lwt.finalize f (fun () ->
    bump (-1);
    Lwt.return_unit)
;;

(* Handle non-DML control / DDL ops (txn control, ATTACH/DETACH, schema
   switch, CREATE/DROP VIEW/TRIGGER, VACUUM).  Returns [Some result] for ops
   it owns and [None] for DML / catch-all ops the caller routes to
   [execute_dml_op].  Shared by [execute] and [execute_change_count]; the
   latter maps the [unit] result to a [0] change count. *)
let execute_control_op top t sql op =
  match op with
  (* #634: checked ABOVE the ROLLBACK exemption, unlike #555's poison.  A
     poisoned handle is recoverable and ROLLBACK is the recovery; a handle
     invalidated by a sibling's VACUUM is not — its store is closed and its
     transaction, if any, died with the file.  Letting ROLLBACK through would
     drive [S.rollback] into a closed store to no purpose.  Both the routed
     handle and the top-level one are tested: the caller holds [top], and a
     statement routed to a live ATTACHed schema off a dead main would otherwise
     look healthy. *)
  | _ when is_stale t || is_stale top -> Some (Lwt.return (Error (Runtime stale_msg)))
  (* #555: ROLLBACK is checked first so it stays reachable on a poisoned
     connection — it is the defined way to clear the poison. *)
  | Sql.Plan.Op_rollback -> Some (rollback_txn t)
  (* #555: [PRAGMA active_database = …] is a ROUTING statement, so
     [resolve_target_ast] sends it to [top] and the generic gate below — which
     tests the routed handle — would wave it through while the ACTIVE schema is
     poisoned.  That let a caller walk away from a poisoned sub-handle, at which
     point the prescribed recovery breaks: the subsequent ROLLBACK routes to the
     new schema and answers "no active transaction" while the poisoned one still
     holds its writer lock.  Refuse to leave a poisoned schema.

     #598 NOTE: this gate used to be paired with "arriving at a poisoned schema
     stays legal, or a schema poisoned from elsewhere could never be reached to
     be rolled back".  That safety valve NO LONGER EXISTS.  Arriving requires a
     switch; a poisoned schema by construction holds a transaction (the poison
     fires on a second BEGIN against an occupied slot); so [any_explicit_txn top]
     is true and the #598 gate below refuses the arrival too.  The state is
     unreachable today — a poisoned schema is always the one the caller is
     already on, because that is where their BEGIN went — so nothing is stranded.
     But it is no longer a valve, and anything that makes a poisoned schema
     reachable from elsewhere (a session object, #555 option 1) must re-open the
     arrival path explicitly rather than assume this comment still guarantees
     it. *)
  | Sql.Plan.Op_active_database_set _ when is_poisoned (active_handle top) ->
    Some (Lwt.return (Error (Runtime poisoned_msg)))
  (* #555: DETACH is a routing statement too, and left ungated it was a SECOND
     exit from the poisoned state — it drops the sub-handle, resets
     [active_schema] and closes the store, discarding the poisoned transaction's
     uncommitted writes.  The outcome is clean, but "ROLLBACK is the sole exit"
     is the contract this fix documents, so keep it true rather than acquire a
     second, undocumented one. *)
  | Sql.Plan.Op_detach _ when is_poisoned (active_handle top) ->
    Some (Lwt.return (Error (Runtime poisoned_msg)))
  (* #598: the poison only fires on a COLLISION — two [BEGIN]s against one slot.
     Under ATTACH nothing collides: main's slot is occupied once, aux's is never
     opened, and a schema switch simply moves the routing out from under the open
     transaction.  The caller's next write then lands in the other database and
     is AUTOCOMMITTED there, durably, with [transaction_poisoned = false]
     throughout; the caller finds out at COMMIT ("no active transaction"), after
     the write is on disk.  There is nothing for the poison to detect, so refuse
     the switch outright while any schema holds a transaction.  Switching to the
     schema already active is a no-op and stays legal, so re-asserting one's own
     routing inside a transaction still works.

     This gate sits BELOW the two poison gates above deliberately: on a poisoned
     connection the caller must be told to ROLLBACK, not that a transaction is
     open.  DETACH gets the same treatment for the same reason — dropping a
     sub-handle whose transaction is open would discard it silently. *)
  | Sql.Plan.Op_active_database_set { schema }
    when (not (String.equal schema top.active_schema)) && any_explicit_txn top ->
    Some (Lwt.return (Error (Runtime (routing_blocked_msg "active_database"))))
  | Sql.Plan.Op_detach _ when any_explicit_txn top ->
    Some (Lwt.return (Error (Runtime (routing_blocked_msg "DETACH"))))
  | _ when is_poisoned t -> Some (Lwt.return (Error (Runtime poisoned_msg)))
  (* #740: below the poison and staleness gates — a poisoned or stale handle has
     a more urgent thing to tell the caller — and above the fall-through that
     would route this to [Sql.Exec]'s [Store.checkpoint] and hang. *)
  | Sql.Plan.Op_pragma_wal_checkpoint when wal_checkpoint_would_deadlock t op ->
    Some (Lwt.return (Error (Runtime wal_checkpoint_in_txn_msg)))
  | Sql.Plan.Op_begin -> Some (begin_txn t)
  | Sql.Plan.Op_commit -> Some (commit_txn t)
  | Sql.Plan.Op_savepoint name -> Some (savepoint_txn t name)
  | Sql.Plan.Op_release name -> Some (release_savepoint t name)
  | Sql.Plan.Op_rollback_to name -> Some (rollback_to_savepoint t name)
  | Sql.Plan.Op_attach { path; schema } ->
    Some
      (if String.equal schema "main"
       then Lwt.return (Error (Runtime "ATTACH: 'main' is reserved"))
       else if Hashtbl.mem top.attached schema
       then
         Lwt.return
           (Error (Runtime (Printf.sprintf "ATTACH: schema '%s' already attached" schema)))
       else (
         match !file_provider_ref with
         | None ->
           Lwt.return
             (Error
                (Runtime
                   "ATTACH requires a file provider; link granary.unix and call \
                    Db.set_file_provider"))
         | Some prov ->
           let* result =
             prov.open_store ~as_of_history:(S.history_enabled top.store) ~path ()
           in
           (match result with
            | Error e -> Lwt.return (Error (Runtime (Format.asprintf "%a" S.pp_error e)))
            | Ok store ->
              let* sub_db =
                of_store
                  ?clock:t.clock
                  ~durability:(S.durability t.store)
                  ~file_path:path
                  store
              in
              Hashtbl.add top.attached schema sub_db;
              Lwt.return (Ok ()))))
  | Sql.Plan.Op_detach { schema } ->
    Some
      (if String.equal schema "main"
       then Lwt.return (Error (Runtime "DETACH: cannot detach 'main'"))
       else (
         match Hashtbl.find_opt top.attached schema with
         | None ->
           Lwt.return
             (Error (Runtime (Printf.sprintf "DETACH: no such schema '%s'" schema)))
         | Some sub ->
           Hashtbl.remove top.attached schema;
           if String.equal top.active_schema schema then top.active_schema <- "main";
           let* () = close sub in
           Lwt.return (Ok ())))
  | Sql.Plan.Op_active_database_set { schema } ->
    Some
      (if String.equal schema "main" || Hashtbl.mem top.attached schema
       then (
         top.active_schema <- schema;
         Lwt.return (Ok ()))
       else
         Lwt.return
           (Error (Runtime (Printf.sprintf "active_database: no such schema '%s'" schema))))
  | Sql.Plan.Op_database_list ->
    (* No rows produced via execute; use [query]/[Db.query] to read. *)
    Some (Lwt.return (Ok ()))
  | Sql.Plan.Op_active_database_get ->
    (* No rows produced via execute; use [query]/[Db.query] to read. *)
    Some (Lwt.return (Ok ()))
  | Sql.Plan.Op_drop_table { table_meta; _ }
    when t == top
         && rv_owned_table top table_meta.Cat.name
         && not (rv_internal_drop_permitted top table_meta.Cat.name) ->
    (* #469: the driver's own teardown/re-type drops pass straight through.
       #475: the permission is per TABLE NAME and held only for the extent of
       the driver's own [DROP TABLE] statement, not for the whole refresh.  The
       old [not top.rv_refreshing] test was open for the entire duration of
       [rv_flush] / [rv_create] / [rv_load] — each of which awaits the store
       repeatedly — so a user [DROP TABLE _rv_<other>] scheduled into one of
       those windows bypassed this guard and destroyed an unrelated live view's
       materialisation. *)
    let tbl = table_meta.Cat.name in
    let view = String.sub tbl 4 (String.length tbl - 4) in
    Some
      (Lwt.return
         (Error
            (Runtime
               (Printf.sprintf
                  "table '%s' is an internal reactive-view materialisation; use DROP \
                   REACTIVE VIEW %s"
                  tbl
                  (rv_sql_name view)))))
  | Sql.Plan.Op_create_view { name; query } ->
    (* #269: participates in an ambient explicit txn (rolls back atomically);
       autocommits otherwise. *)
    Some
      (let prev = Hashtbl.find_opt t.views name in
       let* () =
         staged_schema_change
           t
           ~apply:(fun () -> Hashtbl.replace t.views name query)
           ~undo:(fun () ->
             match prev with
             | Some q -> Hashtbl.replace t.views name q
             | None -> Hashtbl.remove t.views name)
           ~persist:(fun txn -> Cat.persist_view ?txn t.store ~name ~sql)
       in
       Lwt.return (Ok ()))
  | Sql.Plan.Op_create_reactive_view { name; query; refresh } ->
    (* #427: classify, materialise [_rv_<name>], build any delta engine, and
       persist the definition.  Reactive views live on the top-level handle, and
       since #474 the driver's own internal SQL is pinned there too — before
       that, a CREATE issued under [PRAGMA active_database = aux] registered the
       view on [main] while creating and filling its [_rv_<name>] table in
       [aux]. *)
    Some (!rv_create_hook top ~sql ~name query refresh)
  | Sql.Plan.Op_drop_reactive_view { name; if_exists } when t == top ->
    (* #469: deregister, drop [_rv_<name>], and forget the persisted
       definition.  Like CREATE, this is immediate rather than staged. *)
    Some (!rv_drop_hook top ~name ~if_exists)
  | Sql.Plan.Op_drop_reactive_view { name; _ } ->
    (* #469 review: reactive views live on [top] only, so a DROP issued while
       another schema is active names an object that schema does not have.

       #474 NOTE: the ORIGINAL reason for this refusal is gone.  It was that
       [rv_drop]'s internal [DROP TABLE IF EXISTS _rv_<name>] re-entered
       [compile_routed] and routed by [top.active_schema], so under
       [PRAGMA active_database = aux] it aimed at [aux], silently no-opped, and
       left main's [_rv_<name>] table orphaned behind a removed registry entry.
       The driver's internal SQL is now pinned ({!rv_execute}), so that
       corruption cannot happen.  The refusal is KEPT on the remaining half of
       #469's argument, which #474 does not touch: falling through to [top]
       would silently ignore the active schema, and a DDL statement that ignores
       the schema the caller selected is a surprise worth an error.  Fires under
       IF EXISTS too — the statement is misdirected regardless of whether the
       view exists. *)
    Some
      (Lwt.return
         (Error
            (Runtime
               (Printf.sprintf
                  "DROP REACTIVE VIEW %s: reactive views exist only in the main schema; \
                   run this with PRAGMA active_database = main"
                  (rv_sql_name name)))))
  | Sql.Plan.Op_drop_view { name } when t == top && Hashtbl.mem top.reactive_views name ->
    (* #469: DROP VIEW only knows about [t.views]; on a reactive view it would
       succeed while removing nothing.  Fires even under IF EXISTS — the object
       exists, the statement is the wrong one.  Reactive views live on the
       top-level handle only, so [t == top] keeps this from misfiring on a
       DROP VIEW routed to an ATTACHed sub-handle (see the [Op_drop_table]
       guard above). *)
    Some
      (Lwt.return
         (Error
            (Runtime
               (Printf.sprintf
                  "'%s' is a reactive view; use DROP REACTIVE VIEW %s"
                  name
                  (rv_sql_name name)))))
  | Sql.Plan.Op_drop_view { name } ->
    Some
      (let prev = Hashtbl.find_opt t.views name in
       let* () =
         staged_schema_change
           t
           ~apply:(fun () -> Hashtbl.remove t.views name)
           ~undo:(fun () ->
             match prev with
             | Some q -> Hashtbl.replace t.views name q
             | None -> Hashtbl.remove t.views name)
           ~persist:(fun txn -> Cat.remove_view ?txn t.store ~name)
       in
       Lwt.return (Ok ()))
  | Sql.Plan.Op_create_trigger { name; timing; event; table; when_; body } ->
    Some
      (let ast = Sql.Ast.S_create_trigger { name; timing; event; table; when_; body } in
       let prev = Hashtbl.find_opt t.triggers name in
       let* () =
         staged_schema_change
           t
           ~apply:(fun () -> Hashtbl.replace t.triggers name ast)
           ~undo:(fun () ->
             match prev with
             | Some a -> Hashtbl.replace t.triggers name a
             | None -> Hashtbl.remove t.triggers name)
           ~persist:(fun txn -> Cat.persist_trigger ?txn t.store ~name ~sql)
       in
       Lwt.return (Ok ()))
  | Sql.Plan.Op_drop_trigger { name } ->
    Some
      (let prev = Hashtbl.find_opt t.triggers name in
       let* () =
         staged_schema_change
           t
           ~apply:(fun () -> Hashtbl.remove t.triggers name)
           ~undo:(fun () ->
             match prev with
             | Some a -> Hashtbl.replace t.triggers name a
             | None -> Hashtbl.remove t.triggers name)
           ~persist:(fun txn -> Cat.remove_trigger ?txn t.store ~name)
       in
       Lwt.return (Ok ()))
  | Sql.Plan.Op_vacuum ->
    Some
      (Lwt.catch
         (fun () ->
            let* () = vacuum t in
            Lwt.return (Ok ()))
         (function
           | Failure msg -> Lwt.return (Error (Runtime msg))
           | e -> Lwt.return (Error (Runtime (Printexc.to_string e)))))
  | _ -> None
;;

(* Build the trigger BEFORE/AFTER hooks, the REPLACE/UPSERT secondary hooks,
   and the INSERT target table name for a DML op. *)
let dml_hooks t op =
  let before_hook, after_hook =
    match op with
    | Sql.Plan.Op_insert { table_meta; _ } | Sql.Plan.Op_insert_select { table_meta; _ }
      ->
      ( make_trigger_hook t table_meta ~timing:`Before ~event:`Insert
      , make_trigger_hook t table_meta ~timing:`After ~event:`Insert )
    | Sql.Plan.Op_update { table_meta; _ } ->
      ( make_trigger_hook t table_meta ~timing:`Before ~event:`Update
      , make_trigger_hook t table_meta ~timing:`After ~event:`Update )
    | Sql.Plan.Op_delete { table_meta; _ } ->
      ( make_trigger_hook t table_meta ~timing:`Before ~event:`Delete
      , make_trigger_hook t table_meta ~timing:`After ~event:`Delete )
    | _ -> None, None
  in
  let insert_table_name =
    match op with
    | Sql.Plan.Op_insert { table_meta; _ } | Sql.Plan.Op_insert_select { table_meta; _ }
      -> Some table_meta.Cat.name
    | _ -> None
  in
  let rdb, rd, uub, uu = insert_replace_upsert_hooks t op in
  before_hook, after_hook, insert_table_name, rdb, rd, uub, uu
;;

(* Run a DML op via [execute_with_count], updating change counters and the
   last-insert rowid, then draining deferred FK checks in autocommit mode.
   [execute] discards the row count; [execute_change_count] returns it. *)
let run_dml t op ~on_ok =
  let mode =
    match t.explicit_txn with
    | None -> Sql.Exec.Auto
    | Some tx -> Sql.Exec.In_txn tx
  in
  let ( before_hook
      , after_hook
      , insert_table_name
      , on_replace_delete_before
      , on_replace_delete
      , on_upsert_update_before
      , on_upsert_update )
    =
    dml_hooks t op
  in
  match
    Sql.Exec.execute_with_count
      ~mode
      ~clock:t.clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      t.store
      t.catalog
      op
  with
  | exception Failure msg ->
    (* Discard any pending deferred FK checks queued by the failed
        statement — the writes will be rolled back. *)
    Cat.clear_pending_fk_checks t.catalog;
    Lwt.return (Error (Runtime msg))
  | lwt_op ->
    Lwt.catch
      (fun () ->
         let* n = lwt_op in
         t.last_changes <- n;
         t.total_changes <- t.total_changes + n;
         (match insert_table_name with
          | Some tbl when n > 0 ->
            (match Cat.find_table_cached t.catalog ~name:tbl with
             (* #243 (T1): the executor records the actual inserted rowid; an
                explicit INTEGER PRIMARY KEY need not equal next_rowid - 1. *)
             | Some _ -> t.last_insert_rowid <- Cat.last_inserted_rowid t.catalog
             | None -> ())
          | _ -> ());
         on_ok n)
      (function
        | Failure msg ->
          Cat.clear_pending_fk_checks t.catalog;
          Lwt.return (Error (Runtime msg))
        | exn -> Lwt.fail exn)
;;

let execute_dml_op t op =
  (* Auto-commit mode: deferred FK checks behave like immediate.  The txn was
     already committed inside [execute_with_count]; [drain_pending_fks_autocommit]
     opens a fresh RO snapshot that observes the just-committed writes. *)
  run_dml t op ~on_ok:(fun _n ->
    if t.explicit_txn = None then drain_pending_fks_autocommit t else Lwt.return (Ok ()))
;;

let execute_dml_op_count t op =
  run_dml t op ~on_ok:(fun n ->
    if t.explicit_txn = None
    then
      let* r = drain_pending_fks_autocommit t in
      match r with
      | Ok () -> Lwt.return (Ok n)
      | Error e -> Lwt.return (Error e)
    else Lwt.return (Ok n))
;;

(* #474: [?on] pins the statement to a handle regardless of [active_schema] —
   see {!compile_routed}. *)
let execute_core ?on top sql =
  let op_promise, t = compile_routed ?on top sql in
  let* op = op_promise in
  match op with
  (* #634: ahead of everything, including the INSTEAD OF path below, which
     bypasses [execute_control_op] and would otherwise run a trigger body's
     statements against a closed store. *)
  | _ when is_stale t || is_stale top -> Lwt.return (Error (Runtime stale_msg))
  (* #555 (F4): checked ahead of the [Error] branches so a malformed statement on
     a poisoned handle reports the poison rather than a parse error — [query_impl]
     orders it the same way, and the two disagreeing was gratuitous.  Scoped to
     the [Error] arms so [Ok Op_rollback] still reaches [execute_control_op],
     which exempts it — recovery must stay reachable.  This also covers the
     INSTEAD OF path below, which bypasses [execute_control_op] and would
     otherwise run a trigger body's statements ungated. *)
  | Error _ when is_poisoned t -> Lwt.return (Error (Runtime poisoned_msg))
  | Error (Sema (Sql.Sema.Unknown_table view_name)) when Hashtbl.mem t.views view_name ->
    (match parse sql with
     (* #487: unreachable in practice (the same [sql] already parsed for the
        bind that produced [Unknown_table]), but manufacturing a fresh bare
        "syntax error" here would make the message depend on which entry point
        the caller used.  Propagate. *)
     | Error e -> Lwt.return (Error e)
     | Ok ast -> execute_instead_of t view_name ast)
  | Error e -> Lwt.return (Error e)
  | Ok op ->
    (match execute_control_op top t sql op with
     | Some result -> result
     | None -> execute_dml_op t op)
;;

let execute_change_count_core top sql =
  let op_promise, t = compile_routed top sql in
  let* op = op_promise in
  let count_of_unit = function
    | Ok () -> Lwt.return (Ok 0)
    | Error e -> Lwt.return (Error e)
  in
  match op with
  (* #634: same ordering as [execute_core]. *)
  | _ when is_stale t || is_stale top -> Lwt.return (Error (Runtime stale_msg))
  (* #555 (F4): same ordering as [execute_core]. *)
  | Error _ when is_poisoned t -> Lwt.return (Error (Runtime poisoned_msg))
  | Error (Sema (Sql.Sema.Unknown_table view_name)) when Hashtbl.mem t.views view_name ->
    (match parse sql with
     (* #487: same as [execute_core] — propagate the positioned error. *)
     | Error e -> Lwt.return (Error e)
     | Ok ast ->
       let* r = execute_instead_of t view_name ast in
       count_of_unit r)
  | Error e -> Lwt.return (Error e)
  | Ok op ->
    (match execute_control_op top t sql op with
     | Some result ->
       let* r = result in
       count_of_unit r
     | None -> execute_dml_op_count t op)
;;

(* #427: run [core ()], and when reactive views exist, capture the statement's
   row changes and drive maintenance.  On autocommit (and after a COMMIT that
   left [explicit_txn = None]) the accumulated deltas are flushed to the [_rv_…]
   materialisations; inside an explicit transaction they accumulate until COMMIT.
   The fast path (no reactive views, or re-entrant [_rv_…] writes) is unchanged. *)
let drive_reactive top ~core =
  if Hashtbl.length top.reactive_views = 0 || rv_in_driver top
  then core ()
  else
    let* result, acc =
      match Sql.Exec.current_dirty_acc () with
      | Some acc ->
        let* r = core () in
        Lwt.return (r, acc)
      | None ->
        let acc = Sql.Exec.make_change_acc () in
        let* r = Sql.Exec.with_dirty acc core in
        Lwt.return (r, acc)
    in
    (match result with
     | Ok _ -> rv_absorb_changes top acc
     | Error _ -> rv_note_failed_statement top acc);
    if top.explicit_txn = None && (Hashtbl.length top.rv_pending > 0 || top.rv_resync)
    then
      let* fr = !rv_flush_hook top in
      match fr, result with
      (* #737: the failing statement's own error is the informative one, so a
         flush scheduled by it does not replace it. On [Ok] the flush's error
         is still the result, unchanged. *)
      | Error e, Ok _ -> Lwt.return (Error e)
      | _, _ -> Lwt.return result
    else Lwt.return result
;;

let execute top sql = drive_reactive top ~core:(fun () -> execute_core top sql)

(* ------------------------------------------------------------------ *)
(* #585: scoped transactions                                            *)
(* ------------------------------------------------------------------ *)

(* #585: a re-entrant [with_transaction] from inside its own extent.  Refused,
   NOT turned into a savepoint and NOT joined onto the outer transaction — see
   the .mli for why either of those would have to lie to the inner caller.

   This is the one case the owner token lets the engine answer precisely, and it
   is answered WITHOUT poisoning: the token proves the caller is the fiber that
   opened the outer transaction, so there is no cross-fiber contamination to
   contain and no reason to doom work that is provably the caller's own. *)
let nested_txn_msg =
  "Db.with_transaction refused: this fiber is already inside a with_transaction scope on \
   this handle (#585).  Nesting is deliberately not supported - it cannot be a savepoint \
   (the inner scope would RELEASE rather than commit, so returning from it would not \
   mean durable) and it cannot join the outer transaction (the inner scope's rollback \
   would abort the OUTER transaction while returning to code that believes only its own \
   work was undone).  Use SAVEPOINT / RELEASE / ROLLBACK TO explicitly if a partial undo \
   point is what you want.  Nothing was rolled back, the outer transaction is intact and \
   this handle is NOT poisoned."
;;

(* #585: the scope's transaction is no longer the one in the handle's slot.
   Reachable through #584: another fiber's ROLLBACK aborted this scope's
   transaction, after which the slot was either left empty or refilled by that
   fiber's own BEGIN.  Neither COMMIT nor ROLLBACK is issued here — both would
   act on state that is no longer this scope's, which is precisely the
   contamination #584 describes.

   The guard only works because the token is invalidated by every path that ends
   a transaction, not just by this combinator's own exit: [begin_txn] and
   [savepoint_txn]'s auto-begin clear it when they fill the slot, and
   [force_rollback_txn], [commit_txn], [rollback_txn] and [release_savepoint]'s
   auto-commit clear it when they empty it.  Before those clears existed the
   token outlived its transaction, so [Some 1 = Some 1] matched a slot holding
   somebody else's transaction and this scope cheerfully COMMITted it — #584
   verbatim, with an [Ok] returned to the caller. *)
let stolen_txn_msg =
  "Db.with_transaction: the transaction this scope opened is gone - another fiber's \
   ROLLBACK aborted it, and the handle's transaction slot is now either empty or holding \
   a different transaction (#584).  This scope's writes are lost.  Neither COMMIT nor \
   ROLLBACK was issued, because either would have acted on state that is no longer this \
   scope's.  Give each fiber its own handle with Db.create_worker_handle."
;;

(* #585: the handle whose transaction slot a scope's BEGIN actually lands in.
   Under ATTACH that is the active schema's sub-handle, not the top-level handle
   the caller holds — and it is the sub-handle whose [txn_scope] the
   begin/commit/rollback paths clear, so the token must be stamped and checked
   there or the guard reads a field nothing maintains.  Stable for the scope's
   whole extent: since #598 a [PRAGMA active_database] switch is refused while
   any schema holds a transaction. *)
let scope_handle t = active_handle t

(* Drop the token only if it is still ours: a scope that was displaced (#584)
   must not clear the token of the transaction that displaced it. *)
let clear_txn_scope owner token =
  if owner.txn_scope = Some token then owner.txn_scope <- None
;;

let in_transaction_scope t =
  match (scope_handle t).txn_scope, Lwt.get txn_scope_key with
  | Some held, Some tok -> Int.equal held tok
  | _, _ -> false
;;

let owns_txn_scope owner token = owner.txn_scope = Some token

(* Success arm: COMMIT, then release the scope.  A COMMIT error is reported as
   [Error] and no compensating ROLLBACK is issued — [commit_txn] already rolls
   back the arms that leave the transaction uncommittable (#286 in-txn DDL,
   deferred-FK violation), and on a poisoned handle (#555) ROLLBACK is the
   caller's prescribed single exit, not this combinator's to take on their
   behalf. *)
let scoped_commit t owner token v =
  if not (owns_txn_scope owner token)
  then Lwt.return (Error (Runtime stolen_txn_msg))
  else
    let* cr = execute t "COMMIT" in
    clear_txn_scope owner token;
    match cr with
    | Ok () -> Lwt.return (Ok v)
    | Error e -> Lwt.return (Error e)
;;

(* Failure arm: ROLLBACK, release the scope, re-raise the original exception.
   The ROLLBACK's own result is discarded on purpose — the exception the body
   raised is the interesting one, and swallowing it to report a rollback error
   would hide the cause.  Skipped entirely when the scope was displaced (#584):
   there is nothing of ours left to roll back and the statement would abort
   whatever replaced it. *)
let scoped_rollback t owner token exn =
  if not (owns_txn_scope owner token)
  then Lwt.fail exn
  else
    let* _ = execute t "ROLLBACK" in
    clear_txn_scope owner token;
    Lwt.fail exn
;;

let run_txn_scope t owner token body =
  Lwt.catch
    (fun () ->
       let* v = Lwt.with_value txn_scope_key (Some token) (fun () -> body t) in
       scoped_commit t owner token v)
    (fun exn -> scoped_rollback t owner token exn)
;;

let with_transaction t body =
  if in_transaction_scope t
  then Lwt.return (Error (Runtime nested_txn_msg))
  else
    let* r = execute t "BEGIN" in
    match r with
    (* The BEGIN failed.  Nothing was opened, so nothing is rolled back — and
       critically, if it failed because the handle already held a transaction
       then #555 has just poisoned it, and ROLLBACK is the caller's sole exit.
       Issuing one here would make this combinator a second exit. *)
    | Error e -> Lwt.return (Error e)
    | Ok () ->
      incr txn_scope_counter;
      let token = !txn_scope_counter in
      (* Stamped AFTER the BEGIN, which clears the slot's token as it fills it —
         so this is the re-stamp that makes the scope the owner. *)
      let owner = scope_handle t in
      owner.txn_scope <- Some token;
      run_txn_scope t owner token body
;;

let execute_change_count top sql =
  drive_reactive top ~core:(fun () -> execute_change_count_core top sql)
;;

let execute_with_dirty top sql =
  let acc = Sql.Exec.make_change_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> execute top sql) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok () -> Lwt.return (Ok (Sql.Exec.dirty_elements acc))
;;

let execute_change_count_with_dirty top sql =
  let acc = Sql.Exec.make_change_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> execute_change_count top sql) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok n -> Lwt.return (Ok (n, Sql.Exec.dirty_elements acc))
;;

let execute_with_changes top sql =
  let acc = Sql.Exec.make_change_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> execute top sql) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok () -> Lwt.return (Ok (Sql.Exec.dirty_changes acc))
;;

(* #259: single source of truth for the control-op dispatch shared by [query]
   and [query_with_stats].  The canned control ops (changes / last_insert_rowid
   / database_list / ...) do no scan; only the real-op branch touches [stats],
   forwarding it to [Sql.Exec.query].  When [stats] is omitted the call is
   [Sql.Exec.query] with no [~stats] — byte-for-byte the pre-#239 read path
   (no [with_value], no [Lwt_stream.map] wrapper), so existing callers pay
   nothing. *)
let query_impl ?stats ?mode ?on top sql =
  let op_promise, t = compile_routed ?on top sql in
  let* op = op_promise in
  match op with
  (* #634: a read on a handle invalidated by a sibling's VACUUM would descend
     into the CLOSED pre-VACUUM store, or (for the canned control ops) answer
     from counters describing a file that no longer exists. *)
  | _ when is_stale t || is_stale top -> Lwt.return (Error (Runtime stale_msg))
  (* #555: a read on a poisoned handle would resolve [In_txn] from the
     transaction that won the BEGIN race and see its uncommitted writes.  Reject
     it — there is no ROLLBACK to exempt on the read path. *)
  | _ when is_poisoned t -> Lwt.return (Error (Runtime poisoned_msg))
  | Error e -> Lwt.return (Error e)
  | Ok Sql.Plan.Op_changes ->
    Lwt.return (Ok (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int t.last_changes) |] ]))
  | Ok Sql.Plan.Op_last_insert_rowid ->
    Lwt.return (Ok (Lwt_stream.of_list [ [| Row.V_int t.last_insert_rowid |] ]))
  | Ok Sql.Plan.Op_total_changes ->
    Lwt.return
      (Ok (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int t.total_changes) |] ]))
  | Ok Sql.Plan.Op_database_list ->
    let path_str p = Option.value p ~default:"" in
    let main_row =
      [| Row.V_int 0L; Row.V_text "main"; Row.V_text (path_str top.file_path) |]
    in
    let _, rev_extra =
      Hashtbl.fold
        (fun name sub (i, acc) ->
           let row =
             [| Row.V_int (Int64.of_int i)
              ; Row.V_text name
              ; Row.V_text (path_str sub.file_path)
             |]
           in
           i + 1, row :: acc)
        top.attached
        (1, [])
    in
    Lwt.return (Ok (Lwt_stream.of_list (main_row :: List.rev rev_extra)))
  | Ok Sql.Plan.Op_active_database_get ->
    Lwt.return (Ok (Lwt_stream.of_list [ [| Row.V_text top.active_schema |] ]))
  | Ok (Sql.Plan.Op_attach _ | Sql.Plan.Op_detach _ | Sql.Plan.Op_active_database_set _)
    ->
    Lwt.return
      (Error (Runtime "ATTACH/DETACH/active_database = ... is a write op; use Db.execute"))
  | Ok op ->
    let mode =
      match mode with
      (* #274: a caller-supplied ambient read mode (e.g. dump's shared RO
         snapshot of [top.store]) is only valid when routing kept the plan on
         [top]'s store.  If [active_database] flips mid-dump so [compile_routed]
         routes this statement to an attached sub-handle, applying [top]'s
         snapshot to the sub's tree-ids would read the wrong store; fall back to
         the routed handle's own context instead. *)
      | Some m when t == top -> m
      | Some _ | None ->
        (match t.explicit_txn with
         | None -> Sql.Exec.Auto
         | Some tx -> Sql.Exec.In_txn tx)
    in
    (* #627: a post-plan refusal (#558's [agg_subquery_refusal], #592's
       [correlated_filter_refusal], #566's outer-join ON raise) is spelled
       [Lwt.fail_with] / [failwith] *inside* an Lwt callback, so it arrives as a
       REJECTED PROMISE, not a synchronous exception.  The old
       [| exception Failure msg ->] arm only saw the synchronous spelling, so
       every one of those refusals sailed past it and escaped [Db.query] as a
       raw [Failure] — a caller matching [Ok _ | Error _] got an unhandled
       exception instead of the [Error] branch.  [Lwt.catch] covers both
       spellings ([Sql.Exec.query] is applied inside the thunk, so a synchronous
       raise during plan-to-stream construction is caught too), which is exactly
       the shape [iter_impl] and [run_core] already use.  Non-[Failure]
       exceptions still propagate unchanged. *)
    Lwt.catch
      (fun () ->
         let* stream = Sql.Exec.query ~mode ~clock:t.clock ?stats t.store t.catalog op in
         Lwt.return (Ok stream))
      (function
        | Failure msg -> Lwt.return (Error (Runtime msg))
        | exn -> Lwt.fail exn)
;;

let query top sql = query_impl top sql

(* #266: thin Db wrappers over the store-level as-of retention API, so callers
   manage the history floor without reaching through to the raw store. *)

(* #412: resolve a schema name to the store whose as-of history it owns.
   "main" is the top handle's own store; any other name must currently be an
   ATTACHed schema.  Raises [Invalid_argument] on an unknown schema. *)
let store_for_schema (top : t) schema =
  if String.equal schema "main"
  then top.store
  else (
    match Hashtbl.find_opt top.attached schema with
    | Some sub -> sub.store
    | None -> invalid_arg (Printf.sprintf "history: unknown schema '%s'" schema))
;;

let history_pin ?(schema = "main") t ~txn_id =
  S.history_pin (store_for_schema t schema) ~txn_id
;;

let history_floor ?(schema = "main") t = S.history_floor (store_for_schema t schema)
let history_release ?(schema = "main") t = S.history_release (store_for_schema t schema)

(* [history_log] returns an [Lwt.t], so resolve the schema INSIDE a thunk: an
   unknown-schema [Invalid_argument] must surface as a rejected promise (the
   in-thunk [store_for_schema] is caught by [Lwt.catch]), matching the return
   type rather than escaping synchronously. *)
let history_log ?(schema = "main") t =
  Lwt.catch (fun () -> S.history_log (store_for_schema t schema)) Lwt.fail
;;

(* #266: SQL-level time travel.  Opens a read-only snapshot of the committed
   state as it existed at [target] (a past txn id / timestamp) and runs [sql]
   against it in [In_ro_txn] mode.

   Snapshot lifetime — this is the crux.  [query]'s [Auto] path lets the SQL
   executor own a fresh RO snapshot per base scanner and end it on stream
   exhaustion (see [Exec.rh_finish]/[stream_seq_scan]).  An [In_ro_txn] snapshot
   is instead *borrowed*: [Exec] treats it as [RH_borrowed_ro] and never ends it
   (Db owns the lifecycle).  Because the result stream is lazy — base scanners
   pull rows from the snapshot as the consumer drains it — the snapshot MUST stay
   open until the stream is fully consumed, then be ended exactly once.  We mirror
   [Exec]'s own owned-snapshot idiom verbatim: an idempotent [finish] guarded by a
   [ref], wired into a [Lwt_stream.from] that ends the snapshot when the source
   yields [None] (drain) or raises (#164).  Errors before the stream is built end
   the snapshot eagerly. *)
let query_as_of top (target : Granary_store.History.target) sql =
  Lwt.catch
    (fun () ->
       (* #412: route FIRST, then open the historical snapshot on whichever store
          the statement resolves to (MAIN, or the active ATTACHed schema).  The
          executor already runs against the routed handle's store/catalog/clock;
          only the snapshot needs to follow routing.  As-of resolves per store —
          a single query cannot span two databases at one target (their commit
          orders are independent). *)
       let op_promise, t = compile_routed top sql in
       let* op = op_promise in
       (* #555 (F3): deliberately NOT gated on [is_poisoned].  This is the one
          [compile_routed] caller that never resolves its transaction from
          [t.explicit_txn] — it opens its own historical RO snapshot below, so it
          cannot observe the winning transaction's uncommitted writes and has no
          contamination to contain.  Reading history while another fiber's
          transaction is stuck is legitimate, and is arguably the most useful
          thing to be able to do at that moment.  Not an oversight. *)
       match op with
       (* #634: it IS gated on staleness, though — unlike a poisoned handle, a
          stale one's store is closed and VACUUM deleted the [.aslog] whose roots
          this snapshot would resolve against. *)
       | _ when is_stale t || is_stale top -> Lwt.return (Error (Runtime stale_msg))
       | Error e -> Lwt.return (Error e)
       | Ok op ->
         let* ro = S.ro_begin_as_of t.store target in
         (* Idempotent ender shared by the pre-stream guard and the drain path. *)
         let ended = ref false in
         let end_ro () =
           if !ended
           then Lwt.return_unit
           else (
             ended := true;
             S.ro_end ro)
         in
         Lwt.catch
           (fun () ->
              match
                Sql.Exec.query
                  ~mode:(Sql.Exec.In_ro_txn ro)
                  ~clock:t.clock
                  t.store
                  t.catalog
                  op
              with
              | exception Failure msg ->
                let* () = end_ro () in
                Lwt.return (Error (Runtime msg))
              | lwt_stream ->
                let* stream = lwt_stream in
                let wrapped =
                  Lwt_stream.from (fun () ->
                    Lwt.catch
                      (fun () ->
                         let* next = Lwt_stream.get stream in
                         match next with
                         | None ->
                           let* () = end_ro () in
                           Lwt.return_none
                         | Some row -> Lwt.return_some row)
                      (fun exn ->
                         let* () = end_ro () in
                         Lwt.fail exn))
                in
                Lwt.return (Ok wrapped))
           (fun exn ->
              let* () = end_ro () in
              match exn with
              (* #627: same hole as [query_impl] — the [| exception Failure msg ->]
                 arm above only catches the synchronous spelling, so a post-plan
                 refusal (a rejected promise) reached this handler and was
                 re-raised out of [query_as_of].  Map it to the same
                 [Error (Runtime msg)] the synchronous arm produces; the snapshot
                 has already been ended by the idempotent [end_ro] above.
                 [S.History_error] is not a [Failure], so the outer handler's
                 [History_unavailable] / [History_pruned] mapping is untouched. *)
              | Failure msg -> Lwt.return (Error (Runtime msg))
              | exn -> Lwt.fail exn))
    (function
      | S.History_error S.History_unavailable -> Lwt.return (Error History_unavailable)
      | S.History_error S.History_pruned -> Lwt.return (Error History_pruned)
      | exn -> Lwt.fail exn)
;;

(* #239: [query] plus a per-query cost/stats record for an external cost-based
   cache.  Populates [stats] as the stream drains; the canned control ops do no
   scan, so they leave the freshly-zeroed record untouched. *)
let query_with_stats top sql =
  let stats = Sql.Exec.make_query_stats () in
  let* r = query_impl ~stats top sql in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok stream -> Lwt.return (Ok (stream, stats))
;;

(* #387: resolve the projected column names without executing.  Routing,
   parsing and binding mirror [compile_routed] up to the [bind] step; the names
   are derived from the bound statement (with its AST for aliases / bare column
   names) by {!Sql.Sema.output_column_names}.  Side-effect-free: binding only
   reads the in-memory catalog, so this never opens a transaction or touches the
   store. *)
let query_columns top sql =
  match parse sql with
  | Error e -> Lwt.return (Error e)
  | Ok ast ->
    let t = resolve_target_ast top ast in
    let* bound = Sql.Sema.bind ~views:t.views t.catalog ast in
    (match bound with
     | Error e -> Lwt.return (Error (Sema e))
     | Ok b -> Lwt.return (Ok (Sql.Sema.output_column_names ast b)))
;;

(* ------------------------------------------------------------------ *)
(* Prepared statement API                                               *)
(* ------------------------------------------------------------------ *)

(* #474: [?on] pins the prepared statement's [db_ref] to a handle regardless of
   [active_schema] — see {!compile_routed}.  [run] resolves everything from
   [db_ref], so pinning here pins the whole prepared-statement lifecycle. *)
let prepare_impl ?on top sql =
  (* #634: fail at prepare rather than handing back a statement whose [db_ref]
     is already dead. *)
  if is_stale top
  then Lwt.return (Error (Runtime stale_msg))
  else (
    match parse sql with
    | Error e -> Lwt.return (Error e)
    | Ok ast ->
      let t =
        match on with
        | Some t -> t
        | None -> resolve_target_ast top ast
      in
      let* bound = Sql.Sema.bind_returning_params ~views:t.views t.catalog ast in
      (match bound with
       | Error e -> Lwt.return (Error (Sema e))
       | Ok (b, names) ->
         let plan = Sql.Planner.plan ~cat:t.catalog b in
         Lwt.return (Ok { db_ref = t; plan; param_names = names; finalized = false })))
;;

let prepare top sql = prepare_impl top sql

(* #433: plan a statement against this handle's schema without executing it.
   Read-only — no transaction is opened and nothing is written.  It is the
   replacement for the one use of the removed [catalog] accessor a read-only
   schema projection cannot serve: [Sema.bind] and [Planner.plan] both take a
   [Catalog.t].  Staleness is checked for the same reason {!prepare_impl}
   checks it — a post-VACUUM handle's catalog describes a closed store. *)
let plan top sql =
  if is_stale top
  then Lwt.return (Error (Runtime stale_msg))
  else fst (compile_routed top sql)
;;

let param_slot st name = List.assoc_opt name st.param_names

let params_of_named st named =
  let n = List.fold_left (fun acc (_, i) -> max acc (i + 1)) 0 st.param_names in
  let arr = Array.make n Row.V_null in
  List.iter
    (fun (name, v) ->
       match List.assoc_opt name st.param_names with
       | Some i -> arr.(i) <- v
       | None -> ())
    named;
  arr
;;

let run_core st ~params =
  if st.finalized
  then Lwt.return (Error (Runtime "statement already finalized"))
  else if
    (* #634: a statement prepared before a sibling's VACUUM holds [db_ref] to a
       handle over the closed store, and its plan embeds [table_meta] from the
       pre-VACUUM catalog.  Refuse it — the db.mli note that prepared statements
       "continue to work logically" across VACUUM is true only for statements
       prepared on the handle that RAN the vacuum. *)
    is_stale st.db_ref
  then Lwt.return (Error (Runtime stale_msg))
  else if
    (* #555: a prepared write resolves its mode from [explicit_txn] just like a
             one-shot statement, so it is exposed to the same contamination.
             [st.db_ref] is already the routed handle — [prepare] resolved it. *)
    is_poisoned st.db_ref
  then Lwt.return (Error (Runtime poisoned_msg))
  else if
    (* #740: a PREPARED [PRAGMA wal_checkpoint] never reaches
       [execute_control_op] — [run_core] hands [st.plan] straight to
       [Sql.Exec.execute_with_count] — so the refusal is owed here too, or the
       one-shot spelling is refused while the prepared spelling still
       deadlocks. *)
    wal_checkpoint_would_deadlock st.db_ref st.plan
  then Lwt.return (Error (Runtime wal_checkpoint_in_txn_msg))
  else (
    let params_arr = Array.of_list params in
    let t = st.db_ref in
    let mode =
      match t.explicit_txn with
      | None -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    let before_hook, after_hook =
      match st.plan with
      | Sql.Plan.Op_insert { table_meta; _ } ->
        ( make_trigger_hook t table_meta ~timing:`Before ~event:`Insert
        , make_trigger_hook t table_meta ~timing:`After ~event:`Insert )
      | Sql.Plan.Op_insert_select { table_meta; _ } ->
        ( make_trigger_hook t table_meta ~timing:`Before ~event:`Insert
        , make_trigger_hook t table_meta ~timing:`After ~event:`Insert )
      | Sql.Plan.Op_update { table_meta; _ } ->
        ( make_trigger_hook t table_meta ~timing:`Before ~event:`Update
        , make_trigger_hook t table_meta ~timing:`After ~event:`Update )
      | Sql.Plan.Op_delete { table_meta; _ } ->
        ( make_trigger_hook t table_meta ~timing:`Before ~event:`Delete
        , make_trigger_hook t table_meta ~timing:`After ~event:`Delete )
      | _ -> None, None
    in
    let ( on_replace_delete_before
        , on_replace_delete
        , on_upsert_update_before
        , on_upsert_update )
      =
      insert_replace_upsert_hooks t st.plan
    in
    let insert_table_name =
      match st.plan with
      | Sql.Plan.Op_insert { table_meta; _ } -> Some table_meta.Cat.name
      | Sql.Plan.Op_insert_select { table_meta; _ } -> Some table_meta.Cat.name
      | _ -> None
    in
    Lwt.catch
      (fun () ->
         let* n =
           Sql.Exec.execute_with_count
             ~mode
             ~clock:t.clock
             ~params:params_arr
             ~before_hook
             ~after_hook
             ~on_replace_delete_before
             ~on_replace_delete
             ~on_upsert_update_before
             ~on_upsert_update
             t.store
             t.catalog
             st.plan
         in
         t.last_changes <- n;
         t.total_changes <- t.total_changes + n;
         (match insert_table_name with
          | Some tbl when n > 0 ->
            (match Cat.find_table_cached t.catalog ~name:tbl with
             (* #243 (T1): the executor records the actual inserted rowid; an
                explicit INTEGER PRIMARY KEY need not equal next_rowid - 1. *)
             | Some _ -> t.last_insert_rowid <- Cat.last_inserted_rowid t.catalog
             | None -> ())
          | _ -> ());
         if t.explicit_txn = None
         then
           let* r = drain_pending_fks_autocommit t in
           match r with
           | Ok () -> Lwt.return (Ok n)
           | Error e -> Lwt.return (Error e)
         else Lwt.return (Ok n))
      (function
        | Failure msg ->
          Cat.clear_pending_fk_checks t.catalog;
          Lwt.return (Error (Runtime msg))
        | exn -> Lwt.fail exn))
;;

let run st ~params = drive_reactive st.db_ref ~core:(fun () -> run_core st ~params)

let run_with_dirty st ~params =
  let acc = Sql.Exec.make_change_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> run st ~params) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok n -> Lwt.return (Ok (n, Sql.Exec.dirty_elements acc))
;;

(* #259: shared body for [iter] / [iter_with_stats].  As with [query_impl],
   omitting [stats] yields the unchanged zero-overhead read path. *)
let iter_impl ?stats st ~params =
  if st.finalized
  then Lwt.return (Error (Runtime "statement already finalized"))
  else if
    (* #634: as in [run_core] — a statement prepared before a sibling's VACUUM
       would read from the closed pre-VACUUM store. *)
    is_stale st.db_ref
  then Lwt.return (Error (Runtime stale_msg))
  else if
    (* #555: as in [run_core] — the mode below is resolved from
             [explicit_txn], which on a poisoned handle is somebody else's. *)
    is_poisoned st.db_ref
  then Lwt.return (Error (Runtime poisoned_msg))
  else (
    let params_arr = Array.of_list params in
    let t = st.db_ref in
    (* #262: thread the active explicit transaction into the read so a prepared
       SELECT iterated inside [BEGIN … COMMIT] sees the txn's own uncommitted
       writes (read-your-own-writes), matching the one-shot [query] path. *)
    let mode =
      match t.explicit_txn with
      | None -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    Lwt.catch
      (fun () ->
         let* stream =
           Sql.Exec.query
             ~mode
             ~clock:t.clock
             ~params:params_arr
             ?stats
             t.store
             t.catalog
             st.plan
         in
         Lwt.return (Ok stream))
      (function
        | Failure msg -> Lwt.return (Error (Runtime msg))
        | exn -> Lwt.fail exn))
;;

let iter st ~params = iter_impl st ~params

(* #239: [iter] plus a per-query cost/stats record populated as the stream
   drains, for an external cost-based cache. *)
let iter_with_stats st ~params =
  let stats = Sql.Exec.make_query_stats () in
  let* r = iter_impl ~stats st ~params in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok stream -> Lwt.return (Ok (stream, stats))
;;

let finalize st =
  st.finalized <- true;
  Lwt.return_unit
;;

let pp_error fmt = function
  | Parse msg -> Format.fprintf fmt "parse error: %s" msg
  | Sema e -> Format.fprintf fmt "sema error: %a" Sql.Sema.pp_error e
  | Runtime m -> Format.fprintf fmt "runtime error: %s" m
  | History_unavailable ->
    Format.fprintf fmt "history error: as-of reads are not enabled on this database"
  | History_pruned ->
    Format.fprintf fmt "history error: as-of target predates the retained history floor"
;;

(* ------------------------------------------------------------------ *)
(* #264: logical SQL dump (.dump-style export)                          *)
(* ------------------------------------------------------------------ *)

(* Deterministic emission order: tables in creation order (tree_id ascending),
   which also lets FK parents precede children for the common case. *)
let dump_table_order (tables : Cat.table_meta list) =
  List.sort
    (fun (a : Cat.table_meta) b ->
       let tid_of m =
         match m.Cat.storage with
         | Cat.Row { tree_id; _ } -> tree_id
         | Cat.Columnar _ -> max_int
       in
       compare (tid_of a) (tid_of b))
    tables
;;

(* True for an implicit index that replaying the [CREATE TABLE] already
   recreates.  Every other index — user indexes and UNIQUE-constraint indexes —
   encodes a constraint the table DDL omits and so must be emitted, or restoring
   would silently drop it.

   #533: this question used to be answered here, independently of what
   [ddl_of_table] actually rendered, and the two drifted: a table with two
   PRIMARY KEY declarations had the inline suffix suppressed on both columns
   while this still called their indexes implied, so the dump carried neither
   the key nor the index and a restore silently accepted duplicates.  It now
   delegates to [Exec.ddl_implies_index], which shares the renderer's own
   predicate.  As part of that, a composite/table-level PRIMARY KEY is rendered
   into the [CREATE TABLE] (recovered from its implicit index, the only record
   of the key's column order) instead of being downgraded to a bare
   [CREATE UNIQUE INDEX].

   The check keys on [idx_origin] (#273), set when the index is created, not on
   the [__pk_] name prefix [sema.ml] assigns: a user index deliberately named
   [__pk_*] on a PK column is [`User] origin and so is correctly still emitted. *)
let dump_index_is_implied (meta : Cat.table_meta) ~indexes (idx : Cat.index_info) =
  Sql.Exec.ddl_implies_index meta ~indexes idx
;;

(* Emit [INSERT] statements for every row of table/FTS-table [name] by selecting
   [col_idents] (already quoted, in storage order) through the executor — so the
   rowid-alias column resolves to its stored value and FTS content reads through
   the content tree.  [explicit_cols] names the columns in the [INSERT] (needed
   when the selected set omits generated columns); otherwise a bare
   [INSERT INTO t VALUES] is emitted.  [?mode] threads the dump's shared RO
   snapshot so reads are point-in-time. *)
(* #548: a database can hold a NULL in a column its own schema declares NOT NULL.
   Every such file predates #530, which made every PRIMARY KEY column imply NOT
   NULL: a table-level [PRIMARY KEY (k)], every column of a composite
   [PRIMARY KEY (a, b)], and a column added by [ALTER TABLE ... ADD COLUMN ...
   PRIMARY KEY] all used to be nullable, and [Catalog.open_] now re-derives the
   flag from the implicit PK index.  Reading such a file is fine; DUMPING it is
   not, because the dump renders the CURRENT declaration and then emits the
   stored rows, so the schema line and the data lines contradict each other and
   the script dies on the first offending row.

   The decision (#548): REFUSE, loudly and specifically, rather than emit a
   script that cannot replay.  The alternative — quietly dropping the NOT NULL
   from the offending column — produces a restorable script, but it is a silent
   schema downgrade, and silent degradation in exactly this area is what #533 and
   #553 were both about.  A database whose rows contradict its own schema is not
   describable in SQL; saying so is the only answer that never lies.  The message
   names the repair, and [~data_only:true] still dumps the rows.

   #583: the refusal reports the violation the dump STOPPED ON, not the scope —
   it is raised from inside the row stream, so it names one (table, column) and
   knows nothing about the rest of the file.  An operator repairing table by
   table off successive dump failures is doing exactly what #563's report mode
   was built to prevent, so the headline instruction is now [PRAGMA
   not_null_check] (every offending (table, column, count) in one pass) followed
   by [PRAGMA not_null_repair] (deletes them through [apply_delete_row], so
   indexes and ON DELETE cascades are honoured).  The hand-written UPDATE/DELETE
   stay in the message as the manual escape hatch — #548 refuses precisely to
   stop information being destroyed silently, so the non-destructive repair must
   remain visible — but they are no longer what the message leads with.

   Detected while the rows stream past, so a healthy database pays nothing: no
   extra scan, no extra query.  A dump that trips this has already emitted
   [BEGIN] and some statements, so [dump]'s handler closes it with [ROLLBACK] —
   a streaming sink is left holding a script that is safe to replay (it undoes
   itself) rather than one that half-applies. *)
let not_null_violation_message ~table ~column =
  Printf.sprintf
    "dump %s: column %s is declared NOT NULL but a stored row holds NULL, so the emitted \
     schema contradicts the emitted rows and the script would fail to replay (#548).  \
     This database predates #530 (every PRIMARY KEY column implies NOT NULL).  This \
     names the violation the dump stopped on, not the scope: run PRAGMA not_null_check \
     to see every offending (table, column, count) in the file, then PRAGMA \
     not_null_repair to delete those rows through the ordinary delete path (indexes and \
     ON DELETE cascades honoured).  To repair by hand instead, UPDATE %s SET %s = \
     <value> WHERE %s IS NULL keeps the rows and DELETE FROM %s WHERE %s IS NULL drops \
     them.  To extract the rows from the unrepaired file, dump with ~data_only:true."
    table
    column
    (Sql.Exec.quote_ident table)
    (Sql.Exec.quote_ident column)
    (Sql.Exec.quote_ident column)
    (Sql.Exec.quote_ident table)
    (Sql.Exec.quote_ident column)
;;

let emit_rows_as_inserts ?mode t ~name ~col_idents ~explicit_cols ~not_null_cols ~stmt =
  let qname = Sql.Exec.quote_ident name in
  let cols_csv = String.concat ", " col_idents in
  let select_sql = Printf.sprintf "SELECT %s FROM %s" cols_csv qname in
  let* r = query_impl ?mode t select_sql in
  match r with
  | Error e -> Lwt.fail (Failure (Format.asprintf "dump %s: %a" name pp_error e))
  | Ok stream ->
    let prefix =
      if explicit_cols
      then Printf.sprintf "INSERT INTO %s (%s) VALUES" qname cols_csv
      else Printf.sprintf "INSERT INTO %s VALUES" qname
    in
    Lwt_stream.iter_s
      (fun (row : Row.t) ->
         List.iter
           (fun (i, column) ->
              if i < Array.length row && row.(i) = Row.V_null
              then raise (Failure (not_null_violation_message ~table:name ~column)))
           not_null_cols;
         let vals =
           Array.to_list row
           |> List.map Sql.Exec.sql_literal_of_value
           |> String.concat ","
         in
         stmt (Printf.sprintf "%s(%s)" prefix vals))
      stream
;;

(* Emit [INSERT] statements for every row of [meta] (skipping generated columns,
   whose values are derived). *)
let dump_table_rows ?mode ?(check_not_null = true) t (meta : Cat.table_meta) ~stmt =
  let dump_cols =
    List.filter (fun (c : Row.column) -> c.Row.generated_as = None) meta.Cat.columns
  in
  if dump_cols = []
  then Lwt.return_unit
  else (
    let has_generated = List.length dump_cols <> List.length meta.Cat.columns in
    let col_idents =
      List.map (fun (c : Row.column) -> Sql.Exec.quote_ident c.Row.name) dump_cols
    in
    (* #548: the positions in the emitted row that the emitted schema will
       declare NOT NULL.  Empty when no schema is emitted ([~data_only]) — those
       rows replay against a schema the caller supplies, so there is nothing for
       them to contradict. *)
    let not_null_cols =
      if not check_not_null
      then []
      else
        List.mapi (fun i (c : Row.column) -> i, c) dump_cols
        |> List.filter_map (fun (i, (c : Row.column)) ->
          if c.Row.not_null then Some (i, c.Row.name) else None)
    in
    emit_rows_as_inserts
      ?mode
      t
      ~name:meta.Cat.name
      ~col_idents
      ~explicit_cols:has_generated
      ~not_null_cols
      ~stmt)
;;

(* #319/#330: emit [INSERT]s for an FTS5 table's stored content so a replay
   re-inserts the rows and rebuilds the index (matching sqlite3 [.dump] rather
   than restoring the table empty).  Each row carries its [rowid] explicitly
   ([INSERT INTO t(rowid, col..) VALUES(rowid, ..)]) so rowids round-trip exactly
   — a delete leaves a gap that the restore preserves, instead of re-packing.
   Rows are read directly from the content tree (the only place FTS rowids are
   surfaced) through [mode], the dump's shared snapshot / explicit txn. *)
let dump_fts_rows ~mode ~store (m : Cat.fts_table_meta) ~stmt =
  match m.Cat.fts_columns with
  | [] -> Lwt.return_unit
  | cols ->
    let qname = Sql.Exec.quote_ident m.Cat.fts_name in
    let col_idents = List.map Sql.Exec.quote_ident cols in
    let prefix =
      Printf.sprintf
        "INSERT INTO %s (rowid, %s) VALUES"
        qname
        (String.concat ", " col_idents)
    in
    let* rows = Sql.Exec.read_fts_content_rows store mode m in
    Lwt_list.iter_s
      (fun (rowid, texts) ->
         let vals =
           List.map (fun s -> Sql.Exec.sql_literal_of_value (Row.V_text s)) texts
         in
         stmt (Printf.sprintf "%s(%Ld,%s)" prefix rowid (String.concat "," vals)))
      rows
;;

(* Whole-word, case-insensitive occurrence of [word] in [s] (identifier-bounded
   so view "a" does not match inside "table"). *)
let mentions_ident ~word s =
  let s = String.lowercase_ascii s
  and word = String.lowercase_ascii word in
  let wl = String.length word
  and sl = String.length s in
  let is_id c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_' in
  let rec go i =
    if i + wl > sl
    then false
    else if
      String.sub s i wl = word
      && (i = 0 || not (is_id s.[i - 1]))
      && (i + wl = sl || not (is_id s.[i + wl]))
    then true
    else go (i + 1)
  in
  wl > 0 && go 0
;;

(* Order views so each appears after every other view it references.  A view's
   SELECT body is bound at CREATE time (sema binds it against the catalog and the
   already-created views), so a view selecting from another view fails to restore
   unless that dependency was emitted first.  [Cat.load_all_views] returns name
   order, not dependency order, so we topologically sort: an edge v -> w means
   v's body mentions view w's name (over-detection only adds a harmless edge).
   On an unexpected cycle (illegal for valid schemas) we fall back to input order
   for the remainder. *)
let order_views_by_dependency (views : (string * string) list) =
  let names = List.map fst views in
  let deps =
    List.map
      (fun (n, sql) ->
         n, List.filter (fun m -> m <> n && mentions_ident ~word:m sql) names)
      views
  in
  let rec loop done_ remaining =
    if remaining = []
    then done_
    else (
      (* a view is ready once none of its view-dependencies are still pending *)
      let ready =
        List.filter
          (fun n ->
             List.for_all (fun d -> not (List.mem d remaining)) (List.assoc n deps))
          remaining
      in
      let ready = if ready = [] then remaining else ready in
      loop (done_ @ ready) (List.filter (fun n -> not (List.mem n ready)) remaining))
  in
  let ordered = loop [] names in
  List.map (fun n -> n, List.assoc n views) ordered
;;

let dump t ?(schema_only = false) ?(data_only = false) ~sink () =
  let stmt s = sink (s ^ ";\n") in
  let cat = t.catalog in
  (* #274: read every table's data under one snapshot so the source side is a
     single point-in-time committed view, like sqlite3's [.dump] (which reads the
     whole database under one transaction).  Without this, each table's read
     opened its own fresh RO snapshot, so a commit landing between two tables'
     reads yielded a torn dump reflecting no single committed state.

     The shared snapshot only applies when the dump's [SELECT]s actually read
     [t.store]: that is, when no explicit txn is active (an explicit txn already
     gives all reads one consistent read-your-own-writes view) and the active
     schema is [main] (otherwise [compile_routed] routes the unqualified
     per-table [SELECT] to an attached sub-handle's store, for which a snapshot
     of [t.store] would be the wrong store — that path keeps its prior
     per-statement behavior).  The snapshot is ended when finished, even on
     error (#164). *)
  (* [load_views]/[load_triggers] read view & trigger DDL through the SAME txn the
     row data is read through, so the schema section is consistent with the data:
     - explicit txn active: through that txn (#329, read-your-own-writes, #262),
       matching the row data which also reads through it;
     - autocommit on [main]: through the shared #274 RO snapshot (#322/#323);
     - autocommit on a non-main active schema: per-call committed snapshots (the
       row reads route to an attached store with no single snapshot anyway). *)
  (* [fts_mode] is the concrete [txn_mode] equivalent of [read_mode] (which is
     [None] when [query_impl] would resolve it from [t.explicit_txn]); the FTS
     content read (#330) needs an explicit mode rather than the [?mode] optional. *)
  let* read_mode, fts_mode, finish_snapshot, load_views, load_triggers =
    match t.explicit_txn with
    | Some tx ->
      Lwt.return
        ( None
        , Sql.Exec.In_txn tx
        , (fun () -> Lwt.return_unit)
        , (fun () -> Cat.load_all_views_in_tx tx)
        , fun () -> Cat.load_all_triggers_in_tx tx )
    | None when String.equal t.active_schema "main" ->
      let* snap = S.ro_begin t.store in
      Lwt.return
        ( Some (Sql.Exec.In_ro_txn snap)
        , Sql.Exec.In_ro_txn snap
        , (fun () -> S.ro_end snap)
        , (fun () -> Cat.load_all_views_in_tx snap)
        , fun () -> Cat.load_all_triggers_in_tx snap )
    | None ->
      Lwt.return
        ( None
        , Sql.Exec.Auto
        , (fun () -> Lwt.return_unit)
        , (fun () -> Cat.load_all_views t.store)
        , fun () -> Cat.load_all_triggers t.store )
  in
  (* #321: track whether [BEGIN] was actually emitted, so a [Failure] in the
     pre-BEGIN window (catalog enumeration, the [PRAGMA] write, or a streaming
     [sink] that raises on its first write) does not close a transaction that was
     never opened with a dangling [ROLLBACK]. *)
  let began = ref false in
  Lwt.finalize
    (fun () ->
       Lwt.catch
         (fun () ->
            let* tables = Cat.list_tables cat in
            let tables = dump_table_order tables in
            let* () = sink "PRAGMA foreign_keys=OFF;\n" in
            (* #281: every dump is wrapped in [BEGIN] … [COMMIT], like sqlite3 .dump,
          so a restore applies atomically (and faster).  This used to be limited
          to the DML-only [data_only] dump because DDL inside an explicit
          transaction deadlocked the catalog's writer txn (#269); #269 (PR #278)
          removed that deadlock, so a schema-bearing dump now replays atomically
          too — every statement the dump emits (CREATE TABLE/INDEX/VIEW/TRIGGER,
          CREATE VIRTUAL TABLE, INSERT, and the sqlite_sequence DELETE/INSERT) is
          transactional, so there is no per-statement-autocommit fallback to
          keep.  The [PRAGMA foreign_keys=OFF] stays outside the transaction,
          matching sqlite. *)
            let* () = stmt "BEGIN" in
            began := true;
            (* Base tables: DDL immediately followed by that table's data. *)
            let* () =
              Lwt_list.iter_s
                (fun (meta : Cat.table_meta) ->
                   let* () =
                     if data_only
                     then Lwt.return_unit
                     else
                       stmt
                         (Sql.Exec.ddl_of_table
                            ~indexes:(Cat.indexes_for_table cat ~table:meta.Cat.name)
                            meta)
                   in
                   if schema_only
                   then Lwt.return_unit
                   else
                     dump_table_rows
                       ?mode:read_mode
                       ~check_not_null:(not data_only)
                       t
                       meta
                       ~stmt)
                tables
            in
            (* #319: FTS virtual tables — [CREATE VIRTUAL TABLE] (unless
          [data_only]) immediately followed by its content rows as [INSERT]s
          (unless [schema_only]), so a replay re-inserts the rows and rebuilds the
          index instead of restoring the table empty. *)
            let* () =
              Lwt_list.iter_s
                (fun (m : Cat.fts_table_meta) ->
                   let* () =
                     if data_only then Lwt.return_unit else stmt (Sql.Exec.ddl_of_fts m)
                   in
                   if schema_only
                   then Lwt.return_unit
                   else dump_fts_rows ~mode:fts_mode ~store:t.store m ~stmt)
                (Cat.list_fts_tables cat)
            in
            (* Schema objects emitted after all data: explicit indexes, then views
          and triggers. *)
            let* () =
              if data_only
              then Lwt.return_unit
              else
                let* () =
                  Lwt_list.iter_s
                    (fun (meta : Cat.table_meta) ->
                       let indexes = Cat.indexes_for_table cat ~table:meta.Cat.name in
                       Lwt_list.iter_s
                         (fun idx ->
                            if dump_index_is_implied meta ~indexes idx
                            then Lwt.return_unit
                            else stmt (Sql.Exec.ddl_of_index idx))
                         indexes)
                    tables
                in
                (* #322/#323/#329: view & trigger DDL read through the same txn as
              the row data (see [load_views]/[load_triggers] above), so the schema
              section is consistent with the data and a concurrent
              CREATE/DROP VIEW|TRIGGER commit cannot tear the dump. *)
                let* views = load_views () in
                let* () =
                  Lwt_list.iter_s
                    (fun (_n, sql) -> stmt sql)
                    (order_views_by_dependency views)
                in
                let* triggers = load_triggers () in
                Lwt_list.iter_s (fun (_n, sql) -> stmt sql) triggers
            in
            (* #312: emit the AUTOINCREMENT high-water like SQLite's [.dump], so a
          committed-DELETE high-water round-trips — replaying the data rows alone
          only restores [max(rowid)+1].  This is data, so it is skipped in
          [schema_only]; it is emitted before [COMMIT] so a [data_only] dump
          carries it too.  Only tables with a seeded counter appear. *)
            let* () =
              if schema_only
              then Lwt.return_unit
              else (
                let seeded =
                  List.filter
                    (fun (m : Cat.table_meta) ->
                       match m.Cat.storage with
                       | Cat.Row { autoincrement = true; next_rowid; _ }
                         when not (Int64.equal next_rowid Cat.empty_next_rowid) -> true
                       | _ -> false)
                    tables
                in
                if seeded = []
                then Lwt.return_unit
                else
                  let* () = stmt "DELETE FROM sqlite_sequence" in
                  Lwt_list.iter_s
                    (fun (m : Cat.table_meta) ->
                       let _, nrid, _, _ = Cat.row_storage m in
                       stmt
                         (Printf.sprintf
                            "INSERT INTO sqlite_sequence VALUES(%s,%Ld)"
                            (Sql.Exec.sql_literal_of_value (Row.V_text m.Cat.name))
                            (Int64.sub nrid 1L)))
                    seeded)
            in
            let* () = stmt "COMMIT" in
            Lwt.return (Ok ()))
         (function
           | Failure msg ->
             (* #321: only close with [ROLLBACK] if [BEGIN] was actually emitted;
                otherwise (a failure in the pre-BEGIN window) a [ROLLBACK] with no
                matching [BEGIN] would be rejected on replay.  Guard the handler's
                own [sink] call so that if the [sink] itself is the failure source,
                its re-invocation cannot raise out of [Lwt.catch] and break the
                [Ok]/[Error] contract. *)
             let* () =
               if !began
               then Lwt.catch (fun () -> stmt "ROLLBACK") (fun _ -> Lwt.return_unit)
               else Lwt.return_unit
             in
             Lwt.return (Error (Runtime msg))
           | exn -> Lwt.fail exn))
    finish_snapshot
;;

let dump_to_string t ?(schema_only = false) ?(data_only = false) () =
  let buf = Buffer.create 4096 in
  let* r =
    dump
      t
      ~schema_only
      ~data_only
      ~sink:(fun s ->
        Buffer.add_string buf s;
        Lwt.return_unit)
      ()
  in
  match r with
  | Error _ as e -> Lwt.return e
  | Ok () -> Lwt.return (Ok (Buffer.contents buf))
;;

(* ================================================================== *)
(* #427: reactive-view driver implementation                           *)
(* ================================================================== *)

module Rv = Reactive_view

let rv_table_name name = "_rv_" ^ name

(* #474: every statement the driver issues on its own behalf is PINNED to the
   handle the reactive view lives on.

   The driver used to reach the engine through the plain [execute] / [query] /
   [prepare] entry points, which route through {!resolve_target_ast} and
   therefore obey [top.active_schema].  Reactive views live on the top-level
   handle only, so under [ATTACH … AS aux; PRAGMA active_database = aux] every
   internal statement — the [CREATE TABLE _rv_<name>], the materialisation's
   INSERTs and DELETEs, the [SELECT * FROM _rv_<name>] the diff is computed
   against, and the teardown [DROP TABLE] — was aimed at [aux] instead of at the
   schema that owns the view.  #469 pinned its own [DROP REACTIVE VIEW] arm by
   hand and [rv_query_ast] was already pinned (it binds against [top.catalog]
   directly), but the rest of the driver was not.

   Pinning happens once here rather than at each call site, so a new driver
   statement is pinned by construction.  Note this is NOT a routing statement
   and mutates no routing state: it cannot move the CALLER's active schema, so
   it is not a second door of the kind #598 closed. *)
let rv_execute top sql = drive_reactive top ~core:(fun () -> execute_core ~on:top top sql)
let rv_query top sql = query_impl ~on:top top sql
let rv_prepare top sql = prepare_impl ~on:top top sql

(* [rv_quote] / [rv_sql_name] are defined further up, next to [rv_owned_table],
   because {!execute_control_op}'s DROP guards need them. *)

let rv_ty_sql = function
  | Row.Integer -> "INTEGER"
  | Row.Text -> "TEXT"
  | Row.Real -> "REAL"
  | Row.Blob -> "BLOB"
;;

let rv_ty_of_value = function
  | Row.V_int _ -> Row.Integer
  | Row.V_real _ -> Row.Real
  | Row.V_text _ -> Row.Text
  | Row.V_blob _ -> Row.Blob
  | Row.V_null -> Row.Text
;;

let rec rv_iter_ok f = function
  | [] -> Lwt.return (Ok ())
  | x :: xs ->
    let* r = f x in
    (match r with
     | Ok () -> rv_iter_ok f xs
     | Error _ as e -> Lwt.return e)
;;

(* Read all rows of a SQL query into a list. *)
let rv_query_sql top sql =
  let* r = rv_query top sql in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok stream ->
    let* rows = Lwt_stream.to_list stream in
    Lwt.return (Ok rows)
;;

(* Run the view's own SELECT (as a parsed AST) and collect its rows. *)
let rv_query_ast top ast =
  let* bound = Sql.Sema.bind ~views:top.views top.catalog ast in
  match bound with
  | Error e -> Lwt.return (Error (Sema e))
  | Ok b ->
    let op = Sql.Planner.plan ~cat:top.catalog b in
    let mode =
      match top.explicit_txn with
      | None -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    let* stream = Sql.Exec.query ~mode ~clock:top.clock top.store top.catalog op in
    let* rows = Lwt_stream.to_list stream in
    Lwt.return (Ok rows)
;;

let rv_current_rows top entry =
  rv_query_sql
    top
    (Printf.sprintf "SELECT * FROM %s" (rv_quote (rv_table_name entry.rv_name)))
;;

(* Insert a batch of rows into [tbl] (a quoted table name) via one prepared
   statement. *)
let rv_insert_rows top tbl ncols rows =
  let phs = String.concat ", " (List.init ncols (fun _ -> "?")) in
  let sql = Printf.sprintf "INSERT INTO %s VALUES (%s)" tbl phs in
  let* pr = rv_prepare top sql in
  match pr with
  | Error e -> Lwt.return (Error e)
  | Ok st ->
    rv_iter_ok
      (fun r ->
         let* rr = run st ~params:(Array.to_list r) in
         match rr with
         | Ok _ -> Lwt.return (Ok ())
         | Error e -> Lwt.return (Error e))
      rows
;;

(* Apply an output delta [(row, weight)] to [_rv_<name>] and fire callbacks with
   native {!row_change} diffs.  [weight < 0] rows are removed, [weight > 0] rows
   inserted; view-output changes surface as Deleted/Inserted pairs (a changed
   group is a delete of its old aggregate row plus an insert of the new one),
   matching how the row-level feed models identity changes. *)
let rv_apply_and_notify top entry out_delta =
  if out_delta = []
  then Lwt.return (Ok ())
  else (
    let tbl = rv_quote (rv_table_name entry.rv_name) in
    let cols = entry.rv_out_cols in
    let ncols = List.length cols in
    let want_cb = entry.rv_callbacks <> [] in
    let changes = ref [] in
    let dels = List.filter_map (fun (r, w) -> if w < 0 then Some r else None) out_delta in
    let inss = List.filter_map (fun (r, w) -> if w > 0 then Some r else None) out_delta in
    (* Delete a materialised row by exact match.  [IS <expr>] is not supported by
       the parser, so match NULL cells with [IS NULL] and the rest with [= ?]. *)
    let delete_one r =
      let conds =
        List.mapi
          (fun i c ->
             match r.(i) with
             | Row.V_null -> rv_quote c ^ " IS NULL"
             | _ -> rv_quote c ^ " = ?")
          cols
      in
      let params = Array.to_list r |> List.filter (fun v -> v <> Row.V_null) in
      (* [LIMIT 1]: [rv_multiset_diff] yields one [removed] entry per excess copy,
         so each delete must remove exactly one matching row — a full-refresh view
         whose projection is non-distinct can hold several identical rows, and an
         unbounded DELETE would wipe all copies (silent data loss). *)
      let del_sql =
        Printf.sprintf "DELETE FROM %s WHERE %s LIMIT 1" tbl (String.concat " AND " conds)
      in
      let* pr = rv_prepare top del_sql in
      match pr with
      | Error e -> Lwt.return (Error e)
      | Ok st ->
        let* rr = run st ~params in
        (match rr with
         | Ok _ ->
           if want_cb then changes := Deleted { rowid = 0L; row = r } :: !changes;
           Lwt.return (Ok ())
         | Error e -> Lwt.return (Error e))
    in
    let* rdel = rv_iter_ok delete_one dels in
    match rdel with
    | Error _ as e -> Lwt.return e
    | Ok () ->
      let phs = String.concat ", " (List.init ncols (fun _ -> "?")) in
      let ins_sql = Printf.sprintf "INSERT INTO %s VALUES (%s)" tbl phs in
      let* pins = rv_prepare top ins_sql in
      (match pins with
       | Error e -> Lwt.return (Error e)
       | Ok st ->
         let* rins =
           rv_iter_ok
             (fun r ->
                let* rr = run st ~params:(Array.to_list r) in
                match rr with
                | Ok _ ->
                  if want_cb
                  then
                    changes
                    := Inserted { rowid = top.last_insert_rowid; row = r } :: !changes;
                  Lwt.return (Ok ())
                | Error e -> Lwt.return (Error e))
             inss
         in
         (match rins with
          | Error _ as e -> Lwt.return e
          | Ok () ->
            if want_cb && !changes <> []
            then (
              let batch = List.rev !changes in
              let* () = Lwt_list.iter_s (fun cb -> cb batch) entry.rv_callbacks in
              Lwt.return (Ok ()))
            else Lwt.return (Ok ()))))
;;

let rv_meas measure row =
  match measure with
  | RV_count -> 1
  | RV_sum ord ->
    (match row.(ord) with
     | Row.V_int i -> Int64.to_int i
     | Row.V_real f -> int_of_float f
     | _ -> 0)
;;

let rv_proj group_ord measure row : Rv.Agg_engine.input =
  { key = [| row.(group_ord) |]; meas = rv_meas measure row }
;;

let rv_change_of group_ord measure : row_change -> Rv.Agg_engine.change = function
  | Inserted { row; _ } -> Rv.Agg_engine.Ins (rv_proj group_ord measure row)
  | Deleted { row; _ } -> Rv.Agg_engine.Del (rv_proj group_ord measure row)
  | Updated { old_row; new_row; _ } ->
    Rv.Agg_engine.Upd
      (rv_proj group_ord measure old_row, rv_proj group_ord measure new_row)
;;

(* Multiset difference of two row lists → (removed, added). *)
let rv_multiset_diff old_rows new_rows =
  let os = List.sort Rv.row_compare old_rows in
  let ns = List.sort Rv.row_compare new_rows in
  let rec go removed added os ns =
    match os, ns with
    | [], ns -> List.rev removed, List.rev_append added ns
    | os, [] -> List.rev_append removed os, List.rev added
    | o :: ot, n :: nt ->
      let c = Rv.row_compare o n in
      if c = 0
      then go removed added ot nt
      else if c < 0
      then go (o :: removed) added ot ns
      else go removed (n :: added) os nt
  in
  go [] [] os ns
;;

let rv_delta_of_diff (removed, added) =
  List.map (fun r -> r, -1) removed @ List.map (fun r -> r, 1) added
;;

(* Full-refresh: recompute the SELECT, diff against the current materialisation,
   apply the minimal change set. *)
(* Per-column SQL type inferred from data: the first non-null value's type,
   defaulting to TEXT for an all-null (or empty) column. *)
let rv_infer_full_types rows arity =
  List.init arity (fun i ->
    let rec find = function
      | [] -> "TEXT"
      | (r : Row.value array) :: rest ->
        (match r.(i) with
         | Row.V_null -> find rest
         | v -> rv_ty_sql (rv_ty_of_value v))
    in
    find rows)
;;

let rv_create_table top name out_cols coltypes =
  let coldefs =
    List.mapi (fun i c -> rv_quote c ^ " " ^ List.nth coltypes i) out_cols
    |> String.concat ", "
  in
  let sql =
    Printf.sprintf "CREATE TABLE %s (%s)" (rv_quote (rv_table_name name)) coldefs
  in
  rv_execute top sql
;;

(* Current SQL column types of [_rv_<name>], from the catalog. *)
let rv_table_col_types top name =
  match Cat.find_table_cached top.catalog ~name:(rv_table_name name) with
  | Some meta ->
    Some (List.map (fun (c : Row.column) -> rv_ty_sql c.Row.ty) meta.Cat.columns)
  | None -> None
;;

let rv_refresh_full top entry =
  let* nr = rv_query_ast top entry.rv_query in
  match nr with
  | Error _ as e -> Lwt.return e
  | Ok new_rows ->
    let arity = List.length entry.rv_out_cols in
    let want_types =
      if new_rows = [] then None else Some (rv_infer_full_types new_rows arity)
    in
    (* Re-type [_rv_…] when (a) it was a placeholder all-TEXT schema created while
       empty, or (b) a column's runtime type has drifted (nullable/union-typed
       projections) so the inferred type no longer matches — otherwise strict
       column typing would reject the INSERT and fail the (already-committed)
       triggering statement. *)
    let needs_recreate =
      match want_types with
      | None -> false
      | Some want ->
        entry.rv_provisional
        ||
          (match rv_table_col_types top entry.rv_name with
          | Some cur -> cur <> want
          | None -> false)
    in
    if needs_recreate
    then (
      let coltypes = Option.get want_types in
      let* cr0 = rv_current_rows top entry in
      match cr0 with
      | Error _ as e -> Lwt.return e
      | Ok old_rows ->
        let* dr =
          (* #475: permission to bypass the internal-table guard is scoped to
             THIS statement and THIS table name, not to the whole refresh. *)
          rv_dropping_internal top (rv_table_name entry.rv_name) (fun () ->
            rv_execute
              top
              (Printf.sprintf "DROP TABLE %s" (rv_quote (rv_table_name entry.rv_name))))
        in
        (match dr with
         | Error _ as e -> Lwt.return e
         | Ok () ->
           let* cr = rv_create_table top entry.rv_name entry.rv_out_cols coltypes in
           (match cr with
            | Error _ as e -> Lwt.return e
            | Ok () ->
              entry.rv_provisional <- false;
              (* Table is freshly empty: the [removed] deletes are harmless no-ops
                 that still drive Deleted callbacks; the [added] inserts refill it. *)
              rv_apply_and_notify top entry (rv_delta_of_diff (old_rows, new_rows)))))
    else
      let* cr = rv_current_rows top entry in
      (match cr with
       | Error _ as e -> Lwt.return e
       | Ok old_rows ->
         rv_apply_and_notify
           top
           entry
           (rv_delta_of_diff (rv_multiset_diff old_rows new_rows)))
;;

(* Rebuild a delta engine from the current base-table contents (used on open and
   after a savepoint rollback made accumulated deltas untrustworthy). *)
let rv_rebuild_engine top ~base ~group_ord ~measure =
  let* br = rv_query_sql top (Printf.sprintf "SELECT * FROM %s" (rv_quote base)) in
  match br with
  | Error _ as e -> Lwt.return e
  | Ok rows ->
    let engine = Rv.Agg_engine.create () in
    let evs =
      List.map (fun row -> Rv.Agg_engine.Ins (rv_proj group_ord measure row)) rows
    in
    let _ = Rv.Agg_engine.step engine evs in
    Lwt.return (Ok engine)
;;

let rv_refresh_one top entry ~resync =
  match entry.rv_mode with
  | RV_full -> rv_refresh_full top entry
  | RV_delta { group_ord; measure; engine } ->
    if resync
    then (
      let base = List.hd entry.rv_base_tables in
      let* er = rv_rebuild_engine top ~base ~group_ord ~measure in
      match er with
      | Error _ as e -> Lwt.return e
      | Ok fresh ->
        entry.rv_mode <- RV_delta { group_ord; measure; engine = fresh };
        let target = Rv.Agg_engine.snapshot fresh in
        let* cr = rv_current_rows top entry in
        (match cr with
         | Error _ as e -> Lwt.return e
         | Ok cur ->
           rv_apply_and_notify top entry (rv_delta_of_diff (rv_multiset_diff cur target))))
    else (
      let base = List.hd entry.rv_base_tables in
      let changes =
        match Hashtbl.find_opt top.rv_pending base with
        | Some l -> l
        | None -> []
      in
      let evs = List.map (rv_change_of group_ord measure) changes in
      let out_delta = Rv.Agg_engine.step engine evs in
      rv_apply_and_notify top entry out_delta)
;;

let rv_flush_inner top =
  Lwt.finalize
    (fun () ->
       let dirty = Hashtbl.fold (fun k _ acc -> k :: acc) top.rv_pending [] in
       let resync = top.rv_resync in
       let views =
         Hashtbl.fold
           (fun _ e acc ->
              if resync || List.exists (fun bt -> List.mem bt dirty) e.rv_base_tables
              then e :: acc
              else acc)
           top.reactive_views
           []
       in
       rv_iter_ok (fun e -> rv_refresh_one top e ~resync) views)
    (fun () ->
       Hashtbl.reset top.rv_pending;
       top.rv_resync <- false;
       Lwt.return_unit)
;;

let rv_flush top =
  (* #475: the guard is taken and released by the SAME function.  It used to be
     set here and cleared inside [rv_flush_inner]'s finalizer, which is how a
     nested flush released it on behalf of an enclosing [rv_create]/[rv_load]. *)
  rv_guard top (fun () ->
    (* #427 review: the flush writes [_rv_<name>] via internal DML.  If a
       caller's change-capturing accumulator is still bound (e.g.
       [execute_with_changes]), those derived writes would surface as user
       changes.  Shadow it with a throwaway non-capturing accumulator for the
       flush's extent. *)
    Sql.Exec.with_dirty (Sql.Exec.make_dirty_acc ()) (fun () -> rv_flush_inner top))
;;

let rv_ordinal cols name =
  let rec go i = function
    | [] -> None
    | (c : Row.column) :: _ when String.equal c.name name -> Some i
    | _ :: r -> go (i + 1) r
  in
  go 0 cols
;;

(* Resolve a Delta classification into base-row ordinals + a measure.  Fails
   (Error) when the shape cannot actually be maintained (missing column, or a
   SUM over a non-integer column). *)
let rv_build_delta top base_tables group_col agg =
  match base_tables with
  | [ base ] ->
    (match Cat.find_table_cached top.catalog ~name:base with
     | None -> Error (Printf.sprintf "reactive view: base table '%s' not found" base)
     | Some meta ->
       let cols = meta.Cat.columns in
       (match rv_ordinal cols group_col with
        | None ->
          Error (Printf.sprintf "reactive view: GROUP BY column '%s' not found" group_col)
        | Some gord ->
          (match agg with
           | Rv.Count -> Ok (gord, RV_count)
           | Rv.Sum_col c ->
             (match rv_ordinal cols c with
              | None ->
                Error (Printf.sprintf "reactive view: SUM column '%s' not found" c)
              | Some mord ->
                (match (List.nth cols mord).Row.ty with
                 | Row.Integer -> Ok (gord, RV_sum mord)
                 | _ ->
                   Error
                     (Printf.sprintf
                        "reactive view: SUM(%s) over a non-integer column is not \
                         delta-maintainable"
                        c))))))
  | _ -> Error "reactive view: delta maintenance requires a single base table"
;;

let rv_group_col_ty top base group_ord =
  match Cat.find_table_cached top.catalog ~name:base with
  | Some meta -> rv_ty_sql (List.nth meta.Cat.columns group_ord).Row.ty
  | None -> "TEXT"
;;

let rv_create top ~sql ~name query refresh =
  if Hashtbl.mem top.reactive_views name
  then
    Lwt.return (Error (Runtime (Printf.sprintf "reactive view '%s' already exists" name)))
  else (
    let cls = Rv.classify query in
    let base_tables = cls.base_tables in
    let decided =
      match refresh, cls.kind with
      | Sql.Ast.Refresh_full, _ -> Ok `Full
      | Sql.Ast.Refresh_delta, Rv.Delta { group_col; agg } ->
        (match rv_build_delta top base_tables group_col agg with
         | Ok x -> Ok (`Delta x)
         | Error e -> Error e)
      | Sql.Ast.Refresh_delta, Rv.Full ->
        Error
          (Printf.sprintf
             "REFRESH DELTA: view '%s' is not delta-maintainable (only single-column \
              COUNT/SUM GROUP BY is supported incrementally)"
             name)
      | Sql.Ast.Refresh_auto, Rv.Delta { group_col; agg } ->
        (match rv_build_delta top base_tables group_col agg with
         | Ok x -> Ok (`Delta x)
         | Error _ -> Ok `Full)
      | Sql.Ast.Refresh_auto, Rv.Full -> Ok `Full
    in
    match decided with
    | Error e -> Lwt.return (Error (Runtime e))
    | Ok choice ->
      rv_guard top (fun () ->
        let register ~provisional mode out_cols =
          let entry =
            { rv_name = name
            ; rv_query = query
            ; rv_base_tables = base_tables
            ; rv_out_cols = out_cols
            ; rv_mode = mode
            ; rv_provisional = provisional
            ; rv_callbacks = []
            }
          in
          Hashtbl.replace top.reactive_views name entry;
          (* #476: the catalog write reports a store fault as [Error msg]; lift
             it into [Db]'s own error type rather than letting it raise. *)
          let* pr = Cat.persist_reactive_view top.store ~name ~sql in
          match pr with
          | Error msg -> Lwt.return (Error (Runtime msg))
          | Ok () -> Lwt.return (Ok ())
        in
        match choice with
        | `Delta (group_ord, measure) ->
          let base = List.hd base_tables in
          let* er = rv_rebuild_engine top ~base ~group_ord ~measure in
          (match er with
           | Error _ as e -> Lwt.return e
           | Ok engine ->
             let out_cols =
               match cls.out_cols with
               | Some c -> c
               | None -> [ "grp"; "agg" ]
             in
             let coltypes = [ rv_group_col_ty top base group_ord; "INTEGER" ] in
             let* cr = rv_create_table top name out_cols coltypes in
             (match cr with
              | Error _ as e -> Lwt.return e
              | Ok () ->
                let init_rows = Rv.Agg_engine.snapshot engine in
                let* ir =
                  rv_insert_rows
                    top
                    (rv_quote (rv_table_name name))
                    (List.length out_cols)
                    init_rows
                in
                (match ir with
                 | Error _ as e -> Lwt.return e
                 | Ok () ->
                   register
                     ~provisional:false
                     (RV_delta { group_ord; measure; engine })
                     out_cols)))
        | `Full ->
          let* nr = rv_query_ast top query in
          (match nr with
           | Error _ as e -> Lwt.return e
           | Ok new_rows ->
             let arity =
               match new_rows with
               | r :: _ -> Array.length r
               | [] ->
                 (match cls.out_cols with
                  | Some c -> List.length c
                  | None -> 0)
             in
             if arity = 0
             then
               Lwt.return
                 (Error
                    (Runtime
                       (Printf.sprintf
                          "reactive view '%s': cannot determine the output columns from \
                           an empty result; a reactive view needs a projection whose \
                           column list is known statically"
                          name)))
             else (
               let out_cols =
                 match cls.out_cols with
                 | Some c when List.length c = arity -> c
                 | _ -> List.init arity (fun i -> Printf.sprintf "c%d" i)
               in
               (* No rows yet → placeholder all-TEXT schema, re-typed on the
                  first non-empty refresh. *)
               let provisional = new_rows = [] in
               let coltypes =
                 if provisional
                 then List.init arity (fun _ -> "TEXT")
                 else rv_infer_full_types new_rows arity
               in
               let* cr = rv_create_table top name out_cols coltypes in
               match cr with
               | Error _ as e -> Lwt.return e
               | Ok () ->
                 let* ir =
                   rv_insert_rows top (rv_quote (rv_table_name name)) arity new_rows
                 in
                 (match ir with
                  | Error _ as e -> Lwt.return e
                  | Ok () -> register ~provisional RV_full out_cols)))))
;;

(* #469: erase a reactive view's persistent state: catalog row first, then the
   [_rv_<name>] materialisation.

   The order matters, and it is *not* the one the design spec §2 wrote down —
   the spec's stated rationale ("a crash part-way must not leave something that
   resurrects or breaks the db") is what this order actually achieves, so the
   order is what changed, not the goal.  These are two separate autocommit
   transactions ([Cat.remove_reactive_view] always takes its own writer txn, and
   [execute] autocommits), so there is a real window between them.  With the
   table dropped first, a crash in that window reopens with a catalog row and no
   table: [rv_load] re-registers the view, [rv_refresh_one ~resync:true] runs
   [SELECT * FROM _rv_<name>] and fails.  That failure is swallowed at open, but
   [drive_reactive] propagates it thereafter, so *every* user write to the base
   table fails — the view does not merely resurrect, it bricks the write path.

   Removing the catalog row first inverts the failure: the window leaves an
   orphan [_rv_<name>] table with no catalog row and no registry entry.  That is
   inert (nothing consults it) and droppable with a plain [DROP TABLE], since
   the internal-table guard keys off the registry, not the name.  It also
   mirrors [rv_create], which registers registry+catalog last and so fails
   toward an orphan table too. *)
let rv_erase_persistent top ~name =
  (* #476: [Cat.remove_reactive_view] reports a store fault as [Error msg]
     instead of raising, so the catalog half of the erase already speaks
     [Db.execute]'s contract and needs no exception wrapper. *)
  let* rr = Cat.remove_reactive_view top.store ~name in
  match rr with
  | Error msg -> Lwt.return (Error (Runtime msg))
  | Ok () ->
    (* #475: as in [rv_refresh_full] — the guard bypass covers this one
       statement and this one table name. *)
    rv_dropping_internal top (rv_table_name name) (fun () ->
      rv_execute
        top
        (Printf.sprintf "DROP TABLE IF EXISTS %s" (rv_quote (rv_table_name name))))
;;

(* #469 review: the registry is the authority for *live* views, but #437
   deliberately leaves a view whose stored SQL fails to re-parse OUT of the
   registry while keeping its catalog row and [_rv_] table.  Consulting only the
   registry would answer "no such reactive view" for such a view forever, so its
   catalog row could never be removed through SQL and every open would keep
   warning.  Fall back to the catalog before erroring, so [DROP REACTIVE VIEW]
   really is the single removal path. *)
let rv_drop_unloadable top ~name ~if_exists =
  (* #476: as in [rv_erase_persistent] — a store fault reading the registry is
     an [Error], not a raise. *)
  let* lr = Cat.load_all_reactive_views top.store in
  match lr with
  | Error msg -> Lwt.return (Error (Runtime msg))
  | Ok pairs ->
    if List.mem_assoc name pairs
    then rv_erase_persistent top ~name
    else if if_exists
    then Lwt.return (Ok ())
    else Lwt.return (Error (Runtime (Printf.sprintf "no such reactive view: %s" name)))
;;

(* #469: retire a reactive view — erase its persistent state via
   [rv_erase_persistent] (see there for the ordering argument), then deregister
   it.  The internal [DROP TABLE] is admitted past the internal-table guard in
   [execute_control_op] by [rv_dropping_internal] (#475), which names the one
   table being dropped rather than opening the guard for the whole operation.

   #477 review: the persistent work happens FIRST and the in-memory mutations
   (registry removal, [rv_pending] pruning) only on [Ok], so a failed drop
   cannot half-succeed.  Failure mode: on error nothing in memory changed and
   the view stays live — still maintained on base-table writes, still holding
   its callbacks, still in [reactive_view_names] — so the caller's error is the
   truth and a retry is meaningful.  The persistent state may be partially
   erased (catalog row gone, [_rv_] table left), which is exactly the state the
   crash-window reasoning above already covers: an inert orphan table.  (Since
   #476 a catalog failure returns [Error] rather than raising, so the persistent
   work needs no exception wrapper of its own.)

   That in-memory survival is only process-scoped, though: if the failure landed
   after [Cat.remove_reactive_view] succeeded, the catalog row is already gone
   while the registry entry lives on, so the view keeps consuming base-table
   deltas and writing into [_rv_<name>] for the rest of this process — and then
   vanishes at the next open, because [rv_load] has no catalog row left to
   rebuild it from.  An errored drop therefore still becomes an effective drop
   across a restart; the error means "retry now", not "nothing happened".

   #477 re-review / #476: both branches below have a single calling convention —
   they return [Error] for a store/catalog fault whether or not the view is
   live.  That used to be imposed here by an [Lwt.catch] wrapping both, because
   [Cat.remove_reactive_view] and [Cat.load_all_reactive_views] raised; #476
   made those return a [result], so the conversion happens at the source and the
   wrapper is gone.  Non-[Failure] exceptions escaping [Db.execute]'s own
   [DROP TABLE] still propagate, which is [Db.execute]'s contract everywhere
   else. *)
let rv_drop top ~name ~if_exists =
  (* #473: [Cat.remove_reactive_view] below always writes through its own
     fresh writer transaction ([borrow_or_autocommit ?txn:None] ->
     [S.rw_begin]).  Unlike the sibling table/view DDL path
     ([staged_schema_change] at ~line 1490), it does not thread
     [top.explicit_txn] through, so running it while an explicit transaction
     already holds the store's single-writer lock self-deadlocks
     unconditionally.  Threading [?txn] is the real fix but would make
     reactive-view DDL transactional, which is a deliberate design decision
     we are not overturning here (see #473) — so reject up front, before any
     state is mutated, rather than hang. *)
  if top.explicit_txn <> None
  then
    Lwt.return
      (Error
         (Runtime
            "DROP REACTIVE VIEW is not supported inside an explicit transaction (see \
             #473); run it in autocommit"))
  else (
    (* Live in the registry?  Decided once, up front: it selects the erase
       strategy and, below, whether there is any in-memory state to drop. *)
    let live = Hashtbl.mem top.reactive_views name in
    let* r =
      rv_guard top (fun () ->
        if live
        then rv_erase_persistent top ~name
        else rv_drop_unloadable top ~name ~if_exists)
    in
    match r with
    | Error _ as e -> Lwt.return e
    | Ok () when not live -> Lwt.return (Ok ())
    | Ok () ->
      Hashtbl.remove top.reactive_views name;
      (* Forget pending base-table deltas no remaining view depends on.
         Currently dead code: in autocommit [rv_flush_inner]'s finalizer empties
         [rv_pending] after every statement, so it can only ever accumulate
         inside an explicit transaction — which [rv_drop] rejects outright
         above.  Kept because it becomes live the moment #473 threads [?txn]
         through and lets this run under an explicit [BEGIN]. *)
      let stale =
        Hashtbl.fold
          (fun tbl _ acc -> if rv_is_base_table top tbl then acc else tbl :: acc)
          top.rv_pending
          []
      in
      List.iter (Hashtbl.remove top.rv_pending) stale;
      Lwt.return (Ok ()))
;;

(* Rebuild the in-memory registry on open: re-parse each stored definition and
   restore delta-engine state from the current base contents.  Then reconcile
   each [_rv_<name>] table against the freshly-computed materialisation: a crash
   between a base commit and its view flush would otherwise leave the persisted
   table stale forever (and future deltas would stack on a wrong base). *)
let rv_load top =
  (* #476: a store fault reading the registry is an [Error] rather than a raise
     at the catalog level now.  There is no result channel here — [rv_load] runs
     inside [Db.open_*] and returns [unit Lwt.t] — so re-raise, preserving the
     pre-#476 behaviour exactly.  Opening a database whose reactive-view
     registry could not be read must NOT succeed silently: the views would stop
     being maintained on every subsequent base-table write, with no error. *)
  let* lr = Cat.load_all_reactive_views top.store in
  let* pairs =
    match lr with
    | Ok pairs -> Lwt.return pairs
    | Error msg -> Lwt.fail (Failure msg)
  in
  rv_guard top (fun () ->
    let* () =
      Lwt_list.iter_s
        (fun (name, sql) ->
           match parse sql with
           | Ok (Sql.Ast.S_create_reactive_view { query; _ }) ->
             let cls = Rv.classify query in
             let out_cols =
               match Cat.find_table_cached top.catalog ~name:(rv_table_name name) with
               | Some meta -> List.map (fun (c : Row.column) -> c.name) meta.Cat.columns
               | None ->
                 (match cls.out_cols with
                  | Some c -> c
                  | None -> [])
             in
             let register mode =
               Hashtbl.replace
                 top.reactive_views
                 name
                 { rv_name = name
                 ; rv_query = query
                 ; rv_base_tables = cls.base_tables
                 ; rv_out_cols = out_cols
                 ; rv_mode = mode
                 ; rv_provisional = false
                 ; rv_callbacks = []
                 };
               Lwt.return_unit
             in
             (match cls.kind with
              | Rv.Delta { group_col; agg } ->
                (match rv_build_delta top cls.base_tables group_col agg with
                 | Error _ -> register RV_full
                 | Ok (group_ord, measure) ->
                   let base = List.hd cls.base_tables in
                   let* er = rv_rebuild_engine top ~base ~group_ord ~measure in
                   (match er with
                    | Ok engine -> register (RV_delta { group_ord; measure; engine })
                    | Error _ -> register RV_full))
              | Rv.Full -> register RV_full)
           (* #437: a view we cannot re-load leaves its [_rv_<name>] table in
              the catalog with no registry entry.  Say so — silently skipping it
              makes {!register_view_callback} report [`Unknown_view] with no clue
              why. *)
           | Ok _ ->
             Printf.eprintf
               "warning: skipping non-reactive-view SQL for reactive view '%s'\n%!"
               name;
             Lwt.return_unit
           | Error e ->
             Printf.eprintf
               "warning: failed to parse reactive view SQL for '%s': %s\n%!"
               name
               (Format.asprintf "%a" pp_error e);
             Lwt.return_unit)
        pairs
    in
    (* Reconcile each persisted materialisation against recomputed state.  For a
       cleanly-closed db this diff is empty (no writes); after a crash it
       self-heals the stale [_rv_…] rows.  Best-effort: a failure here must not
       block opening the database. *)
    Lwt_list.iter_s
      (fun (_, e) ->
         let* _ = rv_refresh_one top e ~resync:true in
         Lwt.return_unit)
      (Hashtbl.fold (fun k e acc -> (k, e) :: acc) top.reactive_views []))
;;

(* #437: accessors over the in-memory registry.  A caller wiring hooks from
   config needs to tell a live view from a typo, and the registry — not the
   [_rv_<name>] catalog naming convention — is the authority: an [_rv_] table
   can exist without a registry entry (a user table of that name, or a view
   whose stored SQL failed to re-load above). *)
let reactive_view_names top =
  Hashtbl.fold (fun name _ acc -> name :: acc) top.reactive_views []
  |> List.sort String.compare
;;

let is_reactive_view top name = Hashtbl.mem top.reactive_views name

let register_view_callback top ~view_name cb =
  match Hashtbl.find_opt top.reactive_views view_name with
  | Some e ->
    e.rv_callbacks <- e.rv_callbacks @ [ cb ];
    Ok ()
  | None -> Error (`Unknown_view view_name)
;;

let () =
  rv_create_hook := rv_create;
  rv_drop_hook := rv_drop;
  (rv_flush_hook := fun top -> rv_flush top);
  rv_load_hook := rv_load
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
