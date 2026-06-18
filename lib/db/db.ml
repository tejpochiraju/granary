open Lwt.Syntax
module S = Sqlocaml_store.Store
module Cat = Sqlocaml_catalog.Catalog
module Sql = Sqlocaml_sql
module Row = Sqlocaml_encoding.Row

type t =
  { mutable store : S.t
  ; mutable catalog : Cat.t
  ; clock : (unit -> float) option
  ; mutable explicit_txn : S.rw S.txn option
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
        [sqlocaml.unix] driver or ATTACH).  VACUUM needs this to rebuild the
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
  }

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
  ; mutable used_index : bool
  }

(* #240: user tables whose rows a write statement actually mutated, including
   tables touched indirectly by triggers and FK cascades.  Sorted, deduplicated;
   internal/system tables excluded. *)
type dirty_tables = string list

type error =
  | Parse of string
  | Sema of Sql.Sema.error
  | Runtime of string
  | History_unavailable (* #266: as-of query on a db opened without history *)
  | History_pruned (* #266: as-of target predates the retained floor *)

(* File operations the SQL engine needs for ATTACH and VACUUM, injected by a
   platform driver (e.g. [sqlocaml.unix]) through [set_file_provider].  The
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
    ; clock
    ; explicit_txn = None
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
let of_store ?clock ?durability ?file_path store =
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
  Lwt.return
    { store
    ; catalog
    ; clock
    ; explicit_txn = None
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
    }
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
  S.close t.store
;;

let create_worker_handle t =
  let* () = Lwt.return_unit in
  of_store t.store
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
  match t.file_path with
  | None -> Lwt.fail_with "VACUUM: only supported on file-backed databases"
  | Some path ->
    if t.explicit_txn <> None
    then Lwt.fail_with "VACUUM cannot run inside an explicit transaction"
    else (
      match !file_provider_ref with
      | None ->
        Lwt.fail_with
          "VACUUM requires a file provider; link sqlocaml.unix and call \
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
           let* () = S.close t.store in
           (* Best-effort cleanup of WAL sidecar — its contents are now stale. *)
           prov.remove_file (path ^ "-wal");
           prov.rename_file tmp_path path;
           let* new_store_r = prov.open_store ~path () in
           (match new_store_r with
            | Error e ->
              let msg = Format.asprintf "VACUUM reopen: %a" S.pp_error e in
              Lwt.fail_with msg
            | Ok new_store ->
              let* new_catalog = Cat.open_ new_store in
              (* Load persisted columnar data into the fresh catalog. *)
              let* () = Cat.load_columnar_stores new_catalog new_store in
              t.store <- new_store;
              t.catalog <- new_catalog;
              Hashtbl.clear t.views;
              let* () = load_views_into_hashtbl new_store t.views in
              Hashtbl.clear t.triggers;
              let* () = load_triggers_into_hashtbl new_store t.triggers in
              Lwt.return_unit)))
;;

let parse sql =
  match
    let lexbuf = Lexing.from_string sql in
    Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
  with
  | stmt -> Ok stmt
  | exception Sql.Parser.Error -> Error (Parse "syntax error")
  | exception Failure msg -> Error (Parse msg)
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
    downstream execution / state mutation. *)
let compile_routed (top : t) (sql : string) : (Sql.Plan.op, error) result Lwt.t * t =
  match parse sql with
  | Error e -> Lwt.return (Error e), top
  | Ok ast ->
    let target = resolve_target_ast top ast in
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

let begin_txn t =
  match t.explicit_txn with
  | Some _ -> Lwt.return (Error (Runtime "transaction already active"))
  | None ->
    let* tx = S.rw_begin t.store in
    (* #269: defensive — start with an empty schema-undo log so a stale entry
       from a prior op can never leak into this transaction's rollback. (It is
       already cleared by every commit/rollback path; this just hardens it.) *)
    Cat.commit_schema_changes t.catalog;
    t.explicit_txn <- Some tx;
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
     after [S.rollback] released the RW lock so the recompute's RO txn is safe. *)
  let* () = Cat.recompute_rowid_counters_after_rollback t.catalog in
  (* Reload all columnar stores from the rolled-back B-tree.  The RO snapshot
     opened by [load_columnar_stores] sees the last committed state, which is
     correct after a full rollback. *)
  let* () = Cat.load_columnar_stores t.catalog t.store in
  t.explicit_txn <- None;
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
         List.iter Sqlocaml_columnar.Col_store.mark_clean saved;
         (* #269: in-txn DDL's cache changes are now durable — drop the undo log. *)
         Cat.commit_schema_changes t.catalog;
         t.explicit_txn <- None;
         t.savepoint_names <- [];
         t.auto_began <- false;
         Cat.set_defer_fks_pragma t.catalog false;
         Lwt.return (Ok ()))
      (function
        | Failure msg -> Lwt.return (Error (Runtime msg))
        | exn -> Lwt.fail exn)
;;

let rollback_txn t =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction"))
  | Some tx ->
    (* #269: [force_rollback_txn] reverts any in-txn DDL's in-memory cache
       changes (schema-undo log) along with the store. *)
    let* () = force_rollback_txn t tx in
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
        List.iter Sqlocaml_columnar.Col_store.mark_clean saved;
        (* #269: finalize any in-txn DDL's cache changes on this auto-commit. *)
        Cat.commit_schema_changes t.catalog;
        t.explicit_txn <- None;
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

(* Handle non-DML control / DDL ops (txn control, ATTACH/DETACH, schema
   switch, CREATE/DROP VIEW/TRIGGER, VACUUM).  Returns [Some result] for ops
   it owns and [None] for DML / catch-all ops the caller routes to
   [execute_dml_op].  Shared by [execute] and [execute_change_count]; the
   latter maps the [unit] result to a [0] change count. *)
let execute_control_op top t sql op =
  match op with
  | Sql.Plan.Op_begin -> Some (begin_txn t)
  | Sql.Plan.Op_commit -> Some (commit_txn t)
  | Sql.Plan.Op_rollback -> Some (rollback_txn t)
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
                   "ATTACH requires a file provider; link sqlocaml.unix and call \
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

let execute top sql =
  let op_promise, t = compile_routed top sql in
  let* op = op_promise in
  match op with
  | Error (Sema (Sql.Sema.Unknown_table view_name)) when Hashtbl.mem t.views view_name ->
    (match parse sql with
     | Error _ -> Lwt.return (Error (Parse "syntax error"))
     | Ok ast -> execute_instead_of t view_name ast)
  | Error e -> Lwt.return (Error e)
  | Ok op ->
    (match execute_control_op top t sql op with
     | Some result -> result
     | None -> execute_dml_op t op)
;;

let execute_change_count top sql =
  let op_promise, t = compile_routed top sql in
  let* op = op_promise in
  let count_of_unit = function
    | Ok () -> Lwt.return (Ok 0)
    | Error e -> Lwt.return (Error e)
  in
  match op with
  | Error (Sema (Sql.Sema.Unknown_table view_name)) when Hashtbl.mem t.views view_name ->
    (match parse sql with
     | Error _ -> Lwt.return (Error (Parse "syntax error"))
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

let execute_with_dirty top sql =
  let acc = Sql.Exec.make_dirty_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> execute top sql) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok () -> Lwt.return (Ok (Sql.Exec.dirty_elements acc))
;;

let execute_change_count_with_dirty top sql =
  let acc = Sql.Exec.make_dirty_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> execute_change_count top sql) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok n -> Lwt.return (Ok (n, Sql.Exec.dirty_elements acc))
;;

(* #259: single source of truth for the control-op dispatch shared by [query]
   and [query_with_stats].  The canned control ops (changes / last_insert_rowid
   / database_list / ...) do no scan; only the real-op branch touches [stats],
   forwarding it to [Sql.Exec.query].  When [stats] is omitted the call is
   [Sql.Exec.query] with no [~stats] — byte-for-byte the pre-#239 read path
   (no [with_value], no [Lwt_stream.map] wrapper), so existing callers pay
   nothing. *)
let query_impl ?stats ?mode top sql =
  let op_promise, t = compile_routed top sql in
  let* op = op_promise in
  match op with
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
    (match Sql.Exec.query ~mode ~clock:t.clock ?stats t.store t.catalog op with
     | exception Failure msg -> Lwt.return (Error (Runtime msg))
     | lwt_stream ->
       let* stream = lwt_stream in
       Lwt.return (Ok stream))
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
let history_log ?(schema = "main") t = S.history_log (store_for_schema t schema)

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
let query_as_of top (target : Sqlocaml_store.History.target) sql =
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
       match op with
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
              Lwt.fail exn))
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

let prepare top sql =
  match parse sql with
  | Error e -> Lwt.return (Error e)
  | Ok ast ->
    let t = resolve_target_ast top ast in
    let* bound = Sql.Sema.bind_returning_params ~views:t.views t.catalog ast in
    (match bound with
     | Error e -> Lwt.return (Error (Sema e))
     | Ok (b, names) ->
       let plan = Sql.Planner.plan ~cat:t.catalog b in
       Lwt.return (Ok { db_ref = t; plan; param_names = names; finalized = false }))
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

let run st ~params =
  if st.finalized
  then Lwt.return (Error (Runtime "statement already finalized"))
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

let run_with_dirty st ~params =
  let acc = Sql.Exec.make_dirty_acc () in
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
   recreates: a single-column auto-created PRIMARY KEY index whose column carries
   a column-level PRIMARY KEY (our executor rebuilds it from that clause).  Every
   other index — user indexes, UNIQUE-constraint indexes, and multi-column PK
   indexes backing composite/table-level keys — encodes a constraint the table
   DDL omits and so must be emitted, or restoring would silently drop it.

   A multi-column (composite / table-level) PRIMARY KEY therefore round-trips as
   a plain [CREATE UNIQUE INDEX]: faithful to this engine's internal
   representation (so the data round-trips), but the restored schema reports no
   PRIMARY KEY and permits NULLs — an intentional downgrade.

   The check keys on [idx_origin] (#273), set when the index is created, not on
   the [__pk_] name prefix [sema.ml] assigns: a user index deliberately named
   [__pk_*] on a PK column is [`User] origin and so is correctly still emitted. *)
let dump_index_is_implied (meta : Cat.table_meta) (idx : Cat.index_info) =
  match idx.Cat.idx_origin with
  | `Implicit_unique | `User -> false
  | `Implicit_pk ->
    (match idx.Cat.idx_columns with
     | [ col ] ->
       List.exists
         (fun (c : Row.column) -> c.Row.name = col && c.Row.primary_key)
         meta.Cat.columns
     | _ -> false)
;;

(* Emit [INSERT] statements for every row of table/FTS-table [name] by selecting
   [col_idents] (already quoted, in storage order) through the executor — so the
   rowid-alias column resolves to its stored value and FTS content reads through
   the content tree.  [explicit_cols] names the columns in the [INSERT] (needed
   when the selected set omits generated columns); otherwise a bare
   [INSERT INTO t VALUES] is emitted.  [?mode] threads the dump's shared RO
   snapshot so reads are point-in-time. *)
let emit_rows_as_inserts ?mode t ~name ~col_idents ~explicit_cols ~stmt =
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
let dump_table_rows ?mode t (meta : Cat.table_meta) ~stmt =
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
    emit_rows_as_inserts
      ?mode
      t
      ~name:meta.Cat.name
      ~col_idents
      ~explicit_cols:has_generated
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
                     else stmt (Sql.Exec.ddl_of_table meta)
                   in
                   if schema_only
                   then Lwt.return_unit
                   else dump_table_rows ?mode:read_mode t meta ~stmt)
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
                       Lwt_list.iter_s
                         (fun idx ->
                            if dump_index_is_implied meta idx
                            then Lwt.return_unit
                            else stmt (Sql.Exec.ddl_of_index idx))
                         (Cat.indexes_for_table cat ~table:meta.Cat.name))
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

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
