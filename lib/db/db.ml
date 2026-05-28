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

type error =
  | Parse of string
  | Sema of Sql.Sema.error
  | Runtime of string

(* File operations the SQL engine needs for ATTACH and VACUUM, injected by a
   platform driver (e.g. [sqlocaml.unix]) through [set_file_provider].  The
   core itself carries no OS/filesystem dependency (#170); when no provider is
   installed, ATTACH and VACUUM fail with a clear error. *)
type file_provider =
  { open_store : ?geom:S.Geometry.t -> path:string -> unit -> (S.t, S.error) result Lwt.t
  ; remove_file : string -> unit
  ; rename_file : string -> string -> unit
  }

let file_provider_ref : file_provider option ref = ref None
let set_file_provider p = file_provider_ref := Some p

let open_in_memory ?clock () =
  let store = S.create () in
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
   (Some for file-backed handles, None for in-memory / arbitrary devices). *)
let of_store ?clock ?file_path store =
  let* catalog = Cat.open_ store in
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

let open_block ?geom ~read_page ~write_page ~sync ~resize ~n_pages ~close ()
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
    let* db = of_store store in
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

let wal_sync_count t = S.wal_sync_count t.store

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
    t.explicit_txn <- Some tx;
    Lwt.return (Ok ())
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
      then (
        (* Rollback the underlying txn so the caller gets a clean state. *)
        let* () = S.rollback tx in
        t.explicit_txn <- None;
        t.savepoint_names <- [];
        t.auto_began <- false;
        Cat.set_defer_fks_pragma t.catalog false;
        Lwt.fail_with check.Cat.pfk_message)
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
  | Some tx ->
    (* Drain deferred FK checks first; if any still violate, this raises
       and the txn has already been rolled back. *)
    Lwt.catch
      (fun () ->
         let* () = drain_pending_fks_or_fail t tx in
         let* () = S.commit tx in
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
    let* () = S.rollback tx in
    t.explicit_txn <- None;
    t.savepoint_names <- [];
    t.auto_began <- false;
    Cat.clear_pending_fk_checks t.catalog;
    Cat.set_defer_fks_pragma t.catalog false;
    Lwt.return (Ok ())
;;

let savepoint_txn t name =
  let* tx =
    match t.explicit_txn with
    | Some tx -> Lwt.return tx
    | None ->
      let* tx = S.rw_begin t.store in
      t.explicit_txn <- Some tx;
      t.auto_began <- true;
      Lwt.return tx
  in
  let* () = S.savepoint_begin tx name in
  t.savepoint_names <- name :: t.savepoint_names;
  Lwt.return (Ok ())
;;

let release_savepoint t name =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction for RELEASE"))
  | Some tx ->
    let* () = S.savepoint_release tx name in
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
      let* () = S.commit tx in
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
                    (* WHEN clause query runs inside the parent txn so it sees the
                in-flight writes (matches SQLite semantics for AFTER WHEN). *)
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
          matching)

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
           let* result = prov.open_store ~path () in
           (match result with
            | Error e -> Lwt.return (Error (Runtime (Format.asprintf "%a" S.pp_error e)))
            | Ok store ->
              let* sub_db = of_store ~file_path:path store in
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
    (* DDL is not transactional — persist_view commits immediately regardless of any open explicit txn *)
    Some
      (Hashtbl.replace t.views name query;
       let* () = Cat.persist_view t.store ~name ~sql in
       Lwt.return (Ok ()))
  | Sql.Plan.Op_drop_view { name } ->
    (* DDL is not transactional — persist_view commits immediately regardless of any open explicit txn *)
    Some
      (Hashtbl.remove t.views name;
       let* () = Cat.remove_view t.store ~name in
       Lwt.return (Ok ()))
  | Sql.Plan.Op_create_trigger { name; timing; event; table; when_; body } ->
    Some
      (let ast = Sql.Ast.S_create_trigger { name; timing; event; table; when_; body } in
       Hashtbl.replace t.triggers name ast;
       let* () = Cat.persist_trigger t.store ~name ~sql in
       Lwt.return (Ok ()))
  | Sql.Plan.Op_drop_trigger { name } ->
    Some
      (Hashtbl.remove t.triggers name;
       let* () = Cat.remove_trigger t.store ~name in
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
             | Some m -> t.last_insert_rowid <- Int64.sub m.Cat.next_rowid 1L
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

let query top sql =
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
      match t.explicit_txn with
      | None -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    (match Sql.Exec.query ~mode ~clock:t.clock t.store t.catalog op with
     | exception Failure msg -> Lwt.return (Error (Runtime msg))
     | lwt_stream ->
       let* stream = lwt_stream in
       Lwt.return (Ok stream))
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
             | Some m -> t.last_insert_rowid <- Int64.sub m.Cat.next_rowid 1L
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

let iter st ~params =
  if st.finalized
  then Lwt.return (Error (Runtime "statement already finalized"))
  else (
    let params_arr = Array.of_list params in
    let t = st.db_ref in
    Lwt.catch
      (fun () ->
         let* stream =
           Sql.Exec.query ~clock:t.clock ~params:params_arr t.store t.catalog st.plan
         in
         Lwt.return (Ok stream))
      (function
        | Failure msg -> Lwt.return (Error (Runtime msg))
        | exn -> Lwt.fail exn))
;;

let finalize st =
  st.finalized <- true;
  Lwt.return_unit
;;

let pp_error fmt = function
  | Parse msg -> Format.fprintf fmt "parse error: %s" msg
  | Sema e -> Format.fprintf fmt "sema error: %a" Sql.Sema.pp_error e
  | Runtime m -> Format.fprintf fmt "runtime error: %s" m
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
