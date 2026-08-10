open Granary

type t =
  { db : Db.t
  ; path : string
  ; stmt_cache : (string, Db.stmt) Hashtbl.t
    (** One compiled {!Db.stmt} per distinct SQL shape (#697). The shapes are
        static — see {!Tpcc_txn}'s [stmt]/[*_insert_sql] helpers — so caching
        for the lifetime of the connection amortizes the parse+plan cost
        across every call site that reruns the same shape with different
        bound params. Keyed by the raw, unrendered [sql] string, which is the
        stable shape identifier; [BEGIN]/[COMMIT]/[ROLLBACK] are deliberately
        never entered here — see {!is_control_stmt}. *)
  }

let name = "granary"
let pp fmt t = Format.fprintf fmt "tpcc_conn(%s)" t.path

let unwrap = function
  | Ok v -> v
  | Error e -> failwith (Format.asprintf "granary: %a" Db.pp_error e)
;;

(* ── Lwt-native operations ────────────────────────────────────────────── *)

let exec_lwt t sql = Lwt.map (fun r -> ignore (unwrap r)) (Db.execute t.db sql)

let render = function
  | Db.V_int i -> Int64.to_string i
  | Db.V_text s -> s
  | Db.V_null -> "NULL"
  | Db.V_real f -> Printf.sprintf "%.17g" f
  | Db.V_blob _ -> "<blob>"
;;

let query_rows_lwt t sql =
  let open Lwt.Syntax in
  let* stream = Lwt.map unwrap (Db.query t.db sql) in
  let+ rows = Lwt_stream.to_list stream in
  List.map (fun r -> Array.to_list (Array.map render r)) rows
;;

(* ── prepared statements (#697) ───────────────────────────────────────────

   [BEGIN]/[COMMIT]/[ROLLBACK] cannot be prepared: {!Sql.Exec.execute_with_count}
   — the function [Db.run] drives a prepared statement through — refuses all
   of [Op_begin]/[Op_commit]/[Op_rollback]/[Op_savepoint]/[Op_rollback_to]
   outright ("handled by Db layer"), because their transaction-slot and
   poison-flag bookkeeping lives in {!Db.execute}'s control-op path, which a
   prepared [run] bypasses entirely. {!Tpcc_txn.begin_txn}, [commit_txn] and
   [rollback_txn] are the only three statements this harness ever issues with
   no placeholders and no params, so they are recognised by SQL text and kept
   on the one-shot [Db.execute] path used before this change; every other
   statement is a DML/SELECT shape and goes through the cache below. *)
let is_control_stmt (s : Tpcc_txn.stmt) =
  match s.Tpcc_txn.params with
  | [] ->
    String.equal s.Tpcc_txn.sql "BEGIN"
    || String.equal s.Tpcc_txn.sql "COMMIT"
    || String.equal s.Tpcc_txn.sql "ROLLBACK"
  | _ :: _ -> false
;;

let to_db_value = function
  | Tpc_value.VInt i -> Db.V_int (Int64.of_int i)
  | Tpc_value.VReal f -> Db.V_real f
  | Tpc_value.VText s -> Db.V_text s
  | Tpc_value.VNull -> Db.V_null
;;

(* [Db.prepare] compiles [sql] once (parse + plan); every later call with the
   same [sql] reuses the cached {!Db.stmt} and only pays [Db.run]/[Db.iter]'s
   per-call binding and execution cost. *)
let get_stmt t sql =
  match Hashtbl.find_opt t.stmt_cache sql with
  | Some st -> Lwt.return st
  | None ->
    let open Lwt.Syntax in
    let+ st = Lwt.map unwrap (Db.prepare t.db sql) in
    Hashtbl.add t.stmt_cache sql st;
    st
;;

(* [Tpcc_txn.render]'s arity check never runs on this path — [Db.run]/[Db.iter]
   bind params positionally against [Plan.P_param] with no arity check of their
   own, so a params list shorter than the SQL's placeholder count would
   silently bind NULL for the missing tail, and a longer one would be silently
   truncated. Restoring the check here keeps a SQL/params drift a loud
   [invalid_arg] instead of quietly corrupted benchmark data (#697 review). *)
let check_arity (s : Tpcc_txn.stmt) =
  let n_placeholders = Tpcc_txn.count_placeholders s.Tpcc_txn.sql in
  let n_params = List.length s.Tpcc_txn.params in
  if n_placeholders <> n_params
  then
    invalid_arg
      (Printf.sprintf
         "Tpcc_conn: %d placeholders but %d parameter(s)"
         n_placeholders
         n_params)
;;

let prepared_exec_lwt t (s : Tpcc_txn.stmt) =
  let open Lwt.Syntax in
  check_arity s;
  let* stmt = get_stmt t s.Tpcc_txn.sql in
  let params = List.map to_db_value s.Tpcc_txn.params in
  Lwt.map (fun r -> ignore (unwrap r)) (Db.run stmt ~params)
;;

let prepared_query_rows_lwt t (s : Tpcc_txn.stmt) =
  let open Lwt.Syntax in
  check_arity s;
  let* stmt = get_stmt t s.Tpcc_txn.sql in
  let params = List.map to_db_value s.Tpcc_txn.params in
  let* stream = Lwt.map unwrap (Db.iter stmt ~params) in
  let+ rows = Lwt_stream.to_list stream in
  List.map (fun r -> Array.to_list (Array.map render r)) rows
;;

(* The profiles hold parameters apart from the SQL, in {!Tpcc_txn.stmt}; this
   binds them directly through {!Db.run}/{!Db.iter} against a cached
   {!Db.stmt}, rather than substituting them into literal SQL text and paying
   full parse+plan on every call (#697). *)
let ops t =
  { Tpcc_txn.query =
      (fun s ->
        if is_control_stmt s
        then query_rows_lwt t (Tpcc_txn.render s)
        else prepared_query_rows_lwt t s)
  ; Tpcc_txn.exec =
      (fun s ->
        if is_control_stmt s
        then exec_lwt t (Tpcc_txn.render s)
        else prepared_exec_lwt t s)
  }
;;

(* ── synchronous ENGINE view, for the load phase only ─────────────────── *)

let run = Lwt_main.run

let open_db ~dir =
  let path = Filename.concat dir "tpcc.db" in
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal" ];
  { db = unwrap (run (Granary_unix.open_file_wal ~clock:Unix.gettimeofday ~path ()))
  ; path
  ; stmt_cache = Hashtbl.create 32
  }
;;

let exec t sql = run (exec_lwt t sql)
let query_rows t sql = run (query_rows_lwt t sql)

let close t =
  let finalizes = Hashtbl.fold (fun _ st acc -> Db.finalize st :: acc) t.stmt_cache [] in
  run (Lwt.join finalizes);
  Hashtbl.reset t.stmt_cache;
  run (Db.close t.db)
;;
