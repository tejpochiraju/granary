open Granary

type t =
  { db : Db.t
  ; path : string
  ; stmt_cache : (string, Db.stmt * int) Hashtbl.t
    (** One compiled {!Db.stmt} per distinct SQL shape (#697), paired with
        that shape's placeholder count so {!check_arity} can validate every
        call against a count computed once per shape rather than rescanning
        the SQL text on every call (#697/#698 review). The shapes are
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

(* Mirrors {!Tpc_value.literal}'s own guard: a non-finite REAL has no SQL
   literal, and {!render}'s literal-substitution path already raises on one
   via [Tpc_value.literal]. This path binds a value directly through
   {!Db.run}/{!Db.iter} instead of rendering it into SQL text, so it must
   reject nan/infinity itself or it would silently bind a value {!render}
   would have refused to produce — the same value quietly becoming a
   different, still-runnable statement that #697's arity check was restored
   to prevent for a missing/extra parameter (#697/#698 review). Unreachable
   today: every {!Tpc_value.VReal} call site in {!Tpcc_txn} is bounded finite
   arithmetic (money and quantities), so this is a safety net, not a path any
   current input exercises. *)
let to_db_value = function
  | Tpc_value.VInt i -> Db.V_int (Int64.of_int i)
  | Tpc_value.VReal f ->
    if not (Float.is_finite f)
    then
      invalid_arg
        (Printf.sprintf
           "Tpcc_conn.to_db_value: %s has no SQL literal"
           (Float.to_string f));
    Db.V_real f
  | Tpc_value.VText s -> Db.V_text s
  | Tpc_value.VNull -> Db.V_null
;;

(* [Db.prepare] compiles [sql] once (parse + plan); every later call with the
   same [sql] reuses the cached {!Db.stmt} and only pays [Db.run]/[Db.iter]'s
   per-call binding and execution cost. The placeholder count is cached
   alongside it for the same reason — computed once per shape rather than
   rescanned from the SQL text on every call (#697/#698 review).

   Cache-miss check-then-add is not atomic across the [Db.prepare] await: two
   fibers racing to prepare the same not-yet-cached shape would both prepare,
   and the second [Hashtbl.add] would shadow rather than replace the first,
   leaking it. Not reachable today — the benchmark drives a single-fiber
   one-deep worker pool (see {!Tpcc_driver}'s header) — but nothing in this
   module enforces that, so the miss branch re-checks the cache after the
   await and discards its own redundant prepare in favour of whichever fiber
   won, rather than assuming it cannot happen. *)
let get_stmt t sql =
  match Hashtbl.find_opt t.stmt_cache sql with
  | Some entry -> Lwt.return entry
  | None ->
    let open Lwt.Syntax in
    let* st = Lwt.map unwrap (Db.prepare t.db sql) in
    (match Hashtbl.find_opt t.stmt_cache sql with
     | Some entry ->
       (* Another fiber won the race while this one awaited [Db.prepare];
          finalize the redundant copy rather than shadow the winner's entry
          in the cache. *)
       let+ () = Db.finalize st in
       entry
     | None ->
       let entry = st, Tpcc_txn.count_placeholders sql in
       Hashtbl.add t.stmt_cache sql entry;
       Lwt.return entry)
;;

(* [Tpcc_txn.render]'s arity check never runs on this path — [Db.run]/[Db.iter]
   bind params positionally against [Plan.P_param] with no arity check of their
   own, so a params list shorter than the SQL's placeholder count would
   silently bind NULL for the missing tail, and a longer one would be silently
   truncated. Restoring the check here keeps a SQL/params drift a loud
   [invalid_arg] instead of quietly corrupted benchmark data (#697 review).
   Shares its comparison and message with {!Tpcc_txn.render} via
   {!Tpcc_txn.check_arity}, differing only in the error-message prefix
   (#697/#698 review, finding #3). *)
let check_arity ~n_placeholders (s : Tpcc_txn.stmt) =
  Tpcc_txn.check_arity
    ~prefix:"Tpcc_conn"
    ~n_placeholders
    ~n_params:(List.length s.Tpcc_txn.params)
;;

let prepared_exec_lwt t (s : Tpcc_txn.stmt) =
  let open Lwt.Syntax in
  let* stmt, n_placeholders = get_stmt t s.Tpcc_txn.sql in
  check_arity ~n_placeholders s;
  let params = List.map to_db_value s.Tpcc_txn.params in
  Lwt.map (fun r -> ignore (unwrap r)) (Db.run stmt ~params)
;;

let prepared_query_rows_lwt t (s : Tpcc_txn.stmt) =
  let open Lwt.Syntax in
  let* stmt, n_placeholders = get_stmt t s.Tpcc_txn.sql in
  check_arity ~n_placeholders s;
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

(* ── worker handles (#703) ────────────────────────────────────────────────
   [Db.create_worker_handle] gives a fresh [Db.t] its own [explicit_txn] slot
   over the SAME [Store.t] and [Rwlock] as [t.db] — see this function's [.mli]
   doc and CLAUDE.md's "Running explicit transactions from more than one
   fiber" section. A fresh, empty [stmt_cache] is required, not merely
   harmless: a [Db.stmt] returned by [Db.prepare] is compiled against the
   handle it was prepared on, and a worker handle gets its own catalog, so
   reusing [t]'s cache would hand a new connection a statement compiled
   somewhere else. *)
let worker_handle t =
  let open Lwt.Syntax in
  let+ db = Db.create_worker_handle t.db in
  { db; path = t.path; stmt_cache = Hashtbl.create 32 }
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
  (* [Db.finalize] is a documented no-op in this implementation ("should be
     called for forward compatibility"), so a plain iteration is all the
     effect needs — no fold-into-a-list-then-[Lwt.join] to run them
     concurrently (#697/#698 review, finding #5). *)
  Hashtbl.iter (fun _ (st, _) -> run (Db.finalize st)) t.stmt_cache;
  Hashtbl.reset t.stmt_cache;
  run (Db.close t.db)
;;
