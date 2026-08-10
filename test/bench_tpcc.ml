(* #500 — TPC-C-derived OLTP benchmark runner.

   Loads a W-warehouse TPC-C population into granary and into reference C
   SQLite, runs the same zero-think-time saturation driver against each, and
   writes one CSV row per (engine, profile) to stdout with a human summary on
   stderr.  Throughput is reported, never gated: this is a measurement tool.
   A *consistency violation*, by contrast, is a real failure and sets a
   non-zero exit code — as is a transaction that exhausted its retries.

   The headline number is NewOrder transactions per second.  It is NOT tpmC:
   the SQL is rewritten for granary's dialect, there are no keying or think
   times, and there is no audit or pricing disclosure.

   The two engines are driven through pools of DIFFERENT depths, deliberately.

   Granary gets one {!Tpcc_conn.t} per terminal (#703): the load phase runs on
   a single connection, then one {!Tpcc_conn.worker_handle} is minted per
   remaining terminal, all sharing one on-disk {!Store.t} and its
   single-writer [Rwlock] (see CLAUDE.md's "Running explicit transactions from
   more than one fiber", #555/#589/#632/#633). That removes the pool itself as
   a source of queueing — {!Tpcc_driver}'s pool now has exactly as many workers
   as there are terminals — so what remains in wait(ms) is real writer-lock
   contention, not an artifact of sharing a single [Db.t].

   Reference SQLite stays a ONE-worker pool: these bindings are blocking calls
   from a single-domain Lwt program, so a second handle could not overlap
   anything either, and there is no equivalent of [create_worker_handle] to
   reach for. Giving sqlite a deeper pool than its bindings can actually use
   would not make it faster, only add unused connections, so the comparison
   stays a comparison of what each engine's terminal count can do to
   throughput — not a comparison of pool depths, which was the failure mode
   the old one-worker-both-engines setup avoided by being uniformly flat.

   Not audited TPC results — see docs/benchmarks/BENCHMARKS-TPCC.md. *)

module BR = Granary_tpc.Bench_report
module D = Granary_tpc.Tpcc_driver
module T = Granary_tpc.Tpcc_txn
module C = Granary_tpc.Tpcc_check
module G = Granary_tpc.Tpcc_gen
module Conn = Granary_tpc.Tpcc_conn

(* ── reference C SQLite (in-process bindings) ─────────────────────────────
   PRAGMA parity with test/bench_tpch.ml's Ref_sqlite: same page size, same
   WAL journal mode, same fsync-per-commit durability as granary. *)

module Ref_sqlite = struct
  type t = { db : Sqlite3.db }

  let name = "sqlite"

  (* #571 — the crash this exists to prevent.

     The bindings' statement finalizer and database closer do not register
     their argument as a local root, and they read the wrapper struct after
     releasing the runtime system.  Finalizing a statement is the last thing
     anyone does with it, so during that window the custom block is
     unreachable from every OCaml root: the GC may collect it, run its own
     finaliser — which finalizes the statement and frees the wrapper — and
     leave the stub reading freed memory and passing the garbage to C SQLite.
     The process takes SIGSEGV with no exception and no message (exit 139),
     after the pre-run consistency check and before any output, which reads as
     a hung benchmark rather than a crash.

     Passing the value to a function the optimizer may not see through, after
     the call, keeps it live in this frame across the window.  That is the
     entire fix.  Measured at 4 terminals x 40 s: 5/6 runs crashed without
     it, 0/6 with it, and 0/6 with a locally patched binding that adds the
     missing root registration.  Tpc_keepalive_lint keeps the discipline from
     decaying; docs/benchmarks/BENCHMARKS-TPCC.md records the whole chain. *)
  let[@inline never] keep_alive x = ignore (Sys.opaque_identity x)

  let finalize_stmt stmt =
    let rc = Sqlite3.finalize stmt in
    keep_alive stmt;
    rc
  ;;

  let close_db db =
    let closed = Sqlite3.db_close db in
    keep_alive db;
    closed
  ;;

  let ok rc =
    match rc with
    | Sqlite3.Rc.OK | Sqlite3.Rc.DONE | Sqlite3.Rc.ROW -> ()
    | r -> failwith ("sqlite3: " ^ Sqlite3.Rc.to_string r)
  ;;

  let exec t sql = ok (Sqlite3.exec t.db sql)

  let open_db ~dir =
    let path = Filename.concat dir "ref-tpcc.db" in
    List.iter
      (fun p ->
         try Unix.unlink p with
         | _ -> ())
      [ path; path ^ "-wal"; path ^ "-shm" ];
    let t = { db = Sqlite3.db_open path } in
    exec t "PRAGMA page_size=4096";
    exec t "PRAGMA journal_mode=WAL";
    exec t "PRAGMA synchronous=FULL";
    t
  ;;

  (* Rendered exactly as Tpcc_conn renders, so the two engines' rows are
     comparable as text. *)
  let render = function
    | Sqlite3.Data.INT i -> Int64.to_string i
    | Sqlite3.Data.FLOAT f -> Printf.sprintf "%.17g" f
    | Sqlite3.Data.TEXT s -> s
    | Sqlite3.Data.BLOB _ -> "<blob>"
    | Sqlite3.Data.NULL | Sqlite3.Data.NONE -> "NULL"
  ;;

  let query_rows t sql =
    let stmt = Sqlite3.prepare t.db sql in
    let acc = ref [] in
    let row () =
      List.init (Sqlite3.data_count stmt) (fun i -> render (Sqlite3.column stmt i))
    in
    let rec loop () =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
        acc := row () :: !acc;
        loop ()
      | Sqlite3.Rc.DONE -> ()
      | r -> failwith ("sqlite3 step: " ^ Sqlite3.Rc.to_string r)
    in
    loop ();
    ok (finalize_stmt stmt);
    List.rev !acc
  ;;

  let close t = if not (close_db t.db) then failwith "sqlite3: db_close failed"

  (* Blocking calls wrapped in resolved promises.  Safe inside the driver
     precisely because they contain no Lwt_main.run — the reason
     Granary_engine could not be reused for granary's side. *)
  let ops t =
    { T.query = (fun s -> Lwt.return (query_rows t (T.render s)))
    ; T.exec =
        (fun s ->
          exec t (T.render s);
          Lwt.return_unit)
    }
  ;;
end

(* [with type t] so the load functor and the async [ops] above can share one
   open handle: an opaque [t] would force a second connection for the load. *)
module Ref_engine : BR.ENGINE with type t = Ref_sqlite.t = Ref_sqlite

(* ── consistency oracle ───────────────────────────────────────────────────
   Run before and after every measured interval.  A driver run that leaves
   the four clause-3.3 conditions violated is a failure, not a slow number:
   throughput obtained by corrupting the database is not throughput. *)

type check_report =
  { violations : int
  ; lines : string list
  }

let check_conditions query_rows ~engine ~where =
  let one (c : C.condition) =
    let outcome =
      match List.map query_rows c.C.queries with
      | rows -> C.classify c ~rows
      | exception exn ->
        (* A condition whose queries would not run must not read as a pass
           (#502): [Not_run] is a failure outcome of its own. *)
        Printf.eprintf
          "[%s] %s: condition %d could not run: %s\n%!"
          engine
          where
          c.C.number
          (Printexc.to_string exn);
        C.Not_run
    in
    ( C.is_failure outcome
    , Printf.sprintf
        "[%s] %s: condition %d (%s): %s%s"
        engine
        where
        c.C.number
        c.C.description
        (C.label outcome)
        (match outcome with
         | C.Violated report -> " — " ^ report
         | C.Holds | C.Not_run -> "") )
  in
  let results = List.map one C.conditions in
  { violations = List.length (List.filter fst results); lines = List.map snd results }
;;

let print_check r =
  List.iter (fun l -> Printf.eprintf "%s\n%!" l) r.lines;
  r.violations
;;

(* The oracle's own regression guard, run in-harness rather than trusted:
   perturb one accumulator and require condition 1 to notice.  An oracle that
   passes here after the perturbation is a vacuous oracle, and everything it
   said about the real run is worthless — so THAT is what exits non-zero. *)
let prove_the_oracle_can_fail exec query_rows ~engine =
  Printf.eprintf "[%s] oracle self-test: corrupting w_ytd on warehouse 1…\n%!" engine;
  exec "UPDATE warehouse SET w_ytd = w_ytd + 1000.0 WHERE w_id = 1";
  let r = check_conditions query_rows ~engine ~where:"corrupted" in
  ignore (print_check r);
  exec "UPDATE warehouse SET w_ytd = w_ytd - 1000.0 WHERE w_id = 1";
  if r.violations = 0
  then (
    Printf.eprintf
      "[%s] ORACLE SELF-TEST FAILED: a deliberately corrupted database still passed\n%!"
      engine;
    1)
  else (
    Printf.eprintf
      "[%s] oracle self-test passed: %d condition(s) reported the corruption\n%!"
      engine
      r.violations;
    0)
;;

(* ── one engine's run ─────────────────────────────────────────────────── *)

type run_outcome =
  { result : D.result
  ; failures : int
  }

let load_failures r =
  List.fold_left (fun acc (s : D.profile_stats) -> acc + s.D.failed) 0 r.D.per_profile
;;

(* [mk_workers] is a thunk, not a plain list, so it can be evaluated AFTER
   [load] completes — a worker handle minted before the load phase's DDL runs
   would carry a catalog that cannot see the freshly created tables (see
   {!Tpcc_conn.worker_handle}'s doc). *)
let run_engine ~engine ~config ~gen ~load ~mk_workers ~exec ~query_rows ~self_test =
  Printf.eprintf "[%s] loading W=%d…%!" engine (G.warehouses gen);
  let (), load_wall, _ = BR.time_it load in
  Printf.eprintf " %.2fs\n%!" load_wall;
  let workers = mk_workers () in
  let before = print_check (check_conditions query_rows ~engine ~where:"before") in
  let result = Lwt_main.run (D.run config ~workers:(List.map T.run workers)) in
  let after = print_check (check_conditions query_rows ~engine ~where:"after") in
  let self = if self_test then prove_the_oracle_can_fail exec query_rows ~engine else 0 in
  prerr_string (D.summary result);
  flush stderr;
  { result; failures = before + after + self + load_failures result }
;;

(* One worker-handle connection per terminal (#703): [c] itself serves the
   first terminal (and is what pre/post consistency checks and the oracle
   self-test run against — all of them run outside the timed interval, so
   there is no overlap with a terminal using it), and [config.terminals - 1]
   further connections are minted via [Conn.worker_handle c], each sharing
   [c]'s store. Only [c] is ever closed — see [Conn.worker_handle]'s doc on
   why closing a sibling would tear down every other handle's store. *)
let run_granary ~dir ~config ~gen ~self_test =
  let module Load = Granary_tpc.Tpcc_schema.Load (Conn) in
  let c = Conn.open_db ~dir in
  let out =
    run_engine
      ~engine:Conn.name
      ~config
      ~gen
      ~load:(fun () -> Load.run c gen)
      ~mk_workers:(fun () ->
        c
        :: List.init (config.D.terminals - 1) (fun _ ->
          Lwt_main.run (Conn.worker_handle c))
        |> List.map Conn.ops)
      ~exec:(Conn.exec c)
      ~query_rows:(Conn.query_rows c)
      ~self_test
  in
  Conn.close c;
  out
;;

let run_sqlite ~dir ~config ~gen ~self_test =
  let module Load = Granary_tpc.Tpcc_schema.Load (Ref_engine) in
  let s = Ref_sqlite.open_db ~dir in
  let out =
    run_engine
      ~engine:Ref_sqlite.name
      ~config
      ~gen
      ~load:(fun () -> Load.run s gen)
      ~mk_workers:(fun () -> [ Ref_sqlite.ops s ])
      ~exec:(Ref_sqlite.exec s)
      ~query_rows:(Ref_sqlite.query_rows s)
      ~self_test
  in
  Ref_sqlite.close s;
  out
;;

(* ── driver ───────────────────────────────────────────────────────────── *)

let make_tmp_dir () =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "bench-tpcc-%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dir
;;

(* Which engines to run.  Reference SQLite is the default comparison, but a
   granary-only run is what you want while iterating on the engine — and
   loading a W>1 population twice is the most expensive part of the run. *)
let selected_engines () =
  match String.lowercase_ascii (BR.env_str "GRANARY_TPCC_ENGINES" "granary,sqlite") with
  | "granary" -> [ `Granary ]
  | "sqlite" -> [ `Sqlite ]
  | "granary,sqlite" | "sqlite,granary" | "both" -> [ `Granary; `Sqlite ]
  | other ->
    failwith
      (Printf.sprintf
         "GRANARY_TPCC_ENGINES: %S is not one of granary, sqlite, granary,sqlite"
         other)
;;

let () =
  let config = D.config_from_env () in
  let seed = config.D.seed in
  let host = BR.host_label () in
  let self_test = BR.env_str "GRANARY_TPCC_ORACLE_SELFTEST" "" <> "" in
  let dir = make_tmp_dir () in
  Printf.eprintf "tpcc: %s dir=%s\n%!" (Format.asprintf "%a" D.pp_config config) dir;
  let gen = G.create ~seed ~warehouses:config.D.warehouses in
  let engines = selected_engines () in
  let outcomes =
    List.map
      (function
        | `Granary -> Conn.name, run_granary ~dir ~config ~gen ~self_test
        | `Sqlite -> Ref_sqlite.name, run_sqlite ~dir ~config ~gen ~self_test)
      engines
  in
  print_endline (BR.Csv.header D.csv_columns);
  List.iter
    (fun (engine, o) -> List.iter print_endline (D.csv_rows o.result ~host ~engine))
    outcomes;
  Printf.eprintf "\nNewOrder/sec (derived TPC-C, NOT tpmC):\n%!";
  List.iter
    (fun (engine, o) ->
       Printf.eprintf "  %-8s %.3f\n%!" engine (D.new_order_per_sec o.result))
    outcomes;
  let failures = List.fold_left (fun acc (_, o) -> acc + o.failures) 0 outcomes in
  if failures > 0
  then (
    Printf.eprintf
      "\n%d consistency violation(s) and/or exhausted-retry transaction(s)\n%!"
      failures;
    exit 1)
;;
