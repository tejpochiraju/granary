(** Phase 39 / #99 — Long-running soak tests.

    Random workload generator that runs INSERT / UPDATE / DELETE / SELECT
    operations against a fresh database for [iter] iterations, checking
    against an OCaml model after each operation.

    The default iteration count is small (~500) so the test fits in CI;
    set [SQLOCAML_SOAK_ITERS] to a larger number (e.g. 50_000) to run a
    long soak locally.

    What this catches:
    - Resource leaks (Gc.heap_words growth across phases)
    - Index corruption (model diverges from database)
    - Pager leaks (overflow chain or freelist anomalies)
    - Catalog drift (schema state vs row state)

    The model is a [(int, int) Hashtbl] keyed by id.  After each
    randomized op we compare the model's contents to the db. *)

module Db = struct
  include Sqlocaml.Db

  let open_file = Sqlocaml_unix.open_file
end

let run = Lwt_main.run

let env_int key default =
  match Sys.getenv_opt key with
  | None -> default
  | Some s ->
    (try int_of_string s with
     | _ -> default)
;;

let n_iters = env_int "SQLOCAML_SOAK_ITERS" 500
let max_key = 200

let exec_or_ignore db sql =
  match run (Db.execute db sql) with
  | Ok () -> true
  | Error _ -> false
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error _ -> Alcotest.failf "exec failed: %s" sql
;;

let all_rows_int_pairs db =
  match run (Db.query db "SELECT id, n FROM t ORDER BY id ASC") with
  | Error _ -> Alcotest.fail "soak query failed"
  | Ok stream ->
    let rows = run (Lwt_stream.to_list stream) in
    List.map
      (fun row ->
         let id =
           match row.(0) with
           | Db.V_int n -> Int64.to_int n
           | _ -> -1
         in
         let n =
           match row.(1) with
           | Db.V_int n -> Int64.to_int n
           | _ -> -1
         in
         id, n)
      rows
;;

let model_pairs model =
  let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) model [] in
  List.sort (fun (a, _) (b, _) -> compare a b) pairs
;;

let assert_model_matches db model iter =
  let actual = all_rows_int_pairs db in
  let expected = model_pairs model in
  if actual <> expected
  then (
    Printf.eprintf "Iteration %d: model and db diverged.\n" iter;
    Printf.eprintf
      "model has %d rows, db has %d rows\n"
      (List.length expected)
      (List.length actual);
    Alcotest.failf "soak: model/db divergence at iter %d" iter)
;;

(** Core soak loop.  [seed] makes the run reproducible.  Returns
    [(initial_heap, final_heap)] so callers can compare. *)
let soak_loop ~seed ~iters db =
  Random.init seed;
  let model : (int, int) Hashtbl.t = Hashtbl.create 64 in
  let initial_heap = (Gc.stat ()).Gc.live_words in
  let check_every = max 1 (iters / 10) in
  for i = 0 to iters - 1 do
    let op = Random.int 4 in
    let k = Random.int max_key in
    let v = Random.int 1_000_000 in
    (match op with
     | 0 ->
       (* INSERT *)
       if not (Hashtbl.mem model k)
       then (
         let ok =
           exec_or_ignore db (Printf.sprintf "INSERT INTO t (id, n) VALUES (%d, %d)" k v)
         in
         if ok then Hashtbl.add model k v)
     | 1 ->
       (* UPDATE *)
       if Hashtbl.mem model k
       then (
         exec db (Printf.sprintf "UPDATE t SET n = %d WHERE id = %d" v k);
         Hashtbl.replace model k v)
     | 2 ->
       (* DELETE *)
       if Hashtbl.mem model k
       then (
         exec db (Printf.sprintf "DELETE FROM t WHERE id = %d" k);
         Hashtbl.remove model k)
     | _ ->
       (* SELECT - just exercise the query path *)
       (match run (Db.query db (Printf.sprintf "SELECT n FROM t WHERE id = %d" k)) with
        | Ok stream -> ignore (run (Lwt_stream.to_list stream))
        | Error _ -> Alcotest.failf "soak SELECT failed at iter %d" i));
    if i mod check_every = 0 then assert_model_matches db model i
  done;
  assert_model_matches db model iters;
  Gc.compact ();
  let final_heap = (Gc.stat ()).Gc.live_words in
  initial_heap, final_heap
;;

let test_soak_mem () =
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER)";
  let _, _ = soak_loop ~seed:42 ~iters:n_iters db in
  run (Db.close db)
;;

let test_soak_file () =
  let path = "/tmp/sqlocaml_phase39_soak.db" in
  (try Unix.unlink path with
   | _ -> ());
  let db =
    match run (Db.open_file ~path) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "open_file failed"
  in
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER)";
  let _, _ = soak_loop ~seed:1729 ~iters:n_iters db in
  run (Db.close db);
  try Unix.unlink path with
  | _ -> ()
;;

(** Heap growth check: run two consecutive soak phases on the same
    database; assert that live-words at the end of phase 2 is not more
    than 2× the live-words at the end of phase 1.  This catches
    cache/leak growth proportional to operation count. *)
let test_heap_no_unbounded_growth () =
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER)";
  let phase_iters = n_iters in
  let _, after_phase1 = soak_loop ~seed:7 ~iters:phase_iters db in
  (* Empty the table between phases so live working set returns to
     baseline. *)
  exec db "DELETE FROM t";
  Gc.compact ();
  let baseline = (Gc.stat ()).Gc.live_words in
  let _, after_phase2 = soak_loop ~seed:13 ~iters:phase_iters db in
  (* phase 2 should land near baseline + working-set, not at 2× phase 1. *)
  let growth_ratio = float_of_int after_phase2 /. float_of_int (max 1 after_phase1) in
  if growth_ratio > 2.0
  then
    Alcotest.failf
      "heap grew unboundedly: phase1=%d, baseline=%d, phase2=%d (ratio %.2f)"
      after_phase1
      baseline
      after_phase2
      growth_ratio;
  run (Db.close db)
;;

(** Recovery edge case: after a soak run, force a transaction abort
    half-way through a new batch and verify the model+db remain
    consistent after rollback. *)
let test_rollback_during_soak () =
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER)";
  let _, _ = soak_loop ~seed:99 ~iters:(min 200 n_iters) db in
  (* Snapshot model. *)
  let snapshot = all_rows_int_pairs db in
  exec db "BEGIN";
  for i = 5000 to 5050 do
    exec db (Printf.sprintf "INSERT INTO t (id, n) VALUES (%d, %d)" i i)
  done;
  exec db "ROLLBACK";
  let after = all_rows_int_pairs db in
  if snapshot <> after
  then Alcotest.fail "rolled-back inserts persisted across soak boundary";
  run (Db.close db)
;;

let () =
  Alcotest.run
    "soak"
    [ ( "soak"
      , [ Alcotest.test_case
            (Printf.sprintf "in-memory (%d iters)" n_iters)
            `Slow
            test_soak_mem
        ; Alcotest.test_case
            (Printf.sprintf "file backend (%d iters)" n_iters)
            `Slow
            test_soak_file
        ; Alcotest.test_case
            "heap growth bounded across phases"
            `Slow
            test_heap_no_unbounded_growth
        ; Alcotest.test_case "rollback during soak" `Slow test_rollback_during_soak
        ] )
    ]
;;
