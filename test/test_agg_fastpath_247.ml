(** #247: the cursor-level aggregate fast path must be byte-identical to the
    general [stream_aggregate] path, and must allocate strictly less.  We use the
    [SQLOCAML_AGG_FASTPATH] kill-switch as a live foil: each query is run with the
    fast path ON and forced OFF, and the result rows must match exactly.  A Gc
    gate then proves the fast path allocates materially less than the general
    path on the same query. *)

open Lwt.Syntax
module U = Sqlocaml_unix
module Db = Sqlocaml.Db

let run = Lwt_main.run

let unwrap = function
  | Ok x -> x
  | Error _ -> Alcotest.fail "db error"
;;

let set_fastpath on = Unix.putenv "SQLOCAML_AGG_FASTPATH" (if on then "1" else "0")

let vstr = function
  | Db.V_int i -> Printf.sprintf "i:%Ld" i
  | Db.V_real f -> Printf.sprintf "r:%.17g" f
  | Db.V_text s -> Printf.sprintf "t:%s" s
  | Db.V_null -> "null"
  | Db.V_blob _ -> "blob"
;;

let rows_of db sql =
  run
    (let* s = Lwt.map unwrap (Db.query db sql) in
     let* rows = Lwt_stream.to_list s in
     Lwt.return
       (List.map (fun r -> String.concat "," (Array.to_list (Array.map vstr r))) rows))
;;

let with_db f =
  let dir = Filename.temp_file "t247-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "db" in
  let db = run (Lwt.map unwrap (U.open_file_wal ~path ())) in
  Fun.protect
    ~finally:(fun () ->
      run (Db.close db);
      List.iter
        (fun s ->
           try Sys.remove (path ^ s) with
           | _ -> ())
        [ ""; "-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql = ignore (run (Lwt.map unwrap (Db.execute db sql)))

let seed db ~rows =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, f REAL, s TEXT)";
  exec db "CREATE TABLE empt (id INTEGER PRIMARY KEY, k INTEGER)";
  exec db "BEGIN";
  for i = 0 to rows - 1 do
    (* every 7th row has NULL k and NULL f to exercise null handling *)
    let k = if i mod 7 = 0 then "NULL" else string_of_int (i * 3 mod 100) in
    let f =
      if i mod 7 = 0
      then "NULL"
      else Printf.sprintf "%.2f" (float_of_int (i mod 50) +. 0.5)
    in
    exec db (Printf.sprintf "INSERT INTO t (id,k,f,s) VALUES (%d,%s,%s,'s%d')" i k f i)
  done;
  exec db "COMMIT"
;;

let queries =
  [ "SELECT COUNT(*) FROM t"
  ; "SELECT COUNT(k) FROM t"
  ; "SELECT SUM(k) FROM t"
  ; "SELECT SUM(f) FROM t"
  ; "SELECT AVG(k) FROM t"
  ; "SELECT MIN(k), MAX(k) FROM t"
  ; "SELECT MIN(s), MAX(s) FROM t"
  ; "SELECT COUNT(*), SUM(k), AVG(f), MIN(k), MAX(k) FROM t"
  ; "SELECT COUNT(*) FROM t WHERE k > 50"
  ; "SELECT SUM(k) FROM t WHERE k IS NOT NULL"
  ; "SELECT GROUP_CONCAT(s) FROM t WHERE id < 20"
  ; (* empty table: aggregates still return exactly one row *)
    "SELECT COUNT(*), SUM(k), AVG(k), MIN(k), MAX(k) FROM empt"
  ; "SELECT COUNT(*) FROM t WHERE k > 100000" (* filter matches nothing *)
  ]
;;

let test_equivalence () =
  with_db (fun db ->
    seed db ~rows:300;
    List.iter
      (fun sql ->
         set_fastpath false;
         let off = rows_of db sql in
         set_fastpath true;
         let on = rows_of db sql in
         Alcotest.(check (list string)) ("fast==general: " ^ sql) off on)
      queries)
;;

(* Gc gate: the fast path must allocate materially less than the general path on
   the bench-shaped query (no per-row Lwt_stream, no to_list, pruned decode). *)
let test_alloc_reduced () =
  with_db (fun db ->
    seed db ~rows:3000;
    let sql = "SELECT COUNT(*), SUM(k) FROM t" in
    let alloc_of () =
      ignore (rows_of db sql);
      (* warmup *)
      Gc.full_major ();
      let a0 = Gc.allocated_bytes () in
      for _ = 1 to 3 do
        ignore (rows_of db sql)
      done;
      (Gc.allocated_bytes () -. a0) /. 3.
    in
    set_fastpath false;
    let off = alloc_of () in
    set_fastpath true;
    let on = alloc_of () in
    set_fastpath true;
    Printf.printf
      "  [#247 agg alloc] general=%.0f B  fastpath=%.0f B  (%.0f%% of general)\n%!"
      off
      on
      (100. *. on /. off);
    (* Allocation is deterministic (Gc.allocated_bytes is exact), so this margin
       is stable run-to-run.  The measured figure is ~70%% of general (~767 B/row
       saved over the to_list + per-row stream + payload decode the fast path
       skips); the 0.85 bound is a regression guard — if the fast path ever
       silently fell back, [on] would equal [off]. *)
    Alcotest.(check bool)
      (Printf.sprintf "fast path allocates < 85%% of general (on=%.0f off=%.0f)" on off)
      true
      (on < off *. 0.85))
;;

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run
    "agg_fastpath_247"
    [ ( "fast-path"
      , [ Alcotest.test_case "fast == general (equivalence)" `Quick test_equivalence
        ; Alcotest.test_case
            "fast path allocates less (Gc gate)"
            `Quick
            test_alloc_reduced
        ] )
    ]
;;
