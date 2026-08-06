(** #611 — end-to-end: the pager cache participates INSIDE a live WAL
    generation, and a checkpoint does not let it answer from a dead one.

    [test_pager_wal_cache_611.ml] pins the mechanism against a WAL stub, where
    the three invalidation triggers can be provoked exactly. This file pins
    what a user sees, through a real WAL, a real [PRAGMA wal_checkpoint] and a
    real B-tree: {b answers first}, counters second. A counter regression is
    slow; a content regression is wrong.

    Before #611 the un-checkpointed column of [bench_wal_resolve_562.ml] was
    the point of the issue: 584 [Wal_read] on a warm full scan and 3.00
    resolutions per point lookup, every one of them a page the pager cache
    already could have held. The counter assertions here are one-sided
    (strictly fewer, not an exact number) because the exact count depends on
    tree shape and page size. *)

open Granary

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "granary: %a" Db.pp_error e
;;

let run = Lwt_main.run
let exec db sql = ignore (unwrap (run (Db.execute db sql)))
let open_at path = unwrap (run (Granary_unix.open_file_wal ~path ()))

let close db =
  try ignore (run (Db.close db)) with
  | _ -> ()
;;

let with_path f =
  let dir = Filename.temp_file "t611-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "db" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun sfx ->
           try Sys.remove (path ^ sfx) with
           | _ -> ())
        [ ""; "-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f path)
;;

(* Drain [sql], returning (rows, page_reads, wal_reads). *)
let counted db sql =
  let page = ref 0
  and wal = ref 0 in
  Db.set_event_callback
    db
    (Some
       (function
         | Db.Event.Page_read _ -> incr page
         | Db.Event.Wal_read _ -> incr wal
         | _ -> ()));
  let rows =
    run
      (let open Lwt.Syntax in
       let* stream = Lwt.map unwrap (Db.query db sql) in
       Lwt_stream.to_list stream)
  in
  Db.set_event_callback db None;
  rows, !page, !wal
;;

let int_at row i =
  match row.(i) with
  | Granary_encoding.Row.V_int n -> Int64.to_int n
  | _ -> Alcotest.failf "expected an integer at column %d" i
;;

let text_at row i =
  match row.(i) with
  | Granary_encoding.Row.V_text s -> s
  | _ -> Alcotest.failf "expected text at column %d" i
;;

let seed db n =
  exec db "PRAGMA synchronous = off";
  (* Keep the whole seed inside ONE live generation: an autocheckpoint would
     migrate it to the main file and test the wrong thing. *)
  exec db "PRAGMA wal_autocheckpoint = 0";
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "BEGIN";
  for i = 1 to n do
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'v%d')" i i)
  done;
  exec db "COMMIT"
;;

let one_row rows = List.hd rows

(* ------------------------------------------------------------------ *)

(* The issue's headline: inside a live generation, a repeated scan and a
   repeated point lookup must stop re-resolving through the WAL index. *)
let warm_reads_stop_re_resolving_the_wal () =
  with_path (fun path ->
    let db = open_at path in
    seed db 2000;
    let rows, _, wal_cold = counted db "SELECT count(*) FROM t" in
    Alcotest.(check int) "cold scan row count" 2000 (int_at (one_row rows) 0);
    Alcotest.(check bool)
      "the scan really is served by the WAL overlay"
      true
      (wal_cold > 0);
    let rows2, _, wal_warm = counted db "SELECT count(*) FROM t" in
    Alcotest.(check int) "warm scan row count" 2000 (int_at (one_row rows2) 0);
    Alcotest.(check bool)
      (Printf.sprintf "warm scan re-resolves far less (%d vs %d)" wal_warm wal_cold)
      true
      (wal_warm < wal_cold);
    (* Point lookups: the descent (root, interior, leaf) is what was being
       re-resolved once per lookup. *)
    let lookup i =
      let sql = Printf.sprintf "SELECT b FROM t WHERE a = %d" i in
      let rows, _, wal = counted db sql in
      Alcotest.(check string)
        (Printf.sprintf "lookup %d value" i)
        (Printf.sprintf "v%d" i)
        (text_at (one_row rows) 0);
      wal
    in
    let first = lookup 1234 in
    let repeat = lookup 1234 in
    Alcotest.(check int)
      (Printf.sprintf "a repeated point lookup resolves nothing (first was %d)" first)
      0
      repeat;
    close db)
;;

(* The correctness half: a checkpoint recycles frame indices, so every cached
   frame must stop being reachable at that instant. The pattern below is the
   one that would go wrong — cache a generation, checkpoint, write a NEW
   generation whose frames land on the same indices, then read. *)
let answers_survive_repeated_checkpoints () =
  with_path (fun path ->
    let db = open_at path in
    seed db 500;
    for round = 1 to 5 do
      (* Warm the cache with this generation's frames. *)
      let rows, _, _ = counted db "SELECT count(*) FROM t" in
      Alcotest.(check int)
        (Printf.sprintf "round %d pre-checkpoint count" round)
        (500 + ((round - 1) * 100))
        (int_at (one_row rows) 0);
      exec db "PRAGMA wal_checkpoint";
      (* A new generation, reusing the frame indices just retired. *)
      exec db "BEGIN";
      for i = 1 to 100 do
        let k = 500 + ((round - 1) * 100) + i in
        exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'v%d')" k k)
      done;
      exec db "COMMIT";
      (* Every row, old and new, must still read back correctly. *)
      let rows, _, _ = counted db "SELECT count(*) FROM t" in
      Alcotest.(check int)
        (Printf.sprintf "round %d post-insert count" round)
        (500 + (round * 100))
        (int_at (one_row rows) 0);
      let probe k =
        let rows, _, _ = counted db (Printf.sprintf "SELECT b FROM t WHERE a = %d" k) in
        Alcotest.(check string)
          (Printf.sprintf "round %d value of row %d" round k)
          (Printf.sprintf "v%d" k)
          (text_at (one_row rows) 0)
      in
      (* One row from before the very first checkpoint, one from the
         generation that was just retired, one from the live one. *)
      probe 1;
      probe (500 + ((round - 1) * 100));
      probe (500 + (round * 100))
    done;
    (* And a full sweep, not just spot probes: every one of the 1000 rows must
       still hold the value its key implies. *)
    let rows, _, _ = counted db "SELECT a, b FROM t ORDER BY a" in
    Alcotest.(check int) "sweep sees every row" 1000 (List.length rows);
    List.iteri
      (fun i row ->
         let k = int_at row 0 in
         Alcotest.(check int) "sweep key order" (i + 1) k;
         Alcotest.(check string) "sweep value" (Printf.sprintf "v%d" k) (text_at row 1))
      rows;
    close db)
;;

(* Updates in a live generation: the same page acquires frame after frame, and
   every read must land on the newest. Trigger (a), end to end. *)
let updates_within_a_generation_are_never_stale () =
  with_path (fun path ->
    let db = open_at path in
    seed db 200;
    for v = 1 to 25 do
      exec db (Printf.sprintf "UPDATE t SET b = 'r%d' WHERE a = 7" v);
      let rows, _, _ = counted db "SELECT b FROM t WHERE a = 7" in
      Alcotest.(check string)
        (Printf.sprintf "update %d is visible immediately" v)
        (Printf.sprintf "r%d" v)
        (text_at (one_row rows) 0)
    done;
    (* Untouched neighbours are unaffected. *)
    let rows, _, _ = counted db "SELECT b FROM t WHERE a = 8" in
    Alcotest.(check string) "neighbour untouched" "v8" (text_at (one_row rows) 0);
    (* Survives a checkpoint, then more updates in the new generation. *)
    exec db "PRAGMA wal_checkpoint";
    let rows, _, _ = counted db "SELECT b FROM t WHERE a = 7" in
    Alcotest.(check string)
      "last update survived the checkpoint"
      "r25"
      (text_at (one_row rows) 0);
    exec db "UPDATE t SET b = 'post' WHERE a = 7";
    let rows, _, _ = counted db "SELECT b FROM t WHERE a = 7" in
    Alcotest.(check string)
      "post-checkpoint update is visible"
      "post"
      (text_at (one_row rows) 0);
    close db)
;;

(* A rollback must not leave the cache holding anything: the discarded pages
   never became frames, and the committed frames they were built from are
   still the truth. *)
let rollback_inside_a_generation_is_clean () =
  with_path (fun path ->
    let db = open_at path in
    seed db 300;
    let rows, _, _ = counted db "SELECT count(*) FROM t" in
    Alcotest.(check int) "before" 300 (int_at (one_row rows) 0);
    exec db "BEGIN";
    for i = 301 to 400 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'x%d')" i i)
    done;
    exec db "UPDATE t SET b = 'clobbered' WHERE a = 5";
    exec db "ROLLBACK";
    let rows, _, _ = counted db "SELECT count(*) FROM t" in
    Alcotest.(check int) "rollback discarded the inserts" 300 (int_at (one_row rows) 0);
    let rows, _, _ = counted db "SELECT b FROM t WHERE a = 5" in
    Alcotest.(check string)
      "rollback discarded the update"
      "v5"
      (text_at (one_row rows) 0);
    close db)
;;

let () =
  Alcotest.run
    "wal_cache_e2e_611"
    [ ( "participation"
      , [ Alcotest.test_case
            "warm reads stop re-resolving the WAL"
            `Quick
            warm_reads_stop_re_resolving_the_wal
        ] )
    ; ( "invalidation"
      , [ Alcotest.test_case
            "answers survive repeated checkpoints"
            `Quick
            answers_survive_repeated_checkpoints
        ; Alcotest.test_case
            "updates within a generation are never stale"
            `Quick
            updates_within_a_generation_are_never_stale
        ; Alcotest.test_case
            "rollback inside a generation is clean"
            `Quick
            rollback_inside_a_generation_is_clean
        ] )
    ]
;;
