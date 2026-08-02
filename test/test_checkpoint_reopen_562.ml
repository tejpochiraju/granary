(** #562 — end-to-end: a [PRAGMA wal_checkpoint] must still be a checkpoint
    after the handle is closed and reopened.

    The WAL-level mechanics are pinned in [test_wal_generation_562.ml]; this
    file pins what a user sees. Before the fix, reopening a checkpointed
    database replayed the whole stale WAL generation, so:

    - every page access was a [Wal_read] and the pager cache never
      participated — the observation that produced #562;
    - the WAL file never shrank, and every [open] rescanned and
      CRC-verified every frame ever written;
    - a post-checkpoint generation shorter than its predecessor could be
      overwritten by the stale tail on recovery.

    The last one is the reason this file also asserts {e content}, not just
    counters: a counter regression is slow, a content regression is wrong. *)

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
  let dir = Filename.temp_file "t562-" "" in
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
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "BEGIN";
  for i = 1 to n do
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'v%d')" i i)
  done;
  exec db "COMMIT"
;;

let wal_size path =
  try (Unix.stat (path ^ "-wal")).Unix.st_size with
  | _ -> 0
;;

(* ------------------------------------------------------------------ *)

(* After a checkpoint the WAL overlay must be empty on the next open, so the
   main file — and hence the pager cache — serves the read path again. *)
let checkpoint_survives_reopen () =
  with_path (fun path ->
    let db = open_at path in
    seed db 2000;
    exec db "PRAGMA wal_checkpoint";
    close db;
    let db = open_at path in
    let rows, page, wal = counted db "SELECT count(*) FROM t" in
    Alcotest.(check int) "row count" 2000 (int_at (List.hd rows) 0);
    Alcotest.(check int) "no page resolves through the WAL after a checkpoint" 0 wal;
    Alcotest.(check bool) "the main file did serve the scan" true (page > 0);
    (* And the pager cache is now real: a repeat of the same scan re-reads
       nothing. *)
    let _, page2, wal2 = counted db "SELECT count(*) FROM t" in
    Alcotest.(check int) "warm scan: no WAL reads" 0 wal2;
    Alcotest.(check bool)
      (Printf.sprintf "warm scan re-reads far fewer pages (%d vs %d)" page2 page)
      true
      (page2 * 4 < page);
    close db)
;;

(* The rotation must not cost durability: everything committed after the
   checkpoint has to survive the reopen, and everything committed before it has
   to still be in the main file. *)
let post_checkpoint_writes_survive () =
  with_path (fun path ->
    let db = open_at path in
    seed db 500;
    exec db "PRAGMA wal_checkpoint";
    (* A generation deliberately much shorter than the one it replaced — the
       shape that was silently corrupted before #562. *)
    exec db "UPDATE t SET b = 'AFTER' WHERE a = 1";
    exec db "INSERT INTO t VALUES (501, 'new')";
    close db;
    let db = open_at path in
    let rows, _, _ = counted db "SELECT count(*) FROM t" in
    Alcotest.(check int)
      "row count includes the post-checkpoint insert"
      501
      (int_at (List.hd rows) 0);
    let rows, _, _ = counted db "SELECT b FROM t WHERE a = 1" in
    Alcotest.(check string)
      "the post-checkpoint UPDATE is not overwritten by the stale generation"
      "AFTER"
      (text_at (List.hd rows) 0);
    let rows, _, _ = counted db "SELECT b FROM t WHERE a = 500" in
    Alcotest.(check string)
      "a pre-checkpoint row is intact"
      "v500"
      (text_at (List.hd rows) 0);
    let rows, _, _ = counted db "SELECT b FROM t WHERE a = 501" in
    Alcotest.(check string)
      "the post-checkpoint INSERT is there"
      "new"
      (text_at (List.hd rows) 0);
    close db)
;;

(* The WAL is a log, not an archive: repeated checkpoint cycles must not make
   it grow without bound.  Before #562 nothing ever reduced the recovered
   generation, so each open re-indexed everything the file had ever held. *)
let wal_does_not_grow_without_bound () =
  with_path (fun path ->
    let db = open_at path in
    seed db 200;
    exec db "PRAGMA wal_checkpoint";
    let after_first = wal_size path in
    for round = 1 to 6 do
      exec db "BEGIN";
      for i = 1 to 200 do
        exec db (Printf.sprintf "UPDATE t SET b = 'r%d-%d' WHERE a = %d" round i i)
      done;
      exec db "COMMIT";
      exec db "PRAGMA wal_checkpoint"
    done;
    close db;
    let after_rounds = wal_size path in
    Alcotest.(check bool)
      (Printf.sprintf
         "WAL stays bounded across checkpoint cycles (%d -> %d bytes)"
         after_first
         after_rounds)
      true
      (after_rounds <= after_first * 3);
    (* And the reopened handle sees the last round, from the main file. *)
    let db = open_at path in
    let rows, _, wal = counted db "SELECT b FROM t WHERE a = 7" in
    Alcotest.(check string) "last round's value" "r6-7" (text_at (List.hd rows) 0);
    Alcotest.(check int) "still no WAL overlay after the final checkpoint" 0 wal;
    close db)
;;

let () =
  Alcotest.run
    "checkpoint across reopen (#562)"
    [ ( "checkpoint"
      , [ Alcotest.test_case "survives a reopen" `Quick checkpoint_survives_reopen
        ; Alcotest.test_case
            "post-checkpoint writes survive"
            `Quick
            post_checkpoint_writes_survive
        ; Alcotest.test_case
            "WAL does not grow without bound"
            `Quick
            wal_does_not_grow_without_bound
        ] )
    ]
;;
