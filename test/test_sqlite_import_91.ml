(* #91 integration: a real SQLite file imported via [sqlite3 <path> .dump] and
   replayed through {!Repl_engine.import_sqlite_dump}.  Shells out to the
   sqlite3 CLI, so it is an (executable) (default @runtest skips it) and gated
   on sqlite3 being in PATH — run manually with sqlite3 mounted into the dev
   container (see CLAUDE.md / test_sqlite_compare). *)

module Db = Sqlocaml.Db

let ( let* ) = Lwt.bind
let sqlite3_available () = Sys.command "sqlite3 --version >/dev/null 2>&1" = 0

let read_all ic =
  let buf = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    let n = input ic chunk 0 4096 in
    if n > 0
    then (
      Buffer.add_subbytes buf chunk 0 n;
      loop ())
  in
  loop ();
  Buffer.contents buf
;;

let sqlite3_dump path =
  let ic =
    Unix.open_process_in (Printf.sprintf "sqlite3 %s .dump" (Filename.quote path))
  in
  let out = read_all ic in
  ignore (Unix.close_process_in ic);
  out
;;

let strings_of_query db sql =
  let* r = Db.query db sql in
  match r with
  | Error e -> Alcotest.failf "query failed: %a" Db.pp_error e
  | Ok stream ->
    let* rows = Lwt_stream.to_list stream in
    Lwt.return (List.map (fun row -> Repl_engine.value_to_string row.(0)) rows)
;;

let test_real_dump_roundtrip () =
  if not (sqlite3_available ())
  then Printf.printf "[SKIP] sqlite3 not in PATH — #91 import integration\n%!"
  else (
    let db_path = Filename.temp_file "sqlocaml91_" ".sqlite" in
    Fun.protect
      ~finally:(fun () ->
        try Sys.remove db_path with
        | _ -> ())
      (fun () ->
         (* A schema that exercises AUTOINCREMENT (→ sqlite_sequence lines),
            NULLs, an escaped quote, a secondary index, a TEXT value that
            mentions "sqlite_sequence" (PR #400 review #1 — must survive), and
            ANALYZE (→ ANALYZE + sqlite_stat1 lines, PR #400 review #2 — must be
            filtered, not fail). *)
         let setup =
           "CREATE TABLE users(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age \
            INT);INSERT INTO users(name,age) VALUES('alice',30),('bob',NULL);CREATE \
            TABLE t2(x TEXT);INSERT INTO t2 VALUES('he''llo');INSERT INTO t2 \
            VALUES('sqlite_sequence rocks');CREATE INDEX ix ON users(name);ANALYZE;"
         in
         ignore
           (Sys.command
              (Printf.sprintf
                 "sqlite3 %s %s"
                 (Filename.quote db_path)
                 (Filename.quote setup)));
         let dump = sqlite3_dump db_path in
         Lwt_main.run
           (let* db = Db.open_in_memory () in
            let* applied, failures = Repl_engine.import_sqlite_dump db dump in
            Alcotest.(check int)
              "no spurious failures (ANALYZE / sqlite_stat1 filtered)"
              0
              (List.length failures);
            Alcotest.(check bool) "applied something" true (applied > 0);
            let* names = strings_of_query db "SELECT name FROM users ORDER BY id" in
            Alcotest.(check (list string)) "users round-tripped" [ "alice"; "bob" ] names;
            let* xs = strings_of_query db "SELECT x FROM t2 ORDER BY x" in
            Alcotest.(check (list string))
              "escaped quote preserved + sqlite_sequence-in-value row survives"
              [ "he'llo"; "sqlite_sequence rocks" ]
              xs;
            Db.close db)))
;;

let () =
  Alcotest.run
    "sqlite_import_91"
    [ ( "import"
      , [ Alcotest.test_case "real dump round-trip" `Quick test_real_dump_roundtrip ] )
    ]
;;
