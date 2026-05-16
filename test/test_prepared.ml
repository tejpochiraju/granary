open Lwt.Syntax
module D = Sqlocaml.Db

let run f = Lwt_main.run (f ())

let test_prepare_run () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER, val TEXT)" in
    let* stmt_r = D.prepare db "INSERT INTO t (id, val) VALUES (?, ?)" in
    match stmt_r with
    | Error e -> Alcotest.failf "prepare: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok st ->
      let* r1 = D.run st ~params:[D.V_int 1L; D.V_text "hello"] in
      let* r2 = D.run st ~params:[D.V_int 2L; D.V_text "world"] in
      let* () = D.finalize st in
      (match r1, r2 with
       | Ok _, Ok _ ->
         let* rows_r = D.query db "SELECT val FROM t ORDER BY id" in
         (match rows_r with
          | Error e -> Alcotest.failf "query: %s" (Format.asprintf "%a" D.pp_error e)
          | Ok stream ->
            let* rs = Lwt_stream.to_list stream in
            Alcotest.(check int) "row count" 2 (List.length rs);
            Lwt.return_unit)
       | Error e, _ | _, Error e ->
         Alcotest.failf "run: %s" (Format.asprintf "%a" D.pp_error e)))

let test_iter () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE nums (n INTEGER)" in
    let* _ = D.execute db "INSERT INTO nums (n) VALUES (10)" in
    let* _ = D.execute db "INSERT INTO nums (n) VALUES (20)" in
    let* _ = D.execute db "INSERT INTO nums (n) VALUES (30)" in
    let* stmt_r = D.prepare db "SELECT n FROM nums WHERE n > ?" in
    match stmt_r with
    | Error e -> Alcotest.failf "prepare: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok st ->
      let* r = D.iter st ~params:[D.V_int 15L] in
      (match r with
       | Error e -> Alcotest.failf "iter: %s" (Format.asprintf "%a" D.pp_error e)
       | Ok stream ->
         let* rows = Lwt_stream.to_list stream in
         Alcotest.(check int) "rows > 15" 2 (List.length rows);
         let* () = D.finalize st in
         Lwt.return_unit))

let test_reuse () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER)" in
    let* stmt_r = D.prepare db "INSERT INTO t (id) VALUES (?)" in
    match stmt_r with
    | Error e -> Alcotest.failf "prepare: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok st ->
      (* Insert multiple rows by reusing the same prepared statement *)
      let* _ = D.run st ~params:[D.V_int 1L] in
      let* _ = D.run st ~params:[D.V_int 2L] in
      let* _ = D.run st ~params:[D.V_int 3L] in
      let* () = D.finalize st in
      let* rows_r = D.query db "SELECT id FROM t ORDER BY id" in
      (match rows_r with
       | Error e -> Alcotest.failf "query: %s" (Format.asprintf "%a" D.pp_error e)
       | Ok stream ->
         let* rs = Lwt_stream.to_list stream in
         Alcotest.(check int) "reuse count" 3 (List.length rs);
         Lwt.return_unit))

let () =
  Alcotest.run "prepared" [
    "run",   [ Alcotest.test_case "prepare_run" `Quick test_prepare_run ];
    "iter",  [ Alcotest.test_case "iter"        `Quick test_iter        ];
    "reuse", [ Alcotest.test_case "reuse"       `Quick test_reuse       ];
  ]
