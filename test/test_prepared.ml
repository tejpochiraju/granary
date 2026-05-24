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
      let* r1 = D.run st ~params:[ D.V_int 1L; D.V_text "hello" ] in
      let* r2 = D.run st ~params:[ D.V_int 2L; D.V_text "world" ] in
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
;;

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
      let* r = D.iter st ~params:[ D.V_int 15L ] in
      (match r with
       | Error e -> Alcotest.failf "iter: %s" (Format.asprintf "%a" D.pp_error e)
       | Ok stream ->
         let* rows = Lwt_stream.to_list stream in
         Alcotest.(check int) "rows > 15" 2 (List.length rows);
         let* () = D.finalize st in
         Lwt.return_unit))
;;

let test_reuse () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER)" in
    let* stmt_r = D.prepare db "INSERT INTO t (id) VALUES (?)" in
    match stmt_r with
    | Error e -> Alcotest.failf "prepare: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok st ->
      (* Insert multiple rows by reusing the same prepared statement *)
      let* _ = D.run st ~params:[ D.V_int 1L ] in
      let* _ = D.run st ~params:[ D.V_int 2L ] in
      let* _ = D.run st ~params:[ D.V_int 3L ] in
      let* () = D.finalize st in
      let* rows_r = D.query db "SELECT id FROM t ORDER BY id" in
      (match rows_r with
       | Error e -> Alcotest.failf "query: %s" (Format.asprintf "%a" D.pp_error e)
       | Ok stream ->
         let* rs = Lwt_stream.to_list stream in
         Alcotest.(check int) "reuse count" 3 (List.length rs);
         Lwt.return_unit))
;;

let test_update_with_param () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER, val TEXT)" in
    let* _ = D.execute db "INSERT INTO t (id, val) VALUES (1, 'original')" in
    let* stmt_r = D.prepare db "UPDATE t SET val = ? WHERE id = ?" in
    match stmt_r with
    | Error e -> Alcotest.failf "prepare update: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok st ->
      let* r = D.run st ~params:[ D.V_text "updated"; D.V_int 1L ] in
      let* () = D.finalize st in
      (match r with
       | Error e -> Alcotest.failf "run update: %s" (Format.asprintf "%a" D.pp_error e)
       | Ok _ ->
         let* rows_r = D.query db "SELECT val FROM t WHERE id = 1" in
         (match rows_r with
          | Error e -> Alcotest.failf "check: %s" (Format.asprintf "%a" D.pp_error e)
          | Ok stream ->
            let* rs = Lwt_stream.to_list stream in
            (match rs with
             | [| D.V_text s |] :: _ ->
               Alcotest.(check string) "updated" "updated" s;
               Lwt.return_unit
             | _ -> Alcotest.failf "unexpected result"))))
;;

let test_delete_with_param () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER, val TEXT)" in
    let* _ = D.execute db "INSERT INTO t (id, val) VALUES (1, 'keep')" in
    let* _ = D.execute db "INSERT INTO t (id, val) VALUES (2, 'delete_me')" in
    let* stmt_r = D.prepare db "DELETE FROM t WHERE id = ?" in
    match stmt_r with
    | Error e -> Alcotest.failf "prepare delete: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok st ->
      let* r = D.run st ~params:[ D.V_int 2L ] in
      let* () = D.finalize st in
      (match r with
       | Error e -> Alcotest.failf "run delete: %s" (Format.asprintf "%a" D.pp_error e)
       | Ok _ ->
         let* rows_r = D.query db "SELECT COUNT(*) FROM t" in
         (match rows_r with
          | Error e -> Alcotest.failf "count: %s" (Format.asprintf "%a" D.pp_error e)
          | Ok stream ->
            let* rs = Lwt_stream.to_list stream in
            (match rs with
             | [| D.V_int n |] :: _ ->
               Alcotest.(check int64) "remaining" 1L n;
               Lwt.return_unit
             | _ -> Alcotest.failf "unexpected"))))
;;

let () =
  Alcotest.run
    "prepared"
    [ "run", [ Alcotest.test_case "prepare_run" `Quick test_prepare_run ]
    ; "iter", [ Alcotest.test_case "iter" `Quick test_iter ]
    ; "reuse", [ Alcotest.test_case "reuse" `Quick test_reuse ]
    ; "update", [ Alcotest.test_case "update_param" `Quick test_update_with_param ]
    ; "delete", [ Alcotest.test_case "delete_param" `Quick test_delete_with_param ]
    ]
;;
