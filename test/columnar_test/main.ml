open Lwt.Syntax
module Db = Sqlocaml.Db
module Row = Sqlocaml_encoding.Row

let ok_or_fail = function
  | Ok x -> x
  | Error e -> failwith (Format.asprintf "%a" Db.pp_error e)
;;

let make_db () =
  let db = Lwt_main.run (Db.open_in_memory ()) in
  db
;;

let exec db sql =
  let* result = Db.execute db sql in
  let () = ok_or_fail result in
  Lwt.return_unit
;;

let query db sql =
  let* result = Db.query db sql in
  let stream = ok_or_fail result in
  Lwt_stream.to_list stream
;;

let expect_error db sql =
  let* result = Db.execute db sql in
  (match result with
   | Error _ -> ()
   | Ok () -> failwith ("expected error from: " ^ sql));
  Lwt.return_unit
;;

exception Not_an_error

let expect_query_error db sql =
  Lwt.catch
    (fun () ->
       let* result = Db.query db sql in
       match result with
       | Error _ -> Lwt.return_unit
       | Ok stream ->
         let* _ = Lwt_stream.to_list stream in
         Lwt.fail Not_an_error)
    (function
      | Not_an_error -> failwith ("expected error from: " ^ sql)
      | _ -> Lwt.return_unit)
;;

let test_create_columnstore () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (region TEXT, amount REAL) USING COLUMNSTORE" in
  let* rows = query db "SELECT name FROM sqlite_master WHERE type='table'" in
  let names =
    List.filter_map
      (function
        | [| Row.V_text n |] -> Some n
        | _ -> None)
      rows
  in
  assert (List.mem "t" names);
  Lwt.return_unit
;;

let test_create_duplicate_fails () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
  let* result =
    Lwt.catch
      (fun () ->
         let* _ = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
         Lwt.return false)
      (fun _ -> Lwt.return true)
  in
  assert result;
  Lwt.return_unit
;;

let test_insert_values () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO t VALUES (1.0), (2.0), (3.0)" in
  let* rows = query db "SELECT COUNT(*) FROM t" in
  (match rows with
   | [ [| Row.V_int 3L |] ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_insert_select () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE src (region TEXT, amount REAL)" in
  let* () =
    exec db "INSERT INTO src VALUES ('north', 10.0), ('south', 20.0), ('north', 5.0)"
  in
  let* () = exec db "CREATE TABLE col_t (region TEXT, amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_t SELECT region, amount FROM src" in
  let* rows = query db "SELECT COUNT(*) FROM col_t" in
  (match rows with
   | [ [| Row.V_int 3L |] ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_sum () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO t VALUES (10.0), (20.0), (5.0)" in
  let* rows = query db "SELECT SUM(amount) FROM t" in
  (match rows with
   | [ [| Row.V_real s |] ] -> assert (Float.equal s 35.0)
   | _ -> assert false);
  Lwt.return_unit
;;

let test_count () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO t VALUES (1.0), (2.0), (3.0)" in
  let* rows = query db "SELECT COUNT(*) FROM t" in
  (match rows with
   | [ [| Row.V_int 3L |] ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_min_max () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO t VALUES (3.0), (1.0), (2.0)" in
  let* rows = query db "SELECT MIN(x), MAX(x) FROM t" in
  (match rows with
   | [ [| Row.V_real mn; Row.V_real mx |] ] ->
     assert (Float.equal mn 1.0);
     assert (Float.equal mx 3.0)
   | _ -> assert false);
  Lwt.return_unit
;;

let test_group_by () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (region TEXT, amount REAL) USING COLUMNSTORE" in
  let* () =
    exec db "INSERT INTO t VALUES ('north', 10.0), ('south', 20.0), ('north', 5.0)"
  in
  let* rows =
    query db "SELECT region, SUM(amount) FROM t GROUP BY region ORDER BY region"
  in
  (match rows with
   | [ [| Row.V_text "north"; Row.V_real n |]; [| Row.V_text "south"; Row.V_real s |] ] ->
     assert (Float.equal n 15.0);
     assert (Float.equal s 20.0)
   | _ -> assert false);
  Lwt.return_unit
;;

let test_insert_returning_rejected () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO t VALUES (1.0)" in
  let* () = expect_query_error db "INSERT INTO t VALUES (2.0) RETURNING x" in
  Lwt.return_unit
;;

let test_update_returning_rejected () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO t VALUES (1.0)" in
  let* () = expect_query_error db "UPDATE t SET x = 2.0 RETURNING x" in
  Lwt.return_unit
;;

let test_delete_returning_rejected () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO t VALUES (1.0)" in
  let* () = expect_query_error db "DELETE FROM t RETURNING x" in
  Lwt.return_unit
;;

let test_alter_table_rejected () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (x REAL) USING COLUMNSTORE" in
  let* () = expect_error db "ALTER TABLE t ADD COLUMN y TEXT" in
  Lwt.return_unit
;;

let test_fk_on_columnar_parent_rejected () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE col_t (id INTEGER PRIMARY KEY) USING COLUMNSTORE" in
  let* () =
    expect_error db "CREATE TABLE child (id INT, col_id INT REFERENCES col_t(id))"
  in
  Lwt.return_unit
;;

let test_serialize_col_int () =
  let module Col = Sqlocaml_columnar.Col in
  let col = Col.create Row.Integer 4 in
  let col = Col.append_value col (Row.V_int 42L) in
  let col = Col.append_value col (Row.V_int (-1L)) in
  let col = Col.append_value col Row.V_null in
  let col = Col.append_value col (Row.V_int 7L) in
  let encoded = Col.encode col in
  let decoded, _ = Col.decode encoded 0 in
  assert (Col.length decoded = 4);
  assert (Col.get_value decoded 0 = Row.V_int 42L);
  assert (Col.get_value decoded 1 = Row.V_int (-1L));
  assert (Col.get_value decoded 2 = Row.V_null);
  assert (Col.get_value decoded 3 = Row.V_int 7L)
;;

let test_serialize_col_real () =
  let module Col = Sqlocaml_columnar.Col in
  let col = Col.create Row.Real 4 in
  let col = Col.append_value col (Row.V_real 3.14) in
  let col = Col.append_value col Row.V_null in
  let col = Col.append_value col (Row.V_real (-2.5)) in
  let col = Col.append_value col (Row.V_real 0.0) in
  let encoded = Col.encode col in
  let decoded, _ = Col.decode encoded 0 in
  assert (Col.length decoded = 4);
  assert (Col.get_value decoded 0 = Row.V_real 3.14);
  assert (Col.get_value decoded 1 = Row.V_null);
  assert (Col.get_value decoded 2 = Row.V_real (-2.5));
  assert (Col.get_value decoded 3 = Row.V_real 0.0)
;;

let test_serialize_col_text () =
  let module Col = Sqlocaml_columnar.Col in
  let col = Col.create Row.Text 4 in
  let col = Col.append_value col (Row.V_text "hello") in
  let col = Col.append_value col Row.V_null in
  let col = Col.append_value col (Row.V_text "world") in
  let col = Col.append_value col (Row.V_text "hello") in
  let encoded = Col.encode col in
  let decoded, _ = Col.decode encoded 0 in
  assert (Col.length decoded = 4);
  assert (Col.get_value decoded 0 = Row.V_text "hello");
  assert (Col.get_value decoded 1 = Row.V_null);
  assert (Col.get_value decoded 2 = Row.V_text "world");
  assert (Col.get_value decoded 3 = Row.V_text "hello");
  assert (Col.dict_size decoded = 2);
  assert (Col.dict_size (Col.create Row.Integer 0) = 0)
;;

let test_serialize_col_blob () =
  let module Col = Sqlocaml_columnar.Col in
  let col = Col.create Row.Blob 4 in
  let col = Col.append_value col (Row.V_blob (Bytes.of_string "\x00\x01\x02")) in
  let col = Col.append_value col Row.V_null in
  let col = Col.append_value col (Row.V_blob Bytes.empty) in
  let col = Col.append_value col (Row.V_blob (Bytes.of_string "abc")) in
  let encoded = Col.encode col in
  let decoded, _ = Col.decode encoded 0 in
  assert (Col.length decoded = 4);
  assert (Col.get_value decoded 0 = Row.V_blob (Bytes.of_string "\x00\x01\x02"));
  assert (Col.get_value decoded 1 = Row.V_null);
  assert (Col.get_value decoded 2 = Row.V_blob Bytes.empty);
  assert (Col.get_value decoded 3 = Row.V_blob (Bytes.of_string "abc"))
;;

let test_serialize_store_roundtrip () =
  let default = None in
  let check_sql = None in
  let generated_as = None in
  let cols =
    [ Row.
        { name = "id"
        ; ty = Integer
        ; not_null = false
        ; primary_key = false
        ; pk_desc = false
        ; default
        ; check_sql
        ; generated_as
        }
    ; Row.
        { name = "val"
        ; ty = Real
        ; not_null = false
        ; primary_key = false
        ; pk_desc = false
        ; default
        ; check_sql
        ; generated_as
        }
    ]
  in
  let module Col_store = Sqlocaml_columnar.Col_store in
  let store = Col_store.create cols in
  Col_store.insert_rows
    store
    [| [| Row.V_int 1L; Row.V_real 10.0 |]; [| Row.V_int 2L; Row.V_real 20.0 |] |];
  let encoded = Col_store.encode store in
  let decoded = Col_store.decode cols encoded in
  assert (Col_store.nrows decoded = 2);
  let rows = List.of_seq (Col_store.to_row_seq decoded) in
  match rows with
  | [ [| Row.V_int 1L; Row.V_real 10.0 |]; [| Row.V_int 2L; Row.V_real 20.0 |] ] -> ()
  | _ -> assert false
;;

let test_persist_roundtrip () =
  let module Col_store = Sqlocaml_columnar.Col_store in
  let module Persist = Sqlocaml_columnar.Persist in
  let module S = Sqlocaml_store.Store in
  let default = None in
  let check_sql = None in
  let generated_as = None in
  let cols =
    [ Row.
        { name = "id"
        ; ty = Integer
        ; not_null = false
        ; primary_key = false
        ; pk_desc = false
        ; default
        ; check_sql
        ; generated_as
        }
    ; Row.
        { name = "label"
        ; ty = Text
        ; not_null = false
        ; primary_key = false
        ; pk_desc = false
        ; default
        ; check_sql
        ; generated_as
        }
    ; Row.
        { name = "score"
        ; ty = Real
        ; not_null = false
        ; primary_key = false
        ; pk_desc = false
        ; default
        ; check_sql
        ; generated_as
        }
    ]
  in
  let store = S.create () in
  let tid = 16 in
  let col_store = Col_store.create cols in
  Col_store.insert_rows
    col_store
    [| [| Row.V_int 1L; Row.V_text "alpha"; Row.V_real 10.5 |]
     ; [| Row.V_int 2L; Row.V_null; Row.V_real 20.0 |]
     ; [| Row.V_int 3L; Row.V_text "beta"; Row.V_real 30.0 |]
    |];
  assert (Col_store.dirty col_store);
  let* () =
    let* tx = S.rw_begin store in
    let* () = Persist.save tx tid col_store in
    S.commit tx
  in
  Col_store.mark_clean col_store;
  assert (not (Col_store.dirty col_store));
  let* loaded = S.with_ro store (fun tx -> Persist.load tx tid cols) in
  match loaded with
  | Some loaded ->
    assert (Col_store.nrows loaded = 3);
    assert (not (Col_store.dirty loaded));
    let rows = List.of_seq (Col_store.to_row_seq loaded) in
    (match rows with
     | [ [| Row.V_int 1L; Row.V_text "alpha"; Row.V_real 10.5 |]
       ; [| Row.V_int 2L; Row.V_null; Row.V_real 20.0 |]
       ; [| Row.V_int 3L; Row.V_text "beta"; Row.V_real 30.0 |]
       ] -> Lwt.return_unit
     | _ -> assert false)
  | None -> assert false
;;

let test_persist_fresh_table_returns_none () =
  (* A tree_id with no persisted data should return None from Persist.load. *)
  let module Persist = Sqlocaml_columnar.Persist in
  let module S = Sqlocaml_store.Store in
  let default = None in
  let check_sql = None in
  let generated_as = None in
  let cols =
    [ Row.
        { name = "x"
        ; ty = Integer
        ; not_null = false
        ; primary_key = false
        ; pk_desc = false
        ; default
        ; check_sql
        ; generated_as
        }
    ]
  in
  let store = S.create () in
  let tid = 17 in
  let* loaded = S.with_ro store (fun tx -> Persist.load tx tid cols) in
  assert (loaded = None);
  Lwt.return_unit
;;

let test_persist_via_db_commit () =
  (* Verify the commit path does not error when persisting columnar stores. *)
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (a INTEGER, b TEXT) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO t VALUES (10, 'hello'), (20, 'world')" in
  let* rows = query db "SELECT COUNT(*) FROM t" in
  match rows with
  | [ [| Row.V_int 2L |] ] -> Lwt.return_unit
  | _ -> assert false
;;

(* Cross-store JOIN tests *)

let test_join_row_col_inner () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE row_t (id INTEGER PRIMARY KEY, val TEXT)" in
  let* () = exec db "INSERT INTO row_t VALUES (1, 'a'), (2, 'b'), (3, 'c')" in
  let* () = exec db "CREATE TABLE col_t (id INTEGER, amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_t VALUES (1, 10.0), (2, 20.0), (1, 5.0)" in
  let* rows =
    query
      db
      "SELECT row_t.val, col_t.amount FROM row_t JOIN col_t ON row_t.id = col_t.id ORDER \
       BY row_t.id, col_t.amount"
  in
  (match rows with
   | [ [| Row.V_text "a"; Row.V_real 5.0 |]
     ; [| Row.V_text "a"; Row.V_real 10.0 |]
     ; [| Row.V_text "b"; Row.V_real 20.0 |]
     ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_join_col_row_inner () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE col_t (id INTEGER, amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_t VALUES (1, 10.0), (2, 20.0)" in
  let* () = exec db "CREATE TABLE row_t (id INTEGER PRIMARY KEY, val TEXT)" in
  let* () = exec db "INSERT INTO row_t VALUES (1, 'a'), (2, 'b'), (3, 'c')" in
  let* rows =
    query
      db
      "SELECT col_t.amount, row_t.val FROM col_t JOIN row_t ON col_t.id = row_t.id ORDER \
       BY col_t.id"
  in
  (match rows with
   | [ [| Row.V_real 10.0; Row.V_text "a" |]; [| Row.V_real 20.0; Row.V_text "b" |] ] ->
     ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_join_col_col_inner () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE col_a (id INTEGER, val TEXT) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_a VALUES (1, 'a'), (2, 'b')" in
  let* () = exec db "CREATE TABLE col_b (id INTEGER, score REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_b VALUES (1, 10.0), (2, 20.0), (1, 5.0)" in
  let* rows =
    query
      db
      "SELECT col_a.val, col_b.score FROM col_a JOIN col_b ON col_a.id = col_b.id ORDER \
       BY col_a.id, col_b.score"
  in
  (match rows with
   | [ [| Row.V_text "a"; Row.V_real 5.0 |]
     ; [| Row.V_text "a"; Row.V_real 10.0 |]
     ; [| Row.V_text "b"; Row.V_real 20.0 |]
     ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_join_row_col_left () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE row_t (id INTEGER PRIMARY KEY, val TEXT)" in
  let* () = exec db "INSERT INTO row_t VALUES (1, 'a'), (2, 'b'), (3, 'c')" in
  let* () = exec db "CREATE TABLE col_t (id INTEGER, amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_t VALUES (1, 10.0), (2, 20.0)" in
  let* rows =
    query
      db
      "SELECT row_t.val, col_t.amount FROM row_t LEFT JOIN col_t ON row_t.id = col_t.id \
       ORDER BY row_t.id"
  in
  (match rows with
   | [ [| Row.V_text "a"; Row.V_real 10.0 |]
     ; [| Row.V_text "b"; Row.V_real 20.0 |]
     ; [| Row.V_text "c"; Row.V_null |]
     ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_join_col_row_left () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE col_t (id INTEGER, amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_t VALUES (1, 10.0), (2, 20.0), (4, 30.0)" in
  let* () = exec db "CREATE TABLE row_t (id INTEGER PRIMARY KEY, val TEXT)" in
  let* () = exec db "INSERT INTO row_t VALUES (1, 'a'), (2, 'b'), (3, 'c')" in
  let* rows =
    query
      db
      "SELECT col_t.amount, row_t.val FROM col_t LEFT JOIN row_t ON col_t.id = row_t.id \
       ORDER BY col_t.id"
  in
  (match rows with
   | [ [| Row.V_real 10.0; Row.V_text "a" |]
     ; [| Row.V_real 20.0; Row.V_text "b" |]
     ; [| Row.V_real 30.0; Row.V_null |]
     ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_join_col_col_left () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE col_a (id INTEGER, val TEXT) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_a VALUES (1, 'a'), (3, 'c')" in
  let* () = exec db "CREATE TABLE col_b (id INTEGER, score REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_b VALUES (1, 10.0), (2, 20.0)" in
  let* rows =
    query
      db
      "SELECT col_a.val, col_b.score FROM col_a LEFT JOIN col_b ON col_a.id = col_b.id \
       ORDER BY col_a.id"
  in
  (match rows with
   | [ [| Row.V_text "a"; Row.V_real 10.0 |]; [| Row.V_text "c"; Row.V_null |] ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_join_agg_count () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE row_t (id INTEGER PRIMARY KEY, region TEXT)" in
  let* () = exec db "INSERT INTO row_t VALUES (1, 'north'), (2, 'south')" in
  let* () = exec db "CREATE TABLE col_t (id INTEGER, amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_t VALUES (1, 10.0), (2, 20.0), (1, 5.0)" in
  let* rows = query db "SELECT COUNT(*) FROM row_t JOIN col_t ON row_t.id = col_t.id" in
  (match rows with
   | [ [| Row.V_int 3L |] ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_join_agg_group_by () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE row_t (id INTEGER PRIMARY KEY, region TEXT)" in
  let* () = exec db "INSERT INTO row_t VALUES (1, 'north'), (2, 'south')" in
  let* () = exec db "CREATE TABLE col_t (id INTEGER, amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_t VALUES (1, 10.0), (2, 20.0), (1, 5.0)" in
  let* rows =
    query
      db
      "SELECT row_t.region, SUM(col_t.amount) FROM row_t JOIN col_t ON row_t.id = \
       col_t.id GROUP BY row_t.region ORDER BY row_t.region"
  in
  (match rows with
   | [ [| Row.V_text "north"; Row.V_real 15.0 |]
     ; [| Row.V_text "south"; Row.V_real 20.0 |]
     ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let test_join_non_eq_on () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE row_t (id INTEGER PRIMARY KEY, val TEXT)" in
  let* () = exec db "INSERT INTO row_t VALUES (1, 'a'), (2, 'b'), (3, 'c')" in
  let* () = exec db "CREATE TABLE col_t (id INTEGER, amount REAL) USING COLUMNSTORE" in
  let* () = exec db "INSERT INTO col_t VALUES (1, 10.0), (2, 20.0)" in
  let* rows =
    query db "SELECT row_t.val, col_t.amount FROM row_t JOIN col_t ON row_t.id < col_t.id"
  in
  (match rows with
   | [ [| Row.V_text "a"; Row.V_real 20.0 |] ] -> ()
   | _ -> assert false);
  Lwt.return_unit
;;

let () =
  let tests =
    [ "create_columnstore", test_create_columnstore
    ; "create_duplicate_fails", test_create_duplicate_fails
    ; "insert_values", test_insert_values
    ; "insert_select", test_insert_select
    ; "sum", test_sum
    ; "count", test_count
    ; "min_max", test_min_max
    ; "group_by", test_group_by
    ; "insert_returning_rejected", test_insert_returning_rejected
    ; "update_returning_rejected", test_update_returning_rejected
    ; "delete_returning_rejected", test_delete_returning_rejected
    ; "alter_table_rejected", test_alter_table_rejected
    ; "fk_on_columnar_parent_rejected", test_fk_on_columnar_parent_rejected
    ; ("serialize_col_int", fun () -> Lwt.return (test_serialize_col_int ()))
    ; ("serialize_col_real", fun () -> Lwt.return (test_serialize_col_real ()))
    ; ("serialize_col_text", fun () -> Lwt.return (test_serialize_col_text ()))
    ; ("serialize_col_blob", fun () -> Lwt.return (test_serialize_col_blob ()))
    ; ( "serialize_store_roundtrip"
      , fun () -> Lwt.return (test_serialize_store_roundtrip ()) )
    ; "persist_roundtrip", test_persist_roundtrip
    ; "persist_fresh_table_returns_none", test_persist_fresh_table_returns_none
    ; "persist_via_db_commit", test_persist_via_db_commit
    ; "join_row_col_inner", test_join_row_col_inner
    ; "join_col_row_inner", test_join_col_row_inner
    ; "join_col_col_inner", test_join_col_col_inner
    ; "join_row_col_left", test_join_row_col_left
    ; "join_col_row_left", test_join_col_row_left
    ; "join_col_col_left", test_join_col_col_left
    ; "join_agg_count", test_join_agg_count
    ; "join_agg_group_by", test_join_agg_group_by
    ; "join_non_eq_on", test_join_non_eq_on
    ]
  in
  List.iter
    (fun (name, test) ->
       Printf.printf "%-45s" (name ^ "...");
       flush stdout;
       Lwt_main.run (test ());
       print_endline "PASS")
    tests
;;
