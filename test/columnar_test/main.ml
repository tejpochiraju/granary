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
