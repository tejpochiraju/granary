(** Multi-backend SQL conformance: Mem + Unix_file + Mirage_block produce identical results. *)

open Lwt.Syntax

module DB = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
end

module MB = Granary_mirage_block.Mirage_backend.Make (Block)

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let tmp_file () =
  let path = Filename.temp_file "granary_backend_test" ".raw" in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.ftruncate fd (4 * 1024 * 1024);
  Unix.close fd;
  path
;;

let rows_of_stream s = Lwt_stream.to_list s

let error_msg = function
  | DB.Parse s -> s
  | DB.Runtime s -> s
  | DB.Sema _ -> "sema error"
  | e -> Format.asprintf "%a" DB.pp_error e
;;

let execute_ok db sql =
  let* r = DB.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "execute failed (%s): %s" sql (error_msg e)
;;

let query_rows db sql =
  let* r = DB.query db sql in
  match r with
  | Error e -> Alcotest.failf "query failed (%s): %s" sql (error_msg e)
  | Ok stream -> rows_of_stream stream
;;

(* ------------------------------------------------------------------ *)
(* Shared SQL scenario                                                  *)
(* ------------------------------------------------------------------ *)

(* Returns all rows from a sequence of SQL operations:
   CREATE TABLE, INSERT 3, UPDATE 1, DELETE 1, txn rollback of row 99,
   then SELECT remaining ORDER BY id.
   Expected result: rows (1,"one") and (2,"TWO"); row 99 absent. *)
let run_scenario db =
  let* () = execute_ok db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT NOT NULL)" in
  let* () = execute_ok db "INSERT INTO t (id, v) VALUES (1, 'one')" in
  let* () = execute_ok db "INSERT INTO t (id, v) VALUES (2, 'two')" in
  let* () = execute_ok db "INSERT INTO t (id, v) VALUES (3, 'three')" in
  let* () = execute_ok db "UPDATE t SET v = 'TWO' WHERE id = 2" in
  let* () = execute_ok db "DELETE FROM t WHERE id = 3" in
  let* () = execute_ok db "BEGIN" in
  let* () = execute_ok db "INSERT INTO t (id, v) VALUES (99, 'ghost')" in
  let* () = execute_ok db "ROLLBACK" in
  query_rows db "SELECT id, v FROM t ORDER BY id"
;;

let rows_to_strings rows =
  List.map
    (fun row ->
       Array.to_list row
       |> List.map (function
         | DB.V_int n -> Int64.to_string n
         | DB.V_text s -> s
         | DB.V_null -> "NULL"
         | DB.V_real f -> string_of_float f
         | DB.V_blob b -> Printf.sprintf "blob(%d)" (Bytes.length b))
       |> String.concat ",")
    rows
;;

let expected = [ "1,one"; "2,TWO" ]

(* ------------------------------------------------------------------ *)
(* Backend wrappers                                                     *)
(* ------------------------------------------------------------------ *)

let with_mem_db f =
  let* db = DB.open_in_memory () in
  Lwt.finalize (fun () -> f db) (fun () -> DB.close db)
;;

let with_file_db path f =
  let* r = DB.open_file ~path () in
  match r with
  | Error e -> Alcotest.failf "open_file failed: %s" (error_msg e)
  | Ok db -> Lwt.finalize (fun () -> f db) (fun () -> DB.close db)
;;

let with_mirage_db path f =
  let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
  let* adapter = MB.connect dev in
  let* r =
    DB.open_block
      ~read_page:(MB.read_page adapter)
      ~write_page:(MB.write_page adapter)
      ~sync:(MB.sync adapter)
      ~resize:(MB.resize adapter)
      ~n_pages:(MB.n_pages adapter)
      ~close:(fun () -> MB.close adapter)
      ()
  in
  match r with
  | Error e -> Alcotest.failf "open_block failed: %s" (error_msg e)
  | Ok db -> Lwt.finalize (fun () -> f db) (fun () -> DB.close db)
;;

(* ------------------------------------------------------------------ *)
(* Tests                                                                *)
(* ------------------------------------------------------------------ *)

let test_mem_backend () =
  let rows = Lwt_main.run (with_mem_db run_scenario) in
  Alcotest.(check (list string)) "mem results" expected (rows_to_strings rows)
;;

let test_unix_file_backend () =
  let path = Filename.temp_file "granary_cf_unix" ".db" in
  Fun.protect
    ~finally:(fun () ->
      try Unix.unlink path with
      | _ -> ())
    (fun () ->
       let rows = Lwt_main.run (with_file_db path run_scenario) in
       Alcotest.(check (list string)) "unix_file results" expected (rows_to_strings rows))
;;

let test_mirage_backend () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () ->
      try Unix.unlink path with
      | _ -> ())
    (fun () ->
       let rows = Lwt_main.run (with_mirage_db path run_scenario) in
       Alcotest.(check (list string))
         "mirage_block results"
         expected
         (rows_to_strings rows))
;;

let test_all_identical () =
  let mem_rows = Lwt_main.run (with_mem_db run_scenario) in
  let file_path = Filename.temp_file "granary_all_unix" ".db" in
  let mb_path = tmp_file () in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.unlink file_path with
       | _ -> ());
      try Unix.unlink mb_path with
      | _ -> ())
    (fun () ->
       let file_rows = Lwt_main.run (with_file_db file_path run_scenario) in
       let mb_rows = Lwt_main.run (with_mirage_db mb_path run_scenario) in
       let mem_s = rows_to_strings mem_rows in
       let file_s = rows_to_strings file_rows in
       let mb_s = rows_to_strings mb_rows in
       Alcotest.(check (list string)) "mem = expected" expected mem_s;
       Alcotest.(check (list string)) "file = expected" expected file_s;
       Alcotest.(check (list string)) "mirage = expected" expected mb_s;
       Alcotest.(check (list string)) "mem = file" mem_s file_s;
       Alcotest.(check (list string)) "mem = mirage" mem_s mb_s)
;;

let test_mirage_reopen_persists () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () ->
      try Unix.unlink path with
      | _ -> ())
    (fun () ->
       Lwt_main.run
         (with_mirage_db path (fun db ->
            let* () = execute_ok db "CREATE TABLE persist (x INTEGER PRIMARY KEY)" in
            execute_ok db "INSERT INTO persist (x) VALUES (42)"));
       let rows =
         Lwt_main.run
           (with_mirage_db path (fun db -> query_rows db "SELECT x FROM persist"))
       in
       Alcotest.(check (list string))
         "persisted across reopen"
         [ "42" ]
         (rows_to_strings rows))
;;

let () =
  let open Alcotest in
  run
    "all_backends"
    [ ( "conformance"
      , [ test_case "mem_backend" `Quick test_mem_backend
        ; test_case "unix_file_backend" `Quick test_unix_file_backend
        ; test_case "mirage_backend" `Quick test_mirage_backend
        ; test_case "all_identical" `Quick test_all_identical
        ; test_case "mirage_reopen_persists" `Quick test_mirage_reopen_persists
        ] )
    ]
;;
