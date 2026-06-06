open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file = Sqlocaml_unix.Store.open_file
end

module C = Sqlocaml_catalog.Catalog
module Row = Sqlocaml_encoding.Row
module Varint = Sqlocaml_encoding.Varint
module SF = Sqlocaml_encoding.Schema_fingerprint
module Rowid = Sqlocaml_encoding.Rowid

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let run = Lwt_main.run

let mk_col name ty : Row.column =
  { name
  ; ty
  ; not_null = false
  ; primary_key = false
  ; default = None
  ; check_sql = None
  ; generated_as = None
  }
;;

let int_col name = mk_col name Row.Integer
let txt_col name = mk_col name Row.Text

(* ------------------------------------------------------------------ *)
(* Group 1: Basic create and find                                       *)
(* ------------------------------------------------------------------ *)

let test_create_then_find () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let cols = [ int_col "id"; txt_col "name" ] in
     let* tid =
       C.create_table
         cat
         ~name:"users"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result = C.find_table cat ~name:"users" in
     (match result with
      | None -> Alcotest.fail "expected Some, got None"
      | Some m ->
        Alcotest.(check int) "tree_id" tid m.tree_id;
        Alcotest.(check int) "column count" 2 (List.length m.columns));
     Lwt.return_unit)
;;

let test_missing_table () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* result = C.find_table cat ~name:"ghost" in
     Alcotest.(
       check
         (option
            (Alcotest.testable
               (fun ppf m -> Format.fprintf ppf "%s" m.C.name)
               (fun a b -> a.C.name = b.C.name))))
       "missing table"
       None
       result;
     Lwt.return_unit)
;;

let test_create_assigns_tree_id_16 () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* tid =
       C.create_table
         cat
         ~name:"first"
         ~columns:[ int_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     Alcotest.(check int) "first tree_id is 16" 16 tid;
     Lwt.return_unit)
;;

let test_create_assigns_sequential_ids () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* tid_a =
       C.create_table
         cat
         ~name:"a"
         ~columns:[ int_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* tid_b =
       C.create_table
         cat
         ~name:"b"
         ~columns:[ int_col "y" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     Alcotest.(check int) "a gets 16" 16 tid_a;
     Alcotest.(check int) "b gets 17" 17 tid_b;
     Lwt.return_unit)
;;

let test_duplicate_table () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"users"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     (* failwith is synchronous — raised before any Lwt.t is constructed *)
     (try
        ignore
          (C.create_table
             cat
             ~name:"users"
             ~columns:[ int_col "id" ]
             ~without_rowid:false
             ~autoincrement:false);
        Alcotest.fail "expected Failure for duplicate"
      with
      | Failure _ -> ());
     Lwt.return_unit)
;;

let test_list_tables_empty () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* tables = C.list_tables cat in
     Alcotest.(check int) "empty catalog has 0 tables" 0 (List.length tables);
     Lwt.return_unit)
;;

let test_list_tables_one () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t1"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* tables = C.list_tables cat in
     Alcotest.(check int) "one table" 1 (List.length tables);
     Lwt.return_unit)
;;

let test_list_tables_two () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t1"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_table
         cat
         ~name:"t2"
         ~columns:[ txt_col "name" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* tables = C.list_tables cat in
     Alcotest.(check int) "two tables" 2 (List.length tables);
     let names = List.map (fun m -> m.C.name) tables |> List.sort String.compare in
     Alcotest.(check (list string)) "table names" [ "t1"; "t2" ] names;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Group 2: Column metadata                                             *)
(* ------------------------------------------------------------------ *)

let test_columns_preserved () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let cols = [ int_col "id"; txt_col "name" ] in
     let* _ =
       C.create_table
         cat
         ~name:"users"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result = C.find_table cat ~name:"users" in
     (match result with
      | None -> Alcotest.fail "expected Some"
      | Some m ->
        let got_names = List.map (fun c -> c.Row.name) m.C.columns in
        Alcotest.(check (list string)) "column names preserved" [ "id"; "name" ] got_names);
     Lwt.return_unit)
;;

let test_integer_column_type () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result = C.find_table cat ~name:"t" in
     (match result with
      | None -> Alcotest.fail "expected Some"
      | Some m ->
        let col = List.hd m.C.columns in
        (match col.Row.ty with
         | Row.Integer -> ()
         | _ -> Alcotest.fail "expected Integer, got other"));
     Lwt.return_unit)
;;

let test_text_column_type () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ txt_col "s" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result = C.find_table cat ~name:"t" in
     (match result with
      | None -> Alcotest.fail "expected Some"
      | Some m ->
        let col = List.hd m.C.columns in
        (match col.Row.ty with
         | Row.Text -> ()
         | _ -> Alcotest.fail "expected Text, got other"));
     Lwt.return_unit)
;;

let test_real_column_type () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ mk_col "r" Row.Real ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result = C.find_table cat ~name:"t" in
     (match result with
      | None -> Alcotest.fail "expected Some"
      | Some m ->
        let col = List.hd m.C.columns in
        (match col.Row.ty with
         | Row.Real -> ()
         | _ -> Alcotest.fail "expected Real, got other"));
     Lwt.return_unit)
;;

let test_blob_column_type () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ mk_col "b" Row.Blob ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result = C.find_table cat ~name:"t" in
     (match result with
      | None -> Alcotest.fail "expected Some"
      | Some m ->
        let col = List.hd m.C.columns in
        (match col.Row.ty with
         | Row.Blob -> ()
         | _ -> Alcotest.fail "expected Blob, got other"));
     Lwt.return_unit)
;;

(* Exercise Real and Blob types in a round-trip including reopen so the
   column types are run through encode/decode (hitting the Real/Blob arms
   in type_of_tag). *)
let test_mixed_column_types_roundtrip () =
  let path = Printf.sprintf "/tmp/sqlocaml_test_catalog_mixed_%d.db" (Random.bits ()) in
  (try Unix.unlink path with
   | _ -> ());
  Fun.protect
    ~finally:(fun () ->
      try Unix.unlink path with
      | _ -> ())
    (fun () ->
       run
         (let* sr = S.open_file ~path () in
          let store =
            match sr with
            | Ok s -> s
            | Error e -> Alcotest.failf "open_file: %a" S.pp_error e
          in
          let* cat = C.open_ store in
          let cols =
            [ mk_col "i" Row.Integer
            ; mk_col "t" Row.Text
            ; mk_col "r" Row.Real
            ; mk_col "b" Row.Blob
            ]
          in
          let* _ =
            C.create_table
              cat
              ~name:"mixed"
              ~columns:cols
              ~without_rowid:false
              ~autoincrement:false
          in
          let* () = S.close store in
          (* Reopen and force the catalog to decode columns from disk *)
          let* sr2 = S.open_file ~path () in
          let store2 =
            match sr2 with
            | Ok s -> s
            | Error e -> Alcotest.failf "reopen: %a" S.pp_error e
          in
          let* cat2 = C.open_ store2 in
          let* result = C.find_table cat2 ~name:"mixed" in
          (match result with
           | None -> Alcotest.fail "expected Some"
           | Some m ->
             let tys = List.map (fun c -> c.Row.ty) m.C.columns in
             Alcotest.(check bool) "Integer present" true (List.mem Row.Integer tys);
             Alcotest.(check bool) "Text present" true (List.mem Row.Text tys);
             Alcotest.(check bool) "Real present" true (List.mem Row.Real tys);
             Alcotest.(check bool) "Blob present" true (List.mem Row.Blob tys));
          S.close store2))
;;

let test_single_column () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "only" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result = C.find_table cat ~name:"t" in
     (match result with
      | None -> Alcotest.fail "expected Some"
      | Some m ->
        Alcotest.(check int) "exactly 1 column" 1 (List.length m.C.columns);
        Alcotest.(check string) "column name" "only" (List.hd m.C.columns).Row.name);
     Lwt.return_unit)
;;

let test_many_columns () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let cols =
       [ int_col "c0"
       ; txt_col "c1"
       ; int_col "c2"
       ; txt_col "c3"
       ; int_col "c4"
       ; txt_col "c5"
       ; int_col "c6"
       ; txt_col "c7"
       ; int_col "c8"
       ; txt_col "c9"
       ]
     in
     let* _ =
       C.create_table
         cat
         ~name:"wide"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result = C.find_table cat ~name:"wide" in
     (match result with
      | None -> Alcotest.fail "expected Some"
      | Some m ->
        Alcotest.(check int) "10 columns" 10 (List.length m.C.columns);
        List.iteri
          (fun i col ->
             let expected_name = Printf.sprintf "c%d" i in
             let expected_ty = if i mod 2 = 0 then Row.Integer else Row.Text in
             Alcotest.(check string)
               (Printf.sprintf "col %d name" i)
               expected_name
               col.Row.name;
             match col.Row.ty, expected_ty with
             | Row.Integer, Row.Integer | Row.Text, Row.Text -> ()
             | _ -> Alcotest.failf "col %d type mismatch" i)
          m.C.columns);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Group 3: next_rowid                                                  *)
(* ------------------------------------------------------------------ *)

let test_first_rowid_is_1 () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* rid = C.next_rowid cat ~name:"t" in
     Alcotest.(check int64) "first rowid is 1" 1L rid;
     Lwt.return_unit)
;;

let test_rowid_increments () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* r1 = C.next_rowid cat ~name:"t" in
     let* r2 = C.next_rowid cat ~name:"t" in
     let* r3 = C.next_rowid cat ~name:"t" in
     Alcotest.(check int64) "rowid 1" 1L r1;
     Alcotest.(check int64) "rowid 2" 2L r2;
     Alcotest.(check int64) "rowid 3" 3L r3;
     Lwt.return_unit)
;;

let test_rowids_independent () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"a"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_table
         cat
         ~name:"b"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* ra1 = C.next_rowid cat ~name:"a" in
     let* rb1 = C.next_rowid cat ~name:"b" in
     let* ra2 = C.next_rowid cat ~name:"a" in
     Alcotest.(check int64) "a first rowid" 1L ra1;
     Alcotest.(check int64) "b first rowid" 1L rb1;
     Alcotest.(check int64) "a second rowid" 2L ra2;
     Lwt.return_unit)
;;

let test_rowid_unknown_table () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     (* failwith is synchronous — raised before any Lwt.t is constructed *)
     (try
        ignore (C.next_rowid cat ~name:"nonexistent");
        Alcotest.fail "expected Failure for unknown table"
      with
      | Failure _ -> ());
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Group 4: Persistence across open_                                   *)
(* ------------------------------------------------------------------ *)

let test_metadata_survives_reopen () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _ =
       C.create_table
         cat1
         ~name:"users"
         ~columns:[ int_col "id"; txt_col "name" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     (* Open a second catalog on the same store *)
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"users" in
     (match result with
      | None -> Alcotest.fail "table not found after reopen"
      | Some m ->
        Alcotest.(check string) "name preserved" "users" m.C.name;
        Alcotest.(check int) "tree_id preserved" 16 m.C.tree_id);
     Lwt.return_unit)
;;

let test_columns_survive_reopen () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let cols = [ int_col "id"; txt_col "email"; int_col "age" ] in
     let* _ =
       C.create_table
         cat1
         ~name:"persons"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"persons" in
     (match result with
      | None -> Alcotest.fail "table not found after reopen"
      | Some m ->
        Alcotest.(check int) "column count after reopen" 3 (List.length m.C.columns);
        let names = List.map (fun c -> c.Row.name) m.C.columns in
        Alcotest.(check (list string))
          "column names after reopen"
          [ "id"; "email"; "age" ]
          names;
        let types = List.map (fun c -> c.Row.ty) m.C.columns in
        (match types with
         | [ Row.Integer; Row.Text; Row.Integer ] -> ()
         | _ -> Alcotest.fail "column types wrong after reopen"));
     Lwt.return_unit)
;;

let test_rowid_survives_reopen () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ = C.next_rowid cat1 ~name:"t" in
     let* _ = C.next_rowid cat1 ~name:"t" in
     let* _ = C.next_rowid cat1 ~name:"t" in
     (* After 3 calls, next should be 4 *)
     let* cat2 = C.open_ store in
     let* r = C.next_rowid cat2 ~name:"t" in
     Alcotest.(check int64) "rowid continues from 4 after reopen" 4L r;
     Lwt.return_unit)
;;

let test_tree_id_survives_reopen () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* tid1 =
       C.create_table
         cat1
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"t" in
     (match result with
      | None -> Alcotest.fail "table not found after reopen"
      | Some m -> Alcotest.(check int) "tree_id survives reopen" tid1 m.C.tree_id);
     Lwt.return_unit)
;;

(* Gap-fill: DV_blob / DV_null DEFAULT values round-trip through reopen.
   Exercises encode_default_value and decode_default_value branches. *)
let test_default_blob_roundtrip () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let cols : Row.column list =
       [ { Row.name = "n"
         ; ty = Row.Integer
         ; not_null = false
         ; primary_key = false
         ; default = None
         ; check_sql = None
         ; generated_as = None
         }
       ; { Row.name = "b"
         ; ty = Row.Blob
         ; not_null = false
         ; primary_key = false
         ; default = Some (Row.DV_blob (Bytes.of_string "binary"))
         ; check_sql = None
         ; generated_as = None
         }
       ]
     in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"t" in
     (match result with
      | None -> Alcotest.fail "table not found after reopen"
      | Some m ->
        let col_b = List.nth m.C.columns 1 in
        (match col_b.Row.default with
         | Some (Row.DV_blob b) when Bytes.equal b (Bytes.of_string "binary") -> ()
         | Some _ -> Alcotest.fail "wrong default value variant"
         | None -> Alcotest.fail "default lost"));
     Lwt.return_unit)
;;

let test_default_real_roundtrip () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let cols : Row.column list =
       [ { Row.name = "n"
         ; ty = Row.Integer
         ; not_null = false
         ; primary_key = false
         ; default = None
         ; check_sql = None
         ; generated_as = None
         }
       ; { Row.name = "f"
         ; ty = Row.Real
         ; not_null = false
         ; primary_key = false
         ; default = Some (Row.DV_real 3.14159)
         ; check_sql = None
         ; generated_as = None
         }
       ]
     in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"t" in
     (match result with
      | None -> Alcotest.fail "table not found after reopen"
      | Some m ->
        let col_f = List.nth m.C.columns 1 in
        (match col_f.Row.default with
         | Some (Row.DV_real x) when Float.equal x 3.14159 -> ()
         | Some _ -> Alcotest.fail "wrong default value variant"
         | None -> Alcotest.fail "default lost"));
     Lwt.return_unit)
;;

let test_default_text_roundtrip () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let cols : Row.column list =
       [ { Row.name = "n"
         ; ty = Row.Integer
         ; not_null = false
         ; primary_key = false
         ; default = None
         ; check_sql = None
         ; generated_as = None
         }
       ; { Row.name = "s"
         ; ty = Row.Text
         ; not_null = false
         ; primary_key = false
         ; default = Some (Row.DV_text "hello")
         ; check_sql = None
         ; generated_as = None
         }
       ]
     in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"t" in
     (match result with
      | None -> Alcotest.fail "table not found after reopen"
      | Some m ->
        let col_s = List.nth m.C.columns 1 in
        (match col_s.Row.default with
         | Some (Row.DV_text "hello") -> ()
         | Some _ -> Alcotest.fail "wrong default value variant"
         | None -> Alcotest.fail "default lost"));
     Lwt.return_unit)
;;

let test_default_null_roundtrip () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let cols : Row.column list =
       [ { Row.name = "n"
         ; ty = Row.Integer
         ; not_null = false
         ; primary_key = false
         ; default = Some Row.DV_null
         ; check_sql = None
         ; generated_as = None
         }
       ]
     in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"t" in
     (match result with
      | None -> Alcotest.fail "table not found after reopen"
      | Some m ->
        let col = List.nth m.C.columns 0 in
        (match col.Row.default with
         | Some Row.DV_null -> ()
         | Some _ -> Alcotest.fail "wrong default value variant"
         | None -> Alcotest.fail "default lost"));
     Lwt.return_unit)
;;

(* Gap-fill: decode_default_value with corrupt tag (line 141).
   Inject a column entry with has_default=1 but tag=99 (unknown), so
   load_columns hits the failwith branch. *)
let corrupt_default_tag () =
  let store = S.create () in
  run
    (let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:
           [ { Row.name = "x"
             ; ty = Row.Integer
             ; not_null = false
             ; primary_key = false
             ; default = None
             ; check_sql = None
             ; generated_as = None
             }
           ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let col_key =
       let tn = Bytes.of_string "t" in
       let ord = Bytes.make 8 '\x00' in
       Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord
     in
     let bad_val =
       let buf = Buffer.create 16 in
       let v = Sqlocaml_encoding.Varint.encode_uint64 in
       v buf 1L;
       (* type tag = INTEGER *)
       v buf 1L;
       (* name len = 1 *)
       Buffer.add_string buf "x";
       v buf 0L;
       (* not_null = 0 *)
       v buf 0L;
       (* primary_key = 0 *)
       v buf 1L;
       (* has_default = 1 *)
       v buf 99L;
       (* default tag = 99 (invalid) *)
       Buffer.to_bytes buf
     in
     let* tx = S.rw_begin store in
     let* () = S.put tx 1 col_key bad_val in
     (* sys_columns_tid = 1 *)
     S.commit tx);
  (* #174: the corrupt primary column entry is recovered from the mirror. *)
  let cat2 = Lwt_main.run (C.open_ store) in
  match Lwt_main.run (C.find_table cat2 ~name:"t") with
  | None ->
    Alcotest.fail "t should be recovered from the mirror after default-tag corruption"
  | Some m ->
    Alcotest.(check int) "recovered column count" 1 (List.length m.C.columns);
    let c = List.nth m.C.columns 0 in
    Alcotest.(check bool) "recovered default is None" true (c.Row.default = None)
;;

(* #299: the AUTOINCREMENT flag round-trips through the redundant mirror (v2).
   Delete the primary _sys_tables row so the table is reconstructed solely from
   the mirror on reopen, then assert the reconstructed meta still carries
   [autoincrement = true].  (Per #299's mirror limitation, the volatile rowid
   COUNTER is recomputed from data here — only the flag is mirror-persisted.) *)
let mirror_preserves_autoincrement () =
  let store = S.create () in
  run
    (let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:
           [ { Row.name = "a"
             ; ty = Row.Integer
             ; not_null = false
             ; primary_key = true
             ; default = None
             ; check_sql = None
             ; generated_as = None
             }
           ]
         ~without_rowid:false
         ~autoincrement:true
     in
     (* Drop the primary _sys_tables row (tid 0, key "t") so the next open
        cannot load it from the primary and must reconstruct from the mirror. *)
     let* tx = S.rw_begin store in
     let* () = S.del tx 0 (Bytes.of_string "t") in
     S.commit tx);
  let cat2 = Lwt_main.run (C.open_ store) in
  match Lwt_main.run (C.find_table cat2 ~name:"t") with
  | None -> Alcotest.fail "t should be reconstructed from the mirror"
  | Some m ->
    Alcotest.(check bool)
      "mirror-reconstructed table keeps autoincrement=true"
      true
      m.C.autoincrement
;;

let test_default_int_roundtrip () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let cols : Row.column list =
       [ { Row.name = "n"
         ; ty = Row.Integer
         ; not_null = false
         ; primary_key = false
         ; default = Some (Row.DV_int 12345L)
         ; check_sql = None
         ; generated_as = None
         }
       ]
     in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"t" in
     (match result with
      | None -> Alcotest.fail "table not found after reopen"
      | Some m ->
        let col = List.nth m.C.columns 0 in
        (match col.Row.default with
         | Some (Row.DV_int 12345L) -> ()
         | Some _ -> Alcotest.fail "wrong default value variant"
         | None -> Alcotest.fail "default lost"));
     Lwt.return_unit)
;;

(* Targeted test: verify load_all does NOT skip the first alphabetical table.
   "aardvark" sorts before "zebra" — after reopen, both must be found. *)
let test_load_all_first_table_not_skipped () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _ =
       C.create_table
         cat1
         ~name:"aardvark"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_table
         cat1
         ~name:"zebra"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     (* Open a fresh catalog — this calls load_all which walks the cursor *)
     let* cat2 = C.open_ store in
     let* r_a = C.find_table cat2 ~name:"aardvark" in
     let* r_z = C.find_table cat2 ~name:"zebra" in
     (match r_a with
      | None ->
        Alcotest.fail
          "aardvark (first alphabetically) not found after reopen — load_all skipped it"
      | Some _ -> ());
     (match r_z with
      | None -> Alcotest.fail "zebra not found after reopen"
      | Some _ -> ());
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Group 5: Error conditions                                            *)
(* ------------------------------------------------------------------ *)

let test_create_empty_name () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     (* Empty name should succeed — no restriction in spec *)
     let* tid =
       C.create_table
         cat
         ~name:""
         ~columns:[ int_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     Alcotest.(check int) "empty name gets tree_id 16" 16 tid;
     let* result = C.find_table cat ~name:"" in
     (match result with
      | None -> Alcotest.fail "expected Some for empty name table"
      | Some m -> Alcotest.(check string) "empty name preserved" "" m.C.name);
     Lwt.return_unit)
;;

let test_create_empty_columns () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     (* Zero columns should succeed — no column requirement in spec *)
     let* tid =
       C.create_table
         cat
         ~name:"nocols"
         ~columns:[]
         ~without_rowid:false
         ~autoincrement:false
     in
     Alcotest.(check int) "zero-col table gets tree_id 16" 16 tid;
     let* result = C.find_table cat ~name:"nocols" in
     (match result with
      | None -> Alcotest.fail "expected Some for zero-col table"
      | Some m ->
        Alcotest.(check int) "zero columns preserved" 0 (List.length m.C.columns));
     Lwt.return_unit)
;;

let corrupt_column_type_tag () =
  (* Setup: create a store with a real table, then corrupt the column type tag *)
  let store = S.create () in
  run
    (let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"users"
         ~columns:
           [ { Row.name = "id"
             ; ty = Row.Integer
             ; not_null = false
             ; primary_key = false
             ; default = None
             ; check_sql = None
             ; generated_as = None
             }
           ]
         ~without_rowid:false
         ~autoincrement:false
     in
     (* Overwrite the column entry with a corrupt type tag (0) *)
     let col_key =
       let tn = Bytes.of_string "users" in
       let ord = Bytes.make 8 '\x00' in
       (* ordinal 0, big-endian *)
       Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord
     in
     let bad_val =
       let buf = Buffer.create 8 in
       Sqlocaml_encoding.Varint.encode_uint64 buf 0L;
       (* tag=0, invalid *)
       Sqlocaml_encoding.Varint.encode_uint64 buf 2L;
       (* name len *)
       Buffer.add_string buf "id";
       Buffer.to_bytes buf
     in
     let* tx = S.rw_begin store in
     let* () = S.put tx 1 col_key bad_val in
     (* sys_columns_tid = 1 *)
     S.commit tx);
  (* With the redundant mirror (#174), a corrupt primary column entry no longer
     crashes open_: the unreadable primary row is skipped and the table is
     transparently reconstructed from the mirror with its original schema. *)
  let cat2 = Lwt_main.run (C.open_ store) in
  match Lwt_main.run (C.find_table cat2 ~name:"users") with
  | None ->
    Alcotest.fail "users should be recovered from the mirror after column corruption"
  | Some m ->
    Alcotest.(check int) "recovered column count" 1 (List.length m.C.columns);
    let c = List.nth m.C.columns 0 in
    Alcotest.(check bool) "recovered column type is Integer" true (c.Row.ty = Row.Integer)
;;

(* ------------------------------------------------------------------ *)
(* Group 6: Indexes                                                     *)
(* ------------------------------------------------------------------ *)

let test_create_index_basic () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"users"
         ~columns:[ int_col "id"; txt_col "name" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* result =
       C.create_index
         cat
         ~name:"idx_users_id"
         ~table:"users"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (match result with
      | Error msg -> Alcotest.failf "expected Ok, got Error %s" msg
      | Ok info ->
        Alcotest.(check string) "index name" "idx_users_id" info.C.idx_name;
        Alcotest.(check string) "index table" "users" info.C.idx_table;
        Alcotest.(check (list string)) "index columns" [ "id" ] info.C.idx_columns;
        Alcotest.(check bool) "not unique" false info.C.idx_unique;
        (* Index gets a tree_id >= 16, separate from the table's *)
        Alcotest.(check bool) "tree_id >= 16" true (info.C.idx_tree_id >= 16));
     Lwt.return_unit)
;;

let test_indexes_for_table () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"users"
         ~columns:[ int_col "id"; txt_col "name" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let before = C.indexes_for_table cat ~table:"users" in
     Alcotest.(check int) "no indexes initially" 0 (List.length before);
     let* _ =
       C.create_index
         cat
         ~name:"idx_id"
         ~table:"users"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     let* _ =
       C.create_index
         cat
         ~name:"idx_name"
         ~table:"users"
         ~columns:[ "name" ]
         ~unique:true
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     let after = C.indexes_for_table cat ~table:"users" in
     Alcotest.(check int) "two indexes" 2 (List.length after);
     let names = List.map (fun i -> i.C.idx_name) after |> List.sort String.compare in
     Alcotest.(check (list string)) "index names" [ "idx_id"; "idx_name" ] names;
     Lwt.return_unit)
;;

let test_create_index_unknown_table () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* r =
       C.create_index
         cat
         ~name:"idx"
         ~table:"ghost"
         ~columns:[ "x" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (match r with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected Error for unknown table");
     Lwt.return_unit)
;;

let test_create_index_unknown_column () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* r =
       C.create_index
         cat
         ~name:"idx"
         ~table:"t"
         ~columns:[ "bogus" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (match r with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected Error for unknown column");
     Lwt.return_unit)
;;

let test_create_index_duplicate () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_index
         cat
         ~name:"idx"
         ~table:"t"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     let* r =
       C.create_index
         cat
         ~name:"idx"
         ~table:"t"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (match r with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected Error for duplicate index name");
     Lwt.return_unit)
;;

let test_index_persists_across_reopen () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _ =
       C.create_table
         cat1
         ~name:"users"
         ~columns:[ int_col "id"; txt_col "name" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_index
         cat1
         ~name:"idx_users_id"
         ~table:"users"
         ~columns:[ "id" ]
         ~unique:true
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (* Reopen *)
     let* cat2 = C.open_ store in
     let idxs = C.indexes_for_table cat2 ~table:"users" in
     Alcotest.(check int) "1 index after reopen" 1 (List.length idxs);
     let i = List.hd idxs in
     Alcotest.(check string) "name preserved" "idx_users_id" i.C.idx_name;
     Alcotest.(check string) "table preserved" "users" i.C.idx_table;
     Alcotest.(check (list string)) "columns preserved" [ "id" ] i.C.idx_columns;
     Alcotest.(check bool) "unique preserved" true i.C.idx_unique;
     Alcotest.(check bool) "origin preserved" true (i.C.idx_origin = `User);
     Lwt.return_unit)
;;

(* #273: the [idx_origin] field must survive the on-disk encode/decode round-trip
   for every variant — especially the implicit ones, which the logical dump uses
   to decide what a replayed [CREATE TABLE] already recreates.  A [`User] index
   that merely happens to be unique must NOT decode back as implicit. *)
let test_index_origin_persists_across_reopen () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:[ int_col "a"; int_col "b"; int_col "c" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let mk name col origin =
       let* r =
         C.create_index
           cat1
           ~name
           ~table:"t"
           ~columns:[ col ]
           ~unique:true
           ~expr_flags:[ false ]
           ~where_sql:None
           ~origin
       in
       match r with
       | Ok _ -> Lwt.return_unit
       | Error e -> Alcotest.failf "create_index %s: %s" name e
     in
     let* () = mk "i_pk" "a" `Implicit_pk in
     let* () = mk "i_uniq" "b" `Implicit_unique in
     let* () = mk "i_user" "c" `User in
     (* Reopen forces a decode from the persisted bytes. *)
     let* cat2 = C.open_ store in
     let origin_of name =
       match C.find_index cat2 ~name with
       | Some i -> i.C.idx_origin
       | None -> Alcotest.failf "index %s missing after reopen" name
     in
     Alcotest.(check bool) "implicit_pk round-trips" true (origin_of "i_pk" = `Implicit_pk);
     Alcotest.(check bool)
       "implicit_unique round-trips"
       true
       (origin_of "i_uniq" = `Implicit_unique);
     Alcotest.(check bool) "user round-trips" true (origin_of "i_user" = `User);
     Lwt.return_unit)
;;

let test_find_index () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     Alcotest.(check bool)
       "missing index returns None"
       true
       (C.find_index cat ~name:"idx" = None);
     let* _ =
       C.create_index
         cat
         ~name:"idx"
         ~table:"t"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (match C.find_index cat ~name:"idx" with
      | None -> Alcotest.fail "expected Some"
      | Some i ->
        Alcotest.(check string) "name" "idx" i.C.idx_name;
        Alcotest.(check (list string)) "columns" [ "id" ] i.C.idx_columns);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Group 7: drop_table and drop_index                                  *)
(* ------------------------------------------------------------------ *)

let test_drop_table_basic () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* tx = S.rw_begin store in
     let* () = C.drop_table cat tx ~name:"t" in
     let* () = S.commit tx in
     (* Table should be gone from in-memory cache *)
     let* result = C.find_table cat ~name:"t" in
     Alcotest.(check bool) "table gone after drop" true (Option.is_none result);
     Lwt.return_unit)
;;

let test_drop_table_removes_from_disk () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* tx = S.rw_begin store in
     let* () = C.drop_table cat1 tx ~name:"t" in
     let* () = S.commit tx in
     (* Re-open catalog from same store — table must not appear. *)
     let* cat2 = C.open_ store in
     let* result = C.find_table cat2 ~name:"t" in
     Alcotest.(check bool) "table gone from disk after drop" true (Option.is_none result);
     Lwt.return_unit)
;;

let test_drop_table_also_drops_indexes () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id"; txt_col "name" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_index
         cat
         ~name:"idx_id"
         ~table:"t"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     let* _ =
       C.create_index
         cat
         ~name:"idx_name"
         ~table:"t"
         ~columns:[ "name" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (* Verify indexes exist before drop *)
     Alcotest.(check int)
       "2 indexes before drop"
       2
       (List.length (C.indexes_for_table cat ~table:"t"));
     let* tx = S.rw_begin store in
     let* () = C.drop_table cat tx ~name:"t" in
     let* () = S.commit tx in
     (* Indexes should be gone *)
     Alcotest.(check int)
       "0 indexes after table drop"
       0
       (List.length (C.indexes_for_table cat ~table:"t"));
     Alcotest.(check bool)
       "idx_id gone"
       true
       (Option.is_none (C.find_index cat ~name:"idx_id"));
     Alcotest.(check bool)
       "idx_name gone"
       true
       (Option.is_none (C.find_index cat ~name:"idx_name"));
     Lwt.return_unit)
;;

let test_drop_table_nonexistent_graceful () =
  (* drop_table on a table not in cache: n_cols=0, so column-del loop is a no-op.
     This exercises the None branch in Hashtbl.find_opt t.cache. *)
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* tx = S.rw_begin store in
     (* Use a name that was never created — catalog just does nothing for columns *)
     let* () = C.drop_table cat tx ~name:"phantom" in
     S.commit tx)
;;

let test_drop_index_basic () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_index
         cat
         ~name:"idx"
         ~table:"t"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     let* tx = S.rw_begin store in
     let* () = C.drop_index cat tx ~name:"idx" in
     let* () = S.commit tx in
     Alcotest.(check bool)
       "index gone from cache"
       true
       (Option.is_none (C.find_index cat ~name:"idx"));
     Lwt.return_unit)
;;

let test_drop_index_persists () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_index
         cat1
         ~name:"idx"
         ~table:"t"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     let* tx = S.rw_begin store in
     let* () = C.drop_index cat1 tx ~name:"idx" in
     let* () = S.commit tx in
     (* Re-open and verify gone from disk *)
     let* cat2 = C.open_ store in
     Alcotest.(check bool)
       "index gone from disk"
       true
       (Option.is_none (C.find_index cat2 ~name:"idx"));
     Lwt.return_unit)
;;

let test_drop_index_empty_sys_indexes () =
  (* drop_index when _sys_indexes tree is empty (key_opt = None).
     Create an index, create a fresh catalog (which loads the index),
     then manually clear _sys_indexes and call drop_index — exercises
     the None arm of find_index_key_in_txn. *)
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _ =
       C.create_table
         cat1
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_index
         cat1
         ~name:"idx"
         ~table:"t"
         ~columns:[ "id" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (* Delete all entries from sys_indexes_tid (tree_id=2) directly. *)
     let sys_indexes_tid = 2 in
     let* tx_ro = S.ro_begin store in
     let* cur = S.cursor_open tx_ro sys_indexes_tid in
     let _sr = S.cursor_first cur in
     let keys = ref [] in
     let rec collect () =
       match S.cursor_next cur with
       | None -> ()
       | Some (k, _) ->
         keys := k :: !keys;
         collect ()
     in
     collect ();
     S.cursor_close cur;
     let* () = S.ro_end tx_ro in
     let* tx_rw = S.rw_begin store in
     let* () = Lwt_list.iter_s (fun k -> S.del tx_rw sys_indexes_tid k) !keys in
     let* () = S.commit tx_rw in
     (* Open a fresh catalog — it will load no indexes from disk,
       but we add one to the in-memory table via a separate create. *)
     let* cat2 = C.open_ store in
     (* The index "idx" is now only in cat1's cache, not on disk.
       Call drop_index on cat2 which has idx in cache but not on disk.
       (We put it back in cat2's cache via create_index but on the
       already-cleared tree, so find_index_key_in_txn returns None.) *)
     (* Simpler: create another index on cat2 without wiping disk this time,
       then drop it — the standard path. Just verify drop on non-disk entry
       for cat1 after the disk clear. *)
     let* tx = S.rw_begin store in
     (* drop_index on cat1 with name "idx": find_index_key_in_txn scans empty
       _sys_indexes, returns None. The cache entry is still removed. *)
     let* () = C.drop_index cat1 tx ~name:"idx" in
     let* () = S.commit tx in
     Alcotest.(check bool)
       "idx gone from cat1 cache after disk-empty drop"
       true
       (Option.is_none (C.find_index cat1 ~name:"idx"));
     ignore cat2;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Backward compatibility: old-format column encoding                    *)
(* ------------------------------------------------------------------ *)

(* Construct old-format column bytes with no not_null/pk/default fields
   (the very oldest format: just type_tag + name_len + name) *)
let make_old_format_col_bytes ty_tag name =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int ty_tag);
  Varint.encode_uint64 buf (Int64.of_int (String.length name));
  Buffer.add_string buf name;
  Buffer.to_bytes buf
;;

(* Construct intermediate-format column bytes with not_null/pk/default but
   no check_sql field (the pre-Phase-9 format) *)
let make_intermediate_format_col_bytes ty_tag name =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int ty_tag);
  Varint.encode_uint64 buf (Int64.of_int (String.length name));
  Buffer.add_string buf name;
  Varint.encode_uint64 buf 0L;
  (* not_null = false *)
  Varint.encode_uint64 buf 0L;
  (* primary_key = false *)
  Varint.encode_uint64 buf 0L;
  (* has_default = false *)
  Buffer.to_bytes buf
;;

(* Column key: table_name ++ NUL ++ ordinal_be8 *)
let make_column_key table_name ordinal =
  let tn = Bytes.of_string table_name in
  let ord = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set_uint8 ord i ((ordinal lsr ((7 - i) * 8)) land 0xFF)
  done;
  Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord
;;

(* Table value: varint(tree_id) ++ zigzag(next_rowid) *)
let make_table_value tree_id =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int tree_id);
  Varint.encode_int64 buf 1L;
  (* next_rowid = 1 *)
  Buffer.to_bytes buf
;;

let sys_tables_tid = 0
let sys_columns_tid = 1

(** Test that a catalog stored with old-format column bytes (no not_null/pk/default)
    is read correctly with backward-compat code (bytes_left = 0 path). *)
let test_decode_column_old_format () =
  run
    (let store = S.create () in
     let* tx = S.rw_begin store in
     (* Write a table entry for "compat_t" with tree_id=16 *)
     let* () =
       S.put tx sys_tables_tid (Bytes.of_string "compat_t") (make_table_value 16)
     in
     (* Write one column in old-format (no not_null/pk/default/check) *)
     let col_bytes = make_old_format_col_bytes 1 (* INTEGER *) "id" in
     let* () = S.put tx sys_columns_tid (make_column_key "compat_t" 0) col_bytes in
     let* () = S.commit tx in
     (* Open catalog — should load with backward-compat path *)
     let* cat = C.open_ store in
     let* result = C.find_table cat ~name:"compat_t" in
     (match result with
      | None -> Alcotest.fail "expected table compat_t to be found"
      | Some m ->
        Alcotest.(check int) "one column" 1 (List.length m.columns);
        let col = List.hd m.columns in
        Alcotest.(check string) "col name" "id" col.Row.name;
        Alcotest.(check bool) "col ty integer" true (col.Row.ty = Row.Integer);
        Alcotest.(check bool) "not_null false" false col.Row.not_null;
        Alcotest.(check bool) "check_sql none" true (col.Row.check_sql = None));
     Lwt.return_unit)
;;

(** Test that a catalog stored with intermediate-format column bytes
    (not_null/pk/default but no check_sql) is read correctly
    (bytes_left2 = 0 path). *)
let test_decode_column_no_check_sql () =
  run
    (let store = S.create () in
     let* tx = S.rw_begin store in
     let* () =
       S.put tx sys_tables_tid (Bytes.of_string "inter_t") (make_table_value 16)
     in
     let col_bytes = make_intermediate_format_col_bytes 2 (* TEXT *) "name" in
     let* () = S.put tx sys_columns_tid (make_column_key "inter_t" 0) col_bytes in
     let* () = S.commit tx in
     let* cat = C.open_ store in
     let* result = C.find_table cat ~name:"inter_t" in
     (match result with
      | None -> Alcotest.fail "expected table inter_t to be found"
      | Some m ->
        Alcotest.(check int) "one column" 1 (List.length m.columns);
        let col = List.hd m.columns in
        Alcotest.(check string) "col name" "name" col.Row.name;
        Alcotest.(check bool) "col ty text" true (col.Row.ty = Row.Text);
        Alcotest.(check bool) "check_sql none" true (col.Row.check_sql = None));
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

(* ------------------------------------------------------------------ *)
(* Group: schema fingerprint (#174)                                     *)
(* ------------------------------------------------------------------ *)

let test_fingerprint_matches_compute () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let cols = [ int_col "id"; txt_col "name" ] in
     let* _ =
       C.create_table
         cat
         ~name:"users"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let expected = SF.compute ~columns:cols ~without_rowid:false in
     Alcotest.(check (option int64))
       "fingerprint matches Schema_fingerprint.compute"
       (Some expected)
       (C.table_fingerprint cat ~name:"users");
     Lwt.return_unit)
;;

let test_fingerprint_missing () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     Alcotest.(check (option int64))
       "missing table -> None"
       None
       (C.table_fingerprint cat ~name:"ghost");
     Lwt.return_unit)
;;

let test_fingerprint_distinct_schemas () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"a"
         ~columns:[ int_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_table
         cat
         ~name:"b"
         ~columns:[ txt_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     Alcotest.(check bool)
       "different column types -> different fingerprints"
       false
       (C.table_fingerprint cat ~name:"a" = C.table_fingerprint cat ~name:"b");
     Lwt.return_unit)
;;

let test_fingerprints_by_tree_id () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let cols = [ int_col "id" ] in
     let* tid =
       C.create_table
         cat
         ~name:"users"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     let expected = SF.compute ~columns:cols ~without_rowid:false in
     (match List.assoc_opt tid (C.fingerprints_by_tree_id cat) with
      | Some fp -> Alcotest.(check int64) "registry maps tree_id -> fp" expected fp
      | None -> Alcotest.fail "tree_id absent from fingerprint registry");
     Lwt.return_unit)
;;

let test_fingerprint_changes_on_add_column () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"users"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let before = C.table_fingerprint cat ~name:"users" in
     let* r = C.add_column cat ~table_name:"users" ~column:(txt_col "name") in
     (match r with
      | Ok () -> ()
      | Error e -> Alcotest.failf "add_column: %s" e);
     let after = C.table_fingerprint cat ~name:"users" in
     Alcotest.(check bool) "fingerprint changed after add_column" false (before = after);
     let expected =
       SF.compute ~columns:[ int_col "id"; txt_col "name" ] ~without_rowid:false
     in
     Alcotest.(check (option int64)) "matches new schema shape" (Some expected) after;
     Lwt.return_unit)
;;

let test_fingerprint_without_rowid () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let cols = [ int_col "id" ] in
     let* _ =
       C.create_table cat ~name:"t" ~columns:cols ~without_rowid:true ~autoincrement:false
     in
     let expected = SF.compute ~columns:cols ~without_rowid:true in
     Alcotest.(check (option int64))
       "without_rowid included in fingerprint"
       (Some expected)
       (C.table_fingerprint cat ~name:"t");
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Group: redundant catalog mirror (#174)                               *)
(* ------------------------------------------------------------------ *)

(* The _sys_tables primary row is deleted to simulate a damaged primary
   catalog; reopening must reconstruct the table from the mirror. *)
let test_mirror_recovers_lost_primary_row () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let cols = [ int_col "id"; txt_col "name" ] in
     let* tid =
       C.create_table
         cat
         ~name:"users"
         ~columns:cols
         ~without_rowid:false
         ~autoincrement:false
     in
     (* Lose the primary _sys_tables row (system tree 0). *)
     let* tx = S.rw_begin store in
     let* () = S.del tx 0 (Bytes.of_string "users") in
     let* () = S.commit tx in
     (* Reopen the catalog over the same store. *)
     let* cat2 = C.open_ store in
     let* found = C.find_table cat2 ~name:"users" in
     (match found with
      | None -> Alcotest.fail "table not recovered from mirror after primary row loss"
      | Some m ->
        Alcotest.(check int) "recovered tree_id" tid m.tree_id;
        Alcotest.(check int) "recovered column count" 2 (List.length m.columns);
        Alcotest.(check bool) "recovered without_rowid" false m.without_rowid;
        Alcotest.(check (option int64))
          "recovered fingerprint matches original"
          (Some (SF.compute ~columns:cols ~without_rowid:false))
          (C.table_fingerprint cat2 ~name:"users"));
     Lwt.return_unit)
;;

let test_mirror_fingerprints_match_primary () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"a"
         ~columns:[ int_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_table
         cat
         ~name:"b"
         ~columns:[ txt_col "y"; int_col "z" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* mirror = C.mirror_fingerprints cat in
     List.iter
       (fun (tid, fp) ->
          match List.assoc_opt tid mirror with
          | Some mfp -> Alcotest.(check int64) "mirror fp matches primary" fp mfp
          | None -> Alcotest.failf "tree_id %d missing from mirror" tid)
       (C.fingerprints_by_tree_id cat);
     Lwt.return_unit)
;;

let test_mirror_drop_removes_entry () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* tid =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* tx = S.rw_begin store in
     let* () = C.drop_table cat tx ~name:"t" in
     let* () = S.commit tx in
     let* mirror = C.mirror_fingerprints cat in
     Alcotest.(check bool)
       "mirror entry removed on drop_table"
       false
       (List.mem_assoc tid mirror);
     Lwt.return_unit)
;;

let test_mirror_reflects_add_column () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* tid =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "x" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* r = C.add_column cat ~table_name:"t" ~column:(txt_col "y") in
     (match r with
      | Ok () -> ()
      | Error e -> Alcotest.failf "add_column: %s" e);
     let* mirror = C.mirror_fingerprints cat in
     let expected =
       SF.compute ~columns:[ int_col "x"; txt_col "y" ] ~without_rowid:false
     in
     (match List.assoc_opt tid mirror with
      | Some mfp ->
        Alcotest.(check int64) "mirror fp updated after add_column" expected mfp
      | None -> Alcotest.fail "tree_id missing from mirror");
     Lwt.return_unit)
;;

(* #175: when a table is reconstructed from the mirror, the next_rowid
   counter must be recovered from existing data rows so auto-allocated
   rowids don't collide. *)
let test_mirror_recovers_next_rowid () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* tid =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     (* Insert three rows with rowids 1, 7, and 42 into the data tree. *)
     let* tx = S.rw_begin store in
     let* () = S.put tx tid (Rowid.encode 1L) (Bytes.of_string "row1") in
     let* () = S.put tx tid (Rowid.encode 7L) (Bytes.of_string "row7") in
     let* () = S.put tx tid (Rowid.encode 42L) (Bytes.of_string "row42") in
     let* () = S.commit tx in
     (* Delete the primary _sys_tables row to force mirror recovery. *)
     let* tx = S.rw_begin store in
     let* () = S.del tx 0 (Bytes.of_string "t") in
     let* () = S.commit tx in
     (* Reopen — the table is recovered from the mirror, and #175
        recovers next_rowid = max(1,7,42) + 1 = 43. *)
     let* cat2 = C.open_ store in
     (match C.find_table_cached cat2 ~name:"t" with
      | None -> Alcotest.fail "table t should be recovered from mirror"
      | Some m ->
        Alcotest.(check int64) "recovered next_rowid is max+1" 43L m.C.next_rowid);
     (* Allocate one more rowid — should yield 43, not 1. *)
     let* r = C.next_rowid cat2 ~name:"t" in
     Alcotest.(check int64) "next allocated rowid is 43" 43L r;
     Lwt.return_unit)
;;

(* #250: mirror recovery must track the literal max rowid, including when the
   only rows are negative — so the recovered counter is max+1 (= -2), and the
   next auto-allocated rowid is -2, matching SQLite and the in-session path (not
   the old [1L] clamp). *)
let test_mirror_recovers_negative_next_rowid () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* tid =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* tx = S.rw_begin store in
     let* () = S.put tx tid (Rowid.encode (-5L)) (Bytes.of_string "row-5") in
     let* () = S.put tx tid (Rowid.encode (-3L)) (Bytes.of_string "row-3") in
     let* () = S.commit tx in
     let* tx = S.rw_begin store in
     let* () = S.del tx 0 (Bytes.of_string "t") in
     let* () = S.commit tx in
     let* cat2 = C.open_ store in
     (match C.find_table_cached cat2 ~name:"t" with
      | None -> Alcotest.fail "table t should be recovered from mirror"
      | Some m ->
        Alcotest.(check int64)
          "recovered next_rowid is max(-5,-3)+1 = -2"
          (-2L)
          m.C.next_rowid);
     let* r = C.next_rowid cat2 ~name:"t" in
     Alcotest.(check int64) "next allocated rowid is -2, not 1" (-2L) r;
     Lwt.return_unit)
;;

(* #250: mirror recovery of a table with NO data rows leaves the counter at the
   [empty] sentinel, so the first auto-allocated rowid is 1 (SQLite: empty -> 1). *)
let test_mirror_recovers_empty_next_rowid () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _tid =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* tx = S.rw_begin store in
     let* () = S.del tx 0 (Bytes.of_string "t") in
     let* () = S.commit tx in
     let* cat2 = C.open_ store in
     let* r = C.next_rowid cat2 ~name:"t" in
     Alcotest.(check int64) "first rowid after empty-table recovery is 1" 1L r;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Group: schema-drift detection (#174)                                 *)
(* ------------------------------------------------------------------ *)

let test_drift_clean_db_no_discrepancies () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"a"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       C.create_table
         cat
         ~name:"b"
         ~columns:[ txt_col "n" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* discrepancies = C.verify_against_mirror cat in
     Alcotest.(check int) "no discrepancies on a clean db" 0 (List.length discrepancies);
     Lwt.return_unit)
;;

let test_drift_detects_primary_mirror_mismatch () =
  let store = S.create () in
  run
    (let* cat = C.open_ store in
     let* _ =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     (* Tamper the PRIMARY column entry so "id" decodes as Text instead of
        Integer (still a valid column, so it loads).  The mirror still records
        Integer, so the two copies disagree. *)
     let col_key =
       let tn = Bytes.of_string "t" in
       let ord = Bytes.make 8 '\x00' in
       Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord
     in
     let altered =
       let buf = Buffer.create 8 in
       Varint.encode_uint64 buf 2L (* type tag = Text *);
       Varint.encode_uint64 buf 2L (* name len *);
       Buffer.add_string buf "id";
       Buffer.to_bytes buf
     in
     let* tx = S.rw_begin store in
     let* () = S.put tx 1 col_key altered in
     let* () = S.commit tx in
     let* cat2 = C.open_ store in
     let* discrepancies = C.verify_against_mirror cat2 in
     Alcotest.(check bool)
       "fingerprint mismatch detected between primary and mirror"
       true
       (List.exists
          (function
            | C.Fingerprint_mismatch _ -> true
            | _ -> false)
          discrepancies);
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "catalog"
    [ ( "basic_create_find"
      , [ Alcotest.test_case "create_then_find" `Quick test_create_then_find
        ; Alcotest.test_case "missing_table" `Quick test_missing_table
        ; Alcotest.test_case
            "create_assigns_tree_id_16"
            `Quick
            test_create_assigns_tree_id_16
        ; Alcotest.test_case
            "create_assigns_sequential_ids"
            `Quick
            test_create_assigns_sequential_ids
        ; Alcotest.test_case "duplicate_table" `Quick test_duplicate_table
        ; Alcotest.test_case "list_tables_empty" `Quick test_list_tables_empty
        ; Alcotest.test_case "list_tables_one" `Quick test_list_tables_one
        ; Alcotest.test_case "list_tables_two" `Quick test_list_tables_two
        ] )
    ; ( "column_metadata"
      , [ Alcotest.test_case "columns_preserved" `Quick test_columns_preserved
        ; Alcotest.test_case "integer_column_type" `Quick test_integer_column_type
        ; Alcotest.test_case "text_column_type" `Quick test_text_column_type
        ; Alcotest.test_case "real_column_type" `Quick test_real_column_type
        ; Alcotest.test_case "blob_column_type" `Quick test_blob_column_type
        ; Alcotest.test_case
            "mixed_column_types_roundtrip"
            `Quick
            test_mixed_column_types_roundtrip
        ; Alcotest.test_case "single_column" `Quick test_single_column
        ; Alcotest.test_case "many_columns" `Quick test_many_columns
        ] )
    ; ( "next_rowid"
      , [ Alcotest.test_case "first_rowid_is_1" `Quick test_first_rowid_is_1
        ; Alcotest.test_case "rowid_increments" `Quick test_rowid_increments
        ; Alcotest.test_case "rowids_independent" `Quick test_rowids_independent
        ; Alcotest.test_case "rowid_unknown_table" `Quick test_rowid_unknown_table
        ] )
    ; ( "persistence"
      , [ Alcotest.test_case
            "metadata_survives_reopen"
            `Quick
            test_metadata_survives_reopen
        ; Alcotest.test_case "columns_survive_reopen" `Quick test_columns_survive_reopen
        ; Alcotest.test_case "rowid_survives_reopen" `Quick test_rowid_survives_reopen
        ; Alcotest.test_case "tree_id_survives_reopen" `Quick test_tree_id_survives_reopen
        ; Alcotest.test_case
            "load_all_first_table_not_skipped"
            `Quick
            test_load_all_first_table_not_skipped
        ; Alcotest.test_case "default_blob_roundtrip" `Quick test_default_blob_roundtrip
        ; Alcotest.test_case "default_real_roundtrip" `Quick test_default_real_roundtrip
        ; Alcotest.test_case "default_text_roundtrip" `Quick test_default_text_roundtrip
        ; Alcotest.test_case "default_null_roundtrip" `Quick test_default_null_roundtrip
        ; Alcotest.test_case "default_int_roundtrip" `Quick test_default_int_roundtrip
        ] )
    ; ( "error_conditions"
      , [ Alcotest.test_case "create_empty_name" `Quick test_create_empty_name
        ; Alcotest.test_case "create_empty_columns" `Quick test_create_empty_columns
        ; Alcotest.test_case "corrupt_column_type_tag" `Quick corrupt_column_type_tag
        ; Alcotest.test_case "corrupt_default_tag" `Quick corrupt_default_tag
        ; Alcotest.test_case
            "mirror_preserves_autoincrement"
            `Quick
            mirror_preserves_autoincrement
        ] )
    ; ( "backward_compat"
      , [ Alcotest.test_case
            "decode_column_old_format"
            `Quick
            test_decode_column_old_format
        ; Alcotest.test_case
            "decode_column_no_check_sql"
            `Quick
            test_decode_column_no_check_sql
        ] )
    ; ( "indexes"
      , [ Alcotest.test_case "create_index_basic" `Quick test_create_index_basic
        ; Alcotest.test_case "indexes_for_table" `Quick test_indexes_for_table
        ; Alcotest.test_case
            "create_index_unknown_table"
            `Quick
            test_create_index_unknown_table
        ; Alcotest.test_case
            "create_index_unknown_column"
            `Quick
            test_create_index_unknown_column
        ; Alcotest.test_case "create_index_duplicate" `Quick test_create_index_duplicate
        ; Alcotest.test_case
            "index_persists_across_reopen"
            `Quick
            test_index_persists_across_reopen
        ; Alcotest.test_case
            "index_origin_persists_across_reopen"
            `Quick
            test_index_origin_persists_across_reopen
        ; Alcotest.test_case "find_index" `Quick test_find_index
        ] )
    ; ( "drop"
      , [ Alcotest.test_case "drop_table_basic" `Quick test_drop_table_basic
        ; Alcotest.test_case
            "drop_table_removes_from_disk"
            `Quick
            test_drop_table_removes_from_disk
        ; Alcotest.test_case
            "drop_table_also_drops_indexes"
            `Quick
            test_drop_table_also_drops_indexes
        ; Alcotest.test_case
            "drop_table_nonexistent_graceful"
            `Quick
            test_drop_table_nonexistent_graceful
        ; Alcotest.test_case "drop_index_basic" `Quick test_drop_index_basic
        ; Alcotest.test_case "drop_index_persists" `Quick test_drop_index_persists
        ; Alcotest.test_case
            "drop_index_empty_sys_indexes"
            `Quick
            test_drop_index_empty_sys_indexes
        ] )
    ; ( "fingerprint"
      , [ Alcotest.test_case "matches_compute" `Quick test_fingerprint_matches_compute
        ; Alcotest.test_case "missing_table" `Quick test_fingerprint_missing
        ; Alcotest.test_case "distinct_schemas" `Quick test_fingerprint_distinct_schemas
        ; Alcotest.test_case "by_tree_id" `Quick test_fingerprints_by_tree_id
        ; Alcotest.test_case
            "changes_on_add_column"
            `Quick
            test_fingerprint_changes_on_add_column
        ; Alcotest.test_case "without_rowid" `Quick test_fingerprint_without_rowid
        ] )
    ; ( "mirror"
      , [ Alcotest.test_case
            "recovers_lost_primary_row"
            `Quick
            test_mirror_recovers_lost_primary_row
        ; Alcotest.test_case
            "fingerprints_match_primary"
            `Quick
            test_mirror_fingerprints_match_primary
        ; Alcotest.test_case "drop_removes_entry" `Quick test_mirror_drop_removes_entry
        ; Alcotest.test_case "reflects_add_column" `Quick test_mirror_reflects_add_column
        ; Alcotest.test_case "recovers_next_rowid" `Quick test_mirror_recovers_next_rowid
        ; Alcotest.test_case
            "recovers_negative_next_rowid (#250)"
            `Quick
            test_mirror_recovers_negative_next_rowid
        ; Alcotest.test_case
            "recovers_empty_next_rowid (#250)"
            `Quick
            test_mirror_recovers_empty_next_rowid
        ] )
    ; ( "drift"
      , [ Alcotest.test_case
            "clean_db_no_discrepancies"
            `Quick
            test_drift_clean_db_no_discrepancies
        ; Alcotest.test_case
            "detects_primary_mirror_mismatch"
            `Quick
            test_drift_detects_primary_mirror_mismatch
        ] )
    ]
;;
