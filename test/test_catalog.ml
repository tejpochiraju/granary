open Lwt.Syntax

module S = Sqlocaml_store.Store
module C = Sqlocaml_catalog.Catalog
module Row = Sqlocaml_encoding.Row

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let run = Lwt_main.run

let mk_col name ty : Row.column = { name; ty }

let int_col name = mk_col name Row.Integer
let txt_col name = mk_col name Row.Text

(* ------------------------------------------------------------------ *)
(* Group 1: Basic create and find                                       *)
(* ------------------------------------------------------------------ *)

let test_create_then_find () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let cols = [int_col "id"; txt_col "name"] in
    let* tid = C.create_table cat ~name:"users" ~columns:cols in
    let* result = C.find_table cat ~name:"users" in
    (match result with
     | None -> Alcotest.fail "expected Some, got None"
     | Some m ->
       Alcotest.(check int) "tree_id" tid m.tree_id;
       Alcotest.(check int) "column count" 2 (List.length m.columns));
    Lwt.return_unit
  )

let test_missing_table () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* result = C.find_table cat ~name:"ghost" in
    Alcotest.(check (option (Alcotest.testable (fun ppf m -> Format.fprintf ppf "%s" m.C.name) (fun a b -> a.C.name = b.C.name))))
      "missing table" None result;
    Lwt.return_unit
  )

let test_create_assigns_tree_id_16 () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* tid = C.create_table cat ~name:"first" ~columns:[int_col "x"] in
    Alcotest.(check int) "first tree_id is 16" 16 tid;
    Lwt.return_unit
  )

let test_create_assigns_sequential_ids () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* tid_a = C.create_table cat ~name:"a" ~columns:[int_col "x"] in
    let* tid_b = C.create_table cat ~name:"b" ~columns:[int_col "y"] in
    Alcotest.(check int) "a gets 16" 16 tid_a;
    Alcotest.(check int) "b gets 17" 17 tid_b;
    Lwt.return_unit
  )

let test_duplicate_table () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"users" ~columns:[int_col "id"] in
    (* failwith is synchronous — raised before any Lwt.t is constructed *)
    (try
       ignore (C.create_table cat ~name:"users" ~columns:[int_col "id"]);
       Alcotest.fail "expected Failure for duplicate"
     with Failure _ -> ());
    Lwt.return_unit
  )

let test_list_tables_empty () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* tables = C.list_tables cat in
    Alcotest.(check int) "empty catalog has 0 tables" 0 (List.length tables);
    Lwt.return_unit
  )

let test_list_tables_one () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"t1" ~columns:[int_col "id"] in
    let* tables = C.list_tables cat in
    Alcotest.(check int) "one table" 1 (List.length tables);
    Lwt.return_unit
  )

let test_list_tables_two () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"t1" ~columns:[int_col "id"] in
    let* _ = C.create_table cat ~name:"t2" ~columns:[txt_col "name"] in
    let* tables = C.list_tables cat in
    Alcotest.(check int) "two tables" 2 (List.length tables);
    let names = List.map (fun m -> m.C.name) tables |> List.sort String.compare in
    Alcotest.(check (list string)) "table names" ["t1"; "t2"] names;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 2: Column metadata                                             *)
(* ------------------------------------------------------------------ *)

let test_columns_preserved () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let cols = [int_col "id"; txt_col "name"] in
    let* _ = C.create_table cat ~name:"users" ~columns:cols in
    let* result = C.find_table cat ~name:"users" in
    (match result with
     | None -> Alcotest.fail "expected Some"
     | Some m ->
       let got_names = List.map (fun c -> c.Row.name) m.C.columns in
       Alcotest.(check (list string)) "column names preserved" ["id"; "name"] got_names);
    Lwt.return_unit
  )

let test_integer_column_type () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"t" ~columns:[int_col "x"] in
    let* result = C.find_table cat ~name:"t" in
    (match result with
     | None -> Alcotest.fail "expected Some"
     | Some m ->
       let col = List.hd m.C.columns in
       (match col.Row.ty with
        | Row.Integer -> ()
        | Row.Text -> Alcotest.fail "expected Integer, got Text"));
    Lwt.return_unit
  )

let test_text_column_type () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"t" ~columns:[txt_col "s"] in
    let* result = C.find_table cat ~name:"t" in
    (match result with
     | None -> Alcotest.fail "expected Some"
     | Some m ->
       let col = List.hd m.C.columns in
       (match col.Row.ty with
        | Row.Text -> ()
        | Row.Integer -> Alcotest.fail "expected Text, got Integer"));
    Lwt.return_unit
  )

let test_single_column () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"t" ~columns:[int_col "only"] in
    let* result = C.find_table cat ~name:"t" in
    (match result with
     | None -> Alcotest.fail "expected Some"
     | Some m ->
       Alcotest.(check int) "exactly 1 column" 1 (List.length m.C.columns);
       Alcotest.(check string) "column name" "only" (List.hd m.C.columns).Row.name);
    Lwt.return_unit
  )

let test_many_columns () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let cols = [
      int_col "c0"; txt_col "c1"; int_col "c2"; txt_col "c3"; int_col "c4";
      txt_col "c5"; int_col "c6"; txt_col "c7"; int_col "c8"; txt_col "c9";
    ] in
    let* _ = C.create_table cat ~name:"wide" ~columns:cols in
    let* result = C.find_table cat ~name:"wide" in
    (match result with
     | None -> Alcotest.fail "expected Some"
     | Some m ->
       Alcotest.(check int) "10 columns" 10 (List.length m.C.columns);
       List.iteri (fun i col ->
         let expected_name = Printf.sprintf "c%d" i in
         let expected_ty = if i mod 2 = 0 then Row.Integer else Row.Text in
         Alcotest.(check string) (Printf.sprintf "col %d name" i) expected_name col.Row.name;
         (match col.Row.ty, expected_ty with
          | Row.Integer, Row.Integer | Row.Text, Row.Text -> ()
          | _ -> Alcotest.failf "col %d type mismatch" i)
       ) m.C.columns);
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 3: next_rowid                                                  *)
(* ------------------------------------------------------------------ *)

let test_first_rowid_is_1 () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"t" ~columns:[int_col "id"] in
    let* rid = C.next_rowid cat ~name:"t" in
    Alcotest.(check int64) "first rowid is 1" 1L rid;
    Lwt.return_unit
  )

let test_rowid_increments () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"t" ~columns:[int_col "id"] in
    let* r1 = C.next_rowid cat ~name:"t" in
    let* r2 = C.next_rowid cat ~name:"t" in
    let* r3 = C.next_rowid cat ~name:"t" in
    Alcotest.(check int64) "rowid 1" 1L r1;
    Alcotest.(check int64) "rowid 2" 2L r2;
    Alcotest.(check int64) "rowid 3" 3L r3;
    Lwt.return_unit
  )

let test_rowids_independent () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"a" ~columns:[int_col "id"] in
    let* _ = C.create_table cat ~name:"b" ~columns:[int_col "id"] in
    let* ra1 = C.next_rowid cat ~name:"a" in
    let* rb1 = C.next_rowid cat ~name:"b" in
    let* ra2 = C.next_rowid cat ~name:"a" in
    Alcotest.(check int64) "a first rowid" 1L ra1;
    Alcotest.(check int64) "b first rowid" 1L rb1;
    Alcotest.(check int64) "a second rowid" 2L ra2;
    Lwt.return_unit
  )

let test_rowid_unknown_table () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    (* failwith is synchronous — raised before any Lwt.t is constructed *)
    (try
       ignore (C.next_rowid cat ~name:"nonexistent");
       Alcotest.fail "expected Failure for unknown table"
     with Failure _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 4: Persistence across open_                                   *)
(* ------------------------------------------------------------------ *)

let test_metadata_survives_reopen () =
  run (
    let store = S.create () in
    let* cat1 = C.open_ store in
    let* _ = C.create_table cat1 ~name:"users" ~columns:[int_col "id"; txt_col "name"] in
    (* Open a second catalog on the same store *)
    let* cat2 = C.open_ store in
    let* result = C.find_table cat2 ~name:"users" in
    (match result with
     | None -> Alcotest.fail "table not found after reopen"
     | Some m ->
       Alcotest.(check string) "name preserved" "users" m.C.name;
       Alcotest.(check int) "tree_id preserved" 16 m.C.tree_id);
    Lwt.return_unit
  )

let test_columns_survive_reopen () =
  run (
    let store = S.create () in
    let* cat1 = C.open_ store in
    let cols = [int_col "id"; txt_col "email"; int_col "age"] in
    let* _ = C.create_table cat1 ~name:"persons" ~columns:cols in
    let* cat2 = C.open_ store in
    let* result = C.find_table cat2 ~name:"persons" in
    (match result with
     | None -> Alcotest.fail "table not found after reopen"
     | Some m ->
       Alcotest.(check int) "column count after reopen" 3 (List.length m.C.columns);
       let names = List.map (fun c -> c.Row.name) m.C.columns in
       Alcotest.(check (list string)) "column names after reopen" ["id"; "email"; "age"] names;
       let types = List.map (fun c -> c.Row.ty) m.C.columns in
       (match types with
        | [Row.Integer; Row.Text; Row.Integer] -> ()
        | _ -> Alcotest.fail "column types wrong after reopen"));
    Lwt.return_unit
  )

let test_rowid_survives_reopen () =
  run (
    let store = S.create () in
    let* cat1 = C.open_ store in
    let* _ = C.create_table cat1 ~name:"t" ~columns:[int_col "id"] in
    let* _ = C.next_rowid cat1 ~name:"t" in
    let* _ = C.next_rowid cat1 ~name:"t" in
    let* _ = C.next_rowid cat1 ~name:"t" in
    (* After 3 calls, next should be 4 *)
    let* cat2 = C.open_ store in
    let* r = C.next_rowid cat2 ~name:"t" in
    Alcotest.(check int64) "rowid continues from 4 after reopen" 4L r;
    Lwt.return_unit
  )

let test_tree_id_survives_reopen () =
  run (
    let store = S.create () in
    let* cat1 = C.open_ store in
    let* tid1 = C.create_table cat1 ~name:"t" ~columns:[int_col "id"] in
    let* cat2 = C.open_ store in
    let* result = C.find_table cat2 ~name:"t" in
    (match result with
     | None -> Alcotest.fail "table not found after reopen"
     | Some m ->
       Alcotest.(check int) "tree_id survives reopen" tid1 m.C.tree_id);
    Lwt.return_unit
  )

(* Targeted test: verify load_all does NOT skip the first alphabetical table.
   "aardvark" sorts before "zebra" — after reopen, both must be found. *)
let test_load_all_first_table_not_skipped () =
  run (
    let store = S.create () in
    let* cat1 = C.open_ store in
    let* _ = C.create_table cat1 ~name:"aardvark" ~columns:[int_col "id"] in
    let* _ = C.create_table cat1 ~name:"zebra" ~columns:[int_col "id"] in
    (* Open a fresh catalog — this calls load_all which walks the cursor *)
    let* cat2 = C.open_ store in
    let* r_a = C.find_table cat2 ~name:"aardvark" in
    let* r_z = C.find_table cat2 ~name:"zebra" in
    (match r_a with
     | None -> Alcotest.fail "aardvark (first alphabetically) not found after reopen — load_all skipped it"
     | Some _ -> ());
    (match r_z with
     | None -> Alcotest.fail "zebra not found after reopen"
     | Some _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 5: Error conditions                                            *)
(* ------------------------------------------------------------------ *)

let test_create_empty_name () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    (* Empty name should succeed — no restriction in spec *)
    let* tid = C.create_table cat ~name:"" ~columns:[int_col "x"] in
    Alcotest.(check int) "empty name gets tree_id 16" 16 tid;
    let* result = C.find_table cat ~name:"" in
    (match result with
     | None -> Alcotest.fail "expected Some for empty name table"
     | Some m -> Alcotest.(check string) "empty name preserved" "" m.C.name);
    Lwt.return_unit
  )

let test_create_empty_columns () =
  run (
    let store = S.create () in
    let* cat = C.open_ store in
    (* Zero columns should succeed — no column requirement in spec *)
    let* tid = C.create_table cat ~name:"nocols" ~columns:[] in
    Alcotest.(check int) "zero-col table gets tree_id 16" 16 tid;
    let* result = C.find_table cat ~name:"nocols" in
    (match result with
     | None -> Alcotest.fail "expected Some for zero-col table"
     | Some m -> Alcotest.(check int) "zero columns preserved" 0 (List.length m.C.columns));
    Lwt.return_unit
  )

let corrupt_column_type_tag () =
  (* Setup: create a store with a real table, then corrupt the column type tag *)
  let store = S.create () in
  run (
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"users"
      ~columns:[{ Row.name = "id"; ty = Row.Integer }] in
    (* Overwrite the column entry with a corrupt type tag (0) *)
    let col_key =
      let tn = Bytes.of_string "users" in
      let ord = Bytes.make 8 '\x00' in  (* ordinal 0, big-endian *)
      Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord
    in
    let bad_val =
      let buf = Buffer.create 8 in
      Sqlocaml_encoding.Varint.encode_uint64 buf 0L;  (* tag=0, invalid *)
      Sqlocaml_encoding.Varint.encode_uint64 buf 2L;  (* name len *)
      Buffer.add_string buf "id";
      Buffer.to_bytes buf
    in
    let* tx = S.rw_begin store in
    let* () = S.put tx 1 col_key bad_val in  (* sys_columns_tid = 1 *)
    S.commit tx
  );
  (* Now open_ a fresh catalog — load_columns will hit type_of_tag with 0 → failwith *)
  (try
     let _ = Lwt_main.run (C.open_ store) in
     Alcotest.fail "expected Failure for corrupt type tag"
   with Failure msg ->
     Alcotest.(check bool) "error message non-empty"
       true (String.length msg > 0))

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "catalog" [
    "basic_create_find", [
      Alcotest.test_case "create_then_find"            `Quick test_create_then_find;
      Alcotest.test_case "missing_table"               `Quick test_missing_table;
      Alcotest.test_case "create_assigns_tree_id_16"   `Quick test_create_assigns_tree_id_16;
      Alcotest.test_case "create_assigns_sequential_ids" `Quick test_create_assigns_sequential_ids;
      Alcotest.test_case "duplicate_table"             `Quick test_duplicate_table;
      Alcotest.test_case "list_tables_empty"           `Quick test_list_tables_empty;
      Alcotest.test_case "list_tables_one"             `Quick test_list_tables_one;
      Alcotest.test_case "list_tables_two"             `Quick test_list_tables_two;
    ];
    "column_metadata", [
      Alcotest.test_case "columns_preserved"           `Quick test_columns_preserved;
      Alcotest.test_case "integer_column_type"         `Quick test_integer_column_type;
      Alcotest.test_case "text_column_type"            `Quick test_text_column_type;
      Alcotest.test_case "single_column"               `Quick test_single_column;
      Alcotest.test_case "many_columns"                `Quick test_many_columns;
    ];
    "next_rowid", [
      Alcotest.test_case "first_rowid_is_1"            `Quick test_first_rowid_is_1;
      Alcotest.test_case "rowid_increments"            `Quick test_rowid_increments;
      Alcotest.test_case "rowids_independent"          `Quick test_rowids_independent;
      Alcotest.test_case "rowid_unknown_table"         `Quick test_rowid_unknown_table;
    ];
    "persistence", [
      Alcotest.test_case "metadata_survives_reopen"    `Quick test_metadata_survives_reopen;
      Alcotest.test_case "columns_survive_reopen"      `Quick test_columns_survive_reopen;
      Alcotest.test_case "rowid_survives_reopen"       `Quick test_rowid_survives_reopen;
      Alcotest.test_case "tree_id_survives_reopen"     `Quick test_tree_id_survives_reopen;
      Alcotest.test_case "load_all_first_table_not_skipped" `Quick test_load_all_first_table_not_skipped;
    ];
    "error_conditions", [
      Alcotest.test_case "create_empty_name"           `Quick test_create_empty_name;
      Alcotest.test_case "create_empty_columns"        `Quick test_create_empty_columns;
      Alcotest.test_case "corrupt_column_type_tag"     `Quick corrupt_column_type_tag;
    ];
  ]
