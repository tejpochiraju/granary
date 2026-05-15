open Lwt.Syntax

module S    = Sqlocaml_store.Store
module Cat  = Sqlocaml_catalog.Catalog
module Row  = Sqlocaml_encoding.Row
module Ast  = Sqlocaml_sql.Ast
module Plan = Sqlocaml_sql.Plan
module Exec = Sqlocaml_sql.Exec

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let run = Lwt_main.run

let int_col name : Row.column = { name; ty = Row.Integer }
let txt_col name : Row.column = { name; ty = Row.Text    }

(** Standard two-column schema: id INTEGER, name TEXT *)
let id_name_cols = [int_col "id"; txt_col "name"]

let setup () =
  run (
    let store = S.create () in
    let* cat = Cat.open_ store in
    Lwt.return (store, cat)
  )

(** Collect a stream into a list. *)
let collect stream = Lwt_main.run (Lwt_stream.to_list stream)

(** Insert a literal row into [table_name] via Op_insert. *)
let insert store cat table_name (ordinals, values) =
  run (
    let* meta_opt = Cat.find_table cat ~name:table_name in
    let table_meta = match meta_opt with
      | Some m -> m
      | None   -> failwith ("insert helper: table not found: " ^ table_name)
    in
    Exec.execute store cat
      (Plan.Op_insert { table_meta; ordinals; values })
  )

(** Open a cursor on a tree and collect all (key, value) pairs. *)
let cursor_collect store tree_id =
  run (
    let* tx  = S.ro_begin store in
    let* cur = S.cursor_open tx tree_id in
    let _sr  = S.cursor_first cur in
    let rec loop acc =
      match S.cursor_next cur with
      | None        -> List.rev acc
      | Some (k, v) -> loop ((k, v) :: acc)
    in
    let pairs = loop [] in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    Lwt.return pairs
  )

(* ------------------------------------------------------------------ *)
(* Group 1: Op_create_table                                             *)
(* ------------------------------------------------------------------ *)

let exec_create_table () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "items"; columns = id_name_cols }) in
    let* result = Cat.find_table cat ~name:"items" in
    (match result with
     | None   -> Alcotest.fail "expected Some, got None"
     | Some _ -> ());
    Lwt.return_unit
  )

let exec_create_table_tree_id () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "first"; columns = [int_col "x"] }) in
    let* result = Cat.find_table cat ~name:"first" in
    (match result with
     | None   -> Alcotest.fail "expected Some, got None"
     | Some m -> Alcotest.(check int) "tree_id is 16" 16 m.Cat.tree_id);
    Lwt.return_unit
  )

let exec_create_table_columns () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "items"; columns = id_name_cols }) in
    let* result = Cat.find_table cat ~name:"items" in
    (match result with
     | None   -> Alcotest.fail "expected Some"
     | Some m ->
       let names = List.map (fun c -> c.Row.name) m.Cat.columns in
       Alcotest.(check (list string)) "column names" ["id"; "name"] names);
    Lwt.return_unit
  )

let exec_create_duplicate () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "items"; columns = id_name_cols }) in
    (* Second create should raise Failure *)
    (try
       ignore (Exec.execute store cat
                 (Plan.Op_create_table { name = "items"; columns = id_name_cols }));
       Alcotest.fail "expected Failure for duplicate table"
     with Failure _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 2: Op_insert                                                   *)
(* ------------------------------------------------------------------ *)

let exec_insert_one_row () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    let* meta = Cat.find_table cat ~name:"t" in
    let m = Option.get meta in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "alice"]);
    let pairs = cursor_collect store m.Cat.tree_id in
    Alcotest.(check int) "one entry" 1 (List.length pairs);
    Lwt.return_unit
  )

let exec_insert_two_rows () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    let* meta = Cat.find_table cat ~name:"t" in
    let m = Option.get meta in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "alice"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "bob"]);
    let pairs = cursor_collect store m.Cat.tree_id in
    Alcotest.(check int) "two entries" 2 (List.length pairs);
    Lwt.return_unit
  )

let exec_insert_row_content () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    let* meta = Cat.find_table cat ~name:"t" in
    let m = Option.get meta in
    insert store cat "t" ([0; 1], [Ast.L_int 7L; Ast.L_text "hat"]);
    let pairs = cursor_collect store m.Cat.tree_id in
    (match pairs with
     | [(_, vbytes)] ->
       let row = Row.decode id_name_cols vbytes in
       Alcotest.(check int) "row length" 2 (Array.length row);
       (match row.(0) with
        | Row.V_int n  -> Alcotest.(check int64) "id=7" 7L n
        | _            -> Alcotest.fail "expected V_int for id");
       (match row.(1) with
        | Row.V_text s -> Alcotest.(check string) "name=hat" "hat" s
        | _            -> Alcotest.fail "expected V_text for name")
     | _ -> Alcotest.fail "expected exactly one entry");
    Lwt.return_unit
  )

let exec_insert_null () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    let* meta = Cat.find_table cat ~name:"t" in
    let m = Option.get meta in
    (* Insert with NULL in the name column (ordinal 0 = id, ordinal 1 = name) *)
    (* We insert only id=5; name stays NULL (Array.make initialises to V_null) *)
    insert store cat "t" ([0], [Ast.L_int 5L]);
    let pairs = cursor_collect store m.Cat.tree_id in
    (match pairs with
     | [(_, vbytes)] ->
       let row = Row.decode id_name_cols vbytes in
       (match row.(1) with
        | Row.V_null -> ()
        | _          -> Alcotest.fail "expected V_null for name column")
     | _ -> Alcotest.fail "expected exactly one entry");
    Lwt.return_unit
  )

let exec_insert_increments_rowid () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    let* meta = Cat.find_table cat ~name:"t" in
    let m = Option.get meta in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    let pairs = cursor_collect store m.Cat.tree_id in
    (* Two inserts → two distinct keys in the tree *)
    Alcotest.(check int) "two distinct rowid keys" 2 (List.length pairs);
    let k0, _ = List.nth pairs 0 in
    let k1, _ = List.nth pairs 1 in
    Alcotest.(check bool) "keys are distinct" true (not (Bytes.equal k0 k1));
    Lwt.return_unit
  )

let exec_insert_text_only () =
  let schema = [txt_col "word"] in
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "words"; columns = schema }) in
    let* meta = Cat.find_table cat ~name:"words" in
    let m = Option.get meta in
    insert store cat "words" ([0], [Ast.L_text "hello"]);
    let pairs = cursor_collect store m.Cat.tree_id in
    (match pairs with
     | [(_, vbytes)] ->
       let row = Row.decode schema vbytes in
       (match row.(0) with
        | Row.V_text s -> Alcotest.(check string) "word=hello" "hello" s
        | _            -> Alcotest.fail "expected V_text")
     | _ -> Alcotest.fail "expected exactly one entry");
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 3: Op_seq_scan (via query)                                     *)
(* ------------------------------------------------------------------ *)

let query_seqscan_empty () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* stream = Exec.query store cat (Plan.Op_seq_scan { table_meta = m }) in
    let rows = collect stream in
    Alcotest.(check int) "empty table → 0 rows" 0 (List.length rows);
    Lwt.return_unit
  )

let query_seqscan_one_row () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* stream = Exec.query store cat (Plan.Op_seq_scan { table_meta = m }) in
    let rows = collect stream in
    Alcotest.(check int) "one row" 1 (List.length rows);
    Lwt.return_unit
  )

let query_seqscan_three_rows () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* stream = Exec.query store cat (Plan.Op_seq_scan { table_meta = m }) in
    let rows = collect stream in
    Alcotest.(check int) "three rows" 3 (List.length rows);
    (* Rowid keys are ordered, so rows come back in insertion order *)
    let ids = List.map (fun r -> match r.(0) with
        | Row.V_int n -> n | _ -> failwith "expected V_int") rows in
    Alcotest.(check (list int64)) "rows in insertion order" [1L; 2L; 3L] ids;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 4: Op_filter (via query)                                       *)
(* ------------------------------------------------------------------ *)

let query_filter_eq_int () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: id = 2 *)
    let pred = Plan.P_eq (Plan.P_col 0, Plan.P_lit (Ast.L_int 2L)) in
    let op = Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta = m } } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one match" 1 (List.length rows);
    (match (List.hd rows).(0) with
     | Row.V_int n -> Alcotest.(check int64) "id=2" 2L n
     | _           -> Alcotest.fail "expected V_int for id");
    Lwt.return_unit
  )

let query_filter_eq_string () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: name = 'b' *)
    let pred = Plan.P_eq (Plan.P_col 1, Plan.P_lit (Ast.L_text "b")) in
    let op = Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta = m } } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one match" 1 (List.length rows);
    (match (List.hd rows).(1) with
     | Row.V_text s -> Alcotest.(check string) "name=b" "b" s
     | _            -> Alcotest.fail "expected V_text for name");
    Lwt.return_unit
  )

let query_filter_no_match () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: id = 99 → no match *)
    let pred = Plan.P_eq (Plan.P_col 0, Plan.P_lit (Ast.L_int 99L)) in
    let op = Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta = m } } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "no match → 0 rows" 0 (List.length rows);
    Lwt.return_unit
  )

let query_filter_all_match () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: 1 = 1 → always true *)
    let pred = Plan.P_eq (Plan.P_lit (Ast.L_int 1L), Plan.P_lit (Ast.L_int 1L)) in
    let op = Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta = m } } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "all three rows match" 3 (List.length rows);
    Lwt.return_unit
  )

let query_filter_null_col () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    (* Insert a row with only name set; id stays NULL *)
    insert store cat "t" ([1], [Ast.L_text "ghost"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: id = NULL → NULL != NULL → no match *)
    let pred = Plan.P_eq (Plan.P_col 0, Plan.P_lit Ast.L_null) in
    let op = Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta = m } } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "NULL != NULL → 0 rows" 0 (List.length rows);
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 5: Op_project (via query)                                      *)
(* ------------------------------------------------------------------ *)

let query_project_all () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "alice"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_project {
      ordinals = [0; 1];
      child    = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one row" 1 (List.length rows);
    Alcotest.(check int) "2 columns" 2 (Array.length (List.hd rows));
    Lwt.return_unit
  )

let query_project_single_col () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 42L; Ast.L_text "alice"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Project only column 0 (id) *)
    let op = Plan.Op_project {
      ordinals = [0];
      child    = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one row" 1 (List.length rows);
    let row = List.hd rows in
    Alcotest.(check int) "1 column" 1 (Array.length row);
    (match row.(0) with
     | Row.V_int n -> Alcotest.(check int64) "id=42" 42L n
     | _           -> Alcotest.fail "expected V_int for id");
    Lwt.return_unit
  )

let query_project_reversed () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 7L; Ast.L_text "hat"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Reverse the columns: ordinals=[1,0] → row[0]=name, row[1]=id *)
    let op = Plan.Op_project {
      ordinals = [1; 0];
      child    = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one row" 1 (List.length rows);
    let row = List.hd rows in
    Alcotest.(check int) "2 columns" 2 (Array.length row);
    (match row.(0) with
     | Row.V_text s -> Alcotest.(check string) "row[0] is name" "hat" s
     | _            -> Alcotest.fail "expected V_text at row[0]");
    (match row.(1) with
     | Row.V_int n  -> Alcotest.(check int64) "row[1] is id" 7L n
     | _            -> Alcotest.fail "expected V_int at row[1]");
    Lwt.return_unit
  )

let query_project_star_where () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Full pipeline: Project(ordinals=[0,1], Filter(id=2, SeqScan)) *)
    let pred = Plan.P_eq (Plan.P_col 0, Plan.P_lit (Ast.L_int 2L)) in
    let op = Plan.Op_project {
      ordinals = [0; 1];
      child    = Plan.Op_filter {
        pred;
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one filtered row" 1 (List.length rows);
    let row = List.hd rows in
    Alcotest.(check int) "2 columns" 2 (Array.length row);
    (match row.(0) with
     | Row.V_int n -> Alcotest.(check int64) "id=2" 2L n
     | _           -> Alcotest.fail "expected V_int");
    (match row.(1) with
     | Row.V_text s -> Alcotest.(check string) "name=b" "b" s
     | _            -> Alcotest.fail "expected V_text");
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 6: Error conditions                                            *)
(* ------------------------------------------------------------------ *)

let execute_read_op_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Calling execute with a read op should raise Failure *)
    (try
       ignore (Exec.execute store cat (Plan.Op_seq_scan { table_meta = m }));
       Alcotest.fail "expected Failure"
     with Failure _ -> ());
    Lwt.return_unit
  )

let query_write_op_raises () =
  let store, cat = setup () in
  run (
    (* Calling query with a write op should raise Failure *)
    (try
       ignore (Exec.query store cat
                 (Plan.Op_create_table { name = "t"; columns = id_name_cols }));
       Alcotest.fail "expected Failure"
     with Failure _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Exec" [
    "create_table", [
      Alcotest.test_case "exec_create_table"         `Quick exec_create_table;
      Alcotest.test_case "exec_create_table_tree_id" `Quick exec_create_table_tree_id;
      Alcotest.test_case "exec_create_table_columns" `Quick exec_create_table_columns;
      Alcotest.test_case "exec_create_duplicate"     `Quick exec_create_duplicate;
    ];
    "insert", [
      Alcotest.test_case "exec_insert_one_row"           `Quick exec_insert_one_row;
      Alcotest.test_case "exec_insert_two_rows"          `Quick exec_insert_two_rows;
      Alcotest.test_case "exec_insert_row_content"       `Quick exec_insert_row_content;
      Alcotest.test_case "exec_insert_null"              `Quick exec_insert_null;
      Alcotest.test_case "exec_insert_increments_rowid"  `Quick exec_insert_increments_rowid;
      Alcotest.test_case "exec_insert_text_only"         `Quick exec_insert_text_only;
    ];
    "seq_scan", [
      Alcotest.test_case "query_seqscan_empty"      `Quick query_seqscan_empty;
      Alcotest.test_case "query_seqscan_one_row"    `Quick query_seqscan_one_row;
      Alcotest.test_case "query_seqscan_three_rows" `Quick query_seqscan_three_rows;
    ];
    "filter", [
      Alcotest.test_case "query_filter_eq_int"    `Quick query_filter_eq_int;
      Alcotest.test_case "query_filter_eq_string" `Quick query_filter_eq_string;
      Alcotest.test_case "query_filter_no_match"  `Quick query_filter_no_match;
      Alcotest.test_case "query_filter_all_match" `Quick query_filter_all_match;
      Alcotest.test_case "query_filter_null_col"  `Quick query_filter_null_col;
    ];
    "project", [
      Alcotest.test_case "query_project_all"        `Quick query_project_all;
      Alcotest.test_case "query_project_single_col" `Quick query_project_single_col;
      Alcotest.test_case "query_project_reversed"   `Quick query_project_reversed;
      Alcotest.test_case "query_project_star_where" `Quick query_project_star_where;
    ];
    "error_conditions", [
      Alcotest.test_case "execute_read_op_raises" `Quick execute_read_op_raises;
      Alcotest.test_case "query_write_op_raises"  `Quick query_write_op_raises;
    ];
  ]
