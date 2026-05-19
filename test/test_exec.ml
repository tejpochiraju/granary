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

let int_col name : Row.column = { name; ty = Row.Integer; not_null = false; primary_key = false; default = None; check_sql = None }
let txt_col name : Row.column = { name; ty = Row.Text;    not_null = false; primary_key = false; default = None; check_sql = None }

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
      (Plan.Op_insert { table_meta; ordinals; values = [List.map (fun l -> Plan.P_lit l) values];
                        on_conflict = None; returning = []; upsert_update = None })
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
        (Plan.Op_create_table { name = "items"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "first"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "items"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "items"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    (* Second create should raise Failure *)
    (try
       ignore (Exec.execute store cat
                 (Plan.Op_create_table { name = "items"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }));
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "words"; columns = schema; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: id = 2 *)
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_int 2L)) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: name = 'b' *)
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 1, Plan.P_lit (Ast.L_text "b")) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: id = 99 → no match *)
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_int 99L)) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: 1 = 1 → always true *)
    let pred = Plan.P_binop (Plan.Eq, Plan.P_lit (Ast.L_int 1L), Plan.P_lit (Ast.L_int 1L)) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    (* Insert a row with only name set; id stays NULL *)
    insert store cat "t" ([1], [Ast.L_text "ghost"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Filter: id = NULL → NULL != NULL → no match *)
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit Ast.L_null) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Full pipeline: Project(ordinals=[0,1], Filter(id=2, SeqScan)) *)
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_int 2L)) in
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
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* Calling execute with a read op should raise Failure *)
    (try
       ignore (Exec.execute store cat (Plan.Op_seq_scan { table_meta = m }));
       Alcotest.fail "expected Failure"
     with Failure _ -> ());
    Lwt.return_unit
  )

let execute_filter_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "tf"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"tf" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_filter { pred = Plan.P_lit (Ast.L_int 1L);
                           child = Plan.Op_seq_scan { table_meta = m } }));
       Alcotest.fail "expected Failure for Op_filter in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

let execute_project_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "tp"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"tp" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_project { ordinals = [0];
                            child = Plan.Op_seq_scan { table_meta = m } }));
       Alcotest.fail "expected Failure for Op_project in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

let query_write_op_raises () =
  let store, cat = setup () in
  run (
    (* Calling query with a write op should raise Failure *)
    (try
       ignore (Exec.query store cat
                 (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }));
       Alcotest.fail "expected Failure"
     with Failure _ -> ());
    Lwt.return_unit
  )

let query_insert_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "ti"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"ti" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.query store cat
         (Plan.Op_insert { table_meta = m; ordinals = [0];
                           values = [[Plan.P_lit (Ast.L_int 1L)]];
                           on_conflict = None; returning = []; upsert_update = None }));
       Alcotest.fail "expected Failure for Op_insert in query"
     with Failure _ -> ());
    Lwt.return_unit
  )

let query_filter_nonnull_eq_null () =
  (* Test the _, V_null arm: va is non-null, vb is V_null *)
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "tn"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "tn" ([0; 1], [Ast.L_int 5L; Ast.L_text "ghost"]);
    let* meta_opt = Cat.find_table cat ~name:"tn" in
    let m = Option.get meta_opt in
    (* Filter: name = NULL → va=V_text "ghost", vb=V_null → hits _, V_null arm *)
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 1, Plan.P_lit Ast.L_null) in
    let op = Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta = m } } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "non-null = NULL → 0 rows" 0 (List.length rows);
    Lwt.return_unit
  )

let query_filter_type_mismatch () =
  (* Test the final catch-all _ arm in eval_expr P_eq match (L54):
     va is V_int (non-null), vb is V_text (non-null), different types → hits _ -> false *)
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "tm"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "tm" ([0; 1], [Ast.L_int 5L; Ast.L_text "hi"]);
    let* meta_opt = Cat.find_table cat ~name:"tm" in
    let m = Option.get meta_opt in
    (* Filter: id (V_int 5L) = "text" (V_text) → type mismatch → catch-all _ -> false *)
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_text "text")) in
    let op = Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta = m } } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "int = text → type mismatch → 0 rows" 0 (List.length rows);
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 7: Op_sort (via query)                                          *)
(* ------------------------------------------------------------------ *)

let query_sort_asc () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_sort {
      keys = [(Plan.P_col 0, `Asc)];
      child = Plan.Op_project {
        ordinals = [0; 1];
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "three rows" 3 (List.length rows);
    let ids = List.map (fun r -> match r.(0) with
        | Row.V_int n -> n | _ -> failwith "expected V_int") rows in
    Alcotest.(check (list int64)) "sorted ascending" [1L; 2L; 3L] ids;
    Lwt.return_unit
  )

let query_sort_desc () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_sort {
      keys = [(Plan.P_col 0, `Desc)];
      child = Plan.Op_project {
        ordinals = [0; 1];
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "three rows" 3 (List.length rows);
    let ids = List.map (fun r -> match r.(0) with
        | Row.V_int n -> n | _ -> failwith "expected V_int") rows in
    Alcotest.(check (list int64)) "sorted descending" [3L; 2L; 1L] ids;
    Lwt.return_unit
  )

let query_sort_nulls_first () =
  let schema = [int_col "n"] in
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = schema; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_int 5L]);
    insert store cat "t" ([],  []);   (* all NULL *)
    insert store cat "t" ([0], [Ast.L_int 2L]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_sort {
      keys = [(Plan.P_col 0, `Asc)];
      child = Plan.Op_project {
        ordinals = [0];
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "three rows" 3 (List.length rows);
    (* NULLs sort first in ASC — NULL < any non-null value (matches SQLite) *)
    (match (List.nth rows 0).(0) with
     | Row.V_null -> ()
     | _ -> Alcotest.fail "expected NULL first");
    (match (List.nth rows 1).(0) with
     | Row.V_int 2L -> ()
     | _ -> Alcotest.fail "expected 2 second");
    (match (List.nth rows 2).(0) with
     | Row.V_int 5L -> ()
     | _ -> Alcotest.fail "expected 5 last");
    Lwt.return_unit
  )

let query_sort_empty () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_sort {
      keys = [(Plan.P_col 0, `Asc)];
      child = Plan.Op_project {
        ordinals = [0; 1];
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "sort of empty → 0 rows" 0 (List.length rows);
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 8: Op_limit (via query)                                         *)
(* ------------------------------------------------------------------ *)

let query_limit_basic () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    insert store cat "t" ([0; 1], [Ast.L_int 4L; Ast.L_text "d"]);
    insert store cat "t" ([0; 1], [Ast.L_int 5L; Ast.L_text "e"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_limit {
      limit = 3; offset = 0;
      child = Plan.Op_project {
        ordinals = [0; 1];
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "limit 3 → 3 rows" 3 (List.length rows);
    let ids = List.map (fun r -> match r.(0) with
        | Row.V_int n -> n | _ -> failwith "expected V_int") rows in
    Alcotest.(check (list int64)) "first 3" [1L; 2L; 3L] ids;
    Lwt.return_unit
  )

let query_limit_with_offset () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    insert store cat "t" ([0; 1], [Ast.L_int 4L; Ast.L_text "d"]);
    insert store cat "t" ([0; 1], [Ast.L_int 5L; Ast.L_text "e"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_limit {
      limit = 2; offset = 2;
      child = Plan.Op_project {
        ordinals = [0; 1];
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "limit 2 offset 2 → 2 rows" 2 (List.length rows);
    let ids = List.map (fun r -> match r.(0) with
        | Row.V_int n -> n | _ -> failwith "expected V_int") rows in
    Alcotest.(check (list int64)) "rows 2,3 (0-indexed)" [3L; 4L] ids;
    Lwt.return_unit
  )

let query_limit_exceeds_rows () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_limit {
      limit = 100; offset = 0;
      child = Plan.Op_project {
        ordinals = [0; 1];
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "limit larger than rows → 2 rows" 2 (List.length rows);
    Lwt.return_unit
  )

let query_limit_offset_exceeds () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_limit {
      limit = 3; offset = 10;
      child = Plan.Op_project {
        ordinals = [0; 1];
        child = Plan.Op_seq_scan { table_meta = m };
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "offset beyond rows → 0 rows" 0 (List.length rows);
    Lwt.return_unit
  )

let execute_sort_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "ts"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"ts" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_sort { keys = [(Plan.P_col 0, `Asc)];
                         child = Plan.Op_seq_scan { table_meta = m } }));
       Alcotest.fail "expected Failure for Op_sort in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

let execute_limit_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "tl"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"tl" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_limit { limit = 1; offset = 0;
                          child = Plan.Op_seq_scan { table_meta = m } }));
       Alcotest.fail "expected Failure for Op_limit in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

(** Op_union passed to execute raises — hits line 1252 of execute_with_count. *)
let execute_union_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let child = Plan.Op_seq_scan { table_meta = m } in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_union { all = false; left = child; right = child }));
       Alcotest.fail "expected Failure for Op_union in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

(** Op_distinct passed to execute raises — hits line 1259 of execute_with_count. *)
let execute_distinct_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_distinct { child = Plan.Op_seq_scan { table_meta = m } }));
       Alcotest.fail "expected Failure for Op_distinct in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

(** Op_begin passed to execute raises in the query path — hits line 1249. *)
let query_begin_raises () =
  let store, cat = setup () in
  run (
    (try
       ignore (Exec.execute store cat Plan.Op_begin);
       Alcotest.fail "expected Failure for Op_begin in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

(** Op_pragma_rows passed to execute_with_count → returns 0 (line 1251). *)
let execute_with_count_pragma_returns_zero () =
  let store, cat = setup () in
  run (
    let* n = Exec.execute_with_count store cat
        (Plan.Op_pragma_rows { rows = [ [| Row.V_text "foo" |] ] }) in
    Alcotest.(check int) "pragma execute_with_count is 0" 0 n;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 9: Op_create_index (via execute)                               *)
(* ------------------------------------------------------------------ *)

let exec_create_index_basic () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    (* Insert a row so the index-population path runs *)
    insert store cat "t" ([0; 1], [Ast.L_int 42L; Ast.L_text "alice"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name          = "idx_id";
           table         = "t";
           tree_id       = m.Cat.tree_id;
           col_idxs      = [0];
           unique        = false;
           columns       = m.Cat.columns;
           if_not_exists = false;
         }) in
    (* Index should now exist in the catalog *)
    let idx = Cat.find_index cat ~name:"idx_id" in
    Alcotest.(check bool) "index created" true (Option.is_some idx);
    Lwt.return_unit
  )

let query_create_index_raises () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "tci"; columns = [int_col "x"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"tci" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.query store cat
         (Plan.Op_create_index {
            name = "idx"; table = "tci"; tree_id = m.Cat.tree_id;
            col_idxs = [0]; unique = false; columns = m.Cat.columns; if_not_exists = false;
          }));
       Alcotest.fail "expected Failure for Op_create_index in query"
     with Failure _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 10: Op_index_lookup (via query)                                *)
(* ------------------------------------------------------------------ *)

let query_index_lookup_basic () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx_id"; table = "t"; tree_id = m.Cat.tree_id;
           col_idxs = [0]; unique = false; columns = m.Cat.columns; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx_id" in
    let (idx : Cat.index_info) = Option.get idx_opt in
    let op = Plan.Op_index_lookup {
      table_tree = m.Cat.tree_id;
      idx_tree   = idx.Cat.idx_tree_id;
      col_idx    = 0;
      col_type   = Row.Integer;
      lookup_val = Plan.P_lit (Ast.L_int 2L);
      table_meta = m;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "index lookup finds 1 row" 1 (List.length rows);
    (match (List.hd rows).(0) with
     | Row.V_int n -> Alcotest.(check int64) "id=2" 2L n
     | _ -> Alcotest.fail "expected V_int");
    Lwt.return_unit
  )

(* Type mismatch in Op_index_lookup: col_type=Integer but lookup_val is TEXT.
   The branch "| _, _ -> IK_null" is hit, resulting in zero matches. *)
let query_index_lookup_type_mismatch () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx_id2"; table = "t"; tree_id = m.Cat.tree_id;
           col_idxs = [0]; unique = false; columns = m.Cat.columns; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx_id2" in
    let (idx : Cat.index_info) = Option.get idx_opt in
    (* col_type=Integer but lookup_val is TEXT — type mismatch → IK_null → no results *)
    let op = Plan.Op_index_lookup {
      table_tree = m.Cat.tree_id;
      idx_tree   = idx.Cat.idx_tree_id;
      col_idx    = 0;
      col_type   = Row.Integer;
      lookup_val = Plan.P_lit (Ast.L_text "not-an-int");
      table_meta = m;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "type mismatch → 0 rows" 0 (List.length rows);
    Lwt.return_unit
  )

(* Multiple rows match the same index value -> Op_index_lookup must
   keep returning rows until exhausted, exercising the cursor-exhausted
   branch in the index_lookup stream loop. *)
let query_index_lookup_multiple_matches () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    (* Three rows all with id = 7 -> a non-unique index will list all *)
    insert store cat "t" ([0; 1], [Ast.L_int 7L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 7L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 7L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx_id_multi"; table = "t"; tree_id = m.Cat.tree_id;
           col_idxs = [0]; unique = false; columns = m.Cat.columns; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx_id_multi" in
    let (idx : Cat.index_info) = Option.get idx_opt in
    let op = Plan.Op_index_lookup {
      table_tree = m.Cat.tree_id;
      idx_tree   = idx.Cat.idx_tree_id;
      col_idx    = 0;
      col_type   = Row.Integer;
      lookup_val = Plan.P_lit (Ast.L_int 7L);
      table_meta = m;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "3 matching rows" 3 (List.length rows);
    Lwt.return_unit
  )

(* Op_index_lookup with col_type = Text exercises the IK_text arm. *)
let query_index_lookup_text () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "alice"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "bob"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx_name"; table = "t"; tree_id = m.Cat.tree_id;
           col_idxs = [1]; unique = false; columns = m.Cat.columns; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx_name" in
    let (idx : Cat.index_info) = Option.get idx_opt in
    let op = Plan.Op_index_lookup {
      table_tree = m.Cat.tree_id;
      idx_tree   = idx.Cat.idx_tree_id;
      col_idx    = 1;
      col_type   = Row.Text;
      lookup_val = Plan.P_lit (Ast.L_text "alice");
      table_meta = m;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "1 row matched on text key" 1 (List.length rows);
    Lwt.return_unit
  )

(* Op_index_lookup with col_type=Real -> exercises IK_real arm. *)
let query_index_lookup_real () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t";
                                columns = [{ name = "r"; ty = Row.Real; not_null = false; primary_key = false; default = None; check_sql = None }];
                                uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_real 1.5]);
    insert store cat "t" ([0], [Ast.L_real 2.5]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx_r"; table = "t"; tree_id = m.Cat.tree_id;
           col_idxs = [0]; unique = false; columns = m.Cat.columns; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx_r" in
    let (idx : Cat.index_info) = Option.get idx_opt in
    let op = Plan.Op_index_lookup {
      table_tree = m.Cat.tree_id;
      idx_tree   = idx.Cat.idx_tree_id;
      col_idx    = 0;
      col_type   = Row.Real;
      lookup_val = Plan.P_lit (Ast.L_real 2.5);
      table_meta = m;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "1 real row" 1 (List.length rows);
    Lwt.return_unit
  )

(* Op_index_lookup with col_type=Blob -> exercises IK_blob arm. *)
let query_index_lookup_blob () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t";
                                columns = [{ name = "b"; ty = Row.Blob; not_null = false; primary_key = false; default = None; check_sql = None }];
                                uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "AAAA")]);
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "BBBB")]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx_b"; table = "t"; tree_id = m.Cat.tree_id;
           col_idxs = [0]; unique = false; columns = m.Cat.columns; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx_b" in
    let (idx : Cat.index_info) = Option.get idx_opt in
    let op = Plan.Op_index_lookup {
      table_tree = m.Cat.tree_id;
      idx_tree   = idx.Cat.idx_tree_id;
      col_idx    = 0;
      col_type   = Row.Blob;
      lookup_val = Plan.P_lit (Ast.L_blob (Bytes.of_string "BBBB"));
      table_meta = m;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "1 blob row" 1 (List.length rows);
    Lwt.return_unit
  )

(* Op_index_lookup with lookup_val=NULL -> IK_null arm. *)
let query_index_lookup_null () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "x"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx_n"; table = "t"; tree_id = m.Cat.tree_id;
           col_idxs = [0]; unique = false; columns = m.Cat.columns; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx_n" in
    let (idx : Cat.index_info) = Option.get idx_opt in
    let op = Plan.Op_index_lookup {
      table_tree = m.Cat.tree_id;
      idx_tree   = idx.Cat.idx_tree_id;
      col_idx    = 0;
      col_type   = Row.Integer;
      lookup_val = Plan.P_lit Ast.L_null;
      table_meta = m;
    } in
    let* stream = Exec.query store cat op in
    let _rows = collect stream in
    Lwt.return_unit
  )

(* Filter with Op_eq comparing different value types -> exercises
   the v_real/v_blob/null arms of eval_expr. *)
let query_filter_eq_real () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t";
                                columns = [{ name = "r"; ty = Row.Real; not_null = false; primary_key = false; default = None; check_sql = None }];
                                uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_real 1.5]);
    insert store cat "t" ([0], [Ast.L_real 2.5]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_real 2.5)) in
    let op = Plan.Op_filter {
      pred;
      child = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "1 row matches real eq" 1 (List.length rows);
    Lwt.return_unit
  )

let query_filter_eq_blob () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t";
                                columns = [{ name = "b"; ty = Row.Blob; not_null = false; primary_key = false; default = None; check_sql = None }];
                                uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "AA")]);
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "BB")]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let pred = Plan.P_binop (Plan.Eq, Plan.P_col 0,
                             Plan.P_lit (Ast.L_blob (Bytes.of_string "BB"))) in
    let op = Plan.Op_filter {
      pred;
      child = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "1 row matches blob eq" 1 (List.length rows);
    Lwt.return_unit
  )

(* Sort by Real and Blob columns -> exercises compare_values for these types *)
let query_sort_real () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t";
                                columns = [{ name = "r"; ty = Row.Real; not_null = false; primary_key = false; default = None; check_sql = None }];
                                uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_real 3.0]);
    insert store cat "t" ([0], [Ast.L_real 1.0]);
    insert store cat "t" ([0], [Ast.L_real 2.0]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_sort {
      keys = [(Plan.P_col 0, `Asc)];
      child = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "3 rows sorted" 3 (List.length rows);
    Lwt.return_unit
  )

let query_sort_blob () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t";
                                columns = [{ name = "b"; ty = Row.Blob; not_null = false; primary_key = false; default = None; check_sql = None }];
                                uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "CC")]);
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "AA")]);
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "BB")]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_sort {
      keys = [(Plan.P_col 0, `Asc)];
      child = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "3 rows sorted (blob)" 3 (List.length rows);
    Lwt.return_unit
  )

(* Create index on a non-existent table should fail.  The create_index
   path runs in Lwt so we catch via Lwt.catch. *)
let exec_create_index_unknown_table () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* outcome =
      Lwt.catch
        (fun () ->
           let* () = Exec.execute store cat
               (Plan.Op_create_index {
                  name = "i"; table = "no_such_table";
                  tree_id = 99; col_idxs = [0]; unique = false;
                  columns = id_name_cols; if_not_exists = false;
                }) in
           Lwt.return `Ok)
        (fun _ -> Lwt.return `Err)
    in
    Alcotest.(check bool) "create_index on unknown table fails"
      true (outcome = `Err);
    Lwt.return_unit
  )

(* Sort with multiple NULL values -> exercises compare_values NULL/NULL arm. *)
let query_sort_multiple_nulls () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    (* Two rows with NULL in the id column *)
    insert store cat "t" ([1], [Ast.L_text "x"]);
    insert store cat "t" ([1], [Ast.L_text "y"]);
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "z"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_sort {
      keys = [(Plan.P_col 0, `Asc)];
      child = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "all rows present" 3 (List.length rows);
    Lwt.return_unit
  )

(* Unique constraint: inserting the first row should run through the
   UNIQUE-check path without finding a duplicate (covers else-arm). *)
let unique_index_first_insert_succeeds () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx_u"; table = "t"; tree_id = m.Cat.tree_id;
           col_idxs = [0]; unique = true; columns = m.Cat.columns; if_not_exists = false;
         }) in
    (* Insert two distinct values: each triggers the unique check
       (no duplicate) so the else-arm is exercised on both. *)
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 11: Op_update (via execute_with_count)                         *)
(* ------------------------------------------------------------------ *)

let exec_update_no_match_returns_zero () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* n = Exec.execute_with_count store cat
        (Plan.Op_update {
           table_meta  = m;
           assignments = [(1, Plan.P_lit (Ast.L_text "z"))];
           where       = Some (Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_int 99L)));
           indexes     = [];
         
          returning     = [] }) in
    Alcotest.(check int) "no match → 0 rows affected" 0 n;
    Lwt.return_unit
  )

let exec_update_match_returns_count () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* n = Exec.execute_with_count store cat
        (Plan.Op_update {
           table_meta  = m;
           assignments = [(1, Plan.P_lit (Ast.L_text "updated"))];
           where       = None;  (* update all *)
           indexes     = [];
         
          returning     = [] }) in
    Alcotest.(check int) "all 2 rows affected" 2 n;
    Lwt.return_unit
  )

let exec_update_raises_in_query () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.query store cat
         (Plan.Op_update {
            table_meta = m;
            assignments = [(1, Plan.P_lit (Ast.L_text "x"))];
            where = None;
            indexes = [];
          
          returning     = [] }));
       Alcotest.fail "expected Failure for Op_update in query"
     with Failure _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 12: Op_delete (via execute_with_count)                         *)
(* ------------------------------------------------------------------ *)

let exec_delete_no_match_returns_zero () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* n = Exec.execute_with_count store cat
        (Plan.Op_delete {
           table_meta = m;
           where = Some (Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_int 99L)));
           indexes = [];
         
          returning     = [] }) in
    Alcotest.(check int) "no match → 0 deleted" 0 n;
    Lwt.return_unit
  )

let exec_delete_all_returns_count () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* n = Exec.execute_with_count store cat
        (Plan.Op_delete { table_meta = m; where = None; indexes = [];
          returning     = [] }) in
    Alcotest.(check int) "all 3 deleted" 3 n;
    Lwt.return_unit
  )

let exec_delete_raises_in_query () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.query store cat
         (Plan.Op_delete { table_meta = m; where = None; indexes = [];
          returning     = [] }));
       Alcotest.fail "expected Failure for Op_delete in query"
     with Failure _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 13: Op_drop_table / Op_drop_index                              *)
(* ------------------------------------------------------------------ *)

let exec_drop_table_basic () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* () = Exec.execute store cat
        (Plan.Op_drop_table { table_meta = m; indexes = [] }) in
    let* result = Cat.find_table cat ~name:"t" in
    Alcotest.(check bool) "table gone after drop" true (Option.is_none result);
    Lwt.return_unit
  )

let exec_drop_table_returns_zero () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let* n = Exec.execute_with_count store cat
        (Plan.Op_drop_table { table_meta = m; indexes = [] }) in
    Alcotest.(check int) "drop_table returns 0" 0 n;
    Lwt.return_unit
  )

let exec_drop_table_raises_in_query () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.query store cat
         (Plan.Op_drop_table { table_meta = m; indexes = [] }));
       Alcotest.fail "expected Failure for Op_drop_table in query"
     with Failure _ -> ());
    Lwt.return_unit
  )

let exec_drop_index_basic () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx"; table = "t";
           tree_id = 16; col_idxs = [0]; unique = false;
           columns = id_name_cols; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx" in
    let idx = Option.get idx_opt in
    let* () = Exec.execute store cat
        (Plan.Op_drop_index { idx_info = idx }) in
    Alcotest.(check bool) "index gone after drop"
      true (Option.is_none (Cat.find_index cat ~name:"idx"));
    Lwt.return_unit
  )

let exec_drop_index_returns_zero () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx2"; table = "t";
           tree_id = 16; col_idxs = [0]; unique = false;
           columns = id_name_cols; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx2" in
    let idx = Option.get idx_opt in
    let* n = Exec.execute_with_count store cat
        (Plan.Op_drop_index { idx_info = idx }) in
    Alcotest.(check int) "drop_index returns 0" 0 n;
    Lwt.return_unit
  )

let exec_drop_index_raises_in_query () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* () = Exec.execute store cat
        (Plan.Op_create_index {
           name = "idx3"; table = "t";
           tree_id = 16; col_idxs = [0]; unique = false;
           columns = id_name_cols; if_not_exists = false;
         }) in
    let idx_opt = Cat.find_index cat ~name:"idx3" in
    let idx = Option.get idx_opt in
    (try
       ignore (Exec.query store cat
         (Plan.Op_drop_index { idx_info = idx }));
       Alcotest.fail "expected Failure for Op_drop_index in query"
     with Failure _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 14: Op_hash_join and Op_nested_loop_join (direct plan tests)  *)
(* ------------------------------------------------------------------ *)

let query_hash_join_inner () =
  let store, cat = setup () in
  run (
    (* Create two tables: users(id, name) and orders(uid, item) *)
    let users_cols = [int_col "id"; txt_col "name"] in
    let orders_cols = [int_col "uid"; txt_col "item"] in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "users"; columns = users_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "orders"; columns = orders_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* users_opt = Cat.find_table cat ~name:"users" in
    let um = Option.get users_opt in
    let* orders_opt = Cat.find_table cat ~name:"orders" in
    let om = Option.get orders_opt in
    insert store cat "users"  ([0; 1], [Ast.L_int 1L; Ast.L_text "alice"]);
    insert store cat "users"  ([0; 1], [Ast.L_int 2L; Ast.L_text "bob"]);
    insert store cat "orders" ([0; 1], [Ast.L_int 1L; Ast.L_text "book"]);
    insert store cat "orders" ([0; 1], [Ast.L_int 2L; Ast.L_text "pen"]);
    let n_left = List.length users_cols in
    let n_right = List.length orders_cols in
    (* Hash join: left.id (col 0) = right.uid (col 0 of right = offset 0) *)
    let op = Plan.Op_project {
      ordinals = [0; 1; 2; 3];
      child = Plan.Op_hash_join {
        left  = Plan.Op_seq_scan { table_meta = um };
        right = Plan.Op_seq_scan { table_meta = om };
        left_key  = 0;
        right_key = 0;
        join_kind = `Inner;
        right_col_offset = n_left;
        n_right_cols     = n_right;
      };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "2 rows from hash join" 2 (List.length rows);
    Lwt.return_unit
  )

let query_hash_join_left () =
  let store, cat = setup () in
  run (
    let users_cols = [int_col "id"; txt_col "name"] in
    let orders_cols = [int_col "uid"; txt_col "item"] in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "users"; columns = users_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "orders"; columns = orders_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* users_opt = Cat.find_table cat ~name:"users" in
    let um = Option.get users_opt in
    let* orders_opt = Cat.find_table cat ~name:"orders" in
    let om = Option.get orders_opt in
    insert store cat "users"  ([0; 1], [Ast.L_int 1L; Ast.L_text "alice"]);
    insert store cat "users"  ([0; 1], [Ast.L_int 2L; Ast.L_text "bob"]);
    insert store cat "orders" ([0; 1], [Ast.L_int 1L; Ast.L_text "book"]);
    (* bob has no matching order *)
    let n_left = List.length users_cols in
    let n_right = List.length orders_cols in
    let op = Plan.Op_hash_join {
      left  = Plan.Op_seq_scan { table_meta = um };
      right = Plan.Op_seq_scan { table_meta = om };
      left_key  = 0;
      right_key = 0;
      join_kind = `Left;
      right_col_offset = n_left;
      n_right_cols     = n_right;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "2 rows from left hash join" 2 (List.length rows);
    (* Find the row for bob (id=2) — right cols should be NULL. *)
    let bob = List.find (fun r ->
      match r.(0) with Row.V_int 2L -> true | _ -> false) rows
    in
    (match bob.(2) with
     | Row.V_null -> ()
     | _ -> Alcotest.fail "expected NULL uid for bob (no matching order)");
    Lwt.return_unit
  )

let query_hash_join_cartesian () =
  (* left_key = -1, right_key = -1 → cartesian product path *)
  let store, cat = setup () in
  run (
    let users_cols = [int_col "id"] in
    let orders_cols = [int_col "uid"] in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "u2"; columns = users_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "o2"; columns = orders_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* um = Cat.find_table cat ~name:"u2" in
    let um = Option.get um in
    let* om = Cat.find_table cat ~name:"o2" in
    let om = Option.get om in
    insert store cat "u2" ([0], [Ast.L_int 1L]);
    insert store cat "u2" ([0], [Ast.L_int 2L]);
    insert store cat "o2" ([0], [Ast.L_int 10L]);
    let n_right = 1 in
    let n_left = 1 in
    let op = Plan.Op_hash_join {
      left  = Plan.Op_seq_scan { table_meta = um };
      right = Plan.Op_seq_scan { table_meta = om };
      left_key  = -1;
      right_key = -1;
      join_kind = `Inner;
      right_col_offset = n_left;
      n_right_cols     = n_right;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    (* 2 left × 1 right = 2 rows *)
    Alcotest.(check int) "cartesian: 2*1=2 rows" 2 (List.length rows);
    Lwt.return_unit
  )

let query_hash_join_null_key_excluded () =
  (* A NULL join key on the right side must not match any left row. *)
  let store, cat = setup () in
  run (
    let left_cols  = [int_col "id"] in
    let right_cols = [int_col "uid"] in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "l3"; columns = left_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "r3"; columns = right_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* lm = Cat.find_table cat ~name:"l3" in
    let lm = Option.get lm in
    let* rm = Cat.find_table cat ~name:"r3" in
    let rm = Option.get rm in
    insert store cat "l3" ([0], [Ast.L_int 1L]);
    (* Insert a row with NULL uid on the right *)
    insert store cat "r3" ([], []);  (* uid = NULL *)
    let n_right = 1 in
    let n_left = 1 in
    let op = Plan.Op_hash_join {
      left  = Plan.Op_seq_scan { table_meta = lm };
      right = Plan.Op_seq_scan { table_meta = rm };
      left_key  = 0;
      right_key = 0;
      join_kind = `Inner;
      right_col_offset = n_left;
      n_right_cols     = n_right;
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "null right key → 0 joined rows" 0 (List.length rows);
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 15: Op_aggregate (direct plan tests)                           *)
(* ------------------------------------------------------------------ *)

let query_aggregate_count_star () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_aggregate {
      child     = Plan.Op_seq_scan { table_meta = m };
      group_cols = [];
      aggs      = [ { Plan.func = Ast.Agg_count; col_ord = None } ];
      having    = None;
      proj      = [ Plan.PI_agg_slot 0 ];
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one output row" 1 (List.length rows);
    (match (List.hd rows).(0) with
     | Row.V_int n -> Alcotest.(check int64) "count(*) = 3" 3L n
     | _ -> Alcotest.fail "expected V_int");
    Lwt.return_unit
  )

let query_aggregate_sum_int () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = [int_col "n"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_int 10L]);
    insert store cat "t" ([0], [Ast.L_int 20L]);
    insert store cat "t" ([0], [Ast.L_int 30L]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_aggregate {
      child     = Plan.Op_seq_scan { table_meta = m };
      group_cols = [];
      aggs      = [ { Plan.func = Ast.Agg_sum; col_ord = Some 0 } ];
      having    = None;
      proj      = [ Plan.PI_agg_slot 0 ];
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one row" 1 (List.length rows);
    (match (List.hd rows).(0) with
     | Row.V_int n -> Alcotest.(check int64) "sum = 60" 60L n
     | _ -> Alcotest.fail "expected V_int sum");
    Lwt.return_unit
  )

let query_aggregate_sum_real () =
  let store, cat = setup () in
  run (
    let schema = [{ Row.name = "r"; ty = Row.Real; not_null = false; primary_key = false; default = None; check_sql = None }] in
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = schema; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_real 1.5]);
    insert store cat "t" ([0], [Ast.L_real 2.5]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_aggregate {
      child     = Plan.Op_seq_scan { table_meta = m };
      group_cols = [];
      aggs      = [ { Plan.func = Ast.Agg_sum; col_ord = Some 0 } ];
      having    = None;
      proj      = [ Plan.PI_agg_slot 0 ];
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    (match (List.hd rows).(0) with
     | Row.V_real f -> Alcotest.(check bool) "sum_real = 4.0" true (abs_float (f -. 4.0) < 1e-9)
     | _ -> Alcotest.fail "expected V_real sum");
    Lwt.return_unit
  )

let query_aggregate_avg () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = [int_col "n"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_int 10L]);
    insert store cat "t" ([0], [Ast.L_int 20L]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_aggregate {
      child     = Plan.Op_seq_scan { table_meta = m };
      group_cols = [];
      aggs      = [ { Plan.func = Ast.Agg_avg; col_ord = Some 0 } ];
      having    = None;
      proj      = [ Plan.PI_agg_slot 0 ];
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    (match (List.hd rows).(0) with
     | Row.V_real f -> Alcotest.(check bool) "avg = 15.0" true (abs_float (f -. 15.0) < 1e-9)
     | _ -> Alcotest.fail "expected V_real avg");
    Lwt.return_unit
  )

let query_aggregate_min_max () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = [int_col "n"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0], [Ast.L_int 5L]);
    insert store cat "t" ([0], [Ast.L_int 1L]);
    insert store cat "t" ([0], [Ast.L_int 9L]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_aggregate {
      child     = Plan.Op_seq_scan { table_meta = m };
      group_cols = [];
      aggs      = [ { Plan.func = Ast.Agg_min; col_ord = Some 0 };
                    { Plan.func = Ast.Agg_max; col_ord = Some 0 } ];
      having    = None;
      proj      = [ Plan.PI_agg_slot 0; Plan.PI_agg_slot 1 ];
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "one agg row" 1 (List.length rows);
    let r = List.hd rows in
    (match r.(0) with
     | Row.V_int n -> Alcotest.(check int64) "min=1" 1L n
     | _ -> Alcotest.fail "expected V_int min");
    (match r.(1) with
     | Row.V_int n -> Alcotest.(check int64) "max=9" 9L n
     | _ -> Alcotest.fail "expected V_int max");
    Lwt.return_unit
  )

let query_aggregate_count_col_skips_null () =
  (* COUNT(id) should not count NULL values. *)
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([1],   [Ast.L_text "b"]);  (* id = NULL *)
    insert store cat "t" ([0; 1], [Ast.L_int 3L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_aggregate {
      child     = Plan.Op_seq_scan { table_meta = m };
      group_cols = [];
      aggs      = [ { Plan.func = Ast.Agg_count; col_ord = Some 0 } ];
      having    = None;
      proj      = [ Plan.PI_agg_slot 0 ];
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    (match (List.hd rows).(0) with
     | Row.V_int n -> Alcotest.(check int64) "count(id) = 2 (null skipped)" 2L n
     | _ -> Alcotest.fail "expected V_int");
    Lwt.return_unit
  )

let query_aggregate_with_group_by () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_aggregate {
      child     = Plan.Op_seq_scan { table_meta = m };
      group_cols = [0];  (* GROUP BY id *)
      aggs      = [ { Plan.func = Ast.Agg_count; col_ord = None } ];
      having    = None;
      proj      = [ Plan.PI_group_col 0; Plan.PI_agg_slot 0 ];
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "2 groups" 2 (List.length rows);
    Lwt.return_unit
  )

let query_aggregate_with_having () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "c"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (* HAVING count-star > 1 → only group id=1 (count=2) passes *)
    let op = Plan.Op_aggregate {
      child     = Plan.Op_seq_scan { table_meta = m };
      group_cols = [0];
      aggs      = [ { Plan.func = Ast.Agg_count; col_ord = None } ];
      having    = Some (Plan.P_binop (Plan.Gt, Plan.P_col 1, Plan.P_lit (Ast.L_int 1L)));
      proj      = [ Plan.PI_group_col 0; Plan.PI_agg_slot 0 ];
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "1 group passes HAVING" 1 (List.length rows);
    (match (List.hd rows).(0) with
     | Row.V_int n -> Alcotest.(check int64) "group id=1" 1L n
     | _ -> Alcotest.fail "expected V_int group key");
    Lwt.return_unit
  )

let query_aggregate_raises_in_execute () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_aggregate {
            child = Plan.Op_seq_scan { table_meta = m };
            group_cols = [];
            aggs = [ { Plan.func = Ast.Agg_count; col_ord = None } ];
            having = None;
            proj = [ Plan.PI_agg_slot 0 ];
          }));
       Alcotest.fail "expected Failure for Op_aggregate in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

let query_nlj_raises_in_execute () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_nested_loop_join {
            left = Plan.Op_seq_scan { table_meta = m };
            right_meta = m;
            idx_tree = 99;
            right_col_idx = 0;
            left_col_idx = 0;
            join_kind = `Inner;
            right_col_offset = 2;
            n_right_cols = 2;
          }));
       Alcotest.fail "expected Failure for Op_nested_loop_join in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

let query_hash_join_raises_in_execute () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    (try
       ignore (Exec.execute store cat
         (Plan.Op_hash_join {
            left = Plan.Op_seq_scan { table_meta = m };
            right = Plan.Op_seq_scan { table_meta = m };
            left_key = 0; right_key = 0;
            join_kind = `Inner;
            right_col_offset = 2; n_right_cols = 2;
          }));
       Alcotest.fail "expected Failure for Op_hash_join in execute"
     with Failure _ -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group N: Op_distinct with V_blob — exercises row_key V_blob arm      *)
(* ------------------------------------------------------------------ *)

let query_distinct_blob () =
  let blob_col name : Row.column =
    { name; ty = Row.Blob; not_null = false; primary_key = false; default = None; check_sql = None }
  in
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = [blob_col "b"]; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    (* Two identical blob values and one distinct one. *)
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "AA")]);
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "AA")]);
    insert store cat "t" ([0], [Ast.L_blob (Bytes.of_string "BB")]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_distinct {
      child = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    (* DISTINCT should collapse two "AA" blobs into one. *)
    Alcotest.(check int) "distinct blob: 2 rows" 2 (List.length rows);
    Lwt.return_unit
  )

(** Multi-key Op_sort — exercises the fold_left second-key path. *)
let query_multikey_sort () =
  let store, cat = setup () in
  run (
    let* () = Exec.execute store cat
        (Plan.Op_create_table { name = "t"; columns = id_name_cols; uniq_idxs = []; if_not_exists = false; fk_constraints = [] }) in
    (* Four rows with id=1 or id=2; name used as tiebreaker. *)
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "c"]);
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "a"]);
    insert store cat "t" ([0; 1], [Ast.L_int 2L; Ast.L_text "b"]);
    insert store cat "t" ([0; 1], [Ast.L_int 1L; Ast.L_text "b"]);
    let* meta_opt = Cat.find_table cat ~name:"t" in
    let m = Option.get meta_opt in
    let op = Plan.Op_sort {
      keys = [(Plan.P_col 0, `Asc); (Plan.P_col 1, `Asc)];
      child = Plan.Op_seq_scan { table_meta = m };
    } in
    let* stream = Exec.query store cat op in
    let rows = collect stream in
    Alcotest.(check int) "multikey sort: 4 rows" 4 (List.length rows);
    (* First three rows should all have id=1 (sorted a,b,c), last is id=2. *)
    (match (List.nth rows 0).(1) with
     | Row.V_text "a" -> ()
     | _ -> Alcotest.fail "expected 'a' first after multikey sort");
    (match (List.nth rows 1).(1) with
     | Row.V_text "b" -> ()
     | _ -> Alcotest.fail "expected 'b' second after multikey sort");
    (match (List.nth rows 2).(1) with
     | Row.V_text "c" -> ()
     | _ -> Alcotest.fail "expected 'c' third after multikey sort");
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
      Alcotest.test_case "query_filter_eq_int"          `Quick query_filter_eq_int;
      Alcotest.test_case "query_filter_eq_string"       `Quick query_filter_eq_string;
      Alcotest.test_case "query_filter_no_match"        `Quick query_filter_no_match;
      Alcotest.test_case "query_filter_all_match"       `Quick query_filter_all_match;
      Alcotest.test_case "query_filter_null_col"        `Quick query_filter_null_col;
      Alcotest.test_case "query_filter_nonnull_eq_null" `Quick query_filter_nonnull_eq_null;
      Alcotest.test_case "query_filter_type_mismatch"   `Quick query_filter_type_mismatch;
    ];
    "project", [
      Alcotest.test_case "query_project_all"        `Quick query_project_all;
      Alcotest.test_case "query_project_single_col" `Quick query_project_single_col;
      Alcotest.test_case "query_project_reversed"   `Quick query_project_reversed;
      Alcotest.test_case "query_project_star_where" `Quick query_project_star_where;
    ];
    "error_conditions", [
      Alcotest.test_case "execute_read_op_raises"  `Quick execute_read_op_raises;
      Alcotest.test_case "execute_filter_raises"   `Quick execute_filter_raises;
      Alcotest.test_case "execute_project_raises"  `Quick execute_project_raises;
      Alcotest.test_case "query_write_op_raises"   `Quick query_write_op_raises;
      Alcotest.test_case "query_insert_raises"     `Quick query_insert_raises;
      Alcotest.test_case "execute_sort_raises"     `Quick execute_sort_raises;
      Alcotest.test_case "execute_limit_raises"    `Quick execute_limit_raises;
      Alcotest.test_case "execute_union_raises"    `Quick execute_union_raises;
      Alcotest.test_case "execute_distinct_raises" `Quick execute_distinct_raises;
      Alcotest.test_case "query_begin_raises"      `Quick query_begin_raises;
      Alcotest.test_case "execute_with_count_pragma_zero" `Quick execute_with_count_pragma_returns_zero;
    ];
    "sort", [
      Alcotest.test_case "query_sort_asc"       `Quick query_sort_asc;
      Alcotest.test_case "query_sort_desc"      `Quick query_sort_desc;
      Alcotest.test_case "query_sort_nulls_first" `Quick query_sort_nulls_first;
      Alcotest.test_case "query_sort_empty"     `Quick query_sort_empty;
    ];
    "limit", [
      Alcotest.test_case "query_limit_basic"          `Quick query_limit_basic;
      Alcotest.test_case "query_limit_with_offset"    `Quick query_limit_with_offset;
      Alcotest.test_case "query_limit_exceeds_rows"   `Quick query_limit_exceeds_rows;
      Alcotest.test_case "query_limit_offset_exceeds" `Quick query_limit_offset_exceeds;
    ];
    "create_index", [
      Alcotest.test_case "exec_create_index_basic"   `Quick exec_create_index_basic;
      Alcotest.test_case "query_create_index_raises" `Quick query_create_index_raises;
    ];
    "index_lookup", [
      Alcotest.test_case "query_index_lookup_basic"          `Quick query_index_lookup_basic;
      Alcotest.test_case "query_index_lookup_type_mismatch"  `Quick query_index_lookup_type_mismatch;
      Alcotest.test_case "query_index_lookup_multiple"       `Quick query_index_lookup_multiple_matches;
      Alcotest.test_case "query_index_lookup_text"           `Quick query_index_lookup_text;
      Alcotest.test_case "query_index_lookup_real"           `Quick query_index_lookup_real;
      Alcotest.test_case "query_index_lookup_blob"           `Quick query_index_lookup_blob;
      Alcotest.test_case "query_index_lookup_null"           `Quick query_index_lookup_null;
      Alcotest.test_case "query_filter_eq_real"              `Quick query_filter_eq_real;
      Alcotest.test_case "query_filter_eq_blob"              `Quick query_filter_eq_blob;
      Alcotest.test_case "query_sort_real"                   `Quick query_sort_real;
      Alcotest.test_case "query_sort_blob"                   `Quick query_sort_blob;
      Alcotest.test_case "query_sort_multiple_nulls"         `Quick query_sort_multiple_nulls;
      Alcotest.test_case "exec_create_index_unknown_table"   `Quick exec_create_index_unknown_table;
      Alcotest.test_case "unique_first_insert_succeeds"      `Quick unique_index_first_insert_succeeds;
    ];
    "update", [
      Alcotest.test_case "exec_update_no_match_returns_zero" `Quick exec_update_no_match_returns_zero;
      Alcotest.test_case "exec_update_match_returns_count"   `Quick exec_update_match_returns_count;
      Alcotest.test_case "exec_update_raises_in_query"       `Quick exec_update_raises_in_query;
    ];
    "delete", [
      Alcotest.test_case "exec_delete_no_match_returns_zero" `Quick exec_delete_no_match_returns_zero;
      Alcotest.test_case "exec_delete_all_returns_count"     `Quick exec_delete_all_returns_count;
      Alcotest.test_case "exec_delete_raises_in_query"       `Quick exec_delete_raises_in_query;
    ];
    "drop", [
      Alcotest.test_case "exec_drop_table_basic"            `Quick exec_drop_table_basic;
      Alcotest.test_case "exec_drop_table_returns_zero"     `Quick exec_drop_table_returns_zero;
      Alcotest.test_case "exec_drop_table_raises_in_query"  `Quick exec_drop_table_raises_in_query;
      Alcotest.test_case "exec_drop_index_basic"            `Quick exec_drop_index_basic;
      Alcotest.test_case "exec_drop_index_returns_zero"     `Quick exec_drop_index_returns_zero;
      Alcotest.test_case "exec_drop_index_raises_in_query"  `Quick exec_drop_index_raises_in_query;
    ];
    "hash_join", [
      Alcotest.test_case "query_hash_join_inner"           `Quick query_hash_join_inner;
      Alcotest.test_case "query_hash_join_left"            `Quick query_hash_join_left;
      Alcotest.test_case "query_hash_join_cartesian"       `Quick query_hash_join_cartesian;
      Alcotest.test_case "query_hash_join_null_key"        `Quick query_hash_join_null_key_excluded;
      Alcotest.test_case "query_nlj_raises_in_execute"     `Quick query_nlj_raises_in_execute;
      Alcotest.test_case "query_hash_join_raises_in_execute" `Quick query_hash_join_raises_in_execute;
    ];
    "distinct_and_multikey_sort", [
      Alcotest.test_case "query_distinct_blob"   `Quick query_distinct_blob;
      Alcotest.test_case "query_multikey_sort"   `Quick query_multikey_sort;
    ];
    "aggregate", [
      Alcotest.test_case "query_aggregate_count_star"          `Quick query_aggregate_count_star;
      Alcotest.test_case "query_aggregate_sum_int"             `Quick query_aggregate_sum_int;
      Alcotest.test_case "query_aggregate_sum_real"            `Quick query_aggregate_sum_real;
      Alcotest.test_case "query_aggregate_avg"                 `Quick query_aggregate_avg;
      Alcotest.test_case "query_aggregate_min_max"             `Quick query_aggregate_min_max;
      Alcotest.test_case "query_aggregate_count_col_skips_null" `Quick query_aggregate_count_col_skips_null;
      Alcotest.test_case "query_aggregate_with_group_by"       `Quick query_aggregate_with_group_by;
      Alcotest.test_case "query_aggregate_with_having"         `Quick query_aggregate_with_having;
      Alcotest.test_case "query_aggregate_raises_in_execute"   `Quick query_aggregate_raises_in_execute;
    ];
  ]
