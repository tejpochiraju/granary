(** Tests for Phase 2 Task 5: INNER JOIN and LEFT JOIN.

    Covers:
    - INNER JOIN matching rows -> correct cross product
    - INNER JOIN non-matching -> empty
    - LEFT JOIN preserves outer rows (NULL right cols)
    - Indexed nested-loop plan selection
    - Hash join plan selection
    - JOIN with WHERE and ORDER BY
    - Sema errors for ambiguous columns / unknown JOIN table
    - SELECT * with JOIN returns left ++ right cols
    - QCheck (10_000 trials): INNER JOIN ⊆ filtered cartesian
    - QCheck (10_000 trials): LEFT JOIN size ≥ INNER JOIN size *)

open Lwt.Syntax
module Db = Sqlocaml.Db
module Ast = Sqlocaml_sql.Ast
module Sema = Sqlocaml_sql.Sema
module Plan = Sqlocaml_sql.Plan
module Planner = Sqlocaml_sql.Planner
module Parser = Sqlocaml_sql.Parser
module Lexer = Sqlocaml_sql.Lexer
module Cat = Sqlocaml_catalog.Catalog
module Row = Sqlocaml_encoding.Row
module S = Sqlocaml_store.Store

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let run = Lwt_main.run
let fresh_db () = run (Db.open_in_memory ())

let exec db sql =
  run
    (let* result = Db.execute db sql in
     (match result with
      | Ok () -> ()
      | Error _ -> Alcotest.failf "exec: unexpected error for: %s" sql);
     Lwt.return_unit)
;;

let query_ok db sql =
  run
    (let* result = Db.query db sql in
     match result with
     | Error _ -> Alcotest.failf "query_ok: unexpected error for: %s" sql
     | Ok stream -> Lwt_stream.to_list stream)
;;

(** Setup: two tables for join tests.
    [users (id INTEGER, name TEXT)]
    [orders (uid INTEGER, item TEXT)] *)
let two_tables_db () =
  let db = fresh_db () in
  exec db "CREATE TABLE users (id INTEGER, name TEXT)";
  exec db "CREATE TABLE orders (uid INTEGER, item TEXT)";
  db
;;

(* Parse SQL into AST stmt — handy for parser tests. *)
let parse_stmt sql =
  let lexbuf = Lexing.from_string sql in
  try Some (Parser.stmt_eof Lexer.token lexbuf) with
  | _ -> None
;;

(* Build a catalog with two tables (no indexes), for sema/planner tests. *)
let make_two_table_cat ?(orders_idx = false) () =
  run
    (let store = S.create () in
     let* cat = Cat.open_ store in
     let* _ =
       Cat.create_table
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
           ; { Row.name = "name"
             ; ty = Row.Text
             ; not_null = false
             ; primary_key = false
             ; default = None
             ; check_sql = None
             ; generated_as = None
             }
           ]
         ~without_rowid:false
     in
     let* _ =
       Cat.create_table
         cat
         ~name:"orders"
         ~columns:
           [ { Row.name = "uid"
             ; ty = Row.Integer
             ; not_null = false
             ; primary_key = false
             ; default = None
             ; check_sql = None
             ; generated_as = None
             }
           ; { Row.name = "item"
             ; ty = Row.Text
             ; not_null = false
             ; primary_key = false
             ; default = None
             ; check_sql = None
             ; generated_as = None
             }
           ]
         ~without_rowid:false
     in
     let* () =
       if orders_idx
       then (
         let* r =
           Cat.create_index
             cat
             ~name:"idx_orders_uid"
             ~table:"orders"
             ~columns:[ "uid" ]
             ~unique:false
             ~expr_flags:[ false ]
             ~where_sql:None
             ~origin:`User
         in
         (match r with
          | Ok _ -> ()
          | Error e -> failwith e);
         Lwt.return_unit)
       else Lwt.return_unit
     in
     Lwt.return cat)
;;

let bind cat stmt = run (Sema.bind cat stmt)
let bind_ok cat stmt = bind cat stmt |> Result.get_ok

(* ------------------------------------------------------------------ *)
(* Group 1: parser                                                      *)
(* ------------------------------------------------------------------ *)

let parse_inner_join () =
  match parse_stmt "SELECT * FROM users INNER JOIN orders ON users.id = orders.uid" with
  | Some (Ast.S_select { joins = [ j ]; _ }) ->
    Alcotest.(check string) "right table" "orders" j.Ast.table;
    (match j.kind with
     | Ast.Inner -> ()
     | Ast.Left -> Alcotest.fail "expected Inner")
  | _ -> Alcotest.fail "expected S_select with one INNER JOIN"
;;

let parse_left_join () =
  match parse_stmt "SELECT * FROM users LEFT JOIN orders ON users.id = orders.uid" with
  | Some (Ast.S_select { joins = [ j ]; _ }) ->
    (match j.kind with
     | Ast.Left -> ()
     | Ast.Inner -> Alcotest.fail "expected Left")
  | _ -> Alcotest.fail "expected S_select with one LEFT JOIN"
;;

let parse_left_outer_join () =
  match
    parse_stmt "SELECT * FROM users LEFT OUTER JOIN orders ON users.id = orders.uid"
  with
  | Some (Ast.S_select { joins = [ j ]; _ }) ->
    (match j.kind with
     | Ast.Left -> ()
     | _ -> Alcotest.fail "expected Left (from LEFT OUTER)")
  | _ -> Alcotest.fail "expected S_select with one LEFT OUTER JOIN"
;;

let parse_bare_join_is_inner () =
  match parse_stmt "SELECT * FROM users JOIN orders ON users.id = orders.uid" with
  | Some (Ast.S_select { joins = [ j ]; _ }) ->
    (match j.kind with
     | Ast.Inner -> ()
     | _ -> Alcotest.fail "expected Inner (from bare JOIN)")
  | _ -> Alcotest.fail "expected S_select with one bare JOIN (inner)"
;;

let parse_no_join () =
  match parse_stmt "SELECT * FROM users" with
  | Some (Ast.S_select { joins = []; _ }) -> ()
  | _ -> Alcotest.fail "expected S_select with no joins"
;;

(* ------------------------------------------------------------------ *)
(* Group 2: sema — errors                                               *)
(* ------------------------------------------------------------------ *)

let sema_unknown_join_table () =
  let cat = make_two_table_cat () in
  let stmt =
    Ast.S_select
      { distinct = false
      ; proj = `All
      ; table = "users"
      ; table_alias = None
      ; joins =
          [ { kind = Ast.Inner
            ; table = "ghost"
            ; alias = None
            ; on = Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_col "uid")
            }
          ]
      ; where = None
      ; group_by = []
      ; having = None
      ; order = []
      ; limit = None
      ; offset = None
      }
  in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost"
;;

let sema_ambiguous_column () =
  let cat =
    run
      (let store = S.create () in
       let* cat = Cat.open_ store in
       let* _ =
         Cat.create_table
           cat
           ~name:"a"
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
       in
       let* _ =
         Cat.create_table
           cat
           ~name:"b"
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
       in
       Lwt.return cat)
  in
  (* SELECT x FROM a JOIN b ON ...  — "x" appears in both *)
  let stmt =
    Ast.S_select
      { distinct = false
      ; proj = `Cols [ "x" ]
      ; table = "a"
      ; table_alias = None
      ; joins =
          [ { kind = Ast.Inner; table = "b"; alias = None; on = Ast.E_lit (Ast.L_int 1L) }
          ]
      ; where = None
      ; group_by = []
      ; having = None
      ; order = []
      ; limit = None
      ; offset = None
      }
  in
  match bind cat stmt with
  | Error (Sema.Ambiguous_column "x") -> ()
  | _ -> Alcotest.fail "expected Ambiguous_column x"
;;

let sema_qualified_column_resolves () =
  let cat =
    run
      (let store = S.create () in
       let* cat = Cat.open_ store in
       let* _ =
         Cat.create_table
           cat
           ~name:"a"
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
       in
       let* _ =
         Cat.create_table
           cat
           ~name:"b"
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
       in
       Lwt.return cat)
  in
  let stmt =
    Ast.S_select
      { distinct = false
      ; proj = `All
      ; table = "a"
      ; table_alias = None
      ; joins =
          [ { kind = Ast.Inner
            ; table = "b"
            ; alias = None
            ; on = Ast.E_binop (Ast.Eq, Ast.E_tbl_col ("a", "x"), Ast.E_tbl_col ("b", "x"))
            }
          ]
      ; where = None
      ; group_by = []
      ; having = None
      ; order = []
      ; limit = None
      ; offset = None
      }
  in
  match bind cat stmt with
  | Ok (Sema.BS_select { joins = _ :: _; proj; _ }) ->
    Alcotest.(check (list int)) "select * → left ++ right ordinals" [ 0; 1 ] proj
  | _ -> Alcotest.fail "expected Ok BS_select with qualified columns resolved"
;;

(* ------------------------------------------------------------------ *)
(* Group 3: planner — operator selection                                *)
(* ------------------------------------------------------------------ *)

let planner_picks_hash_join_no_index () =
  let cat = make_two_table_cat () in
  let stmt =
    Ast.S_select
      { distinct = false
      ; proj = `All
      ; table = "users"
      ; table_alias = None
      ; joins =
          [ { kind = Ast.Inner
            ; table = "orders"
            ; alias = None
            ; on =
                Ast.E_binop
                  (Ast.Eq, Ast.E_tbl_col ("users", "id"), Ast.E_tbl_col ("orders", "uid"))
            }
          ]
      ; where = None
      ; group_by = []
      ; having = None
      ; order = []
      ; limit = None
      ; offset = None
      }
  in
  let bound = bind_ok cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_project { child = Plan.Op_hash_join _; _ } -> ()
  | _ -> Alcotest.fail "expected Op_project { child = Op_hash_join _ }"
;;

let planner_picks_nlj_with_index () =
  let cat = make_two_table_cat ~orders_idx:true () in
  let stmt =
    Ast.S_select
      { distinct = false
      ; proj = `All
      ; table = "users"
      ; table_alias = None
      ; joins =
          [ { kind = Ast.Inner
            ; table = "orders"
            ; alias = None
            ; on =
                Ast.E_binop
                  (Ast.Eq, Ast.E_tbl_col ("users", "id"), Ast.E_tbl_col ("orders", "uid"))
            }
          ]
      ; where = None
      ; group_by = []
      ; having = None
      ; order = []
      ; limit = None
      ; offset = None
      }
  in
  let bound = bind_ok cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_project { child = Plan.Op_nested_loop_join _; _ } -> ()
  | _ -> Alcotest.fail "expected Op_project { child = Op_nested_loop_join _ }"
;;

(* ------------------------------------------------------------------ *)
(* Group 4: execution                                                   *)
(* ------------------------------------------------------------------ *)

let inner_join_matching () =
  let db = two_tables_db () in
  exec db "INSERT INTO users (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO users (id, name) VALUES (2, 'bob')";
  exec db "INSERT INTO orders (uid, item) VALUES (1, 'book')";
  exec db "INSERT INTO orders (uid, item) VALUES (2, 'pen')";
  let rows =
    query_ok db "SELECT * FROM users INNER JOIN orders ON users.id = orders.uid"
  in
  Alcotest.(check int) "two matched rows" 2 (List.length rows);
  List.iter
    (fun r ->
       Alcotest.(check int) "4 cols" 4 (Array.length r);
       (* uid (col 2) must equal id (col 0). *)
       match r.(0), r.(2) with
       | Row.V_int a, Row.V_int b -> Alcotest.(check int64) "id == uid" a b
       | _ -> Alcotest.fail "expected V_int in cols 0,2")
    rows
;;

let inner_join_no_matches () =
  let db = two_tables_db () in
  exec db "INSERT INTO users (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO orders (uid, item) VALUES (99, 'mystery')";
  let rows =
    query_ok db "SELECT * FROM users INNER JOIN orders ON users.id = orders.uid"
  in
  Alcotest.(check int) "no matches → 0 rows" 0 (List.length rows)
;;

let left_join_preserves_outer () =
  let db = two_tables_db () in
  exec db "INSERT INTO users (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO users (id, name) VALUES (2, 'bob')";
  exec db "INSERT INTO orders (uid, item) VALUES (1, 'book')";
  let rows =
    query_ok db "SELECT * FROM users LEFT JOIN orders ON users.id = orders.uid"
  in
  Alcotest.(check int) "two rows (alice matched, bob unmatched)" 2 (List.length rows);
  (* Find bob: should have V_null in cols 2,3 *)
  let bob_row =
    List.find
      (fun r ->
         match r.(0) with
         | Row.V_int 2L -> true
         | _ -> false)
      rows
  in
  match bob_row.(2), bob_row.(3) with
  | Row.V_null, Row.V_null -> ()
  | _ -> Alcotest.fail "expected NULLs for unmatched right side (bob)"
;;

let left_join_matching_rows () =
  let db = two_tables_db () in
  exec db "INSERT INTO users (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO orders (uid, item) VALUES (1, 'book')";
  let rows =
    query_ok db "SELECT * FROM users LEFT JOIN orders ON users.id = orders.uid"
  in
  Alcotest.(check int) "one matched row" 1 (List.length rows);
  let r = List.hd rows in
  match r.(3) with
  | Row.V_text s -> Alcotest.(check string) "item=book" "book" s
  | _ -> Alcotest.fail "expected V_text for item"
;;

let join_with_where () =
  let db = two_tables_db () in
  exec db "INSERT INTO users (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO users (id, name) VALUES (2, 'bob')";
  exec db "INSERT INTO orders (uid, item) VALUES (1, 'book')";
  exec db "INSERT INTO orders (uid, item) VALUES (2, 'pen')";
  let rows =
    query_ok
      db
      "SELECT * FROM users INNER JOIN orders ON users.id = orders.uid WHERE users.name = \
       'alice'"
  in
  Alcotest.(check int) "one row matches WHERE filter" 1 (List.length rows);
  match (List.hd rows).(1) with
  | Row.V_text s -> Alcotest.(check string) "name=alice" "alice" s
  | _ -> Alcotest.fail "expected V_text for name"
;;

let join_with_order_by () =
  let db = two_tables_db () in
  exec db "INSERT INTO users (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO users (id, name) VALUES (2, 'bob')";
  exec db "INSERT INTO orders (uid, item) VALUES (1, 'zebra')";
  exec db "INSERT INTO orders (uid, item) VALUES (2, 'apple')";
  let rows =
    query_ok
      db
      "SELECT * FROM users INNER JOIN orders ON users.id = orders.uid ORDER BY item ASC"
  in
  Alcotest.(check int) "two rows" 2 (List.length rows);
  let items =
    List.map
      (fun r ->
         match r.(3) with
         | Row.V_text s -> s
         | _ -> "")
      rows
  in
  Alcotest.(check (list string)) "sorted ascending by item" [ "apple"; "zebra" ] items
;;

let select_star_columns () =
  let db = two_tables_db () in
  exec db "INSERT INTO users (id, name) VALUES (7, 'gus')";
  exec db "INSERT INTO orders (uid, item) VALUES (7, 'pen')";
  let rows =
    query_ok db "SELECT * FROM users INNER JOIN orders ON users.id = orders.uid"
  in
  let r = List.hd rows in
  Alcotest.(check int) "4 cols (id,name,uid,item)" 4 (Array.length r);
  match r.(0), r.(1), r.(2), r.(3) with
  | Row.V_int 7L, Row.V_text "gus", Row.V_int 7L, Row.V_text "pen" -> ()
  | _ -> Alcotest.fail "wrong combined-row content"
;;

(** NULL = NULL must not produce a join match (SQL 3-valued logic). *)
let null_key_inner_join_empty_hash_join () =
  (* No index on R.k — planner selects hash join. *)
  let db = fresh_db () in
  exec db "CREATE TABLE l (id INTEGER, k INTEGER)";
  exec db "CREATE TABLE r (id INTEGER, k INTEGER)";
  exec db "INSERT INTO l (id, k) VALUES (1, NULL)";
  exec db "INSERT INTO r (id, k) VALUES (2, NULL)";
  let rows = query_ok db "SELECT * FROM l INNER JOIN r ON l.k = r.k" in
  Alcotest.(check int) "NULL=NULL inner join (hash): empty result" 0 (List.length rows)
;;

let null_key_inner_join_empty_nlj () =
  (* Index on r.k — planner selects nested-loop join. *)
  let db = fresh_db () in
  exec db "CREATE TABLE l (id INTEGER, k INTEGER)";
  exec db "CREATE TABLE r (id INTEGER, k INTEGER)";
  exec db "CREATE INDEX idx_r_k ON r (k)";
  exec db "INSERT INTO l (id, k) VALUES (1, NULL)";
  exec db "INSERT INTO r (id, k) VALUES (2, NULL)";
  let rows = query_ok db "SELECT * FROM l INNER JOIN r ON l.k = r.k" in
  Alcotest.(check int) "NULL=NULL inner join (NLJ): empty result" 0 (List.length rows)
;;

let null_key_left_join_pads_hash_join () =
  (* No index — hash join path.  The left row (NULL key) must appear with
     NULL-padded right columns since no right row matches. *)
  let db = fresh_db () in
  exec db "CREATE TABLE l (id INTEGER, k INTEGER)";
  exec db "CREATE TABLE r (id INTEGER, k INTEGER)";
  exec db "INSERT INTO l (id, k) VALUES (1, NULL)";
  exec db "INSERT INTO r (id, k) VALUES (2, NULL)";
  let rows = query_ok db "SELECT * FROM l LEFT JOIN r ON l.k = r.k" in
  Alcotest.(check int) "NULL=NULL left join (hash): one padded row" 1 (List.length rows);
  let row = List.hd rows in
  (* r.id and r.k must be NULL in the result. *)
  match row.(2), row.(3) with
  | Row.V_null, Row.V_null -> ()
  | _ -> Alcotest.fail "expected NULL-padded right columns for unmatched left row"
;;

let null_key_left_join_pads_nlj () =
  (* Index on r.k — NLJ path.  Same expectation as above. *)
  let db = fresh_db () in
  exec db "CREATE TABLE l (id INTEGER, k INTEGER)";
  exec db "CREATE TABLE r (id INTEGER, k INTEGER)";
  exec db "CREATE INDEX idx_r_k ON r (k)";
  exec db "INSERT INTO l (id, k) VALUES (1, NULL)";
  exec db "INSERT INTO r (id, k) VALUES (2, NULL)";
  let rows = query_ok db "SELECT * FROM l LEFT JOIN r ON l.k = r.k" in
  Alcotest.(check int) "NULL=NULL left join (NLJ): one padded row" 1 (List.length rows);
  let row = List.hd rows in
  match row.(2), row.(3) with
  | Row.V_null, Row.V_null -> ()
  | _ -> Alcotest.fail "expected NULL-padded right columns for unmatched left row"
;;

let nlj_with_index_returns_same_as_seqscan () =
  (* Same data with and without an index — INNER JOIN result must match. *)
  let mk_db ~with_index =
    let db = fresh_db () in
    exec db "CREATE TABLE users (id INTEGER, name TEXT)";
    exec db "CREATE TABLE orders (uid INTEGER, item TEXT)";
    if with_index then exec db "CREATE INDEX idx_orders_uid ON orders (uid)";
    exec db "INSERT INTO users (id, name) VALUES (1, 'alice')";
    exec db "INSERT INTO users (id, name) VALUES (2, 'bob')";
    exec db "INSERT INTO users (id, name) VALUES (3, 'carol')";
    exec db "INSERT INTO orders (uid, item) VALUES (1, 'a')";
    exec db "INSERT INTO orders (uid, item) VALUES (2, 'b')";
    exec db "INSERT INTO orders (uid, item) VALUES (2, 'c')";
    exec db "INSERT INTO orders (uid, item) VALUES (4, 'd')";
    db
  in
  let q =
    "SELECT id, item FROM users INNER JOIN orders ON users.id = orders.uid ORDER BY id \
     ASC"
  in
  let r1 = query_ok (mk_db ~with_index:false) q in
  let r2 = query_ok (mk_db ~with_index:true) q in
  let extract rows =
    List.map
      (fun r ->
         match r.(0), r.(1) with
         | Row.V_int n, Row.V_text s -> Some (Int64.to_int n, s)
         | _ -> None)
      rows
  in
  Alcotest.(check int) "same row count" (List.length r1) (List.length r2);
  Alcotest.(check (list (option (pair int string))))
    "same content"
    (List.sort compare (extract r1))
    (List.sort compare (extract r2))
;;

(* ------------------------------------------------------------------ *)
(* Group 5: QCheck properties (10_000 trials each)                      *)
(* ------------------------------------------------------------------ *)

(* Cartesian product of two int lists, paired. *)
let cartesian_inner us os =
  List.concat_map
    (fun u ->
       List.filter_map (fun (uid, item) -> if u = uid then Some (u, item) else None) os)
    us
;;

let qcheck_inner_join_subset_cartesian =
  QCheck.Test.make
    ~name:"inner_join ⊆ filter(cartesian, on_pred)"
    ~count:10_000
    QCheck.(
      pair
        (list_size Gen.(0 -- 8) (1 -- 5)) (* users.id values *)
        (list_size Gen.(0 -- 8) (pair (1 -- 5) (1 -- 5))))
    (* (uid, item) *)
    (fun (us, os) ->
       let db = fresh_db () in
       exec db "CREATE TABLE users (id INTEGER, name TEXT)";
       exec db "CREATE TABLE orders (uid INTEGER, item INTEGER)";
       List.iter
         (fun u ->
            let sql = Printf.sprintf "INSERT INTO users (id) VALUES (%d)" u in
            exec db sql)
         us;
       List.iter
         (fun (uid, item) ->
            let sql =
              Printf.sprintf "INSERT INTO orders (uid, item) VALUES (%d, %d)" uid item
            in
            exec db sql)
         os;
       let rows =
         query_ok
           db
           "SELECT id, item FROM users INNER JOIN orders ON users.id = orders.uid"
       in
       let got =
         List.map
           (fun r ->
              match r.(0), r.(1) with
              | Row.V_int a, Row.V_int b -> Int64.to_int a, Int64.to_int b
              | _ -> -1, -1)
           rows
       in
       (* Expected: all (u, item) where u appears in `us` and (u, item) appears in os *)
       let expected = cartesian_inner us os in
       (* Sort both for comparison *)
       List.sort compare got = List.sort compare expected)
;;

let qcheck_left_join_ge_inner =
  QCheck.Test.make
    ~name:"|left join| ≥ |inner join| (every left row appears at least once)"
    ~count:10_000
    QCheck.(
      pair
        (list_size Gen.(0 -- 8) (1 -- 5))
        (list_size Gen.(0 -- 8) (pair (1 -- 5) (1 -- 5))))
    (fun (us, os) ->
       let db = fresh_db () in
       exec db "CREATE TABLE users (id INTEGER, name TEXT)";
       exec db "CREATE TABLE orders (uid INTEGER, item INTEGER)";
       List.iter
         (fun u -> exec db (Printf.sprintf "INSERT INTO users (id) VALUES (%d)" u))
         us;
       List.iter
         (fun (uid, item) ->
            exec
              db
              (Printf.sprintf "INSERT INTO orders (uid, item) VALUES (%d, %d)" uid item))
         os;
       let inner =
         query_ok db "SELECT * FROM users INNER JOIN orders ON users.id = orders.uid"
       in
       let left =
         query_ok db "SELECT * FROM users LEFT JOIN orders ON users.id = orders.uid"
       in
       (* LEFT JOIN size ≥ INNER JOIN size, and LEFT JOIN contains all
          left rows (each appears at least once, even with NULL right). *)
       List.length left >= List.length inner && List.length left >= List.length us)
;;

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "JOIN"
    [ ( "parser"
      , [ Alcotest.test_case "INNER JOIN" `Quick parse_inner_join
        ; Alcotest.test_case "LEFT JOIN" `Quick parse_left_join
        ; Alcotest.test_case "LEFT OUTER JOIN" `Quick parse_left_outer_join
        ; Alcotest.test_case "bare JOIN = INNER" `Quick parse_bare_join_is_inner
        ; Alcotest.test_case "no JOIN" `Quick parse_no_join
        ] )
    ; ( "sema_errors"
      , [ Alcotest.test_case "Unknown_table on JOIN" `Quick sema_unknown_join_table
        ; Alcotest.test_case "Ambiguous_column" `Quick sema_ambiguous_column
        ; Alcotest.test_case
            "qualified column resolves"
            `Quick
            sema_qualified_column_resolves
        ] )
    ; ( "planner"
      , [ Alcotest.test_case
            "hash join when no index"
            `Quick
            planner_picks_hash_join_no_index
        ; Alcotest.test_case "nested-loop with index" `Quick planner_picks_nlj_with_index
        ] )
    ; ( "execution"
      , [ Alcotest.test_case "INNER JOIN matching" `Quick inner_join_matching
        ; Alcotest.test_case "INNER JOIN no matches" `Quick inner_join_no_matches
        ; Alcotest.test_case "LEFT JOIN preserves outer" `Quick left_join_preserves_outer
        ; Alcotest.test_case "LEFT JOIN matching" `Quick left_join_matching_rows
        ; Alcotest.test_case "JOIN with WHERE" `Quick join_with_where
        ; Alcotest.test_case "JOIN with ORDER BY" `Quick join_with_order_by
        ; Alcotest.test_case "SELECT * yields all cols" `Quick select_star_columns
        ; Alcotest.test_case
            "NLJ with index = seq scan"
            `Quick
            nlj_with_index_returns_same_as_seqscan
        ; Alcotest.test_case
            "NULL=NULL inner join empty (hash)"
            `Quick
            null_key_inner_join_empty_hash_join
        ; Alcotest.test_case
            "NULL=NULL inner join empty (NLJ)"
            `Quick
            null_key_inner_join_empty_nlj
        ; Alcotest.test_case
            "NULL=NULL left join pads (hash)"
            `Quick
            null_key_left_join_pads_hash_join
        ; Alcotest.test_case
            "NULL=NULL left join pads (NLJ)"
            `Quick
            null_key_left_join_pads_nlj
        ] )
    ; ( "qcheck"
      , List.map
          QCheck_alcotest.to_alcotest
          [ qcheck_inner_join_subset_cartesian; qcheck_left_join_ge_inner ] )
    ]
;;
