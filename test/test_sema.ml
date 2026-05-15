open Lwt.Syntax

module C   = Sqlocaml_catalog.Catalog
module S   = Sqlocaml_store.Store
module Row = Sqlocaml_encoding.Row
module Sema = Sqlocaml_sql.Sema
module Ast  = Sqlocaml_sql.Ast

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let make_catalog cols =
  Lwt_main.run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"users" ~columns:cols in
    Lwt.return cat
  )

let two_col_cat () = make_catalog [
  { Row.name = "id";   ty = Row.Integer };
  { Row.name = "name"; ty = Row.Text };
]

let bind cat stmt = Lwt_main.run (Sema.bind cat stmt)

(* ------------------------------------------------------------------ *)
(* Group 1: CREATE TABLE binding                                        *)
(* ------------------------------------------------------------------ *)

let bind_create_new () =
  let cat = two_col_cat () in
  let cols = [
    Ast.{ name = "sku"; ty = Ty_text;    not_null = false; primary_key = false };
    Ast.{ name = "qty"; ty = Ty_int;     not_null = false; primary_key = false };
  ] in
  let stmt = Ast.S_create_table { name = "items"; columns = cols } in
  match bind cat stmt with
  | Ok (Sema.BS_create_table { name; _ }) ->
    Alcotest.(check string) "table name" "items" name
  | Ok _ -> Alcotest.fail "expected BS_create_table"
  | Error e ->
    Alcotest.failf "unexpected error: %s"
      (match e with
       | Sema.Already_exists n -> "Already_exists " ^ n
       | _ -> "other error")

let bind_create_duplicate () =
  let cat = two_col_cat () in
  (* "users" already exists in two_col_cat *)
  let cols = [Ast.{ name = "id"; ty = Ty_int; not_null = false; primary_key = false }] in
  let stmt = Ast.S_create_table { name = "users"; columns = cols } in
  match bind cat stmt with
  | Error (Sema.Already_exists "users") -> ()
  | Error _ -> Alcotest.fail "expected Already_exists \"users\""
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_create_preserves_cols () =
  let cat = two_col_cat () in
  let cols = [
    Ast.{ name = "sku"; ty = Ty_text; not_null = false; primary_key = false };
    Ast.{ name = "qty"; ty = Ty_int;  not_null = false; primary_key = false };
  ] in
  let stmt = Ast.S_create_table { name = "items"; columns = cols } in
  match bind cat stmt with
  | Ok (Sema.BS_create_table { columns; _ }) ->
    Alcotest.(check int) "two columns" 2 (List.length columns);
    Alcotest.(check string) "col 0 name" "sku" (List.nth columns 0).Row.name;
    Alcotest.(check string) "col 1 name" "qty" (List.nth columns 1).Row.name;
    (match (List.nth columns 0).Row.ty with Row.Text -> () | _ -> Alcotest.fail "col 0 should be Text");
    (match (List.nth columns 1).Row.ty with Row.Integer -> () | _ -> Alcotest.fail "col 1 should be Integer")
  | Ok _ -> Alcotest.fail "expected BS_create_table"
  | Error _ -> Alcotest.fail "unexpected error"

(* ------------------------------------------------------------------ *)
(* Group 2: INSERT binding                                              *)
(* ------------------------------------------------------------------ *)

let bind_insert_basic () =
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"; "name"];
    values = [Ast.L_int 1L; Ast.L_text "alice"];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { ordinals; values; _ }) ->
    Alcotest.(check (list int)) "ordinals" [0; 1] ordinals;
    Alcotest.(check int) "values count" 2 (List.length values)
  | Ok _ -> Alcotest.fail "expected BS_insert"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_insert_reversed_cols () =
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["name"; "id"];
    values = [Ast.L_text "bob"; Ast.L_int 2L];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { ordinals; _ }) ->
    Alcotest.(check (list int)) "ordinals reversed" [1; 0] ordinals
  | Ok _ -> Alcotest.fail "expected BS_insert"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_insert_single_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"];
    values = [Ast.L_int 99L];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { ordinals; _ }) ->
    Alcotest.(check (list int)) "ordinals single" [0] ordinals
  | Ok _ -> Alcotest.fail "expected BS_insert"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_insert_unknown_table () =
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "ghost";
    columns = ["id"];
    values = [Ast.L_int 1L];
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | Error _ -> Alcotest.fail "expected Unknown_table \"ghost\""
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_insert_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["bogus"];
    values = [Ast.L_int 1L];
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { table = "users"; column = "bogus" }) -> ()
  | Error _ -> Alcotest.fail "expected Unknown_column {table=users; column=bogus}"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_insert_arity_mismatch () =
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"; "name"];
    values = [Ast.L_int 1L];       (* 2 cols, 1 value *)
  } in
  match bind cat stmt with
  | Error (Sema.Arity_mismatch { expected = 2; got = 1 }) -> ()
  | Error _ -> Alcotest.fail "expected Arity_mismatch {expected=2; got=1}"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_insert_type_mismatch_int_col () =
  let cat = two_col_cat () in
  (* "id" is INTEGER, inserting TEXT *)
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"];
    values = [Ast.L_text "text"];
  } in
  match bind cat stmt with
  | Error (Sema.Type_mismatch { expected = Row.Integer; got = Row.Text }) -> ()
  | Error _ -> Alcotest.fail "expected Type_mismatch Integer/Text"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_insert_type_mismatch_text_col () =
  let cat = two_col_cat () in
  (* "name" is TEXT, inserting INTEGER *)
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["name"];
    values = [Ast.L_int 42L];
  } in
  match bind cat stmt with
  | Error (Sema.Type_mismatch { expected = Row.Text; got = Row.Integer }) -> ()
  | Error _ -> Alcotest.fail "expected Type_mismatch Text/Integer"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_insert_null_allowed () =
  let cat = two_col_cat () in
  (* NULL is allowed in any column — no type check *)
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"];
    values = [Ast.L_null];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { ordinals; _ }) ->
    Alcotest.(check (list int)) "null insert ordinals" [0] ordinals
  | Ok _ -> Alcotest.fail "expected BS_insert"
  | Error _ -> Alcotest.fail "unexpected error: NULL should be allowed"

(* ------------------------------------------------------------------ *)
(* Group 3: SELECT binding                                              *)
(* ------------------------------------------------------------------ *)

let bind_select_star () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `All; table = "users"; where = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj; _ }) ->
    Alcotest.(check (list int)) "proj all" [0; 1] proj
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_cols () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `Cols ["id"; "name"]; table = "users"; where = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj; _ }) ->
    Alcotest.(check (list int)) "proj cols in order" [0; 1] proj
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_reversed () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `Cols ["name"; "id"]; table = "users"; where = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj; _ }) ->
    Alcotest.(check (list int)) "proj reversed" [1; 0] proj
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_single () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `Cols ["id"]; table = "users"; where = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj; _ }) ->
    Alcotest.(check (list int)) "proj single" [0] proj
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_unknown_table () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `All; table = "ghost"; where = None } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | Error _ -> Alcotest.fail "expected Unknown_table \"ghost\""
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_select_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `Cols ["bogus"]; table = "users"; where = None } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { table = "users"; column = "bogus" }) -> ()
  | Error _ -> Alcotest.fail "expected Unknown_column {table=users; column=bogus}"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_select_no_where () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `All; table = "users"; where = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { where = None; _ }) -> ()
  | Ok (Sema.BS_select { where = Some _; _ }) -> Alcotest.fail "expected where = None"
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_where_col_eq_lit () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users";
    where = Some (Ast.E_eq (Ast.E_col "id", Ast.E_lit (Ast.L_int 42L)));
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { where = Some (Sema.BE_eq (Sema.BE_col 0, Sema.BE_lit (Ast.L_int 42L))); _ }) -> ()
  | Ok (Sema.BS_select { where = Some _; _ }) -> Alcotest.fail "where bound incorrectly"
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_where_lit_eq_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users";
    where = Some (Ast.E_eq (Ast.E_lit (Ast.L_int 42L), Ast.E_col "id"));
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { where = Some (Sema.BE_eq (Sema.BE_lit (Ast.L_int 42L), Sema.BE_col 0)); _ }) -> ()
  | Ok (Sema.BS_select { where = Some _; _ }) -> Alcotest.fail "where bound incorrectly (reversed)"
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_where_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users";
    where = Some (Ast.E_eq (Ast.E_col "bogus", Ast.E_lit (Ast.L_int 1L)));
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { table = "users"; column = "bogus" }) -> ()
  | Error _ -> Alcotest.fail "expected Unknown_column in where"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_select_where_text_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users";
    where = Some (Ast.E_eq (Ast.E_col "name", Ast.E_lit (Ast.L_text "alice")));
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { where = Some (Sema.BE_eq (Sema.BE_col 1, Sema.BE_lit (Ast.L_text "alice"))); _ }) -> ()
  | Ok (Sema.BS_select { where = Some _; _ }) -> Alcotest.fail "where bound incorrectly (text col)"
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

(* ------------------------------------------------------------------ *)
(* Group 4: Error values                                                *)
(* ------------------------------------------------------------------ *)

let error_already_exists_message () =
  match Sema.Already_exists "mytable" with
  | Sema.Already_exists n -> Alcotest.(check string) "carries table name" "mytable" n
  | _ -> Alcotest.fail "wrong variant"

let error_unknown_table_message () =
  match Sema.Unknown_table "ghost" with
  | Sema.Unknown_table n -> Alcotest.(check string) "carries table name" "ghost" n
  | _ -> Alcotest.fail "wrong variant"

let error_unknown_col_fields () =
  match Sema.Unknown_column { table = "users"; column = "bogus" } with
  | Sema.Unknown_column { table; column } ->
    Alcotest.(check string) "table field" "users" table;
    Alcotest.(check string) "column field" "bogus" column
  | _ -> Alcotest.fail "wrong variant"

let error_arity_fields () =
  match Sema.Arity_mismatch { expected = 3; got = 1 } with
  | Sema.Arity_mismatch { expected; got } ->
    Alcotest.(check int) "expected field" 3 expected;
    Alcotest.(check int) "got field" 1 got
  | _ -> Alcotest.fail "wrong variant"

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Sema" [
    "create-table", [
      Alcotest.test_case "bind_create_new"           `Quick bind_create_new;
      Alcotest.test_case "bind_create_duplicate"     `Quick bind_create_duplicate;
      Alcotest.test_case "bind_create_preserves_cols" `Quick bind_create_preserves_cols;
    ];
    "insert", [
      Alcotest.test_case "bind_insert_basic"               `Quick bind_insert_basic;
      Alcotest.test_case "bind_insert_reversed_cols"       `Quick bind_insert_reversed_cols;
      Alcotest.test_case "bind_insert_single_col"          `Quick bind_insert_single_col;
      Alcotest.test_case "bind_insert_unknown_table"       `Quick bind_insert_unknown_table;
      Alcotest.test_case "bind_insert_unknown_col"         `Quick bind_insert_unknown_col;
      Alcotest.test_case "bind_insert_arity_mismatch"      `Quick bind_insert_arity_mismatch;
      Alcotest.test_case "bind_insert_type_mismatch_int_col"  `Quick bind_insert_type_mismatch_int_col;
      Alcotest.test_case "bind_insert_type_mismatch_text_col" `Quick bind_insert_type_mismatch_text_col;
      Alcotest.test_case "bind_insert_null_allowed"        `Quick bind_insert_null_allowed;
    ];
    "select", [
      Alcotest.test_case "bind_select_star"              `Quick bind_select_star;
      Alcotest.test_case "bind_select_cols"              `Quick bind_select_cols;
      Alcotest.test_case "bind_select_reversed"          `Quick bind_select_reversed;
      Alcotest.test_case "bind_select_single"            `Quick bind_select_single;
      Alcotest.test_case "bind_select_unknown_table"     `Quick bind_select_unknown_table;
      Alcotest.test_case "bind_select_unknown_col"       `Quick bind_select_unknown_col;
      Alcotest.test_case "bind_select_no_where"          `Quick bind_select_no_where;
      Alcotest.test_case "bind_select_where_col_eq_lit"  `Quick bind_select_where_col_eq_lit;
      Alcotest.test_case "bind_select_where_lit_eq_col"  `Quick bind_select_where_lit_eq_col;
      Alcotest.test_case "bind_select_where_unknown_col" `Quick bind_select_where_unknown_col;
      Alcotest.test_case "bind_select_where_text_col"    `Quick bind_select_where_text_col;
    ];
    "error-values", [
      Alcotest.test_case "error_already_exists_message" `Quick error_already_exists_message;
      Alcotest.test_case "error_unknown_table_message"  `Quick error_unknown_table_message;
      Alcotest.test_case "error_unknown_col_fields"     `Quick error_unknown_col_fields;
      Alcotest.test_case "error_arity_fields"           `Quick error_arity_fields;
    ];
  ]
