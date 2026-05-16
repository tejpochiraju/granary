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
  { Row.name = "id";   ty = Row.Integer; not_null = false; primary_key = false; default = None };
  { Row.name = "name"; ty = Row.Text;    not_null = false; primary_key = false; default = None };
]

let bind cat stmt = Lwt_main.run (Sema.bind cat stmt)

(* ------------------------------------------------------------------ *)
(* Group 1: CREATE TABLE binding                                        *)
(* ------------------------------------------------------------------ *)

let bind_create_new () =
  let cat = two_col_cat () in
  let cols = [
    Ast.{ name = "sku"; ty = Ty_text;    not_null = false; primary_key = false; default = None };
    Ast.{ name = "qty"; ty = Ty_int;     not_null = false; primary_key = false; default = None };
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
  let cols = [Ast.{ name = "id"; ty = Ty_int; not_null = false; primary_key = false; default = None }] in
  let stmt = Ast.S_create_table { name = "users"; columns = cols } in
  match bind cat stmt with
  | Error (Sema.Already_exists "users") -> ()
  | Error _ -> Alcotest.fail "expected Already_exists \"users\""
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_create_preserves_cols () =
  let cat = two_col_cat () in
  let cols = [
    Ast.{ name = "sku"; ty = Ty_text; not_null = false; primary_key = false; default = None };
    Ast.{ name = "qty"; ty = Ty_int;  not_null = false; primary_key = false; default = None };
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
    values = [Ast.E_lit (Ast.L_int 1L); Ast.E_lit (Ast.L_text "alice")];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { ordinals; values; _ }) ->
    Alcotest.(check (list int)) "ordinals" [0; 1] ordinals;
    Alcotest.(check int) "values count" 2 (List.length values)
  | Ok _ -> Alcotest.fail "expected BS_insert"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_insert_reversed_cols () =
  (* INSERT (name, id) VALUES ('bob', 2) — columns specified in user order.
     After Task 4, bind_insert normalises to full-width table order:
     ordinals = [0; 1] (id first, then name), values = [BE_lit (L_int 2); BE_lit (L_text "bob")]. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["name"; "id"];
    values = [Ast.E_lit (Ast.L_text "bob"); Ast.E_lit (Ast.L_int 2L)];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { ordinals; values; _ }) ->
    Alcotest.(check (list int)) "ordinals full-width table-order" [0; 1] ordinals;
    Alcotest.(check int) "values count full-width" 2 (List.length values);
    (* id (ordinal 0) should be BE_lit (L_int 2L), name (ordinal 1) BE_lit (L_text "bob") *)
    (match List.nth values 0 with
     | Sema.BE_lit (Ast.L_int 2L) -> ()
     | _ -> Alcotest.fail "expected id=BE_lit(L_int 2L) at position 0");
    (match List.nth values 1 with
     | Sema.BE_lit (Ast.L_text "bob") -> ()
     | _ -> Alcotest.fail "expected name=BE_lit(L_text bob) at position 1")
  | Ok _ -> Alcotest.fail "expected BS_insert"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_insert_single_col () =
  (* INSERT (id) VALUES (99) — omitted name column fills with NULL.
     After Task 4, bind_insert normalises to full-width: ordinals = [0; 1],
     values = [BE_lit (L_int 99); BE_lit L_null]. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"];
    values = [Ast.E_lit (Ast.L_int 99L)];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { ordinals; values; _ }) ->
    Alcotest.(check (list int)) "ordinals full-width" [0; 1] ordinals;
    Alcotest.(check int) "values count full-width" 2 (List.length values);
    (match List.nth values 0 with
     | Sema.BE_lit (Ast.L_int 99L) -> ()
     | _ -> Alcotest.fail "expected id=BE_lit(L_int 99L)");
    (match List.nth values 1 with
     | Sema.BE_lit Ast.L_null -> ()
     | _ -> Alcotest.fail "expected name=BE_lit(L_null) (omitted)")
  | Ok _ -> Alcotest.fail "expected BS_insert"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_insert_unknown_table () =
  let cat = two_col_cat () in
  let stmt = Ast.S_insert {
    table = "ghost";
    columns = ["id"];
    values = [Ast.E_lit (Ast.L_int 1L)];
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
    values = [Ast.E_lit (Ast.L_int 1L)];
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
    values = [Ast.E_lit (Ast.L_int 1L)];       (* 2 cols, 1 value *)
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
    values = [Ast.E_lit (Ast.L_text "text")];
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
    values = [Ast.E_lit (Ast.L_int 42L)];
  } in
  match bind cat stmt with
  | Error (Sema.Type_mismatch { expected = Row.Text; got = Row.Integer }) -> ()
  | Error _ -> Alcotest.fail "expected Type_mismatch Text/Integer"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_insert_null_allowed () =
  let cat = two_col_cat () in
  (* NULL is allowed in any nullable column — no type check.
     After Task 4, full-width ordinals = [0; 1]. *)
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"];
    values = [Ast.E_lit Ast.L_null];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { ordinals; _ }) ->
    Alcotest.(check (list int)) "null insert ordinals full-width" [0; 1] ordinals
  | Ok _ -> Alcotest.fail "expected BS_insert"
  | Error _ -> Alcotest.fail "unexpected error: NULL should be allowed"

(* ------------------------------------------------------------------ *)
(* Group 3: SELECT binding                                              *)
(* ------------------------------------------------------------------ *)

let bind_select_star () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `All; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj; _ }) ->
    Alcotest.(check (list int)) "proj all" [0; 1] proj
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_cols () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `Cols ["id"; "name"]; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj; _ }) ->
    Alcotest.(check (list int)) "proj cols in order" [0; 1] proj
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_reversed () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `Cols ["name"; "id"]; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj; _ }) ->
    Alcotest.(check (list int)) "proj reversed" [1; 0] proj
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_single () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `Cols ["id"]; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj; _ }) ->
    Alcotest.(check (list int)) "proj single" [0] proj
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_unknown_table () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `All; table = "ghost"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | Error _ -> Alcotest.fail "expected Unknown_table \"ghost\""
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_select_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `Cols ["bogus"]; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { table = "users"; column = "bogus" }) -> ()
  | Error _ -> Alcotest.fail "expected Unknown_column {table=users; column=bogus}"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_select_no_where () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select { proj = `All; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
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
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_lit (Ast.L_int 42L)));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { where = Some (Sema.BE_binop (Sema.Eq, Sema.BE_col 0, Sema.BE_lit (Ast.L_int 42L))); _ }) -> ()
  | Ok (Sema.BS_select { where = Some _; _ }) -> Alcotest.fail "where bound incorrectly"
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_where_lit_eq_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_lit (Ast.L_int 42L), Ast.E_col "id"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { where = Some (Sema.BE_binop (Sema.Eq, Sema.BE_lit (Ast.L_int 42L), Sema.BE_col 0)); _ }) -> ()
  | Ok (Sema.BS_select { where = Some _; _ }) -> Alcotest.fail "where bound incorrectly (reversed)"
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_select_where_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "bogus", Ast.E_lit (Ast.L_int 1L)));
    group_by = []; having = None; order = []; limit = None; offset = None;
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
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "name", Ast.E_lit (Ast.L_text "alice")));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { where = Some (Sema.BE_binop (Sema.Eq, Sema.BE_col 1, Sema.BE_lit (Ast.L_text "alice"))); _ }) -> ()
  | Ok (Sema.BS_select { where = Some _; _ }) -> Alcotest.fail "where bound incorrectly (text col)"
  | Ok _ -> Alcotest.fail "expected BS_select"
  | Error _ -> Alcotest.fail "unexpected error"

(* ------------------------------------------------------------------ *)
(* Gap-fill: right-side unknown col, second insert col unknown,         *)
(* second select proj col unknown                                        *)
(* ------------------------------------------------------------------ *)

let bind_select_where_right_unknown () =
  let cat = two_col_cat () in
  (* WHERE id = bogus — left (id) resolves, right (bogus) fails *)
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_col "bogus"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column for right side"

let bind_insert_second_col_unknown () =
  (* Use 3 columns so the fold's short-circuit arm (| Error _ -> acc) is exercised.
     id resolves, bogus fails, name is never reached — the 3rd iteration hits L99. *)
  let cat = make_catalog [
    { Row.name = "id";   ty = Row.Integer; not_null = false; primary_key = false; default = None };
    { Row.name = "name"; ty = Row.Text;    not_null = false; primary_key = false; default = None };
    { Row.name = "age";  ty = Row.Integer; not_null = false; primary_key = false; default = None };
  ] in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"; "bogus"; "age"];
    values = [Ast.E_lit (Ast.L_int 1L); Ast.E_lit (Ast.L_int 2L); Ast.E_lit (Ast.L_int 3L)];
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column for second column"

let bind_select_second_col_unknown () =
  (* Use 3 columns so the fold's short-circuit arm (| Error _ -> acc) is exercised.
     id resolves, bogus fails, age is never reached — the 3rd iteration hits L134. *)
  let cat = make_catalog [
    { Row.name = "id";   ty = Row.Integer; not_null = false; primary_key = false; default = None };
    { Row.name = "name"; ty = Row.Text;    not_null = false; primary_key = false; default = None };
    { Row.name = "age";  ty = Row.Integer; not_null = false; primary_key = false; default = None };
  ] in
  let stmt = Ast.S_select {
    proj = `Cols ["id"; "bogus"; "age"];
    table = "users";
    joins = [];
    where = None;
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column for second proj column"

let bind_select_multi_order_by_rejected () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users";
    joins = [];
    where = None;
    group_by = []; having = None;
    order = [
      Ast.{ col = "id";   dir = Ast.Asc };
      Ast.{ col = "name"; dir = Ast.Asc };
    ];
    limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | Error _ -> Alcotest.fail "expected Unsupported error for multi-column ORDER BY"
  | Ok _ -> Alcotest.fail "expected error, got Ok"

let bind_select_order_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users";
    joins = [];
    where = None;
    group_by = []; having = None;
    order = [Ast.{ col = "bogus"; dir = Ast.Asc }];
    limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column for ORDER BY on non-existent col"

let bind_select_negative_limit () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users"; where = None; order = [];
    joins = []; group_by = []; having = None;
    limit = Some (-1); offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Invalid_limit _) -> ()
  | _ -> Alcotest.fail "expected Invalid_limit for negative LIMIT"

let bind_select_negative_offset () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users"; where = None; order = [];
    joins = []; group_by = []; having = None;
    limit = None; offset = Some (-5);
  } in
  match bind cat stmt with
  | Error (Sema.Invalid_limit _) -> ()
  | _ -> Alcotest.fail "expected Invalid_limit for negative OFFSET"

(* ------------------------------------------------------------------ *)
(* Group 3b: CREATE INDEX binding                                       *)
(* ------------------------------------------------------------------ *)

let bind_create_index_basic () =
  let cat = two_col_cat () in
  let stmt = Ast.S_create_index {
    name = "idx_id"; table = "users"; column = "id"; unique = false;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_create_index { name; col_idx; unique; _ }) ->
    Alcotest.(check string) "name" "idx_id" name;
    Alcotest.(check int) "col_idx" 0 col_idx;
    Alcotest.(check bool) "not unique" false unique
  | _ -> Alcotest.fail "expected BS_create_index"

let bind_create_index_unique () =
  let cat = two_col_cat () in
  let stmt = Ast.S_create_index {
    name = "uidx"; table = "users"; column = "name"; unique = true;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_create_index { col_idx = 1; unique = true; _ }) -> ()
  | _ -> Alcotest.fail "expected BS_create_index unique with col_idx=1"

let bind_create_index_unknown_table () =
  let cat = two_col_cat () in
  let stmt = Ast.S_create_index {
    name = "idx"; table = "ghost"; column = "x"; unique = false;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost"

let bind_create_index_unknown_column () =
  let cat = two_col_cat () in
  let stmt = Ast.S_create_index {
    name = "idx"; table = "users"; column = "bogus"; unique = false;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { table = "users"; column = "bogus" }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus"

let bind_create_index_duplicate () =
  let cat = two_col_cat () in
  (* First creation succeeds via catalog directly *)
  let _ = Lwt_main.run (
    Sqlocaml_catalog.Catalog.create_index cat ~name:"idx" ~table:"users"
      ~column:"id" ~unique:false
  ) in
  let stmt = Ast.S_create_index {
    name = "idx"; table = "users"; column = "id"; unique = false;
  } in
  match bind cat stmt with
  | Error (Sema.Already_exists "idx") -> ()
  | _ -> Alcotest.fail "expected Already_exists idx"

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
(* Group 5: UPDATE binding                                              *)
(* ------------------------------------------------------------------ *)

let bind_update_basic () =
  let cat = two_col_cat () in
  let stmt = Ast.S_update {
    table = "users";
    assignments = [("name", Ast.E_lit (Ast.L_text "carol"))];
    where = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_update { assignments = [(1, _)]; where = None; _ }) -> ()
  | Ok _ -> Alcotest.fail "unexpected BS_update shape"
  | Error _ -> Alcotest.fail "unexpected error"

let bind_update_unknown_table () =
  let cat = two_col_cat () in
  let stmt = Ast.S_update {
    table = "ghost"; assignments = [("x", Ast.E_lit (Ast.L_int 1L))]; where = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost"

let bind_update_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_update {
    table = "users"; assignments = [("bogus", Ast.E_lit (Ast.L_int 1L))]; where = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus"

let bind_update_type_mismatch () =
  let cat = two_col_cat () in
  (* SET id (INTEGER) = 'text' *)
  let stmt = Ast.S_update {
    table = "users"; assignments = [("id", Ast.E_lit (Ast.L_text "bad"))]; where = None;
  } in
  match bind cat stmt with
  | Error (Sema.Type_mismatch { expected = Row.Integer; got = Row.Text }) -> ()
  | _ -> Alcotest.fail "expected Type_mismatch Integer/Text"

let bind_update_not_null_violation () =
  let cat = make_catalog [
    { Row.name = "id"; ty = Row.Integer; not_null = true; primary_key = false; default = None };
    { Row.name = "name"; ty = Row.Text; not_null = false; primary_key = false; default = None };
  ] in
  (* SET id = NULL on a NOT NULL column *)
  let stmt = Ast.S_update {
    table = "users"; assignments = [("id", Ast.E_lit Ast.L_null)]; where = None;
  } in
  match bind cat stmt with
  | Error (Sema.Not_null_violation "id") -> ()
  | _ -> Alcotest.fail "expected Not_null_violation for id"

let bind_update_with_where () =
  let cat = two_col_cat () in
  let stmt = Ast.S_update {
    table = "users";
    assignments = [("name", Ast.E_lit (Ast.L_text "x"))];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_lit (Ast.L_int 1L)));
  } in
  match bind cat stmt with
  | Ok (Sema.BS_update { where = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected BS_update with where"

let bind_update_where_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_update {
    table = "users";
    assignments = [("name", Ast.E_lit (Ast.L_text "x"))];
    where = Some (Ast.E_col "bogus");
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column for where"

(* ------------------------------------------------------------------ *)
(* Group 6: DELETE binding                                              *)
(* ------------------------------------------------------------------ *)

let bind_delete_basic () =
  let cat = two_col_cat () in
  let stmt = Ast.S_delete { table = "users"; where = None } in
  match bind cat stmt with
  | Ok (Sema.BS_delete { where = None; _ }) -> ()
  | _ -> Alcotest.fail "expected BS_delete with no where"

let bind_delete_with_where () =
  let cat = two_col_cat () in
  let stmt = Ast.S_delete {
    table = "users";
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_lit (Ast.L_int 1L)));
  } in
  match bind cat stmt with
  | Ok (Sema.BS_delete { where = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected BS_delete with where"

let bind_delete_unknown_table () =
  let cat = two_col_cat () in
  let stmt = Ast.S_delete { table = "ghost"; where = None } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost"

let bind_delete_where_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_delete {
    table = "users";
    where = Some (Ast.E_col "bogus");
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus"

(* ------------------------------------------------------------------ *)
(* Group 7: DROP TABLE / DROP INDEX binding                             *)
(* ------------------------------------------------------------------ *)

let bind_drop_table_basic () =
  let cat = two_col_cat () in
  let stmt = Ast.S_drop_table { name = "users" } in
  match bind cat stmt with
  | Ok (Sema.BS_drop_table { name; _ }) ->
    Alcotest.(check string) "name" "users" name
  | _ -> Alcotest.fail "expected BS_drop_table"

let bind_drop_table_unknown () =
  let cat = two_col_cat () in
  let stmt = Ast.S_drop_table { name = "ghost" } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost"

let bind_drop_index_basic () =
  let cat = two_col_cat () in
  let _ = Lwt_main.run (
    Sqlocaml_catalog.Catalog.create_index cat ~name:"idx" ~table:"users"
      ~column:"id" ~unique:false
  ) in
  let stmt = Ast.S_drop_index { name = "idx" } in
  match bind cat stmt with
  | Ok (Sema.BS_drop_index { name; _ }) ->
    Alcotest.(check string) "index name" "idx" name
  | _ -> Alcotest.fail "expected BS_drop_index"

let bind_drop_index_unknown () =
  let cat = two_col_cat () in
  let stmt = Ast.S_drop_index { name = "no_such_idx" } in
  match bind cat stmt with
  | Error (Sema.Unknown_index "no_such_idx") -> ()
  | _ -> Alcotest.fail "expected Unknown_index"

(* ------------------------------------------------------------------ *)
(* Group 8: JOIN / bind_expr_join resolution                            *)
(* ------------------------------------------------------------------ *)

let make_join_cat () =
  Lwt_main.run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"users" ~columns:[
      { Row.name = "id";   ty = Row.Integer; not_null = false; primary_key = false; default = None };
      { Row.name = "name"; ty = Row.Text;    not_null = false; primary_key = false; default = None };
    ] in
    let* _ = C.create_table cat ~name:"orders" ~columns:[
      { Row.name = "uid";  ty = Row.Integer; not_null = false; primary_key = false; default = None };
      { Row.name = "item"; ty = Row.Text;    not_null = false; primary_key = false; default = None };
    ] in
    Lwt.return cat
  )

let bind_select_join_unknown_table () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "ghost"; alias = None;
                on = Ast.E_lit (Ast.L_int 1L) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost for join"

let bind_select_join_ambiguous_col () =
  (* Both tables have a column named 'uid' to trigger Ambiguous_column. *)
  let cat = Lwt_main.run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"a" ~columns:[
      { Row.name = "uid"; ty = Row.Integer; not_null = false; primary_key = false; default = None };
    ] in
    let* _ = C.create_table cat ~name:"b" ~columns:[
      { Row.name = "uid"; ty = Row.Integer; not_null = false; primary_key = false; default = None };
    ] in
    Lwt.return cat
  ) in
  let stmt = Ast.S_select {
    proj = `Cols ["uid"]; table = "a";
    joins = [ { Ast.kind = Ast.Inner; table = "b"; alias = None;
                on = Ast.E_lit (Ast.L_int 1L) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Ambiguous_column "uid") -> ()
  | _ -> Alcotest.fail "expected Ambiguous_column uid"

let bind_select_join_unknown_proj_col () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `Cols ["bogus"]; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_lit (Ast.L_int 1L) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in join proj"

let bind_select_join_qualified_col_ok () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("users", "id"),
                  Ast.E_tbl_col ("orders", "uid")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { join = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok BS_select with join"

let bind_select_join_qualified_unknown_table () =
  (* E_tbl_col with a table name that matches neither left nor right *)
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_tbl_col ("ghost", "id") } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table "ghost") -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost in ON expr"

let bind_select_join_unknown_col_unqual () =
  (* An unqualified column name that doesn't exist in either table. *)
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_col "bogus" } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in ON"

let bind_select_join_agg_in_on_rejected () =
  (* E_agg in ON clause is an error. *)
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_agg (Ast.Agg_count, None) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for agg in ON"

let bind_select_join_qualified_col_unknown_col () =
  (* E_tbl_col ("users", "bogus") — table name matches but column doesn't. *)
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_tbl_col ("orders", "bogus") } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { table = "orders"; column = "bogus" }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus for orders in ON"

let bind_select_join_where_unknown_col () =
  (* WHERE references a column that exists in neither table in a JOIN query *)
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("users","id"),
                  Ast.E_tbl_col ("orders","uid")) } ];
    where = Some (Ast.E_col "bogus");
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in join WHERE"

(* ------------------------------------------------------------------ *)
(* Group 9: Aggregate / GROUP BY / HAVING binding                       *)
(* ------------------------------------------------------------------ *)

let bind_select_count_star () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { aggs = [{ func = Ast.Agg_count; col_ord = None }]; _ }) -> ()
  | _ -> Alcotest.fail "expected BS_select with COUNT(*) agg"

let bind_select_sum_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_sum, Some (Ast.E_col "id"))];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { aggs = [{ func = Ast.Agg_sum; col_ord = Some 0 }]; _ }) -> ()
  | _ -> Alcotest.fail "expected BS_select with SUM(id)"

let bind_select_sum_text_col_rejected () =
  (* SUM on a TEXT column must be rejected as Type_mismatch. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_sum, Some (Ast.E_col "name"))];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Type_mismatch _) -> ()
  | _ -> Alcotest.fail "expected Type_mismatch for SUM(name)"

let bind_select_avg_text_col_rejected () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_avg, Some (Ast.E_col "name"))];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Type_mismatch _) -> ()
  | _ -> Alcotest.fail "expected Type_mismatch for AVG(name)"

let bind_select_group_by_basic () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"]; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { group_by = Some 0; _ }) -> ()
  | _ -> Alcotest.fail "expected BS_select GROUP BY id"

let bind_select_group_by_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["bogus"]; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in GROUP BY"

let bind_select_group_by_multi_rejected () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"; "name"]; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for multi-col GROUP BY"

let bind_select_having_basic () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_binop (Ast.Gt,
      Ast.E_agg (Ast.Agg_count, None), Ast.E_lit (Ast.L_int 0L)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected BS_select with HAVING"

let bind_select_having_no_group_by_rejected () =
  (* HAVING without GROUP BY and without aggregates in projection → Unsupported *)
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Cols ["id"];
    table = "users"; joins = [];
    where = None; group_by = [];
    having = Some (Ast.E_binop (Ast.Gt, Ast.E_col "id", Ast.E_lit (Ast.L_int 0L)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for HAVING without GROUP BY"

let bind_select_agg_star_non_count_rejected () =
  (* COUNT-star is OK but SUM-star is not. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_sum, None)];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for SUM(*)"

let bind_select_agg_complex_arg_rejected () =
  (* Aggregate argument must be a column reference, not e.g. a binop. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_sum,
      Some (Ast.E_binop (Ast.Add, Ast.E_col "id", Ast.E_lit (Ast.L_int 1L))))];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for complex agg arg"

let bind_select_col_not_in_group_by_rejected () =
  (* A bare column reference in aggregated SELECT must be the GROUP BY col. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "name"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"]; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported: name not in GROUP BY"

let bind_select_agg_star_in_select_rejected () =
  (* SELECT * with aggregates is not allowed. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All;
    table = "users"; joins = [];
    where = None; group_by = [];
    having = Some (Ast.E_agg (Ast.Agg_count, None));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for SELECT * with aggregates"

let bind_select_agg_unknown_col_arg () =
  (* SUM(bogus) — column doesn't exist. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_sum, Some (Ast.E_col "bogus"))];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in SUM arg"

let bind_select_agg_in_where_rejected () =
  (* Aggregate in WHERE clause should be Unsupported. *)
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Cols ["id"];
    table = "users"; joins = [];
    where = Some (Ast.E_agg (Ast.Agg_count, None));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for agg in WHERE"

let bind_select_more_than_one_join_rejected () =
  let cat = make_join_cat () in
  let join_clause tbl = { Ast.kind = Ast.Inner; table = tbl; alias = None;
                          on = Ast.E_lit (Ast.L_int 1L) } in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ join_clause "orders"; join_clause "orders" ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for 2+ JOINs"

(* ------------------------------------------------------------------ *)
(* Group 10: NOT NULL violation in INSERT                               *)
(* ------------------------------------------------------------------ *)

let bind_insert_not_null_violation () =
  let cat = make_catalog [
    { Row.name = "id";   ty = Row.Integer; not_null = true;  primary_key = false; default = None };
    { Row.name = "name"; ty = Row.Text;    not_null = false; primary_key = false; default = None };
  ] in
  (* Omit the NOT NULL column entirely; it fills with NULL → violation. *)
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["name"];
    values = [Ast.E_lit (Ast.L_text "alice")];
  } in
  match bind cat stmt with
  | Error (Sema.Not_null_violation "id") -> ()
  | _ -> Alcotest.fail "expected Not_null_violation for id"

(* ------------------------------------------------------------------ *)
(* Group 11: Coverage-gap tests for bind_expr / bind_expr_join /        *)
(*           bind_expr_agg / Real and Blob lit_ty / dv_to_lit /         *)
(*           qual_lookup branches.                                      *)
(* ------------------------------------------------------------------ *)

(** lit_ty L_real (line 113) — type-mismatch INTEGER col vs REAL literal. *)
let bind_insert_real_lit_type_mismatch () =
  let cat = two_col_cat () in
  (* "id" is INTEGER, inserting REAL *)
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["id"];
    values = [Ast.E_lit (Ast.L_real 1.5)];
  } in
  match bind cat stmt with
  | Error (Sema.Type_mismatch { expected = Row.Integer; got = Row.Real }) -> ()
  | _ -> Alcotest.fail "expected Type_mismatch Integer/Real"

(** lit_ty L_blob (line 114) and ty_equal Row.Blob (line 120) — column with
    BLOB type accepts a blob literal. *)
let bind_insert_blob_lit_ok () =
  let cat = make_catalog [
    { Row.name = "b"; ty = Row.Blob; not_null = false; primary_key = false; default = None };
  ] in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["b"];
    values = [Ast.E_lit (Ast.L_blob (Bytes.of_string "hello"))];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert _) -> ()
  | _ -> Alcotest.fail "expected Ok BS_insert for blob"

(** ty_equal Row.Real (line 119) — Real-typed col accepts Real lit. *)
let bind_insert_real_lit_ok () =
  let cat = make_catalog [
    { Row.name = "f"; ty = Row.Real; not_null = false; primary_key = false; default = None };
  ] in
  let stmt = Ast.S_insert {
    table = "users";
    columns = ["f"];
    values = [Ast.E_lit (Ast.L_real 3.14)];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert _) -> ()
  | _ -> Alcotest.fail "expected Ok BS_insert for real"

(** bind_expr E_tbl_col error (line 141): unknown column inside qualified
    reference in WHERE in a single-table query (no JOIN). *)
let bind_select_tbl_col_unknown_no_join () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users"; joins = [];
    where = Some (Ast.E_tbl_col ("users", "bogus"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in tbl_col"

(** bind_expr E_not error path (line 151). *)
let bind_select_not_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users"; joins = [];
    where = Some (Ast.E_not (Ast.E_col "bogus"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_not"

(** bind_expr E_is_null error path (line 155). *)
let bind_select_is_null_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users"; joins = [];
    where = Some (Ast.E_is_null (Ast.E_col "bogus"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_is_null"

(** bind_expr E_is_not_null error path (line 159). *)
let bind_select_is_not_null_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users"; joins = [];
    where = Some (Ast.E_is_not_null (Ast.E_col "bogus"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_is_not_null"

(** bind_expr E_neg error path (line 163). *)
let bind_select_neg_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users"; joins = [];
    where = Some (Ast.E_neg (Ast.E_col "bogus"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_neg"

(** bind_expr_join with `Some i, None` (right-only) for unqualified col
    referenced in ON predicate. Covers line 185. *)
let bind_select_join_unqual_right_only () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq, Ast.E_col "item", Ast.E_lit (Ast.L_text "x")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { join = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok BS_select with right-only col in ON"

(** bind_expr_join E_tbl_col on RIGHT table OK (lines 194-196). *)
let bind_select_join_qual_right_col () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("orders", "item"),
                  Ast.E_lit (Ast.L_text "x")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { join = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok BS_select with right-table qual in ON"

(** bind_expr_join E_binop right-side error path (line 205). *)
let bind_select_join_binop_right_error () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("users", "id"),
                  Ast.E_col "bogus") } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in binop right of ON"

(** bind_expr_join E_not (lines 206-208). *)
let bind_select_join_on_not () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_not (Ast.E_tbl_col ("users", "id")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { join = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok BS_select with E_not in ON"

(** bind_expr_join E_not error (line 208). *)
let bind_select_join_on_not_error () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_not (Ast.E_col "bogus") } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_not in ON"

(** bind_expr_join E_is_null (lines 209-211). *)
let bind_select_join_on_is_null () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_is_null (Ast.E_tbl_col ("users", "id")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { join = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok BS_select with E_is_null in ON"

(** bind_expr_join E_is_null error (line 211). *)
let bind_select_join_on_is_null_error () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_is_null (Ast.E_col "bogus") } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_is_null in ON"

(** bind_expr_join E_is_not_null (lines 212-214). *)
let bind_select_join_on_is_not_null () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_is_not_null (Ast.E_tbl_col ("users", "id")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { join = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok BS_select with E_is_not_null in ON"

(** bind_expr_join E_is_not_null error (line 214). *)
let bind_select_join_on_is_not_null_error () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_is_not_null (Ast.E_col "bogus") } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_is_not_null in ON"

(** bind_expr_join E_neg (lines 215-217). *)
let bind_select_join_on_neg () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_neg (Ast.E_tbl_col ("users", "id")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { join = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok BS_select with E_neg in ON"

(** bind_expr_join E_neg error (line 217). *)
let bind_select_join_on_neg_error () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_neg (Ast.E_col "bogus") } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_neg in ON"

(** bind_expr_agg with E_col inside HAVING (lines 256-259).  Tests resolver
    via plain col reference that maps to the GROUP BY column. *)
let bind_select_having_with_plain_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_lit (Ast.L_int 1L)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with plain group-by col in HAVING"

(** bind_expr_agg with E_col error in HAVING (line 258). *)
let bind_select_having_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_binop (Ast.Eq, Ast.E_col "bogus", Ast.E_lit (Ast.L_int 1L)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in HAVING"

(** bind_expr_agg E_tbl_col (lines 260-263). *)
let bind_select_having_qual_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_binop (Ast.Eq,
      Ast.E_tbl_col ("users", "id"), Ast.E_lit (Ast.L_int 1L)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with qual group-by col in HAVING"

(** bind_expr_agg E_tbl_col error (line 262). *)
let bind_select_having_qual_col_error () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_binop (Ast.Eq,
      Ast.E_tbl_col ("ghost", "id"), Ast.E_lit (Ast.L_int 1L)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table _) -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost in HAVING qual"

(** bind_expr_agg E_not / E_neg / E_is_null / E_is_not_null shared branch
    (line 270/272/274/276 — used by HAVING).  Covers expr_has_agg line 311
    too when E_not contains an aggregate. *)
let bind_select_having_with_not_agg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_not (Ast.E_agg (Ast.Agg_count, None)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with E_not(agg) in HAVING"

(** Covers bind_expr_agg E_neg (line 276) and expr_has_agg neg branch. *)
let bind_select_having_with_neg_agg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_binop (Ast.Gt,
      Ast.E_neg (Ast.E_agg (Ast.Agg_count, None)),
      Ast.E_lit (Ast.L_int (-10L))));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with E_neg(agg) in HAVING"

(** Covers bind_expr_agg E_is_null and E_is_not_null branches. *)
let bind_select_having_with_is_null_agg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_is_null (Ast.E_agg (Ast.Agg_count, None)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with E_is_null(agg) in HAVING"

let bind_select_having_with_is_not_null_agg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_is_not_null (Ast.E_agg (Ast.Agg_count, None)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with E_is_not_null(agg) in HAVING"

(** Covers bind_expr_agg E_tbl_col aggregate-argument branch (lines 289-292). *)
let bind_select_agg_qual_col_arg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_sum, Some (Ast.E_tbl_col ("users", "id")))];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { aggs = [{ func = Ast.Agg_sum; col_ord = Some 0 }]; _ }) -> ()
  | _ -> Alcotest.fail "expected SUM(users.id) bound"

(** Covers bind_expr_agg E_tbl_col aggregate-argument error path (line 291). *)
let bind_select_agg_qual_col_arg_error () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_agg (Ast.Agg_sum, Some (Ast.E_tbl_col ("ghost", "id")))];
    table = "users"; joins = [];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table _) -> ()
  | _ -> Alcotest.fail "expected Unknown_table for ghost in agg arg"

(** CREATE TABLE with NULL DEFAULT (line 326). *)
let bind_create_default_null () =
  let cat = two_col_cat () in
  let cols = [
    Ast.{ name = "x"; ty = Ty_int; not_null = false; primary_key = false;
          default = Some Ast.L_null };
  ] in
  let stmt = Ast.S_create_table { name = "items"; columns = cols } in
  match bind cat stmt with
  | Ok (Sema.BS_create_table { columns; _ }) ->
    (match (List.hd columns).Row.default with
     | Some Row.DV_null -> ()
     | _ -> Alcotest.fail "expected DV_null default")
  | _ -> Alcotest.fail "expected BS_create_table"

(** CREATE TABLE with REAL DEFAULT (line 327). *)
let bind_create_default_real () =
  let cat = two_col_cat () in
  let cols = [
    Ast.{ name = "x"; ty = Ty_real; not_null = false; primary_key = false;
          default = Some (Ast.L_real 3.14) };
  ] in
  let stmt = Ast.S_create_table { name = "items"; columns = cols } in
  match bind cat stmt with
  | Ok (Sema.BS_create_table { columns; _ }) ->
    (match (List.hd columns).Row.default with
     | Some (Row.DV_real 3.14) -> ()
     | _ -> Alcotest.fail "expected DV_real default")
  | _ -> Alcotest.fail "expected BS_create_table"

(** CREATE TABLE with BLOB DEFAULT (line 328). *)
let bind_create_default_blob () =
  let cat = two_col_cat () in
  let cols = [
    Ast.{ name = "x"; ty = Ty_blob; not_null = false; primary_key = false;
          default = Some (Ast.L_blob (Bytes.of_string "hi")) };
  ] in
  let stmt = Ast.S_create_table { name = "items"; columns = cols } in
  match bind cat stmt with
  | Ok (Sema.BS_create_table { columns; _ }) ->
    (match (List.hd columns).Row.default with
     | Some (Row.DV_blob b) when Bytes.equal b (Bytes.of_string "hi") -> ()
     | _ -> Alcotest.fail "expected DV_blob default")
  | _ -> Alcotest.fail "expected BS_create_table"

(** dv_to_lit DV_null in INSERT default-application (line 351). *)
let bind_insert_default_null () =
  let cat = make_catalog [
    { Row.name = "id"; ty = Row.Integer; not_null = false; primary_key = false;
      default = Some Row.DV_null };
    { Row.name = "n";  ty = Row.Integer; not_null = false; primary_key = false; default = None };
  ] in
  let stmt = Ast.S_insert {
    table = "users"; columns = ["n"]; values = [Ast.E_lit (Ast.L_int 7L)];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { values; _ }) ->
    (* id should be BE_lit L_null from default; values are in ordinal order: [id; n]. *)
    Alcotest.(check int) "values" 2 (List.length values);
    (match List.nth values 0 with
     | Sema.BE_lit Ast.L_null -> ()
     | _ -> Alcotest.fail "expected id=BE_lit(L_null) from DV_null default")
  | _ -> Alcotest.fail "expected BS_insert"

(** dv_to_lit DV_real in INSERT default-application (line 352). *)
let bind_insert_default_real () =
  let cat = make_catalog [
    { Row.name = "f"; ty = Row.Real; not_null = false; primary_key = false;
      default = Some (Row.DV_real 2.5) };
    { Row.name = "n"; ty = Row.Integer; not_null = false; primary_key = false; default = None };
  ] in
  let stmt = Ast.S_insert {
    table = "users"; columns = ["n"]; values = [Ast.E_lit (Ast.L_int 7L)];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { values; _ }) ->
    (match List.nth values 0 with
     | Sema.BE_lit (Ast.L_real 2.5) -> ()
     | _ -> Alcotest.fail "expected f=BE_lit(L_real 2.5) from DV_real default")
  | _ -> Alcotest.fail "expected BS_insert"

(** dv_to_lit DV_blob in INSERT default-application (line 353). *)
let bind_insert_default_blob () =
  let cat = make_catalog [
    { Row.name = "b"; ty = Row.Blob; not_null = false; primary_key = false;
      default = Some (Row.DV_blob (Bytes.of_string "x")) };
    { Row.name = "n"; ty = Row.Integer; not_null = false; primary_key = false; default = None };
  ] in
  let stmt = Ast.S_insert {
    table = "users"; columns = ["n"]; values = [Ast.E_lit (Ast.L_int 7L)];
  } in
  match bind cat stmt with
  | Ok (Sema.BS_insert { values; _ }) ->
    (match List.nth values 0 with
     | Sema.BE_lit (Ast.L_blob b) when Bytes.equal b (Bytes.of_string "x") -> ()
     | _ -> Alcotest.fail "expected b=BE_lit(L_blob 'x') from DV_blob default")
  | _ -> Alcotest.fail "expected BS_insert"

(** Insert with multiple NOT NULL columns where the first column is fine
    but a later column violates NOT NULL — exercises the fold short-circuit
    `| Error _ -> acc` branch (line 413). *)
let bind_insert_not_null_late () =
  let cat = make_catalog [
    { Row.name = "a"; ty = Row.Integer; not_null = false; primary_key = false; default = None };
    { Row.name = "b"; ty = Row.Integer; not_null = true;  primary_key = false; default = None };
    { Row.name = "c"; ty = Row.Integer; not_null = true;  primary_key = false; default = None };
  ] in
  let stmt = Ast.S_insert {
    table = "users"; columns = ["a"]; values = [Ast.E_lit (Ast.L_int 1L)];
  } in
  match bind cat stmt with
  | Error (Sema.Not_null_violation _) -> ()
  | _ -> Alcotest.fail "expected Not_null_violation"

(** Multi-JOIN explicit error path (line 444). *)
let bind_select_multi_join_explicit_error () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [
      { Ast.kind = Ast.Inner; table = "orders"; alias = None;
        on = Ast.E_lit (Ast.L_int 1L) };
      { Ast.kind = Ast.Inner; table = "orders"; alias = None;
        on = Ast.E_lit (Ast.L_int 1L) };
    ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for > 1 JOIN"

(** proj_lookup with JOIN where col is only in right table (line 476).
    users has 2 cols (id, name) so right_offset = 2; orders.item is at
    orders.columns[1] so absolute index = 3. *)
let bind_select_join_right_only_col_proj () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `Cols ["item"]; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("users", "id"),
                  Ast.E_tbl_col ("orders", "uid")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { proj = [3]; _ }) -> ()
  | _ -> Alcotest.fail "expected proj=[3] for right-only col 'item'"

(** qual_lookup with no join, qualified ref to existing col (line 484 OK path).
    Exercises lines 480-484. *)
let bind_select_qual_no_join () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_tbl_col ("users", "id"); Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"]; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { group_by = Some 0; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with qual col, no join"

(** qual_lookup no-join, unknown column on the table (line 485). *)
let bind_select_qual_no_join_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_tbl_col ("users", "bogus"); Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"]; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in qual_lookup no-join"

(** qual_lookup no-join, unknown table (line 486). *)
let bind_select_qual_no_join_unknown_table () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_tbl_col ("ghost", "id"); Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"]; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table _) -> ()
  | _ -> Alcotest.fail "expected Unknown_table ghost in qual_lookup no-join"

(** bind_expr_join E_col Ambiguous (line 183) when ambiguous column appears
    inside an ON predicate (not just projection). *)
let bind_select_join_on_ambiguous () =
  let cat = Lwt_main.run (
    let store = S.create () in
    let* cat = C.open_ store in
    let* _ = C.create_table cat ~name:"a" ~columns:[
      { Row.name = "x"; ty = Row.Integer; not_null = false; primary_key = false; default = None };
    ] in
    let* _ = C.create_table cat ~name:"b" ~columns:[
      { Row.name = "x"; ty = Row.Integer; not_null = false; primary_key = false; default = None };
    ] in
    Lwt.return cat
  ) in
  let stmt = Ast.S_select {
    proj = `All; table = "a";
    joins = [ { Ast.kind = Ast.Inner; table = "b"; alias = None;
                on = Ast.E_col "x" } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Ambiguous_column "x") -> ()
  | _ -> Alcotest.fail "expected Ambiguous_column x in ON"

(** bind_expr_join unqualified left-only column inside ON (line 184). *)
let bind_select_join_on_unqual_left_only () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                (* "name" is only in users *)
                on = Ast.E_binop (Ast.Eq, Ast.E_col "name", Ast.E_lit (Ast.L_text "x")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { join = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok BS_select with left-only unqual col in ON"

(** bind_expr_join qualified left-table unknown column inside ON (line 193). *)
let bind_select_join_on_qual_left_unknown () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_tbl_col ("users", "bogus") } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { table = "users"; column = "bogus" }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column users.bogus in ON"

(** bind_expr_join binop left-side error (line 204). *)
let bind_select_join_binop_left_error () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_col "bogus",
                  Ast.E_tbl_col ("orders", "uid")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in left of ON binop"

(** bind_expr_agg E_binop right-error path (line 268) — aggregate-context
    binop with bad right side (in HAVING). *)
let bind_select_having_binop_right_error () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    (* Left side OK (group col), right side unknown col *)
    having = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_col "bogus"));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus on right of HAVING binop"

(** bind_expr_agg E_col arg error in HAVING aggregate (lines 286-287).
    Tests aggregate with E_col arg referring to unknown column. *)
let bind_select_having_agg_unknown_arg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_agg (Ast.Agg_sum, Some (Ast.E_col "bogus")));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in HAVING agg arg"

(** bind_expr_agg E_col arg OK path in HAVING aggregate (line 288). *)
let bind_select_having_agg_with_col_arg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_binop (Ast.Gt,
      Ast.E_agg (Ast.Agg_sum, Some (Ast.E_col "id")),
      Ast.E_lit (Ast.L_int 0L)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with SUM(id) in HAVING"

(** bind_expr_agg E_tbl_col agg arg OK (lines 289-292) in HAVING. *)
let bind_select_having_agg_with_qual_arg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_binop (Ast.Gt,
      Ast.E_agg (Ast.Agg_sum, Some (Ast.E_tbl_col ("users", "id"))),
      Ast.E_lit (Ast.L_int 0L)));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Ok (Sema.BS_select { having = Some _; _ }) -> ()
  | _ -> Alcotest.fail "expected Ok with SUM(users.id) in HAVING"

(** bind_expr_agg E_tbl_col agg arg error (line 291) — qual arg with bad
    table. *)
let bind_select_having_agg_with_qual_arg_error () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_agg (Ast.Agg_sum, Some (Ast.E_tbl_col ("ghost", "id"))));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_table _) -> ()
  | _ -> Alcotest.fail "expected Unknown_table for ghost in HAVING agg qual"

(** bind_expr_agg complex agg arg (line 293) — agg arg not col reference
    inside HAVING. *)
let bind_select_having_agg_complex_arg () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_agg (Ast.Agg_sum,
      Some (Ast.E_binop (Ast.Add, Ast.E_col "id", Ast.E_lit (Ast.L_int 1L)))));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for complex agg arg in HAVING"

(** bind_expr_agg non-COUNT agg without args inside HAVING (line 284). *)
let bind_select_having_sum_star_rejected () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    (* SUM with no arg - only COUNT permits no argument *)
    having = Some (Ast.E_agg (Ast.Agg_sum, None));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unsupported _) -> ()
  | _ -> Alcotest.fail "expected Unsupported for SUM(*) in HAVING"

(** qual_lookup JOIN, left table, unknown col (line 491). *)
let bind_select_join_qual_left_unknown_col_proj () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_tbl_col ("users", "bogus"); Ast.E_agg (Ast.Agg_count, None)];
    table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("users", "id"),
                  Ast.E_tbl_col ("orders", "uid")) } ];
    where = None; group_by = ["id"]; having = None; order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus for users.bogus"

(* ------------------------------------------------------------------ *)
(* Group 11b: bind_expr_agg_proj error paths inside E_not/E_is_null/  *)
(*            E_is_not_null/E_neg (lines 273, 275, 277, 279 in sema.ml) *)
(* ------------------------------------------------------------------ *)

(** E_not with an unknown column inside HAVING (Error arm of line 273). *)
let bind_select_having_not_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_not (Ast.E_col "bogus"));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_not in HAVING agg proj"

(** E_is_null with an unknown column inside HAVING (Error arm of line 275). *)
let bind_select_having_is_null_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_is_null (Ast.E_col "bogus"));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_is_null in HAVING agg proj"

(** E_is_not_null with an unknown column inside HAVING (Error arm of line 277). *)
let bind_select_having_is_not_null_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_is_not_null (Ast.E_col "bogus"));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_is_not_null in HAVING agg proj"

(** E_neg with an unknown column inside HAVING (Error arm of line 279). *)
let bind_select_having_neg_unknown_col () =
  let cat = two_col_cat () in
  let stmt = Ast.S_select {
    proj = `Exprs [Ast.E_col "id"; Ast.E_agg (Ast.Agg_count, None)];
    table = "users"; joins = [];
    where = None; group_by = ["id"];
    having = Some (Ast.E_neg (Ast.E_col "bogus"));
    order = []; limit = None; offset = None;
  } in
  match bind cat stmt with
  | Error (Sema.Unknown_column { column = "bogus"; _ }) -> ()
  | _ -> Alcotest.fail "expected Unknown_column bogus in E_neg in HAVING agg proj"

(* ------------------------------------------------------------------ *)
(* Group 12: BS_begin / BS_commit / BS_rollback binding                 *)
(* ------------------------------------------------------------------ *)

let bind_begin () =
  let cat = two_col_cat () in
  match bind cat Ast.S_begin with
  | Ok Sema.BS_begin -> ()
  | _ -> Alcotest.fail "expected BS_begin"

let bind_commit () =
  let cat = two_col_cat () in
  match bind cat Ast.S_commit with
  | Ok Sema.BS_commit -> ()
  | _ -> Alcotest.fail "expected BS_commit"

let bind_rollback () =
  let cat = two_col_cat () in
  match bind cat Ast.S_rollback with
  | Ok Sema.BS_rollback -> ()
  | _ -> Alcotest.fail "expected BS_rollback"

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
      Alcotest.test_case "bind_insert_second_col_unknown"  `Quick bind_insert_second_col_unknown;
      Alcotest.test_case "bind_insert_not_null_violation"  `Quick bind_insert_not_null_violation;
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
      Alcotest.test_case "bind_select_where_right_unknown"      `Quick bind_select_where_right_unknown;
      Alcotest.test_case "bind_select_second_col_unknown"        `Quick bind_select_second_col_unknown;
      Alcotest.test_case "bind_select_multi_order_by_rejected"   `Quick bind_select_multi_order_by_rejected;
      Alcotest.test_case "bind_select_order_unknown_col"         `Quick bind_select_order_unknown_col;
      Alcotest.test_case "bind_select_negative_limit"            `Quick bind_select_negative_limit;
      Alcotest.test_case "bind_select_negative_offset"           `Quick bind_select_negative_offset;
    ];
    "create-index", [
      Alcotest.test_case "bind_create_index_basic"          `Quick bind_create_index_basic;
      Alcotest.test_case "bind_create_index_unique"         `Quick bind_create_index_unique;
      Alcotest.test_case "bind_create_index_unknown_table"  `Quick bind_create_index_unknown_table;
      Alcotest.test_case "bind_create_index_unknown_column" `Quick bind_create_index_unknown_column;
      Alcotest.test_case "bind_create_index_duplicate"      `Quick bind_create_index_duplicate;
    ];
    "update", [
      Alcotest.test_case "bind_update_basic"              `Quick bind_update_basic;
      Alcotest.test_case "bind_update_unknown_table"      `Quick bind_update_unknown_table;
      Alcotest.test_case "bind_update_unknown_col"        `Quick bind_update_unknown_col;
      Alcotest.test_case "bind_update_type_mismatch"      `Quick bind_update_type_mismatch;
      Alcotest.test_case "bind_update_not_null_violation" `Quick bind_update_not_null_violation;
      Alcotest.test_case "bind_update_with_where"         `Quick bind_update_with_where;
      Alcotest.test_case "bind_update_where_unknown_col"  `Quick bind_update_where_unknown_col;
    ];
    "delete", [
      Alcotest.test_case "bind_delete_basic"              `Quick bind_delete_basic;
      Alcotest.test_case "bind_delete_with_where"         `Quick bind_delete_with_where;
      Alcotest.test_case "bind_delete_unknown_table"      `Quick bind_delete_unknown_table;
      Alcotest.test_case "bind_delete_where_unknown_col"  `Quick bind_delete_where_unknown_col;
    ];
    "drop", [
      Alcotest.test_case "bind_drop_table_basic"   `Quick bind_drop_table_basic;
      Alcotest.test_case "bind_drop_table_unknown" `Quick bind_drop_table_unknown;
      Alcotest.test_case "bind_drop_index_basic"   `Quick bind_drop_index_basic;
      Alcotest.test_case "bind_drop_index_unknown" `Quick bind_drop_index_unknown;
    ];
    "join", [
      Alcotest.test_case "bind_select_join_unknown_table"       `Quick bind_select_join_unknown_table;
      Alcotest.test_case "bind_select_join_ambiguous_col"       `Quick bind_select_join_ambiguous_col;
      Alcotest.test_case "bind_select_join_unknown_proj_col"    `Quick bind_select_join_unknown_proj_col;
      Alcotest.test_case "bind_select_join_qualified_col_ok"    `Quick bind_select_join_qualified_col_ok;
      Alcotest.test_case "bind_select_join_qualified_unknown_table" `Quick bind_select_join_qualified_unknown_table;
      Alcotest.test_case "bind_select_join_unknown_col_unqual"  `Quick bind_select_join_unknown_col_unqual;
      Alcotest.test_case "bind_select_join_agg_in_on_rejected"  `Quick bind_select_join_agg_in_on_rejected;
      Alcotest.test_case "bind_select_join_qualified_col_unknown_col" `Quick bind_select_join_qualified_col_unknown_col;
      Alcotest.test_case "bind_select_join_where_unknown_col"   `Quick bind_select_join_where_unknown_col;
      Alcotest.test_case "bind_select_more_than_one_join"       `Quick bind_select_more_than_one_join_rejected;
    ];
    "aggregate", [
      Alcotest.test_case "bind_select_count_star"                  `Quick bind_select_count_star;
      Alcotest.test_case "bind_select_sum_col"                     `Quick bind_select_sum_col;
      Alcotest.test_case "bind_select_sum_text_col_rejected"       `Quick bind_select_sum_text_col_rejected;
      Alcotest.test_case "bind_select_avg_text_col_rejected"       `Quick bind_select_avg_text_col_rejected;
      Alcotest.test_case "bind_select_group_by_basic"              `Quick bind_select_group_by_basic;
      Alcotest.test_case "bind_select_group_by_unknown_col"        `Quick bind_select_group_by_unknown_col;
      Alcotest.test_case "bind_select_group_by_multi_rejected"     `Quick bind_select_group_by_multi_rejected;
      Alcotest.test_case "bind_select_having_basic"                `Quick bind_select_having_basic;
      Alcotest.test_case "bind_select_having_no_group_by_rejected" `Quick bind_select_having_no_group_by_rejected;
      Alcotest.test_case "bind_select_agg_star_non_count_rejected" `Quick bind_select_agg_star_non_count_rejected;
      Alcotest.test_case "bind_select_agg_complex_arg_rejected"    `Quick bind_select_agg_complex_arg_rejected;
      Alcotest.test_case "bind_select_col_not_in_group_by_rejected" `Quick bind_select_col_not_in_group_by_rejected;
      Alcotest.test_case "bind_select_agg_star_in_select_rejected" `Quick bind_select_agg_star_in_select_rejected;
      Alcotest.test_case "bind_select_agg_unknown_col_arg"         `Quick bind_select_agg_unknown_col_arg;
      Alcotest.test_case "bind_select_agg_in_where_rejected"       `Quick bind_select_agg_in_where_rejected;
    ];
    "error-values", [
      Alcotest.test_case "error_already_exists_message" `Quick error_already_exists_message;
      Alcotest.test_case "error_unknown_table_message"  `Quick error_unknown_table_message;
      Alcotest.test_case "error_unknown_col_fields"     `Quick error_unknown_col_fields;
      Alcotest.test_case "error_arity_fields"           `Quick error_arity_fields;
    ];
    "gap-fill", [
      Alcotest.test_case "bind_insert_real_lit_type_mismatch" `Quick bind_insert_real_lit_type_mismatch;
      Alcotest.test_case "bind_insert_blob_lit_ok"            `Quick bind_insert_blob_lit_ok;
      Alcotest.test_case "bind_insert_real_lit_ok"            `Quick bind_insert_real_lit_ok;
      Alcotest.test_case "bind_select_tbl_col_unknown_no_join" `Quick bind_select_tbl_col_unknown_no_join;
      Alcotest.test_case "bind_select_not_unknown_col"        `Quick bind_select_not_unknown_col;
      Alcotest.test_case "bind_select_is_null_unknown_col"    `Quick bind_select_is_null_unknown_col;
      Alcotest.test_case "bind_select_is_not_null_unknown_col" `Quick bind_select_is_not_null_unknown_col;
      Alcotest.test_case "bind_select_neg_unknown_col"        `Quick bind_select_neg_unknown_col;
      Alcotest.test_case "bind_select_join_unqual_right_only" `Quick bind_select_join_unqual_right_only;
      Alcotest.test_case "bind_select_join_qual_right_col"    `Quick bind_select_join_qual_right_col;
      Alcotest.test_case "bind_select_join_binop_right_error" `Quick bind_select_join_binop_right_error;
      Alcotest.test_case "bind_select_join_on_not"            `Quick bind_select_join_on_not;
      Alcotest.test_case "bind_select_join_on_not_error"      `Quick bind_select_join_on_not_error;
      Alcotest.test_case "bind_select_join_on_is_null"        `Quick bind_select_join_on_is_null;
      Alcotest.test_case "bind_select_join_on_is_null_error"  `Quick bind_select_join_on_is_null_error;
      Alcotest.test_case "bind_select_join_on_is_not_null"    `Quick bind_select_join_on_is_not_null;
      Alcotest.test_case "bind_select_join_on_is_not_null_error" `Quick bind_select_join_on_is_not_null_error;
      Alcotest.test_case "bind_select_join_on_neg"            `Quick bind_select_join_on_neg;
      Alcotest.test_case "bind_select_join_on_neg_error"      `Quick bind_select_join_on_neg_error;
      Alcotest.test_case "bind_select_having_with_plain_col"  `Quick bind_select_having_with_plain_col;
      Alcotest.test_case "bind_select_having_unknown_col"     `Quick bind_select_having_unknown_col;
      Alcotest.test_case "bind_select_having_qual_col"        `Quick bind_select_having_qual_col;
      Alcotest.test_case "bind_select_having_qual_col_error"  `Quick bind_select_having_qual_col_error;
      Alcotest.test_case "bind_select_having_with_not_agg"    `Quick bind_select_having_with_not_agg;
      Alcotest.test_case "bind_select_having_with_neg_agg"    `Quick bind_select_having_with_neg_agg;
      Alcotest.test_case "bind_select_having_with_is_null_agg" `Quick bind_select_having_with_is_null_agg;
      Alcotest.test_case "bind_select_having_with_is_not_null_agg" `Quick bind_select_having_with_is_not_null_agg;
      Alcotest.test_case "bind_select_agg_qual_col_arg"       `Quick bind_select_agg_qual_col_arg;
      Alcotest.test_case "bind_select_agg_qual_col_arg_error" `Quick bind_select_agg_qual_col_arg_error;
      Alcotest.test_case "bind_create_default_null"           `Quick bind_create_default_null;
      Alcotest.test_case "bind_create_default_real"           `Quick bind_create_default_real;
      Alcotest.test_case "bind_create_default_blob"           `Quick bind_create_default_blob;
      Alcotest.test_case "bind_insert_default_null"           `Quick bind_insert_default_null;
      Alcotest.test_case "bind_insert_default_real"           `Quick bind_insert_default_real;
      Alcotest.test_case "bind_insert_default_blob"           `Quick bind_insert_default_blob;
      Alcotest.test_case "bind_insert_not_null_late"          `Quick bind_insert_not_null_late;
      Alcotest.test_case "bind_select_multi_join_explicit_error" `Quick bind_select_multi_join_explicit_error;
      Alcotest.test_case "bind_select_join_right_only_col_proj" `Quick bind_select_join_right_only_col_proj;
      Alcotest.test_case "bind_select_qual_no_join"           `Quick bind_select_qual_no_join;
      Alcotest.test_case "bind_select_qual_no_join_unknown_col" `Quick bind_select_qual_no_join_unknown_col;
      Alcotest.test_case "bind_select_qual_no_join_unknown_table" `Quick bind_select_qual_no_join_unknown_table;
      Alcotest.test_case "bind_select_join_qual_left_unknown_col_proj" `Quick bind_select_join_qual_left_unknown_col_proj;
      Alcotest.test_case "bind_select_join_on_ambiguous"      `Quick bind_select_join_on_ambiguous;
      Alcotest.test_case "bind_select_join_on_unqual_left_only" `Quick bind_select_join_on_unqual_left_only;
      Alcotest.test_case "bind_select_join_on_qual_left_unknown" `Quick bind_select_join_on_qual_left_unknown;
      Alcotest.test_case "bind_select_join_binop_left_error"  `Quick bind_select_join_binop_left_error;
      Alcotest.test_case "bind_select_having_binop_right_error" `Quick bind_select_having_binop_right_error;
      Alcotest.test_case "bind_select_having_agg_unknown_arg" `Quick bind_select_having_agg_unknown_arg;
      Alcotest.test_case "bind_select_having_agg_with_col_arg" `Quick bind_select_having_agg_with_col_arg;
      Alcotest.test_case "bind_select_having_agg_with_qual_arg" `Quick bind_select_having_agg_with_qual_arg;
      Alcotest.test_case "bind_select_having_agg_with_qual_arg_error" `Quick bind_select_having_agg_with_qual_arg_error;
      Alcotest.test_case "bind_select_having_agg_complex_arg" `Quick bind_select_having_agg_complex_arg;
      Alcotest.test_case "bind_select_having_sum_star_rejected" `Quick bind_select_having_sum_star_rejected;
    ];
    "agg-proj-errors", [
      Alcotest.test_case "having_not_unknown_col"         `Quick bind_select_having_not_unknown_col;
      Alcotest.test_case "having_is_null_unknown_col"     `Quick bind_select_having_is_null_unknown_col;
      Alcotest.test_case "having_is_not_null_unknown_col" `Quick bind_select_having_is_not_null_unknown_col;
      Alcotest.test_case "having_neg_unknown_col"         `Quick bind_select_having_neg_unknown_col;
    ];
    "txn-stmts", [
      Alcotest.test_case "bind_begin"    `Quick bind_begin;
      Alcotest.test_case "bind_commit"   `Quick bind_commit;
      Alcotest.test_case "bind_rollback" `Quick bind_rollback;
    ];
  ]
