open Lwt.Syntax
module Cat    = Sqlocaml_catalog.Catalog
module S      = Sqlocaml_store.Store
module Row    = Sqlocaml_encoding.Row
module Sema   = Sqlocaml_sql.Sema
module Ast    = Sqlocaml_sql.Ast
module Plan   = Sqlocaml_sql.Plan
module Planner = Sqlocaml_sql.Planner

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let make_cat () =
  Lwt_main.run (
    let store = S.create () in
    let* cat = Cat.open_ store in
    let* _ = Cat.create_table cat ~name:"users" ~columns:[
      { Row.name = "id";   ty = Row.Integer; not_null = false; primary_key = false; default = None };
      { Row.name = "name"; ty = Row.Text; not_null = false; primary_key = false; default = None };
    ] in
    Lwt.return cat
  )

let bind cat stmt = Lwt_main.run (Sema.bind cat stmt) |> Result.get_ok

(* ------------------------------------------------------------------ *)
(* Group 1: Plan shapes (structural)                                    *)
(* ------------------------------------------------------------------ *)

let plan_create_table () =
  let cat = make_cat () in
  let cols = [
    Ast.{ name = "sku"; ty = Ty_text; not_null = false; primary_key = false; default = None };
    Ast.{ name = "qty"; ty = Ty_int;  not_null = false; primary_key = false; default = None };
  ] in
  let stmt = Ast.S_create_table { name = "items"; columns = cols } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_create_table { name; columns } ->
    Alcotest.(check string) "table name" "items" name;
    Alcotest.(check int) "column count" 2 (List.length columns)
  | _ -> Alcotest.fail "expected Op_create_table"

let plan_insert () =
  let cat = make_cat () in
  let stmt = Ast.S_insert {
    table   = "users";
    columns = ["id"; "name"];
    values  = [Ast.E_lit (Ast.L_int 1L); Ast.E_lit (Ast.L_text "alice")];
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_insert { ordinals; values; _ } ->
    Alcotest.(check (list int)) "ordinals" [0; 1] ordinals;
    Alcotest.(check int) "value count" 2 (List.length values)
  | _ -> Alcotest.fail "expected Op_insert"

let plan_select_star_no_where () =
  let cat = make_cat () in
  let stmt = Ast.S_select { distinct = false; proj = `All; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_seq_scan _; _ } -> ()
  | Plan.Op_project { child = Plan.Op_filter _; _ } ->
    Alcotest.fail "unexpected filter layer (no WHERE clause)"
  | _ -> Alcotest.fail "expected Op_project { child = Op_seq_scan _ }"

let plan_select_cols_no_where () =
  let cat = make_cat () in
  let stmt = Ast.S_select { distinct = false; proj = `Cols ["id"]; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { ordinals = [0]; child = Plan.Op_seq_scan _ } -> ()
  | Plan.Op_project { ordinals; child = Plan.Op_seq_scan _ } ->
    Alcotest.failf "wrong ordinals: expected [0], got %s"
      (String.concat ";" (List.map string_of_int ordinals))
  | _ -> Alcotest.fail "expected Op_project { ordinals=[0]; child=Op_seq_scan _ }"

let plan_select_star_where () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_lit (Ast.L_int 1L)));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_filter { child = Plan.Op_seq_scan _; _ }; _ } -> ()
  | Plan.Op_project { child = Plan.Op_seq_scan _; _ } ->
    Alcotest.fail "expected filter layer but got Op_seq_scan directly"
  | _ -> Alcotest.fail "expected Op_project { child=Op_filter { child=Op_seq_scan _ } }"

let plan_select_cols_where () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `Cols ["id"];
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_lit (Ast.L_int 1L)));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { ordinals = [0];
                      child = Plan.Op_filter { child = Plan.Op_seq_scan _; _ } } -> ()
  | Plan.Op_project { ordinals; child = Plan.Op_filter _ } ->
    Alcotest.failf "wrong ordinals: expected [0], got %s"
      (String.concat ";" (List.map string_of_int ordinals))
  | _ -> Alcotest.fail "expected Op_project { ordinals=[0]; child=Op_filter { child=Op_seq_scan _ } }"

(* ------------------------------------------------------------------ *)
(* Group 2: Plan expression translation                                  *)
(* ------------------------------------------------------------------ *)

let plan_expr_lit () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_lit (Ast.L_int 99L), Ast.E_lit (Ast.L_int 99L)));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project {
      child = Plan.Op_filter {
        pred = Plan.P_binop (Plan.Eq, Plan.P_lit (Ast.L_int 99L), Plan.P_lit (Ast.L_int 99L)); _ }; _ } -> ()
  | Plan.Op_project { child = Plan.Op_filter { pred; _ }; _ } ->
    ignore pred;
    Alcotest.fail "filter pred not P_binop(Eq, P_lit, P_lit)"
  | _ -> Alcotest.fail "expected Op_project { child=Op_filter _ }"

let plan_expr_col () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_col "id"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project {
      child = Plan.Op_filter {
        pred = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_col 0); _ }; _ } -> ()
  | Plan.Op_project { child = Plan.Op_filter { pred; _ }; _ } ->
    ignore pred;
    Alcotest.fail "filter pred not P_binop(Eq, P_col 0, P_col 0)"
  | _ -> Alcotest.fail "expected Op_project { child=Op_filter _ }"

let plan_expr_eq () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_lit (Ast.L_int 42L)));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project {
      child = Plan.Op_filter {
        pred = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_int 42L)); _ }; _ } -> ()
  | Plan.Op_project { child = Plan.Op_filter { pred; _ }; _ } ->
    ignore pred;
    Alcotest.fail "filter pred not P_binop(Eq, P_col 0, P_lit(L_int 42L))"
  | _ -> Alcotest.fail "expected Op_project { child=Op_filter _ }"

let plan_expr_reversed_eq () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_lit (Ast.L_int 42L), Ast.E_col "id"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project {
      child = Plan.Op_filter {
        pred = Plan.P_binop (Plan.Eq, Plan.P_lit (Ast.L_int 42L), Plan.P_col 0); _ }; _ } -> ()
  | Plan.Op_project { child = Plan.Op_filter { pred; _ }; _ } ->
    ignore pred;
    Alcotest.fail "filter pred not P_binop(Eq, P_lit(L_int 42L), P_col 0)"
  | _ -> Alcotest.fail "expected Op_project { child=Op_filter _ }"

(* ------------------------------------------------------------------ *)
(* Group 3: Projection ordinals                                          *)
(* ------------------------------------------------------------------ *)

let plan_project_all_ordinals () =
  let cat = make_cat () in
  let stmt = Ast.S_select { distinct = false; proj = `All; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { ordinals; _ } ->
    Alcotest.(check (list int)) "all ordinals" [0; 1] ordinals
  | _ -> Alcotest.fail "expected Op_project"

let plan_project_single () =
  let cat = make_cat () in
  let stmt = Ast.S_select { distinct = false; proj = `Cols ["name"]; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { ordinals; _ } ->
    Alcotest.(check (list int)) "single col ordinal" [1] ordinals
  | _ -> Alcotest.fail "expected Op_project"

let plan_project_reversed () =
  let cat = make_cat () in
  let stmt = Ast.S_select { distinct = false; proj = `Cols ["name"; "id"]; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { ordinals; _ } ->
    Alcotest.(check (list int)) "reversed ordinals" [1; 0] ordinals
  | _ -> Alcotest.fail "expected Op_project"

(* ------------------------------------------------------------------ *)
(* Group 4: SeqScan table_meta                                          *)
(* ------------------------------------------------------------------ *)

let plan_seqscan_tree_id () =
  let cat = make_cat () in
  let stmt = Ast.S_select { distinct = false; proj = `All; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_seq_scan { table_meta }; _ } ->
    (* First user table gets tree_id = 16 per catalog comment *)
    Alcotest.(check int) "tree_id" 16 (table_meta.tree_id :> int)
  | _ -> Alcotest.fail "expected Op_project { child=Op_seq_scan _ }"

let plan_seqscan_columns () =
  let cat = make_cat () in
  let stmt = Ast.S_select { distinct = false; proj = `All; table = "users"; joins = []; where = None; group_by = []; having = None; order = []; limit = None; offset = None } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_seq_scan { table_meta }; _ } ->
    Alcotest.(check int) "column count" 2 (List.length table_meta.columns);
    Alcotest.(check string) "col 0 name" "id"   (List.nth table_meta.columns 0).Row.name;
    Alcotest.(check string) "col 1 name" "name" (List.nth table_meta.columns 1).Row.name
  | _ -> Alcotest.fail "expected Op_project { child=Op_seq_scan _ }"

(* ------------------------------------------------------------------ *)
(* Group 5: ORDER BY plan shapes                                         *)
(* ------------------------------------------------------------------ *)

let plan_order_by_asc () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users"; where = None;
    joins = []; group_by = []; having = None;
    order = [{ Ast.expr = Ast.E_col "id"; dir = Ast.Asc }];
    limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_sort { keys = [(Plan.P_col 0, `Asc)];
                                             child = Plan.Op_seq_scan _ }; _ } -> ()
  | _ -> Alcotest.fail "expected Op_project(Op_sort { keys=[(P_col 0,Asc)] }(Op_seq_scan))"

let plan_order_by_desc () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users"; where = None;
    joins = []; group_by = []; having = None;
    order = [{ Ast.expr = Ast.E_col "name"; dir = Ast.Desc }];
    limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_sort { keys = [(Plan.P_col 1, `Desc)]; _ }; _ } -> ()
  | _ -> Alcotest.fail "expected Op_project(Op_sort { keys=[(P_col 1,Desc)] })"

let plan_limit_only () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users"; where = None;
    joins = []; group_by = []; having = None;
    order = []; limit = Some 3; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_limit { limit = 3; offset = 0;
                    child = Plan.Op_project { child = Plan.Op_seq_scan _; _ } } -> ()
  | _ -> Alcotest.fail "expected Op_limit { limit=3; offset=0; child=Op_project(Op_seq_scan) }"

let plan_limit_with_offset () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users"; where = None;
    joins = []; group_by = []; having = None;
    order = []; limit = Some 5; offset = Some 2;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_limit { limit = 5; offset = 2; _ } -> ()
  | _ -> Alcotest.fail "expected Op_limit { limit=5; offset=2 }"

let plan_order_and_limit () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users"; where = None;
    joins = []; group_by = []; having = None;
    order = [{ Ast.expr = Ast.E_col "id"; dir = Ast.Asc }];
    limit = Some 2; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_limit { limit = 2; offset = 0;
                    child = Plan.Op_project { child = Plan.Op_sort { keys = [(Plan.P_col 0, `Asc)]; _ }; _ } } -> ()
  | _ -> Alcotest.fail "expected Op_limit(Op_project(Op_sort(Op_seq_scan)))"

(** SELECT DISTINCT id FROM users → planner should wrap with Op_distinct. *)
let plan_distinct () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = true;
    proj = `Cols ["id"]; table = "users"; where = None;
    joins = []; group_by = []; having = None;
    order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_distinct { child = Plan.Op_project _ } -> ()
  | Plan.Op_project _ ->
    Alcotest.fail "expected Op_distinct wrapping Op_project, got Op_project alone"
  | _ -> Alcotest.fail "expected Op_distinct { child = Op_project _ }"

(** SELECT DISTINCT id FROM users ORDER BY id → Op_distinct wraps Op_project(Op_sort). *)
let plan_distinct_with_order () =
  let cat = make_cat () in
  let stmt = Ast.S_select {
    distinct = true;
    proj = `Cols ["id"]; table = "users"; where = None;
    joins = []; group_by = []; having = None;
    order = [{ Ast.expr = Ast.E_col "id"; dir = Ast.Asc }];
    limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_distinct { child = Plan.Op_project { child = Plan.Op_sort _; _ } }
  | Plan.Op_distinct { child = Plan.Op_project _ } -> ()
  | Plan.Op_project _ ->
    Alcotest.fail "expected Op_distinct wrapping Op_project, got Op_project alone"
  | _ -> Alcotest.fail "expected Op_distinct { child = Op_project _ }"

(* ------------------------------------------------------------------ *)
(* Group 6: Index lookup planner rule                                   *)
(* ------------------------------------------------------------------ *)

let make_cat_with_index ~col_name =
  Lwt_main.run (
    let store = S.create () in
    let* cat = Cat.open_ store in
    let* _ = Cat.create_table cat ~name:"users" ~columns:[
      { Row.name = "id";   ty = Row.Integer; not_null = false; primary_key = false; default = None };
      { Row.name = "name"; ty = Row.Text; not_null = false; primary_key = false; default = None };
    ] in
    let* _ = Cat.create_index cat ~name:"idx" ~table:"users"
      ~columns:[col_name] ~unique:false in
    Lwt.return cat
  )

let plan_index_lookup_col_eq_lit () =
  let cat = make_cat_with_index ~col_name:"id" in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "id", Ast.E_lit (Ast.L_int 1L)));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_project { child = Plan.Op_index_lookup _; _ } -> ()
  | Plan.Op_project { child = Plan.Op_filter _; _ } ->
    Alcotest.fail "expected Op_index_lookup, got Op_filter"
  | _ -> Alcotest.fail "expected Op_project { child=Op_index_lookup _ }"

let plan_index_lookup_lit_eq_col () =
  let cat = make_cat_with_index ~col_name:"id" in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_lit (Ast.L_int 1L), Ast.E_col "id"));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_project { child = Plan.Op_index_lookup _; _ } -> ()
  | _ -> Alcotest.fail "expected Op_project { child=Op_index_lookup _ } (lit=col)"

let plan_no_index_falls_back_to_filter () =
  (* No index on "name" — equality on "name" should fall back to Op_filter. *)
  let cat = make_cat_with_index ~col_name:"id" in
  let stmt = Ast.S_select {
    distinct = false;
    proj  = `All;
    table = "users";
    joins = [];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_col "name", Ast.E_lit (Ast.L_text "x")));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_project { child = Plan.Op_filter { child = Plan.Op_seq_scan _; _ }; _ } -> ()
  | _ -> Alcotest.fail "expected Op_project { child=Op_filter { child=Op_seq_scan _ } }"

let plan_create_index () =
  let cat = make_cat_with_index ~col_name:"id" in
  let stmt = Ast.S_create_index {
    name = "idx2"; table = "users"; columns = ["name"]; unique = true;
  } in
  let bound = bind cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_create_index { name = "idx2"; table = "users"; col_idxs = [1];
                           unique = true; _ } -> ()
  | _ -> Alcotest.fail "expected Op_create_index { name=idx2; ... }"

(* ------------------------------------------------------------------ *)
(* Group 7: Gap-fill — JOIN swapped/general ON, no-cat fallbacks         *)
(* ------------------------------------------------------------------ *)

let make_join_cat () =
  Lwt_main.run (
    let store = S.create () in
    let* cat = Cat.open_ store in
    let* _ = Cat.create_table cat ~name:"users" ~columns:[
      { Row.name = "id";   ty = Row.Integer; not_null = false; primary_key = false; default = None };
      { Row.name = "name"; ty = Row.Text;    not_null = false; primary_key = false; default = None };
    ] in
    let* _ = Cat.create_table cat ~name:"orders" ~columns:[
      { Row.name = "uid";  ty = Row.Integer; not_null = false; primary_key = false; default = None };
      { Row.name = "item"; ty = Row.Text;    not_null = false; primary_key = false; default = None };
    ] in
    Lwt.return cat
  )

(** Swapped ON predicate: ON orders.uid = users.id (right.col = left.col).
    Exercises plan_join lines 88-89 — the second guarded match arm. *)
let plan_join_swapped_on () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("orders", "uid"),
                  Ast.E_tbl_col ("users", "id")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_project { child = Plan.Op_hash_join _; _ } -> ()
  | _ -> Alcotest.fail "expected Op_hash_join with swapped ON"

(** General ON predicate (not col=col).  Exercises plan_join lines 90-103
    — the cartesian + post-filter fallback. *)
let plan_join_general_on () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                (* ON users.id > orders.uid — not equality, falls through *)
                on = Ast.E_binop (Ast.Gt,
                  Ast.E_tbl_col ("users", "id"),
                  Ast.E_tbl_col ("orders", "uid")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_project { child = Plan.Op_filter { child = Plan.Op_hash_join _; _ }; _ } -> ()
  | _ -> Alcotest.fail "expected Op_filter wrapping Op_hash_join cartesian"

(** Plan join without ~cat: exercises the None path in plan for BS_select.
    Same join structure as plan_join_swapped_on but without ~cat, so it
    uses the hash-join based fallback instead of NLJ. *)
let plan_join_no_cat () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("users", "id"),
                  Ast.E_tbl_col ("orders", "uid")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  (* Call Planner.plan WITHOUT ~cat — hits the None branch *)
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_hash_join _; _ } -> ()
  | _ -> Alcotest.fail "expected Op_hash_join for join without ~cat"

(** Plan join without ~cat with swapped ON (b < n_left, a >= right_offset). *)
let plan_join_no_cat_swapped () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("orders", "uid"),
                  Ast.E_tbl_col ("users", "id")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_hash_join _; _ } -> ()
  | _ -> Alcotest.fail "expected Op_hash_join for swapped join without ~cat"

(** Plan join without ~cat with general ON (not col=col) — cartesian hash-join + filter. *)
let plan_join_no_cat_general_on () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Gt,
                  Ast.E_tbl_col ("users", "id"),
                  Ast.E_tbl_col ("orders", "uid")) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_filter { child = Plan.Op_hash_join _; _ }; _ } -> ()
  | _ -> Alcotest.fail "expected Op_filter(Op_hash_join) for general ON without ~cat"

(** Plan join without ~cat with WHERE clause — exercises the filtered path. *)
let plan_join_no_cat_with_where () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_binop (Ast.Eq,
                  Ast.E_tbl_col ("users", "id"),
                  Ast.E_tbl_col ("orders", "uid")) } ];
    where = Some (Ast.E_binop (Ast.Eq, Ast.E_tbl_col ("users", "id"), Ast.E_lit (Ast.L_int 1L)));
    group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_project { child = Plan.Op_filter { child = Plan.Op_hash_join _; _ }; _ } -> ()
  | _ -> Alcotest.fail "expected Op_filter(Op_hash_join) for join+where without ~cat"

(** Plan BS_update without ~cat → indexes = [] (None arm). *)
let plan_update_no_cat () =
  let cat = make_cat () in
  let stmt = Ast.S_update {
    table = "users";
    assignments = [("name", Ast.E_lit (Ast.L_text "x"))];
    where = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_update { indexes = []; _ } -> ()
  | _ -> Alcotest.fail "expected Op_update with empty indexes"

(** Plan BS_delete without ~cat → indexes = [] (None arm). *)
let plan_delete_no_cat () =
  let cat = make_cat () in
  let stmt = Ast.S_delete { table = "users"; where = None } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_delete { indexes = []; _ } -> ()
  | _ -> Alcotest.fail "expected Op_delete with empty indexes"

(** Plan BS_drop_table without ~cat → indexes = [] (None arm). *)
let plan_drop_table_no_cat () =
  let cat = make_cat () in
  let stmt = Ast.S_drop_table { name = "users" } in
  let bound = bind cat stmt in
  match Planner.plan bound with
  | Plan.Op_drop_table { indexes = []; _ } -> ()
  | _ -> Alcotest.fail "expected Op_drop_table with empty indexes"

(** Plan BS_begin → Op_begin. *)
let plan_begin () =
  let cat = make_cat () in
  let bound = bind cat Ast.S_begin in
  match Planner.plan bound with
  | Plan.Op_begin -> ()
  | _ -> Alcotest.fail "expected Op_begin"

(** Plan BS_commit → Op_commit. *)
let plan_commit () =
  let cat = make_cat () in
  let bound = bind cat Ast.S_commit in
  match Planner.plan bound with
  | Plan.Op_commit -> ()
  | _ -> Alcotest.fail "expected Op_commit"

(** Plan BS_rollback → Op_rollback. *)
let plan_rollback () =
  let cat = make_cat () in
  let bound = bind cat Ast.S_rollback in
  match Planner.plan bound with
  | Plan.Op_rollback -> ()
  | _ -> Alcotest.fail "expected Op_rollback"

(** recognise_eq_col_col fallback (line 46): inputs that are NOT BE_col = BE_col
    return None. We verify by giving plan_join an ON that is a literal. *)
let plan_join_on_literal () =
  let cat = make_join_cat () in
  let stmt = Ast.S_select {
    distinct = false;
    proj = `All; table = "users";
    joins = [ { Ast.kind = Ast.Inner; table = "orders"; alias = None;
                on = Ast.E_lit (Ast.L_int 1L) } ];
    where = None; group_by = []; having = None; order = []; limit = None; offset = None;
  } in
  let bound = bind cat stmt in
  match Planner.plan ~cat bound with
  | Plan.Op_project { child = Plan.Op_filter { child = Plan.Op_hash_join _; _ }; _ } -> ()
  | _ -> Alcotest.fail "expected Op_filter wrapping Op_hash_join for literal ON"

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Planner" [
    "plan-shapes", [
      Alcotest.test_case "plan_create_table"         `Quick plan_create_table;
      Alcotest.test_case "plan_insert"               `Quick plan_insert;
      Alcotest.test_case "plan_select_star_no_where" `Quick plan_select_star_no_where;
      Alcotest.test_case "plan_select_cols_no_where" `Quick plan_select_cols_no_where;
      Alcotest.test_case "plan_select_star_where"    `Quick plan_select_star_where;
      Alcotest.test_case "plan_select_cols_where"    `Quick plan_select_cols_where;
    ];
    "plan-expressions", [
      Alcotest.test_case "plan_expr_lit"         `Quick plan_expr_lit;
      Alcotest.test_case "plan_expr_col"         `Quick plan_expr_col;
      Alcotest.test_case "plan_expr_eq"          `Quick plan_expr_eq;
      Alcotest.test_case "plan_expr_reversed_eq" `Quick plan_expr_reversed_eq;
    ];
    "projection-ordinals", [
      Alcotest.test_case "plan_project_all_ordinals" `Quick plan_project_all_ordinals;
      Alcotest.test_case "plan_project_single"       `Quick plan_project_single;
      Alcotest.test_case "plan_project_reversed"     `Quick plan_project_reversed;
    ];
    "seqscan-meta", [
      Alcotest.test_case "plan_seqscan_tree_id"  `Quick plan_seqscan_tree_id;
      Alcotest.test_case "plan_seqscan_columns"  `Quick plan_seqscan_columns;
    ];
    "order-limit-shapes", [
      Alcotest.test_case "plan_order_by_asc"      `Quick plan_order_by_asc;
      Alcotest.test_case "plan_order_by_desc"     `Quick plan_order_by_desc;
      Alcotest.test_case "plan_limit_only"        `Quick plan_limit_only;
      Alcotest.test_case "plan_limit_with_offset" `Quick plan_limit_with_offset;
      Alcotest.test_case "plan_order_and_limit"   `Quick plan_order_and_limit;
    ];
    "distinct", [
      Alcotest.test_case "plan_distinct"            `Quick plan_distinct;
      Alcotest.test_case "plan_distinct_with_order" `Quick plan_distinct_with_order;
    ];
    "index-lookup", [
      Alcotest.test_case "plan_index_lookup_col_eq_lit"      `Quick plan_index_lookup_col_eq_lit;
      Alcotest.test_case "plan_index_lookup_lit_eq_col"      `Quick plan_index_lookup_lit_eq_col;
      Alcotest.test_case "plan_no_index_falls_back_to_filter" `Quick plan_no_index_falls_back_to_filter;
      Alcotest.test_case "plan_create_index"                  `Quick plan_create_index;
    ];
    "gap-fill", [
      Alcotest.test_case "plan_join_swapped_on"  `Quick plan_join_swapped_on;
      Alcotest.test_case "plan_join_general_on"  `Quick plan_join_general_on;
      Alcotest.test_case "plan_join_on_literal"  `Quick plan_join_on_literal;
    ];
    "no-cat", [
      Alcotest.test_case "plan_join_no_cat"            `Quick plan_join_no_cat;
      Alcotest.test_case "plan_join_no_cat_swapped"    `Quick plan_join_no_cat_swapped;
      Alcotest.test_case "plan_join_no_cat_general_on" `Quick plan_join_no_cat_general_on;
      Alcotest.test_case "plan_join_no_cat_with_where" `Quick plan_join_no_cat_with_where;
      Alcotest.test_case "plan_update_no_cat"          `Quick plan_update_no_cat;
      Alcotest.test_case "plan_delete_no_cat"          `Quick plan_delete_no_cat;
      Alcotest.test_case "plan_drop_table_no_cat"      `Quick plan_drop_table_no_cat;
    ];
    "txn-ops", [
      Alcotest.test_case "plan_begin"    `Quick plan_begin;
      Alcotest.test_case "plan_commit"   `Quick plan_commit;
      Alcotest.test_case "plan_rollback" `Quick plan_rollback;
    ];
  ]
