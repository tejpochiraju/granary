module Cat = Sqlocaml_catalog.Catalog
module Row = Sqlocaml_encoding.Row

let rec plan_expr = function
  | Sema.BE_lit l     -> Plan.P_lit l
  | Sema.BE_col i     -> Plan.P_col i
  | Sema.BE_eq (a, b) -> Plan.P_eq (plan_expr a, plan_expr b)

(** Try to recognise an equality predicate of the form
    [col = lit] (or [lit = col]) at the top level of the WHERE clause.
    Returns [Some (col_idx, lit_expr)] if matched, [None] otherwise.
    [col = NULL] is intentionally NOT matched here because the SQL
    semantics for [WHERE col = NULL] are "never matches" — falling back
    to [Op_filter] (which short-circuits on NULL) gives correct
    behaviour. *)
let recognise_eq_col_lit = function
  | Sema.BE_eq (Sema.BE_col i, (Sema.BE_lit l as e))
  | Sema.BE_eq ((Sema.BE_lit l as e), Sema.BE_col i) ->
    (match l with
     | Ast.L_null -> None
     | _          -> Some (i, e))
  | _ -> None

(** If the catalog has an index on [(table, col_idx)], return the
    matching [index_info].  Otherwise [None]. *)
let find_index_on_col cat (meta : Cat.table_meta) col_idx =
  let col_name = (List.nth meta.columns col_idx).Row.name in
  let candidates = Cat.indexes_for_table cat ~table:meta.name in
  List.find_opt (fun (i : Cat.index_info) -> i.idx_column = col_name) candidates

let plan_select cat
    ~table_meta ~proj ~where ~order ~limit ~offset =
  (* Try to use an index lookup if possible. *)
  let base =
    match where with
    | None -> Plan.Op_seq_scan { table_meta }
    | Some e ->
      (match recognise_eq_col_lit e with
       | Some (col_idx, lit_expr) ->
         (match find_index_on_col cat table_meta col_idx with
          | Some idx ->
            let col_type =
              (List.nth table_meta.columns col_idx).Row.ty
            in
            Plan.Op_index_lookup {
              table_tree = table_meta.tree_id;
              idx_tree   = idx.idx_tree_id;
              col_idx;
              col_type;
              lookup_val = plan_expr lit_expr;
              table_meta;
            }
          | None ->
            Plan.Op_filter {
              pred = plan_expr e;
              child = Plan.Op_seq_scan { table_meta };
            })
       | None ->
         Plan.Op_filter {
           pred = plan_expr e;
           child = Plan.Op_seq_scan { table_meta };
         })
  in
  let projected = Plan.Op_project { ordinals = proj; child = base } in
  (* Wrap with Op_sort for the first ORDER BY key *)
  let sorted = match order with
    | [] -> projected
    | (key :: _) ->
      let dir = match key.Sema.dir with
        | Ast.Asc  -> `Asc
        | Ast.Desc -> `Desc
      in
      Plan.Op_sort { col_idx = key.col_idx; dir; child = projected }
  in
  (* Wrap with Op_limit if LIMIT present *)
  match limit with
  | None   -> sorted
  | Some n ->
    let off = Option.value ~default:0 offset in
    Plan.Op_limit { limit = n; offset = off; child = sorted }

let plan ?cat = function
  | Sema.BS_create_table { name; columns } ->
    Plan.Op_create_table { name; columns }
  | Sema.BS_insert { table_meta; ordinals; values } ->
    Plan.Op_insert { table_meta; ordinals; values }
  | Sema.BS_select { table_meta; proj; where; order; limit; offset } ->
    (match cat with
     | Some cat -> plan_select cat ~table_meta ~proj ~where ~order ~limit ~offset
     | None ->
       (* Backwards-compatible path: no catalog, no index lookup.
          Build a plan with Op_seq_scan + optional Op_filter. *)
       let scan = Plan.Op_seq_scan { table_meta } in
       let filtered = match where with
         | None   -> scan
         | Some e -> Plan.Op_filter { pred = plan_expr e; child = scan }
       in
       let projected = Plan.Op_project { ordinals = proj; child = filtered } in
       let sorted = match order with
         | [] -> projected
         | (key :: _) ->
           let dir = match key.Sema.dir with
             | Ast.Asc  -> `Asc
             | Ast.Desc -> `Desc
           in
           Plan.Op_sort { col_idx = key.col_idx; dir; child = projected }
       in
       match limit with
       | None   -> sorted
       | Some n ->
         let off = Option.value ~default:0 offset in
         Plan.Op_limit { limit = n; offset = off; child = sorted })
  | Sema.BS_create_index { name; table_meta; col_idx; unique } ->
    Plan.Op_create_index {
      name;
      table    = table_meta.name;
      tree_id  = table_meta.tree_id;
      col_idx;
      unique;
      columns  = table_meta.columns;
    }
