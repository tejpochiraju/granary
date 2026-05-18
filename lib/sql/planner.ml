module Cat = Sqlocaml_catalog.Catalog
module Row = Sqlocaml_encoding.Row

let plan_binop : Sema.binop -> Plan.binop = function
  | Sema.Eq  -> Plan.Eq  | Sema.Ne  -> Plan.Ne
  | Sema.Lt  -> Plan.Lt  | Sema.Le  -> Plan.Le
  | Sema.Gt  -> Plan.Gt  | Sema.Ge  -> Plan.Ge
  | Sema.Add -> Plan.Add | Sema.Sub -> Plan.Sub
  | Sema.Mul -> Plan.Mul | Sema.Div -> Plan.Div
  | Sema.And -> Plan.And | Sema.Or  -> Plan.Or
  | Sema.Concat  -> Plan.Concat
  | Sema.Mod     -> Plan.Mod
  | Sema.Bit_and -> Plan.Bit_and | Sema.Bit_or -> Plan.Bit_or
  | Sema.Lshift  -> Plan.Lshift  | Sema.Rshift -> Plan.Rshift
  | Sema.Like -> Plan.Like | Sema.Glob -> Plan.Glob

let rec plan_expr = function
  | Sema.BE_lit l               -> Plan.P_lit l
  | Sema.BE_col i               -> Plan.P_col i
  | Sema.BE_binop (op, a, b)    -> Plan.P_binop (plan_binop op, plan_expr a, plan_expr b)
  | Sema.BE_not e               -> Plan.P_not (plan_expr e)
  | Sema.BE_is_null e           -> Plan.P_is_null (plan_expr e)
  | Sema.BE_is_not_null e       -> Plan.P_is_not_null (plan_expr e)
  | Sema.BE_neg e               -> Plan.P_neg (plan_expr e)
  | Sema.BE_bitnot e            -> Plan.P_bitnot (plan_expr e)
  | Sema.BE_between (x, lo, hi) -> Plan.P_between (plan_expr x, plan_expr lo, plan_expr hi)
  | Sema.BE_in (x, vals)        -> Plan.P_in (plan_expr x, List.map plan_expr vals)
  | Sema.BE_func (func, args)   -> Plan.P_func (func, List.map plan_expr args)
  | Sema.BE_param i             -> Plan.P_param i
  | Sema.BE_match _             ->
    failwith "plan_expr: BE_match should be handled at statement level, not as an expr"
  | Sema.BE_subquery inner      -> Plan.P_subquery inner
  | Sema.BE_exists inner        -> Plan.P_exists inner
  | Sema.BE_in_select (bx, inner) -> Plan.P_in_select (plan_expr bx, inner)
  | Sema.BE_case { scrutinee; branches; else_ } ->
    Plan.P_case {
      scrutinee = Option.map plan_expr scrutinee;
      branches  = List.map (fun (c, r) -> (plan_expr c, plan_expr r)) branches;
      else_     = Option.map plan_expr else_;
    }
  | Sema.BE_cast (e, ty) -> Plan.P_cast (plan_expr e, ty)
  | Sema.BE_excluded_col i -> Plan.P_excluded_col i
  | Sema.BE_window_slot i -> Plan.P_window_slot i
  | Sema.BE_collate (be, c) -> Plan.P_collate (plan_expr be, c)

(** Try to recognise an equality predicate of the form
    [col = lit] (or [lit = col]) at the top level of the WHERE clause.
    Returns [Some (col_idx, lit_expr)] if matched, [None] otherwise.
    [col = NULL] is intentionally NOT matched here because the SQL
    semantics for [WHERE col = NULL] are "never matches" — falling back
    to [Op_filter] (which short-circuits on NULL) gives correct
    behaviour. *)
let recognise_eq_col_lit = function
  | Sema.BE_binop (Sema.Eq, Sema.BE_col i, (Sema.BE_lit l as e))
  | Sema.BE_binop (Sema.Eq, (Sema.BE_lit l as e), Sema.BE_col i) ->
    (match l with
     | Ast.L_null -> None
     | _          -> Some (i, e))
  | _ -> None

(** If the catalog has a single-column index on [(table, col_idx)], return the
    matching [index_info].  Multi-column indexes are not used for lookup
    optimization (deferred).  Otherwise [None]. *)
let find_index_on_col cat (meta : Cat.table_meta) col_idx =
  let col_name = (List.nth meta.columns col_idx).Row.name in
  let candidates = Cat.indexes_for_table cat ~table:meta.name in
  List.find_opt (fun (i : Cat.index_info) ->
    match i.idx_columns with
    | [col] -> col = col_name
    | _ -> false  (* multi-column indexes not used for lookup optimization *)
  ) candidates

(** Detect [BE_col a = BE_col b] equality at the top level. *)
let recognise_eq_col_col = function
  | Sema.BE_binop (Sema.Eq, Sema.BE_col a, Sema.BE_col b) -> Some (a, b)
  | _ -> None

let make_scan (meta : Cat.table_meta) : Plan.op =
  if meta.Cat.tree_id = -1 then
    Plan.Op_cte_scan { cte_name = meta.Cat.name; n_cols = List.length meta.Cat.columns }
  else
    Plan.Op_seq_scan { table_meta = meta }

(** Plan a JOIN.  [left_op] produces left-table rows; we wrap it with
    either Op_nested_loop_join (when the right join column has an index)
    or Op_hash_join (otherwise).  If the ON predicate is not a simple
    equality between a left and a right column, fall back to a hash
    cartesian product wrapped in an Op_filter. *)
let plan_join cat (bj : Sema.bound_join) (left_op : Plan.op) (n_left : int) : Plan.op =
  let join_kind = match bj.kind with
    | Ast.Inner -> `Inner
    | Ast.Left  -> `Left
  in
  let right_offset = bj.right_col_offset in
  let n_right_cols = List.length bj.right_meta.Cat.columns in
  let mk_with_left_col_right_col left_col right_col : Plan.op =
    let idx_opt = find_index_on_col cat bj.right_meta right_col in
    match idx_opt with
    | Some idx ->
      Plan.Op_nested_loop_join {
        left = left_op;
        right_meta = bj.right_meta;
        idx_tree = idx.Cat.idx_tree_id;
        right_col_idx = right_col;
        left_col_idx = left_col;
        join_kind;
        right_col_offset = right_offset;
        n_right_cols;
      }
    | None ->
      Plan.Op_hash_join {
        left = left_op;
        right = make_scan bj.right_meta;
        left_key  = left_col;
        right_key = right_col;
        join_kind;
        right_col_offset = right_offset;
        n_right_cols;
      }
  in
  match recognise_eq_col_col bj.on with
  | Some (a, b) when (a < n_left) && (b >= right_offset) ->
    mk_with_left_col_right_col a (b - right_offset)
  | Some (a, b) when (b < n_left) && (a >= right_offset) ->
    mk_with_left_col_right_col b (a - right_offset)
  | _ ->
    (* General ON predicate: cartesian hash-join + post-filter. *)
    let cart =
      Plan.Op_hash_join {
        left = left_op;
        right = make_scan bj.right_meta;
        left_key  = -1;
        right_key = -1;
        join_kind;
        right_col_offset = right_offset;
        n_right_cols;
      }
    in
    Plan.Op_filter { pred = plan_expr bj.on; child = cart }

let sema_agg_to_plan (a : Sema.agg_spec) : Plan.agg_spec =
  { Plan.func = a.func; col_ord = a.col_ord }

let sema_agg_proj_to_plan : Sema.agg_proj_item -> Plan.proj_item = function
  | Sema.AP_group_col i -> Plan.PI_group_col i
  | Sema.AP_agg_slot i  -> Plan.PI_agg_slot i

let plan_window_item (ws : Sema.window_sema) : Plan.window_plan_item =
  { Plan.func         = ws.Sema.func;
    args         = List.map plan_expr ws.Sema.args;
    partition_by = List.map plan_expr ws.Sema.partition_by;
    order_by     = List.map (fun (bk : Sema.bound_order_key) ->
      let dir = match bk.Sema.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      (plan_expr bk.Sema.key, dir)
    ) ws.Sema.order_by;
    frame        = ws.Sema.frame;
  }

let rec substitute_window_slots ~n_input_cols (e : Plan.expr) : Plan.expr =
  let go = substitute_window_slots ~n_input_cols in
  match e with
  | Plan.P_window_slot i -> Plan.P_col (n_input_cols + i)
  | Plan.P_binop (op, a, b) -> Plan.P_binop (op, go a, go b)
  | Plan.P_not e -> Plan.P_not (go e)
  | Plan.P_is_null e -> Plan.P_is_null (go e)
  | Plan.P_is_not_null e -> Plan.P_is_not_null (go e)
  | Plan.P_neg e -> Plan.P_neg (go e)
  | Plan.P_bitnot e -> Plan.P_bitnot (go e)
  | Plan.P_between (x, lo, hi) -> Plan.P_between (go x, go lo, go hi)
  | Plan.P_in (x, vs) -> Plan.P_in (go x, List.map go vs)
  | Plan.P_func (f, args) -> Plan.P_func (f, List.map go args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    Plan.P_case { scrutinee = Option.map go scrutinee;
                  branches = List.map (fun (c, r) -> (go c, go r)) branches;
                  else_ = Option.map go else_ }
  | Plan.P_cast (e, ty) -> Plan.P_cast (go e, ty)
  | Plan.P_collate (e, c) -> Plan.P_collate (go e, c)
  | e' -> e'

let plan_select cat
    ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset ~joins
    ~group_by ~aggs ~having ~agg_proj ~distinct ~windows =
  let has_joins = joins <> [] in
  let n_input_cols =
    List.length table_meta.Cat.columns
    + List.fold_left (fun acc (bj : Sema.bound_join) ->
        acc + List.length bj.Sema.right_meta.Cat.columns
      ) 0 joins
  in
  (* Try to use an index lookup if possible (single-table path). *)
  let base =
    if has_joins then
      (* With JOINs, we always start from a seq scan of the left table
         and let plan_join wrap it.  WHERE applies to the combined row
         (handled below). *)
      make_scan table_meta
    else
      (match where with
       | None -> make_scan table_meta
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
                 child = make_scan table_meta;
               })
          | None ->
            Plan.Op_filter {
              pred = plan_expr e;
              child = make_scan table_meta;
            }))
  in
  (* Chain all joins left to right *)
  let (after_joins, _) =
    List.fold_left (fun (op, n_left) (bj : Sema.bound_join) ->
      let joined = plan_join cat bj op n_left in
      let n_left' = n_left + List.length bj.Sema.right_meta.Cat.columns in
      (joined, n_left')
    ) (base, List.length table_meta.columns) joins
  in
  let after_where =
    if has_joins then
      (match where with
       | None   -> after_joins
       | Some e -> Plan.Op_filter { pred = plan_expr e; child = after_joins })
    else
      after_joins
  in
  let is_aggregated = aggs <> [] || group_by <> [] in
  (* Insert Op_window after scan+filter+joins when windows are present. *)
  let after_window =
    if windows = [] then after_where
    else
      Plan.Op_window {
        child        = after_where;
        windows      = List.map plan_window_item windows;
        n_input_cols;
      }
  in
  (* For non-aggregate queries: sort BEFORE projection so col_idx correctly
     addresses the original table schema (pre-projection row layout).
     For aggregate queries: sort AFTER aggregation because ORDER BY refers
     to the aggregated output row layout. *)
  let make_sort_keys () =
    List.map (fun (bkey : Sema.bound_order_key) ->
      let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      let e = plan_expr bkey.key in
      let e' = if windows = [] then e
               else substitute_window_slots ~n_input_cols e in
      (e', dir)
    ) order
  in
  let make_sort child =
    let keys = make_sort_keys () in
    if keys = [] then child
    else Plan.Op_sort { keys; child }
  in
  let after_sort =
    if is_aggregated then after_window
    else make_sort after_window
  in
  let projected =
    if is_aggregated then
      Plan.Op_aggregate {
        child = after_sort;
        group_cols = group_by;
        aggs = List.map sema_agg_to_plan aggs;
        having = Option.map plan_expr having;
        proj = List.map sema_agg_proj_to_plan agg_proj;
      }
    else if expr_proj <> [] then
      Plan.Op_expr_project {
        exprs = List.map (fun (be, alias) ->
          let e = plan_expr be in
          let e' = if windows = [] then e
                   else substitute_window_slots ~n_input_cols e in
          (e', alias)
        ) expr_proj;
        child = after_sort;
      }
    else
      Plan.Op_project { ordinals = proj; child = after_sort }
  in
  (* Post-aggregation sort (only for aggregated queries). *)
  let sorted =
    if is_aggregated then make_sort projected
    else projected
  in
  let after_distinct =
    if distinct then Plan.Op_distinct { child = sorted }
    else sorted
  in
  match limit with
  | None   -> after_distinct
  | Some n ->
    let off = Option.value ~default:0 offset in
    Plan.Op_limit { limit = n; offset = off; child = after_distinct }

let rec plan ?cat = function
  | Sema.BS_create_table { name; columns; uniq_idxs; if_not_exists } ->
    Plan.Op_create_table { name; columns; uniq_idxs; if_not_exists }
  | Sema.BS_insert { table_meta; ordinals; values; on_conflict; returning; upsert_update } ->
    let plan_upsert = match upsert_update with
      | None -> None
      | Some (cols, assigns) ->
        Some (cols, List.map (fun (i, e) -> (i, plan_expr e)) assigns)
    in
    Plan.Op_insert { table_meta; ordinals;
                     values = List.map (List.map plan_expr) values;
                     on_conflict;
                     returning = List.map plan_expr returning;
                     upsert_update = plan_upsert }
  | Sema.BS_select { distinct; table_meta; proj; expr_proj; where; order; limit; offset;
                     joins; group_by; aggs; having; agg_proj; windows } ->
    (match cat with
     | Some cat ->
       plan_select cat ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset
         ~joins ~group_by ~aggs ~having ~agg_proj ~distinct ~windows
     | None ->
       (* Backwards-compatible path: no catalog → no index lookup, and
          (for JOIN) no index-based NLJ.  Build a hash-join + filter
          chain manually. *)
       let base = make_scan table_meta in
       let (after_joins, _) =
         List.fold_left (fun (op, n_left) (bj : Sema.bound_join) ->
           let n_right_cols = List.length bj.right_meta.Cat.columns in
           let right_offset = bj.right_col_offset in
           let join_kind = match bj.kind with
             | Ast.Inner -> `Inner | Ast.Left -> `Left
           in
           let joined =
             (match recognise_eq_col_col bj.on with
              | Some (a, b) when (a < n_left) && (b >= right_offset) ->
                Plan.Op_hash_join {
                  left = op;
                  right = make_scan bj.right_meta;
                  left_key = a; right_key = b - right_offset;
                  join_kind; right_col_offset = right_offset; n_right_cols;
                }
              | Some (a, b) when (b < n_left) && (a >= right_offset) ->
                Plan.Op_hash_join {
                  left = op;
                  right = make_scan bj.right_meta;
                  left_key = b; right_key = a - right_offset;
                  join_kind; right_col_offset = right_offset; n_right_cols;
                }
              | _ ->
                let cart = Plan.Op_hash_join {
                  left = op;
                  right = make_scan bj.right_meta;
                  left_key = -1; right_key = -1;
                  join_kind; right_col_offset = right_offset; n_right_cols;
                } in
                Plan.Op_filter { pred = plan_expr bj.on; child = cart })
           in
           (joined, n_left + n_right_cols)
         ) (base, List.length table_meta.columns) joins
       in
       let filtered = match where with
         | None   -> after_joins
         | Some e -> Plan.Op_filter { pred = plan_expr e; child = after_joins }
       in
       let n_input_cols_no_cat =
         List.length table_meta.Cat.columns
         + List.fold_left (fun acc (bj : Sema.bound_join) ->
             acc + List.length bj.Sema.right_meta.Cat.columns
           ) 0 joins
       in
       let after_window_no_cat =
         if windows = [] then filtered
         else
           Plan.Op_window {
             child        = filtered;
             windows      = List.map plan_window_item windows;
             n_input_cols = n_input_cols_no_cat;
           }
       in
       let is_aggregated = aggs <> [] || group_by <> [] in
       let make_sort_keys () =
         List.map (fun (bkey : Sema.bound_order_key) ->
           let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
           (plan_expr bkey.key, dir)
         ) order
       in
       let make_sort child =
         let keys = make_sort_keys () in
         if keys = [] then child
         else Plan.Op_sort { keys; child }
       in
       let after_sort =
         if is_aggregated then after_window_no_cat
         else make_sort after_window_no_cat
       in
       let projected =
         if is_aggregated then
           Plan.Op_aggregate {
             child = after_sort;
             group_cols = group_by;
             aggs = List.map sema_agg_to_plan aggs;
             having = Option.map plan_expr having;
             proj = List.map sema_agg_proj_to_plan agg_proj;
           }
         else if expr_proj <> [] then
           Plan.Op_expr_project {
             exprs = List.map (fun (be, alias) ->
               let e = plan_expr be in
               let e' = if windows = [] then e
                        else substitute_window_slots ~n_input_cols:n_input_cols_no_cat e in
               (e', alias)
             ) expr_proj;
             child = after_sort;
           }
         else
           Plan.Op_project { ordinals = proj; child = after_sort }
       in
       let sorted =
         if is_aggregated then make_sort projected
         else projected
       in
       let after_distinct =
         if distinct then Plan.Op_distinct { child = sorted }
         else sorted
       in
       match limit with
       | None   -> after_distinct
       | Some n ->
         let off = Option.value ~default:0 offset in
         Plan.Op_limit { limit = n; offset = off; child = after_distinct })
  | Sema.BS_create_index { name; table_meta; col_idxs; unique; if_not_exists } ->
    Plan.Op_create_index {
      name;
      table    = table_meta.name;
      tree_id  = table_meta.tree_id;
      col_idxs;
      unique;
      columns  = table_meta.columns;
      if_not_exists;
    }
  | Sema.BS_update { table_meta; assignments; where; returning } ->
    let indexes = match cat with
      | Some c -> Cat.indexes_for_table c ~table:table_meta.Cat.name
      | None   -> []
    in
    let plan_assignments =
      List.map (fun (i, e) -> (i, plan_expr e)) assignments
    in
    let plan_where = Option.map plan_expr where in
    Plan.Op_update {
      table_meta;
      assignments = plan_assignments;
      where       = plan_where;
      indexes;
      returning   = List.map plan_expr returning;
    }
  | Sema.BS_delete { table_meta; where; returning } ->
    let indexes = match cat with
      | Some c -> Cat.indexes_for_table c ~table:table_meta.Cat.name
      | None   -> []
    in
    Plan.Op_delete {
      table_meta;
      where     = Option.map plan_expr where;
      indexes;
      returning = List.map plan_expr returning;
    }
  | Sema.BS_drop_table { table_meta; _ } ->
    let indexes = match cat with
      | Some c -> Cat.indexes_for_table c ~table:table_meta.Cat.name
      | None   -> []
    in
    Plan.Op_drop_table { table_meta; indexes }
  | Sema.BS_drop_index { idx_info; _ } ->
    Plan.Op_drop_index { idx_info }
  | Sema.BS_alter_table { table_meta; action } ->
    Plan.Op_alter_table { table_meta; action }
  | Sema.BS_begin    -> Plan.Op_begin
  | Sema.BS_commit   -> Plan.Op_commit
  | Sema.BS_rollback -> Plan.Op_rollback
  | Sema.BS_create_fts_table { name; columns } ->
    Plan.Op_create_fts_table { name; columns }
  | Sema.BS_fts_insert { fts_meta; col_names; col_values } ->
    Plan.Op_fts_insert { fts_meta; col_names;
      col_values = List.map plan_expr col_values }
  | Sema.BS_fts_delete { fts_meta; where } ->
    Plan.Op_fts_delete { fts_meta; where = Option.map plan_expr where }
  | Sema.BS_fts_seq_scan { fts_meta; where } ->
    Plan.Op_fts_seq_scan { fts_meta; where = Option.map plan_expr where }
  | Sema.BS_fts_match_scan { fts_meta; query; proj; include_rank } ->
    Plan.Op_fts_match_scan { fts_meta; query; proj; include_rank }
  | Sema.BS_compound { op; left; right } ->
    let l = plan ?cat left in
    let r = plan ?cat right in
    (match op with
     | Ast.Union     -> Plan.Op_union     { all = false; left = l; right = r }
     | Ast.Union_all -> Plan.Op_union     { all = true;  left = l; right = r }
     | Ast.Intersect -> Plan.Op_intersect { left = l; right = r }
     | Ast.Except    -> Plan.Op_except    { left = l; right = r })
  | Sema.BS_const_select { exprs } ->
    Plan.Op_const_select { exprs = List.map (fun (e, alias) -> (plan_expr e, alias)) exprs }
  | Sema.BS_pragma { kind } ->
    let rows = match kind with
      | Ast.Pragma_table_info table_name ->
        (match cat with
         | None -> []
         | Some c ->
           (match Cat.find_table_cached c ~name:table_name with
            | None -> []
            | Some meta ->
              List.mapi (fun i (col : Row.column) ->
                [| Row.V_int (Int64.of_int i);
                   Row.V_text col.name;
                   Row.V_text (match col.ty with
                     | Row.Integer -> "INTEGER" | Row.Text -> "TEXT"
                     | Row.Real    -> "REAL"    | Row.Blob -> "BLOB");
                   Row.V_int (if col.not_null then 1L else 0L);
                   Row.V_null;  (* dflt_value — simplified *)
                   Row.V_int (if col.primary_key then 1L else 0L) |]
              ) meta.columns))
      | Ast.Pragma_index_list table_name ->
        let idxs = match cat with
          | None -> []
          | Some c -> Cat.indexes_for_table c ~table:table_name
        in
        List.mapi (fun i (idx : Cat.index_info) ->
          [| Row.V_int (Int64.of_int i);
             Row.V_text idx.idx_name;
             Row.V_int (if idx.idx_unique then 1L else 0L) |]
        ) idxs
    in
    Plan.Op_pragma_rows { rows }
  | Sema.BS_with_cte { name; def; query; recursive } ->
    Plan.Op_with_cte {
      cte_name  = name;
      def       = plan ?cat def;
      query     = plan ?cat query;
      recursive;
    }
  | Sema.BS_create_view { name; query } ->
    Plan.Op_create_view { name; query }
  | Sema.BS_drop_view { name } ->
    Plan.Op_drop_view { name }
