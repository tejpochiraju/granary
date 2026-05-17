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

(** If the catalog has an index on [(table, col_idx)], return the
    matching [index_info].  Otherwise [None]. *)
let find_index_on_col cat (meta : Cat.table_meta) col_idx =
  let col_name = (List.nth meta.columns col_idx).Row.name in
  let candidates = Cat.indexes_for_table cat ~table:meta.name in
  List.find_opt (fun (i : Cat.index_info) -> i.idx_column = col_name) candidates

(** Detect [BE_col a = BE_col b] equality at the top level. *)
let recognise_eq_col_col = function
  | Sema.BE_binop (Sema.Eq, Sema.BE_col a, Sema.BE_col b) -> Some (a, b)
  | _ -> None

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
        right = Plan.Op_seq_scan { table_meta = bj.right_meta };
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
        right = Plan.Op_seq_scan { table_meta = bj.right_meta };
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
  | Sema.AP_group_col   -> Plan.PI_group_col
  | Sema.AP_agg_slot i  -> Plan.PI_agg_slot i

let plan_select cat
    ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset ~join
    ~group_by ~aggs ~having ~agg_proj =
  (* Try to use an index lookup if possible (single-table path). *)
  let base =
    match join with
    | Some _ ->
      (* With a JOIN, we always start from a seq scan of the left table
         and let plan_join wrap it.  WHERE applies to the combined row
         (handled below). *)
      Plan.Op_seq_scan { table_meta }
    | None ->
      (match where with
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
            }))
  in
  (* Apply JOIN (if any), then WHERE (post-join). *)
  let after_join =
    match join with
    | None -> base
    | Some bj ->
      let n_left = List.length table_meta.columns in
      plan_join cat bj base n_left
  in
  let after_where =
    match join, where with
    | Some _, Some e ->
      Plan.Op_filter { pred = plan_expr e; child = after_join }
    | _ -> after_join
  in
  let is_aggregated = aggs <> [] || group_by <> None in
  (* For non-aggregate queries: sort BEFORE projection so col_idx correctly
     addresses the original table schema (pre-projection row layout).
     For aggregate queries: sort AFTER aggregation because ORDER BY refers
     to the aggregated output row layout. *)
  let sort_key = match order with [] -> None | k :: _ -> Some k in
  let make_sort child (bkey : Sema.bound_order_key) =
    let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
    Plan.Op_sort { key = plan_expr bkey.key; dir; child }
  in
  let after_sort =
    if is_aggregated then after_where
    else match sort_key with None -> after_where | Some k -> make_sort after_where k
  in
  let projected =
    if is_aggregated then
      Plan.Op_aggregate {
        child = after_sort;
        group_col = group_by;
        aggs = List.map sema_agg_to_plan aggs;
        having = Option.map plan_expr having;
        proj = List.map sema_agg_proj_to_plan agg_proj;
      }
    else if expr_proj <> [] then
      Plan.Op_expr_project {
        exprs = List.map plan_expr expr_proj;
        child = after_sort;
      }
    else
      Plan.Op_project { ordinals = proj; child = after_sort }
  in
  (* Post-aggregation sort (only for aggregated queries). *)
  let sorted =
    if is_aggregated then
      match sort_key with None -> projected | Some k -> make_sort projected k
    else projected
  in
  match limit with
  | None   -> sorted
  | Some n ->
    let off = Option.value ~default:0 offset in
    Plan.Op_limit { limit = n; offset = off; child = sorted }

let plan ?cat = function
  | Sema.BS_create_table { name; columns } ->
    Plan.Op_create_table { name; columns }
  | Sema.BS_insert { table_meta; ordinals; values } ->
    Plan.Op_insert { table_meta; ordinals; values = List.map plan_expr values }
  | Sema.BS_select { table_meta; proj; expr_proj; where; order; limit; offset;
                     join; group_by; aggs; having; agg_proj } ->
    (match cat with
     | Some cat ->
       plan_select cat ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset
         ~join ~group_by ~aggs ~having ~agg_proj
     | None ->
       (* Backwards-compatible path: no catalog → no index lookup, and
          (for JOIN) no index-based NLJ.  Build a hash-join + filter
          chain manually. *)
       let base = Plan.Op_seq_scan { table_meta } in
       let after_join : Plan.op = match join with
         | None -> base
         | Some bj ->
           let n_left = List.length table_meta.columns in
           let n_right_cols = List.length bj.Sema.right_meta.Cat.columns in
           let right_offset = bj.right_col_offset in
           let join_kind = match bj.kind with
             | Ast.Inner -> `Inner | Ast.Left -> `Left
           in
           (match recognise_eq_col_col bj.on with
            | Some (a, b) when (a < n_left) && (b >= right_offset) ->
              Plan.Op_hash_join {
                left = base;
                right = Plan.Op_seq_scan { table_meta = bj.right_meta };
                left_key = a; right_key = b - right_offset;
                join_kind; right_col_offset = right_offset; n_right_cols;
              }
            | Some (a, b) when (b < n_left) && (a >= right_offset) ->
              Plan.Op_hash_join {
                left = base;
                right = Plan.Op_seq_scan { table_meta = bj.right_meta };
                left_key = b; right_key = a - right_offset;
                join_kind; right_col_offset = right_offset; n_right_cols;
              }
            | _ ->
              let cart =
                Plan.Op_hash_join {
                  left = base;
                  right = Plan.Op_seq_scan { table_meta = bj.right_meta };
                  left_key = -1; right_key = -1;
                  join_kind; right_col_offset = right_offset; n_right_cols;
                }
              in
              Plan.Op_filter { pred = plan_expr bj.on; child = cart })
       in
       let filtered = match where with
         | None   -> after_join
         | Some e -> Plan.Op_filter { pred = plan_expr e; child = after_join }
       in
       let is_aggregated = aggs <> [] || group_by <> None in
       let sort_key = match order with [] -> None | k :: _ -> Some k in
       let make_sort child (bkey : Sema.bound_order_key) =
         let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
         Plan.Op_sort { key = plan_expr bkey.key; dir; child }
       in
       let after_sort =
         if is_aggregated then filtered
         else match sort_key with None -> filtered | Some k -> make_sort filtered k
       in
       let projected =
         if is_aggregated then
           Plan.Op_aggregate {
             child = after_sort;
             group_col = group_by;
             aggs = List.map sema_agg_to_plan aggs;
             having = Option.map plan_expr having;
             proj = List.map sema_agg_proj_to_plan agg_proj;
           }
         else if expr_proj <> [] then
           Plan.Op_expr_project {
             exprs = List.map plan_expr expr_proj;
             child = after_sort;
           }
         else
           Plan.Op_project { ordinals = proj; child = after_sort }
       in
       let sorted =
         if is_aggregated then
           match sort_key with None -> projected | Some k -> make_sort projected k
         else projected
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
  | Sema.BS_update { table_meta; assignments; where } ->
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
    }
  | Sema.BS_delete { table_meta; where } ->
    let indexes = match cat with
      | Some c -> Cat.indexes_for_table c ~table:table_meta.Cat.name
      | None   -> []
    in
    Plan.Op_delete {
      table_meta;
      where   = Option.map plan_expr where;
      indexes;
    }
  | Sema.BS_drop_table { table_meta; _ } ->
    let indexes = match cat with
      | Some c -> Cat.indexes_for_table c ~table:table_meta.Cat.name
      | None   -> []
    in
    Plan.Op_drop_table { table_meta; indexes }
  | Sema.BS_drop_index { idx_info; _ } ->
    Plan.Op_drop_index { idx_info }
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
