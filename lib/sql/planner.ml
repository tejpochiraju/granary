let rec plan_expr = function
  | Sema.BE_lit l     -> Plan.P_lit l
  | Sema.BE_col i     -> Plan.P_col i
  | Sema.BE_eq (a, b) -> Plan.P_eq (plan_expr a, plan_expr b)

let plan = function
  | Sema.BS_create_table { name; columns } ->
    Plan.Op_create_table { name; columns }
  | Sema.BS_insert { table_meta; ordinals; values } ->
    Plan.Op_insert { table_meta; ordinals; values }
  | Sema.BS_select { table_meta; proj; where; order; limit; offset } ->
    let scan = Plan.Op_seq_scan { table_meta } in
    let filtered = match where with
      | None   -> scan
      | Some e -> Plan.Op_filter { pred = plan_expr e; child = scan }
    in
    let projected = Plan.Op_project { ordinals = proj; child = filtered } in
    (* Wrap with Op_sort for each ORDER BY key (single column in Phase 1) *)
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
