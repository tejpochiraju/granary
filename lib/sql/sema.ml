open Lwt.Syntax
module Row = Sqlocaml_encoding.Row
module Cat = Sqlocaml_catalog.Catalog

let sqlite_master_meta : Cat.table_meta = {
  Cat.name        = "sqlite_master";
  Cat.tree_id     = -2;
  Cat.columns     = [
    { Row.name = "type";     Row.ty = Row.Text;    Row.not_null = false;
      Row.primary_key = false; Row.default = None;
      Row.check_sql = None; Row.generated_as = None };
    { Row.name = "name";     Row.ty = Row.Text;    Row.not_null = false;
      Row.primary_key = false; Row.default = None;
      Row.check_sql = None; Row.generated_as = None };
    { Row.name = "tbl_name"; Row.ty = Row.Text;    Row.not_null = false;
      Row.primary_key = false; Row.default = None;
      Row.check_sql = None; Row.generated_as = None };
    { Row.name = "rootpage"; Row.ty = Row.Integer; Row.not_null = false;
      Row.primary_key = false; Row.default = None;
      Row.check_sql = None; Row.generated_as = None };
    { Row.name = "sql";      Row.ty = Row.Text;    Row.not_null = false;
      Row.primary_key = false; Row.default = None;
      Row.check_sql = None; Row.generated_as = None };
  ];
  Cat.next_rowid  = 0L;
  Cat.fk_constraints = [];
}

type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or
           | Concat | Mod | Bit_and | Bit_or | Lshift | Rshift
           | Like | Glob

type bound_expr =
  | BE_lit         of Ast.literal
  | BE_col         of int
  | BE_binop       of binop * bound_expr * bound_expr
  | BE_not         of bound_expr
  | BE_is_null     of bound_expr
  | BE_is_not_null of bound_expr
  | BE_neg         of bound_expr
  | BE_bitnot      of bound_expr
  | BE_between     of bound_expr * bound_expr * bound_expr
  | BE_in          of bound_expr * bound_expr list
  | BE_func        of Ast.scalar_func * bound_expr list
  | BE_param       of int
  | BE_match       of Cat.fts_table_meta * Fts_query.fts_query
  | BE_subquery  of Ast.stmt
  | BE_exists    of Ast.stmt
  | BE_in_select of bound_expr * Ast.stmt
  | BE_case of {
      scrutinee : bound_expr option;
      branches  : (bound_expr * bound_expr) list;
      else_     : bound_expr option;
    }
  | BE_cast of bound_expr * Ast.ty
  | BE_excluded_col of int
    (** Reference to the i-th column of the proposed INSERT row (the 'excluded' pseudo-table). *)

  | BE_window_slot of int
    (** Reference to the i-th window function result appended after input columns by Op_window. *)
  | BE_collate of bound_expr * Ast.collation
    (** expr COLLATE collation_name *)

type bound_order_key = {
  key   : bound_expr;
  dir   : Ast.order_dir;
  nulls : [`Nulls_first | `Nulls_last] option;
}

type window_sema = {
  func         : Ast.window_func;
  args         : bound_expr list;
  partition_by : bound_expr list;
  order_by     : bound_order_key list;
  frame        : Ast.frame_spec option;
}

type agg_spec = {
  func    : Ast.agg_func;
  col_ord : int option;
}

type agg_proj_item =
  | AP_group_col of int     (** index into group_cols list *)
  | AP_agg_slot of int
  | AP_window_slot of int   (** post-aggregate window function result *)

(** A bound JOIN clause.  See sema.mli for layout details. *)
type bound_join = {
  kind             : Ast.join_kind;
  right_meta       : Cat.table_meta;
  on               : bound_expr;
  right_col_offset : int;
}

type bound_stmt =
  | BS_no_op
    (** Emitted by IF EXISTS DROP when the named object does not exist. *)
  | BS_create_table of {
      name           : string;
      columns        : Row.column list;
      uniq_idxs      : (string * string list) list;
      if_not_exists  : bool;
      fk_constraints : (string * string * string * Cat.fk_action * Cat.fk_action) list;
        (** [(local_col, parent_table, parent_col, on_delete, on_update)] *)
    }
  | BS_insert of {
      table_meta    : Cat.table_meta;
      ordinals      : int list;
      values        : bound_expr list list;   (* one sublist per VALUES row *)
      on_conflict   : Ast.conflict_action option;
      returning     : bound_expr list;
      upsert_update : (string list * (int * bound_expr) list) option;
    }
  | BS_insert_select of {
      table_meta  : Cat.table_meta;
      ordinals    : int list;
      source      : bound_stmt;
      on_conflict : Ast.conflict_action option;
    }
  | BS_select of {
      distinct   : bool;
      table_meta : Cat.table_meta;
      proj       : int list;
      expr_proj  : (bound_expr * string option) list;
        (** Non-empty when projection contains scalar functions (Phase 5)
            or aliased expressions (Phase 11).
            When non-empty, [proj] is empty and [expr_proj] governs the
            output columns. *)
      where      : bound_expr option;
      order      : bound_order_key list;
      limit      : int option;
      offset     : int option;
      joins      : bound_join list;
      group_by   : int list;
      aggs       : agg_spec list;
      having     : bound_expr option;
      agg_proj    : agg_proj_item list;
      windows     : window_sema list;
      agg_windows : window_sema list;
        (** Window functions computed AFTER aggregation, over aggregated output rows. *)
    }
  | BS_create_index of {
      name           : string;
      table_meta     : Cat.table_meta;
      col_sqls       : string list;          (* col name (plain) or expr SQL (expression) *)
      col_expr_flags : bool list;            (* true = expression index *)
      where_expr     : bound_expr option;
      where_ast      : Ast.expr option;
      unique         : bool;
      if_not_exists  : bool;
    }
  | BS_update of {
      table_meta  : Cat.table_meta;
      assignments : (int * bound_expr) list;
      where       : bound_expr option;
      order       : bound_order_key list;
      limit       : int option;
      offset      : int option;
      returning   : bound_expr list;
    }
  | BS_delete of {
      table_meta : Cat.table_meta;
      where      : bound_expr option;
      order      : bound_order_key list;
      limit      : int option;
      offset     : int option;
      returning  : bound_expr list;
    }
  | BS_drop_table of {
      name       : string;
      table_meta : Cat.table_meta;
    }
  | BS_drop_index of {
      name     : string;
      idx_info : Cat.index_info;
    }
  | BS_begin
  | BS_commit
  | BS_rollback
  | BS_savepoint   of string
  | BS_release     of string
  | BS_rollback_to of string
  | BS_create_fts_table of {
      name    : string;
      columns : string list;
    }
  | BS_fts_insert of {
      fts_meta   : Cat.fts_table_meta;
      col_names  : string list;
      col_values : bound_expr list;
    }
  | BS_fts_delete of {
      fts_meta : Cat.fts_table_meta;
      where    : bound_expr option;
    }
  | BS_fts_seq_scan of {
      fts_meta : Cat.fts_table_meta;
      where    : bound_expr option;
    }
  | BS_fts_match_scan of {
      fts_meta     : Cat.fts_table_meta;
      query        : Fts_query.fts_query;
      proj         : int list;
      include_rank : bool;
      snippets     : Plan.snippet_spec list;
    }
  | BS_pragma of {
      kind : Ast.pragma_kind;
    }
  | BS_alter_table of {
      table_meta : Cat.table_meta;
      action     : Ast.alter_action;
    }
  | BS_compound of {
      op    : Ast.set_op;
      left  : bound_stmt;
      right : bound_stmt;
    }
  | BS_const_select of {
      exprs : (bound_expr * string option) list;
    }
  | BS_with_cte of {
      name      : string;
      def       : bound_stmt;
      query     : bound_stmt;
      recursive : bool;
    }
  | BS_create_view of { name: string; query: Ast.stmt }
  | BS_drop_view   of { name: string }
  | BS_create_trigger of {
      name    : string;
      timing  : Ast.trigger_timing;
      event   : Ast.trigger_event;
      table   : string;
      when_   : Ast.expr option;
      body    : Ast.stmt list;
    }
  | BS_drop_trigger of { name : string }
  | BS_explain of {
      analyze : bool;
      inner   : bound_stmt;
    }

type error =
  | Unknown_table       of string
  | Unknown_column      of { table : string; column : string }
  | Ambiguous_column    of string
  | Type_mismatch       of { expected : Row.ty; got : Row.ty }
  | Arity_mismatch      of { expected : int; got : int }
  | Already_exists      of string
  | Invalid_limit       of string
  | Unsupported         of string
  | Not_null_violation  of string   (* column name *)
  | Unknown_index       of string   (* index name *)

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let col_index (cols : Row.column list) name =
  let rec go i = function
    | [] -> None
    | c :: _ when String.equal c.Row.name name -> Some i
    | _ :: rest -> go (i + 1) rest
  in
  go 0 cols

(** Convert an FTS table meta to a synthetic [Cat.table_meta] for use
    with [bind_expr] (column resolution in WHERE/VALUES expressions). *)
let fts_as_table_meta (m : Cat.fts_table_meta) : Cat.table_meta =
  let columns = List.map (fun name ->
    Row.{ name; ty = Row.Text; not_null = true;
          primary_key = false; default = None; check_sql = None;
          generated_as = None }
  ) m.Cat.fts_columns in
  { Cat.name           = m.Cat.fts_name;
    Cat.tree_id        = m.Cat.fts_content_tree;
    Cat.columns;
    Cat.next_rowid     = 0L;
    Cat.fk_constraints = [] }

let lit_ty = function
  | Ast.L_int _  -> Some Row.Integer
  | Ast.L_text _ -> Some Row.Text
  | Ast.L_null   -> None   (* NULL is compatible with any column *)
  | Ast.L_real _ -> Some Row.Real
  | Ast.L_blob _ -> Some Row.Blob
  | Ast.L_current_timestamp
  | Ast.L_current_date
  | Ast.L_current_time -> Some Row.Text  (* resolved to TEXT at runtime *)

let ty_equal (a : Row.ty) (b : Row.ty) = match a, b with
  | Row.Integer, Row.Integer -> true
  | Row.Text,    Row.Text    -> true
  | Row.Real,    Row.Real    -> true
  | Row.Blob,    Row.Blob    -> true
  | _,           _           -> false

let ast_binop_to_sema : Ast.binop -> binop = function
  | Ast.Eq  -> Eq  | Ast.Ne  -> Ne
  | Ast.Lt  -> Lt  | Ast.Le  -> Le
  | Ast.Gt  -> Gt  | Ast.Ge  -> Ge
  | Ast.Add -> Add | Ast.Sub -> Sub
  | Ast.Mul -> Mul | Ast.Div -> Div
  | Ast.And -> And | Ast.Or  -> Or
  | Ast.Concat  -> Concat
  | Ast.Mod     -> Mod
  | Ast.Bit_and -> Bit_and | Ast.Bit_or -> Bit_or
  | Ast.Lshift  -> Lshift  | Ast.Rshift -> Rshift
  | Ast.Like -> Like | Ast.Glob -> Glob

let resolve_param ~param_counter ~named_params = function
  | Ast.Param_anon ->
    let i = !param_counter in
    incr param_counter;
    i
  | Ast.Param_index n ->
    let i = n - 1 in          (* convert 1-indexed to 0-indexed *)
    if !param_counter <= i then param_counter := i + 1;
    i
  | Ast.Param_name name ->
    (match Hashtbl.find_opt named_params name with
     | Some i -> i
     | None   ->
       let i = !param_counter in
       incr param_counter;
       Hashtbl.add named_params name i;
       i)

let rec bind_expr ~param_counter ~named_params (meta : Cat.table_meta) = function
  | Ast.E_lit l -> Ok (BE_lit l)
  | Ast.E_col name ->
    (match col_index meta.columns name with
     | None   -> Error (Unknown_column { table = meta.name; column = name })
     | Some i -> Ok (BE_col i))
  | Ast.E_tbl_col (_tbl, name) ->
    (* Phase 2 Task 1: single-table queries — ignore table qualifier.
       Multi-table resolution arrives with JOIN support. *)
    (match col_index meta.columns name with
     | None   -> Error (Unknown_column { table = meta.name; column = name })
     | Some i -> Ok (BE_col i))
  | Ast.E_binop (op, a, b) ->
    (match bind_expr ~param_counter ~named_params meta a,
           bind_expr ~param_counter ~named_params meta b with
     | Ok ba, Ok bb  -> Ok (BE_binop (ast_binop_to_sema op, ba, bb))
     | Error e, _    -> Error e
     | Ok _,  Error e -> Error e)
  | Ast.E_not e ->
    (match bind_expr ~param_counter ~named_params meta e with
     | Ok be   -> Ok (BE_not be)
     | Error e -> Error e)
  | Ast.E_is_null e ->
    (match bind_expr ~param_counter ~named_params meta e with
     | Ok be   -> Ok (BE_is_null be)
     | Error e -> Error e)
  | Ast.E_is_not_null e ->
    (match bind_expr ~param_counter ~named_params meta e with
     | Ok be   -> Ok (BE_is_not_null be)
     | Error e -> Error e)
  | Ast.E_neg e ->
    (match bind_expr ~param_counter ~named_params meta e with
     | Ok be   -> Ok (BE_neg be)
     | Error e -> Error e)
  | Ast.E_bitnot e ->
    (match bind_expr ~param_counter ~named_params meta e with
     | Ok be   -> Ok (BE_bitnot be)
     | Error e -> Error e)
  | Ast.E_between (x, lo, hi) ->
    (match bind_expr ~param_counter ~named_params meta x,
           bind_expr ~param_counter ~named_params meta lo,
           bind_expr ~param_counter ~named_params meta hi with
     | Ok bx, Ok blo, Ok bhi -> Ok (BE_between (bx, blo, bhi))
     | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | Ast.E_in (x, vals) ->
    let bx = bind_expr ~param_counter ~named_params meta x in
    let bvals = List.map (bind_expr ~param_counter ~named_params meta) vals in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bvals in
    (match bx, errors with
     | Error e, _ -> Error e
     | _, e :: _  -> Error e
     | Ok bx', [] ->
       let ok_vals = List.filter_map (function Ok v -> Some v | Error _ -> None) bvals in
       Ok (BE_in (bx', ok_vals)))
  | Ast.E_param p ->
    Ok (BE_param (resolve_param ~param_counter ~named_params p))
  | Ast.E_agg _ ->
    Error (Unsupported "aggregate in WHERE")
  | Ast.E_func (func, args) ->
    let bound = List.map (bind_expr ~param_counter ~named_params meta) args in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bound in
    (match errors with
     | e :: _ -> Error e
     | [] ->
       let ok_args = List.filter_map (function Ok e -> Some e | Error _ -> None) bound in
       let n = List.length ok_args in
       let arity_ok = match func with
         | Ast.Fn_length | Ast.Fn_lower | Ast.Fn_upper
         | Ast.Fn_abs    | Ast.Fn_typeof -> n = 1
         | Ast.Fn_ifnull | Ast.Fn_instr -> n = 2
         | Ast.Fn_coalesce -> n >= 1
         | Ast.Fn_substr -> n = 2 || n = 3
         | Ast.Fn_trim | Ast.Fn_ltrim | Ast.Fn_rtrim -> n = 1 || n = 2
         | Ast.Fn_replace -> n = 3
         | Ast.Fn_round -> n = 1 || n = 2
         | Ast.Fn_date | Ast.Fn_time | Ast.Fn_datetime
         | Ast.Fn_julianday | Ast.Fn_unixepoch -> n >= 1
         | Ast.Fn_strftime -> n >= 2
         | Ast.Fn_ceil | Ast.Fn_floor | Ast.Fn_sqrt | Ast.Fn_exp
         | Ast.Fn_ln | Ast.Fn_sign | Ast.Fn_sin | Ast.Fn_cos | Ast.Fn_tan
         | Ast.Fn_asin | Ast.Fn_acos | Ast.Fn_atan
         | Ast.Fn_degrees | Ast.Fn_radians | Ast.Fn_log2 | Ast.Fn_log10 -> n = 1
         | Ast.Fn_pow | Ast.Fn_atan2 -> n = 2
         | Ast.Fn_log -> n = 1 || n = 2
         | Ast.Fn_trunc -> n = 1 || n = 2
         | Ast.Fn_pi -> n = 0
         | Ast.Fn_json_extract -> n = 2
         | Ast.Fn_json_object -> n mod 2 = 0
         | Ast.Fn_json_array -> true
         | Ast.Fn_json_type -> n = 1 || n = 2
         | Ast.Fn_json_valid -> n = 1
         | Ast.Fn_json_set | Ast.Fn_json_insert | Ast.Fn_json_replace -> n >= 3 && (n - 1) mod 2 = 0
         | Ast.Fn_json_remove -> n >= 2
       in
       if not arity_ok then
         Error (Arity_mismatch { expected = (match func with Ast.Fn_ifnull -> 2 | _ -> 1); got = n })
       else
         Ok (BE_func (func, ok_args)))
  | Ast.E_match _ ->
    Error (Unsupported "MATCH is only valid as a top-level WHERE clause on FTS tables")
  | Ast.E_subquery inner ->
    Ok (BE_subquery inner)
  | Ast.E_exists inner ->
    Ok (BE_exists inner)
  | Ast.E_in_select (x, inner) ->
    (match bind_expr ~param_counter ~named_params meta x with
     | Error e -> Error e
     | Ok bx   -> Ok (BE_in_select (bx, inner)))
  | Ast.E_case { scrutinee; branches; else_ } ->
    let scrutinee_result =
      match scrutinee with
      | None   -> Ok None
      | Some e ->
        (match bind_expr ~param_counter ~named_params meta e with
         | Ok be   -> Ok (Some be)
         | Error e -> Error e)
    in
    (match scrutinee_result with
     | Error e -> Error e
     | Ok bound_scr ->
       let branch_results =
         List.map (fun (cond, res) ->
           match bind_expr ~param_counter ~named_params meta cond,
                 bind_expr ~param_counter ~named_params meta res with
           | Ok bc, Ok br -> Ok (bc, br)
           | Error e, _   -> Error e
           | _, Error e   -> Error e
         ) branches
       in
       let branch_errors = List.filter_map
         (function Error e -> Some e | Ok _ -> None) branch_results in
       (match branch_errors with
        | e :: _ -> Error e
        | [] ->
          let bound_branches =
            List.filter_map (function Ok p -> Some p | Error _ -> None) branch_results
          in
          let else_result =
            match else_ with
            | None   -> Ok None
            | Some e ->
              (match bind_expr ~param_counter ~named_params meta e with
               | Ok be   -> Ok (Some be)
               | Error e -> Error e)
          in
          (match else_result with
           | Error e -> Error e
           | Ok bound_else ->
             Ok (BE_case { scrutinee = bound_scr; branches = bound_branches; else_ = bound_else }))))
  | Ast.E_cast (e, ty) ->
    (match bind_expr ~param_counter ~named_params meta e with
     | Ok be   -> Ok (BE_cast (be, ty))
     | Error e -> Error e)
  | Ast.E_collate (e, c) ->
    (match bind_expr ~param_counter ~named_params meta e with
     | Ok be   -> Ok (BE_collate (be, c))
     | Error e -> Error e)
  | Ast.E_window _ ->
    Error (Unsupported "window functions not yet supported in single-table context")
  | Ast.E_fts_snippet _ ->
    Error (Unsupported "snippet() is only supported in FTS SELECT projection")

(* ------------------------------------------------------------------ *)
(* Two-table column resolution used when a JOIN is present.            *)
(* The combined row layout is [left_cols ... right_cols].              *)
(* Right table ordinal i becomes absolute ordinal [right_offset + i].  *)
(* ------------------------------------------------------------------ *)

let rec bind_expr_join
    ~param_counter
    ~named_params
    ~(tables : (Cat.table_meta * int * string option) list)
  = function
  | Ast.E_lit l -> Ok (BE_lit l)
  | Ast.E_col name ->
    let matches = List.filter_map (fun (tm, base, _alias) ->
      match col_index tm.Cat.columns name with
      | Some i -> Some (BE_col (base + i))
      | None   -> None
    ) tables in
    (match matches with
     | [be]   -> Ok be
     | []     ->
       let (tm0, _, _) = List.hd tables in
       Error (Unknown_column { table = tm0.Cat.name; column = name })
     | _ :: _ -> Error (Ambiguous_column name))
  | Ast.E_tbl_col (tbl, name) ->
    (match List.find_opt (fun (tm, _, alias_opt) ->
       String.equal tm.Cat.name tbl ||
       (match alias_opt with Some a -> String.equal tbl a | None -> false)
     ) tables with
     | None -> Error (Unknown_table tbl)
     | Some (tm, base, _) ->
       (match col_index tm.Cat.columns name with
        | Some i -> Ok (BE_col (base + i))
        | None   -> Error (Unknown_column { table = tbl; column = name })))
  | Ast.E_binop (op, a, b) ->
    (match bind_expr_join ~param_counter ~named_params ~tables a,
           bind_expr_join ~param_counter ~named_params ~tables b with
     | Ok ba, Ok bb  -> Ok (BE_binop (ast_binop_to_sema op, ba, bb))
     | Error e, _    -> Error e
     | Ok _,  Error e -> Error e)
  | Ast.E_not e ->
    (match bind_expr_join ~param_counter ~named_params ~tables e with
     | Ok be -> Ok (BE_not be) | Error e -> Error e)
  | Ast.E_is_null e ->
    (match bind_expr_join ~param_counter ~named_params ~tables e with
     | Ok be -> Ok (BE_is_null be) | Error e -> Error e)
  | Ast.E_is_not_null e ->
    (match bind_expr_join ~param_counter ~named_params ~tables e with
     | Ok be -> Ok (BE_is_not_null be) | Error e -> Error e)
  | Ast.E_neg e ->
    (match bind_expr_join ~param_counter ~named_params ~tables e with
     | Ok be -> Ok (BE_neg be) | Error e -> Error e)
  | Ast.E_bitnot e ->
    (match bind_expr_join ~param_counter ~named_params ~tables e with
     | Ok be -> Ok (BE_bitnot be) | Error e -> Error e)
  | Ast.E_between (x, lo, hi) ->
    (match bind_expr_join ~param_counter ~named_params ~tables x,
           bind_expr_join ~param_counter ~named_params ~tables lo,
           bind_expr_join ~param_counter ~named_params ~tables hi with
     | Ok bx, Ok blo, Ok bhi -> Ok (BE_between (bx, blo, bhi))
     | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | Ast.E_in (x, vals) ->
    let bx = bind_expr_join ~param_counter ~named_params ~tables x in
    let bvals = List.map (bind_expr_join ~param_counter ~named_params ~tables) vals in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bvals in
    (match bx, errors with
     | Error e, _ -> Error e
     | _, e :: _  -> Error e
     | Ok bx', [] ->
       let ok_vals = List.filter_map (function Ok v -> Some v | Error _ -> None) bvals in
       Ok (BE_in (bx', ok_vals)))
  | Ast.E_param p ->
    Ok (BE_param (resolve_param ~param_counter ~named_params p))
  | Ast.E_agg _ ->
    Error (Unsupported "aggregate in WHERE")
  | Ast.E_func (func, args) ->
    let bound = List.map (bind_expr_join ~param_counter ~named_params ~tables) args in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bound in
    (match errors with
     | e :: _ -> Error e
     | [] ->
       let ok_args = List.filter_map (function Ok e -> Some e | Error _ -> None) bound in
       let n = List.length ok_args in
       let arity_ok = match func with
         | Ast.Fn_length | Ast.Fn_lower | Ast.Fn_upper
         | Ast.Fn_abs    | Ast.Fn_typeof -> n = 1
         | Ast.Fn_ifnull | Ast.Fn_instr -> n = 2
         | Ast.Fn_coalesce -> n >= 1
         | Ast.Fn_substr -> n = 2 || n = 3
         | Ast.Fn_trim | Ast.Fn_ltrim | Ast.Fn_rtrim -> n = 1 || n = 2
         | Ast.Fn_replace -> n = 3
         | Ast.Fn_round -> n = 1 || n = 2
         | Ast.Fn_date | Ast.Fn_time | Ast.Fn_datetime
         | Ast.Fn_julianday | Ast.Fn_unixepoch -> n >= 1
         | Ast.Fn_strftime -> n >= 2
         | Ast.Fn_ceil | Ast.Fn_floor | Ast.Fn_sqrt | Ast.Fn_exp
         | Ast.Fn_ln | Ast.Fn_sign | Ast.Fn_sin | Ast.Fn_cos | Ast.Fn_tan
         | Ast.Fn_asin | Ast.Fn_acos | Ast.Fn_atan
         | Ast.Fn_degrees | Ast.Fn_radians | Ast.Fn_log2 | Ast.Fn_log10 -> n = 1
         | Ast.Fn_pow | Ast.Fn_atan2 -> n = 2
         | Ast.Fn_log -> n = 1 || n = 2
         | Ast.Fn_trunc -> n = 1 || n = 2
         | Ast.Fn_pi -> n = 0
         | Ast.Fn_json_extract -> n = 2
         | Ast.Fn_json_object -> n mod 2 = 0
         | Ast.Fn_json_array -> true
         | Ast.Fn_json_type -> n = 1 || n = 2
         | Ast.Fn_json_valid -> n = 1
         | Ast.Fn_json_set | Ast.Fn_json_insert | Ast.Fn_json_replace -> n >= 3 && (n - 1) mod 2 = 0
         | Ast.Fn_json_remove -> n >= 2
       in
       if not arity_ok then
         Error (Arity_mismatch { expected = (match func with Ast.Fn_ifnull -> 2 | _ -> 1); got = n })
       else
         Ok (BE_func (func, ok_args)))
  | Ast.E_match _ ->
    Error (Unsupported "MATCH in JOIN context")
  | Ast.E_subquery _ | Ast.E_exists _ | Ast.E_in_select _ ->
    Error (Unsupported "subqueries are not supported in JOIN ON conditions")
  | Ast.E_case { scrutinee; branches; else_ } ->
    let scrutinee_result =
      match scrutinee with
      | None   -> Ok None
      | Some e ->
        (match bind_expr_join ~param_counter ~named_params ~tables e with
         | Ok be   -> Ok (Some be)
         | Error e -> Error e)
    in
    (match scrutinee_result with
     | Error e -> Error e
     | Ok bound_scr ->
       let branch_results =
         List.map (fun (cond, res) ->
           match bind_expr_join ~param_counter ~named_params ~tables cond,
                 bind_expr_join ~param_counter ~named_params ~tables res with
           | Ok bc, Ok br -> Ok (bc, br)
           | Error e, _   -> Error e
           | _, Error e   -> Error e
         ) branches
       in
       let branch_errors = List.filter_map
         (function Error e -> Some e | Ok _ -> None) branch_results in
       (match branch_errors with
        | e :: _ -> Error e
        | [] ->
          let bound_branches =
            List.filter_map (function Ok p -> Some p | Error _ -> None) branch_results
          in
          let else_result =
            match else_ with
            | None   -> Ok None
            | Some e ->
              (match bind_expr_join ~param_counter ~named_params ~tables e with
               | Ok be   -> Ok (Some be)
               | Error e -> Error e)
          in
          (match else_result with
           | Error e -> Error e
           | Ok bound_else ->
             Ok (BE_case { scrutinee = bound_scr; branches = bound_branches; else_ = bound_else }))))
  | Ast.E_cast (e, ty) ->
    (match bind_expr_join ~param_counter ~named_params ~tables e with
     | Ok be   -> Ok (BE_cast (be, ty))
     | Error e -> Error e)
  | Ast.E_collate (e, c) ->
    (match bind_expr_join ~param_counter ~named_params ~tables e with
     | Ok be   -> Ok (BE_collate (be, c))
     | Error e -> Error e)
  | Ast.E_window _ ->
    Error (Unsupported "window functions not yet supported in join context")
  | Ast.E_fts_snippet _ ->
    Error (Unsupported "snippet() is only supported in FTS SELECT projection")

(* ------------------------------------------------------------------ *)
(* Aggregate-aware binding.                                             *)
(* Walks an [expr], using a name-resolver for plain column references,  *)
(* and collects aggregate calls into an accumulator.  Aggregates in the *)
(* returned [bound_expr] become [BE_col k] where [k] is the column      *)
(* ordinal in the AGGREGATE OUTPUT ROW (NOT the input row), with the    *)
(* offset [group_col_present_offset] applied.                            *)
(*                                                                       *)
(* Returns (bound_expr, list_of_agg_specs_in_traversal_order).           *)
(* ------------------------------------------------------------------ *)

(** Resolve a column reference into a [BE_col i] using a resolver
    function.  Used by [bind_expr_agg]. *)
type col_resolver = {
  resolve_unqual  : string -> (int, error) result;
  resolve_qual    : string -> string -> (int, error) result;
  (* Resolver used for column refs inside aggregate-function arguments —
     may differ from resolve_unqual in HAVING contexts. *)
  resolve_agg_arg       : string -> (int, error) result;
  resolve_agg_arg_qual  : string -> string -> (int, error) result;
}

(** Bind expression, collecting aggregates.  Aggregates become
    [BE_col (offset + slot)] referring to the aggregate output row.
    [offset] is 1 when GROUP BY is present (slot 0 holds the group key)
    and 0 otherwise. *)
let bind_expr_agg
    ~param_counter
    ~named_params
    ~(resolver : col_resolver)
    ~(offset : int)
    (e : Ast.expr)
  : (bound_expr * agg_spec list, error) result =
  let aggs = ref [] in
  let add_agg spec =
    let idx = List.length !aggs in
    aggs := !aggs @ [spec];
    idx
  in
  let rec go = function
    | Ast.E_lit l -> Ok (BE_lit l)
    | Ast.E_col name ->
      (match resolver.resolve_unqual name with
       | Error e -> Error e
       | Ok i -> Ok (BE_col i))
    | Ast.E_tbl_col (t, c) ->
      (match resolver.resolve_qual t c with
       | Error e -> Error e
       | Ok i -> Ok (BE_col i))
    | Ast.E_binop (op, a, b) ->
      (match go a, go b with
       | Ok ba, Ok bb  -> Ok (BE_binop (ast_binop_to_sema op, ba, bb))
       | Error e, _    -> Error e
       | Ok _,  Error e -> Error e)
    | Ast.E_not e ->
      (match go e with Ok be -> Ok (BE_not be) | Error e -> Error e)
    | Ast.E_is_null e ->
      (match go e with Ok be -> Ok (BE_is_null be) | Error e -> Error e)
    | Ast.E_is_not_null e ->
      (match go e with Ok be -> Ok (BE_is_not_null be) | Error e -> Error e)
    | Ast.E_neg e ->
      (match go e with Ok be -> Ok (BE_neg be) | Error e -> Error e)
    | Ast.E_bitnot e ->
      (match go e with Ok be -> Ok (BE_bitnot be) | Error e -> Error e)
    | Ast.E_between (x, lo, hi) ->
      (match go x, go lo, go hi with
       | Ok bx, Ok blo, Ok bhi -> Ok (BE_between (bx, blo, bhi))
       | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
    | Ast.E_in (x, vals) ->
      let bx = go x in
      let bvals = List.map go vals in
      let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bvals in
      (match bx, errors with
       | Error e, _ -> Error e
       | _, e :: _  -> Error e
       | Ok bx', [] ->
         let ok_vals = List.filter_map (function Ok v -> Some v | Error _ -> None) bvals in
         Ok (BE_in (bx', ok_vals)))
    | Ast.E_param p ->
      Ok (BE_param (resolve_param ~param_counter ~named_params p))
    | Ast.E_agg (func, arg_opt) ->
      let col_ord_result : (int option, error) result =
        match arg_opt with
        | None ->
          (* COUNT-star — only legal here for Agg_count *)
          (match func with
           | Ast.Agg_count -> Ok None
           | _ -> Error (Unsupported "non-COUNT aggregate requires an argument"))
        | Some (Ast.E_col name) ->
          (match resolver.resolve_agg_arg name with
           | Error e -> Error e
           | Ok i    -> Ok (Some i))
        | Some (Ast.E_tbl_col (t, c)) ->
          (match resolver.resolve_agg_arg_qual t c with
           | Error e -> Error e
           | Ok i    -> Ok (Some i))
        | Some _ ->
          Error (Unsupported "aggregate argument must be a column reference")
      in
      (match col_ord_result with
       | Error e -> Error e
       | Ok col_ord ->
         let slot = add_agg { func; col_ord } in
         Ok (BE_col (offset + slot)))
    | Ast.E_func (func, args) ->
      let bound = List.map go args in
      let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bound in
      (match errors with
       | e :: _ -> Error e
       | [] ->
         let ok_args = List.filter_map (function Ok e -> Some e | Error _ -> None) bound in
         let n = List.length ok_args in
         let arity_ok = match func with
           | Ast.Fn_length | Ast.Fn_lower | Ast.Fn_upper
           | Ast.Fn_abs    | Ast.Fn_typeof -> n = 1
           | Ast.Fn_ifnull | Ast.Fn_instr -> n = 2
           | Ast.Fn_coalesce -> n >= 1
           | Ast.Fn_substr -> n = 2 || n = 3
           | Ast.Fn_trim | Ast.Fn_ltrim | Ast.Fn_rtrim -> n = 1 || n = 2
           | Ast.Fn_replace -> n = 3
           | Ast.Fn_round -> n = 1 || n = 2
           | Ast.Fn_date | Ast.Fn_time | Ast.Fn_datetime
           | Ast.Fn_julianday | Ast.Fn_unixepoch -> n >= 1
           | Ast.Fn_strftime -> n >= 2
           | Ast.Fn_ceil | Ast.Fn_floor | Ast.Fn_sqrt | Ast.Fn_exp
           | Ast.Fn_ln | Ast.Fn_sign | Ast.Fn_sin | Ast.Fn_cos | Ast.Fn_tan
           | Ast.Fn_asin | Ast.Fn_acos | Ast.Fn_atan
           | Ast.Fn_degrees | Ast.Fn_radians | Ast.Fn_log2 | Ast.Fn_log10 -> n = 1
           | Ast.Fn_pow | Ast.Fn_atan2 -> n = 2
           | Ast.Fn_log -> n = 1 || n = 2
           | Ast.Fn_trunc -> n = 1 || n = 2
           | Ast.Fn_pi -> n = 0
           | Ast.Fn_json_extract -> n = 2
           | Ast.Fn_json_object -> n mod 2 = 0
           | Ast.Fn_json_array -> true
           | Ast.Fn_json_type -> n = 1 || n = 2
           | Ast.Fn_json_valid -> n = 1
           | Ast.Fn_json_set | Ast.Fn_json_insert | Ast.Fn_json_replace -> n >= 3 && (n - 1) mod 2 = 0
           | Ast.Fn_json_remove -> n >= 2
         in
         if not arity_ok then
           Error (Arity_mismatch { expected = (match func with Ast.Fn_ifnull -> 2 | _ -> 1); got = n })
         else
           Ok (BE_func (func, ok_args)))
    | Ast.E_match _ ->
      Error (Unsupported "MATCH is only valid as a top-level WHERE clause on FTS tables")
    | Ast.E_subquery _ | Ast.E_exists _ | Ast.E_in_select _ ->
      Error (Unsupported "subqueries are not supported in aggregate expressions")
    | Ast.E_case { scrutinee; branches; else_ } ->
      let scrutinee_result =
        match scrutinee with
        | None   -> Ok None
        | Some e -> (match go e with Ok be -> Ok (Some be) | Error e -> Error e)
      in
      (match scrutinee_result with
       | Error e -> Error e
       | Ok bound_scr ->
         let branch_results =
           List.map (fun (cond, res) ->
             match go cond, go res with
             | Ok bc, Ok br -> Ok (bc, br)
             | Error e, _   -> Error e
             | _, Error e   -> Error e
           ) branches
         in
         let branch_errors = List.filter_map
           (function Error e -> Some e | Ok _ -> None) branch_results in
         (match branch_errors with
          | e :: _ -> Error e
          | [] ->
            let bound_branches =
              List.filter_map (function Ok p -> Some p | Error _ -> None) branch_results
            in
            let else_result =
              match else_ with
              | None   -> Ok None
              | Some e -> (match go e with Ok be -> Ok (Some be) | Error e -> Error e)
            in
            (match else_result with
             | Error e -> Error e
             | Ok bound_else ->
               Ok (BE_case { scrutinee = bound_scr; branches = bound_branches; else_ = bound_else }))))
    | Ast.E_cast (e, ty) ->
      (match go e with Ok be -> Ok (BE_cast (be, ty)) | Error e -> Error e)
    | Ast.E_collate (e, c) ->
      (match go e with Ok be -> Ok (BE_collate (be, c)) | Error e -> Error e)
    | Ast.E_window _ ->
      Error (Unsupported "window functions not yet supported in aggregate context")
    | Ast.E_fts_snippet _ ->
      Error (Unsupported "snippet() is only supported in FTS SELECT projection")
  in
  match go e with
  | Error e -> Error e
  | Ok be   -> Ok (be, !aggs)

(** Check if any subquery node appears anywhere in a [bound_expr]. *)
let rec expr_has_subquery = function
  | BE_subquery _ | BE_exists _ -> true
  | BE_in_select _ -> true
  | BE_binop (_, a, b) -> expr_has_subquery a || expr_has_subquery b
  | BE_not e | BE_is_null e | BE_is_not_null e | BE_neg e | BE_bitnot e ->
    expr_has_subquery e
  | BE_between (x, lo, hi) ->
    expr_has_subquery x || expr_has_subquery lo || expr_has_subquery hi
  | BE_in (x, vals) ->
    expr_has_subquery x || List.exists expr_has_subquery vals
  | BE_func (_, args) -> List.exists expr_has_subquery args
  | BE_lit _ | BE_col _ | BE_param _ | BE_match _ -> false
  | BE_case { scrutinee; branches; else_ } ->
    (match scrutinee with Some e -> expr_has_subquery e | None -> false)
    || List.exists (fun (c, r) -> expr_has_subquery c || expr_has_subquery r) branches
    || (match else_ with Some e -> expr_has_subquery e | None -> false)
  | BE_cast (e, _) -> expr_has_subquery e
  | BE_collate (e, _) -> expr_has_subquery e
  | BE_excluded_col _ -> false
  | BE_window_slot _ -> false

(** Check if any [E_agg] appears anywhere in an [expr]. *)
let rec expr_has_agg = function
  | Ast.E_agg _ -> true
  | Ast.E_lit _ | Ast.E_col _ | Ast.E_tbl_col _ | Ast.E_param _ | Ast.E_match _ -> false
  | Ast.E_subquery _ | Ast.E_exists _ -> false
  | Ast.E_in_select (x, _) -> expr_has_agg x
  | Ast.E_binop (_, a, b) -> expr_has_agg a || expr_has_agg b
  | Ast.E_not e | Ast.E_is_null e | Ast.E_is_not_null e | Ast.E_neg e | Ast.E_bitnot e ->
    expr_has_agg e
  | Ast.E_between (x, lo, hi) -> expr_has_agg x || expr_has_agg lo || expr_has_agg hi
  | Ast.E_in (x, vals) -> expr_has_agg x || List.exists expr_has_agg vals
  | Ast.E_func (_, args) -> List.exists expr_has_agg args
  | Ast.E_case { scrutinee; branches; else_ } ->
    (match scrutinee with Some e -> expr_has_agg e | None -> false)
    || List.exists (fun (c, r) -> expr_has_agg c || expr_has_agg r) branches
    || (match else_ with Some e -> expr_has_agg e | None -> false)
  | Ast.E_cast (e, _) -> expr_has_agg e
  | Ast.E_collate (e, _) -> expr_has_agg e
  | Ast.E_window _ -> false
  | Ast.E_fts_snippet _ -> false

let rec expr_has_window = function
  | Ast.E_window _ -> true
  | Ast.E_binop (_, a, b) -> expr_has_window a || expr_has_window b
  | Ast.E_not e | Ast.E_neg e | Ast.E_is_null e | Ast.E_is_not_null e
  | Ast.E_bitnot e -> expr_has_window e
  | Ast.E_between (x, lo, hi) ->
    expr_has_window x || expr_has_window lo || expr_has_window hi
  | Ast.E_in (x, vals) -> expr_has_window x || List.exists expr_has_window vals
  | Ast.E_func (_, args) -> List.exists expr_has_window args
  | Ast.E_case { scrutinee; branches; else_ } ->
    (match scrutinee with Some e -> expr_has_window e | None -> false)
    || List.exists (fun (c, r) -> expr_has_window c || expr_has_window r) branches
    || (match else_ with Some e -> expr_has_window e | None -> false)
  | Ast.E_cast (e, _) -> expr_has_window e
  | Ast.E_collate (e, _) -> expr_has_window e
  | _ -> false

(* ------------------------------------------------------------------ *)
(* CREATE TABLE                                                         *)
(* ------------------------------------------------------------------ *)

let bind_create cat ~name ~columns ~constraints ~if_not_exists =
  let* existing = Cat.find_table cat ~name in
  match existing with
  | Some _ when not if_not_exists -> Lwt.return (Error (Already_exists name))
  | Some _ (* if_not_exists = true: silently succeed *) ->
    Lwt.return (Ok (BS_create_table { name; columns = []; uniq_idxs = []; if_not_exists = true; fk_constraints = [] }))
  | None ->
    (* Validate CHECK expressions — reject forms that can't be serialized *)
    let rec check_expr_unsupported = function
      | Ast.E_agg _
      | Ast.E_match _
      | Ast.E_subquery _
      | Ast.E_exists _
      | Ast.E_in_select _
      | Ast.E_param _   -> true
      | Ast.E_binop (_, a, b) -> check_expr_unsupported a || check_expr_unsupported b
      | Ast.E_not e | Ast.E_is_null e | Ast.E_is_not_null e
      | Ast.E_neg e | Ast.E_bitnot e -> check_expr_unsupported e
      | Ast.E_between (x, lo, hi) ->
        check_expr_unsupported x || check_expr_unsupported lo || check_expr_unsupported hi
      | Ast.E_in (x, vals) ->
        check_expr_unsupported x || List.exists check_expr_unsupported vals
      | Ast.E_func (_, args) -> List.exists check_expr_unsupported args
      | Ast.E_lit _ | Ast.E_col _ | Ast.E_tbl_col _ -> false
      | Ast.E_case { scrutinee; branches; else_ } ->
        (match scrutinee with Some e -> check_expr_unsupported e | None -> false)
        || List.exists (fun (c, r) -> check_expr_unsupported c || check_expr_unsupported r) branches
        || (match else_ with Some e -> check_expr_unsupported e | None -> false)
      | Ast.E_cast _ -> false
      | Ast.E_collate (e, _) -> check_expr_unsupported e
      | Ast.E_window _ -> true
      | Ast.E_fts_snippet _ -> true
    in
    let unsupported_check = List.find_opt (fun (c : Ast.column_def) ->
      match c.check with
      | None -> false
      | Some e -> check_expr_unsupported e
    ) columns in
    match unsupported_check with
    | Some col ->
      Lwt.return (Error (Unsupported
        (Printf.sprintf "CHECK constraint on column '%s' contains unsupported expression form (aggregates, subqueries, and parameters are not allowed)" col.name)))
    | None ->
      let ast_lit_to_dv : Ast.literal -> Row.default_value = function
        | Ast.L_int  n -> Row.DV_int n
        | Ast.L_text s -> Row.DV_text s
        | Ast.L_null   -> Row.DV_null
        | Ast.L_real f -> Row.DV_real f
        | Ast.L_blob b -> Row.DV_blob b
        | Ast.L_current_timestamp -> Row.DV_current_timestamp
        | Ast.L_current_date      -> Row.DV_current_date
        | Ast.L_current_time      -> Row.DV_current_time
      in
      let row_cols = List.map (fun (c : Ast.column_def) ->
        Row.{ name        = c.name;
              ty          = (match c.ty with
                             | Ast.Ty_int  -> Row.Integer
                             | Ast.Ty_text -> Row.Text
                             | Ast.Ty_real -> Row.Real
                             | Ast.Ty_blob -> Row.Blob);
              not_null    = c.not_null || c.primary_key;
              primary_key = c.primary_key;
              default     = Option.map ast_lit_to_dv c.default;
              check_sql   = Option.map Ast.expr_to_sql c.check;
              generated_as = Option.map (fun (e, s) ->
                (Ast.expr_to_sql e, s = `Stored)) c.generated_as }
      ) columns in
      (* Generate auto-UNIQUE index specs for table-level constraints *)
      let tbl_uniq_idxs = List.filter_map (fun (i, tc) ->
        match tc with
        | Ast.TC_unique cols ->
          let idx_name = Printf.sprintf "__uniq_%s_%s_%d"
              name (String.concat "_" cols) i in
          Some (idx_name, cols)
        | Ast.TC_primary_key cols ->
          let idx_name = Printf.sprintf "__pk_%s_%s_%d"
              name (String.concat "_" cols) i in
          Some (idx_name, cols)
        | Ast.TC_foreign_key _ -> None
      ) (List.mapi (fun i tc -> (i, tc)) constraints) in
      (* Column-level PRIMARY KEY: also emit a unique index *)
      let col_pk_idxs = List.filter_map (fun (c : Ast.column_def) ->
        if c.primary_key then
          let idx_name = Printf.sprintf "__pk_%s_%s" name c.name in
          Some (idx_name, [c.name])
        else None
      ) columns in
      let uniq_idxs = tbl_uniq_idxs @ col_pk_idxs in
      (* Mark columns that are declared primary key via table-level PRIMARY KEY (col) *)
      let row_cols =
        List.fold_left (fun cols c ->
          match c with
          | Ast.TC_primary_key [pk_col] ->
            List.map (fun (col : Row.column) ->
              if String.equal col.name pk_col then { col with primary_key = true }
              else col
            ) cols
          | _ -> cols
        ) row_cols constraints
      in
      (* Extract FK constraints from column-level REFERENCES.
         When parent_col is empty, infer from the parent table's PRIMARY KEY. *)
      let col_fks_result =
        List.fold_left (fun acc (cd : Ast.column_def) ->
          match acc with
          | Error _ as e -> e
          | Ok fks ->
            (match cd.Ast.fk_ref with
             | None -> Ok fks
             | Some (parent_table, "", od, ou) ->
               (match Cat.find_table_cached cat ~name:parent_table with
                | None ->
                  Error (Unsupported (Printf.sprintf
                    "FOREIGN KEY on '%s': parent table '%s' not found"
                    cd.Ast.name parent_table))
                | Some parent_meta ->
                  (match List.find_opt (fun (c : Row.column) -> c.primary_key)
                                       parent_meta.Cat.columns with
                   | None ->
                     Error (Unsupported (Printf.sprintf
                       "FOREIGN KEY on '%s': table '%s' has no PRIMARY KEY to infer column"
                       cd.Ast.name parent_table))
                   | Some pk_col ->
                     Ok (fks @ [(cd.Ast.name, parent_table, pk_col.name, od, ou)])))
             | Some (parent_table, parent_col, od, ou) ->
               Ok (fks @ [(cd.Ast.name, parent_table, parent_col, od, ou)]))
        ) (Ok []) columns
      in
      (match col_fks_result with
       | Error e -> Lwt.return (Error e)
       | Ok col_fks ->
      (* Extract FK constraints from table-level FOREIGN KEY *)
      let tbl_fks_result =
        List.fold_left (fun acc c ->
          match acc with
          | Error _ as e -> e
          | Ok fks ->
            (match c with
             | Ast.TC_foreign_key { local_cols; parent_table; parent_cols; on_delete; on_update } ->
               (match local_cols, parent_cols with
                | [lc], [pc] -> Ok (fks @ [(lc, parent_table, pc, on_delete, on_update)])
                | _ ->
                  Error (Unsupported "multi-column FOREIGN KEY constraints are not yet supported"))
             | _ -> Ok fks)
        ) (Ok []) constraints
      in
      (match tbl_fks_result with
       | Error e -> Lwt.return (Error e)
       | Ok tbl_fks ->
      let fk_constraints = col_fks @ tbl_fks in
      Lwt.return (Ok (BS_create_table { name; columns = row_cols; uniq_idxs; if_not_exists; fk_constraints }))))

(* ------------------------------------------------------------------ *)
(* INSERT                                                               *)
(* ------------------------------------------------------------------ *)

(** Convert a [Row.default_value] to a [bound_expr] suitable for INSERT planning. *)
let dv_to_bound_expr : Row.default_value -> bound_expr = function
  | Row.DV_int  n -> BE_lit (Ast.L_int n)
  | Row.DV_text s -> BE_lit (Ast.L_text s)
  | Row.DV_null   -> BE_lit Ast.L_null
  | Row.DV_real f -> BE_lit (Ast.L_real f)
  | Row.DV_blob b -> BE_lit (Ast.L_blob b)
  | Row.DV_current_timestamp -> BE_func (Ast.Fn_datetime, [BE_lit (Ast.L_text "now")])
  | Row.DV_current_date      -> BE_func (Ast.Fn_date,     [BE_lit (Ast.L_text "now")])
  | Row.DV_current_time      -> BE_func (Ast.Fn_time,     [BE_lit (Ast.L_text "now")])

let bind_fts_insert cat ~param_counter ~named_params ~table ~columns ~values =
  match Cat.find_fts cat table with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some fts_meta ->
    let fts_cols = fts_meta.Cat.fts_columns in
    (* If no columns specified, default to all FTS columns in order *)
    let columns = if columns = [] then fts_cols else columns in
    (* Validate that all specified columns exist in fts_meta.fts_columns *)
    let bad = List.find_opt (fun c -> not (List.mem c fts_cols)) columns in
    (match bad with
     | Some col -> Lwt.return (Error (Unknown_column { table; column = col }))
     | None ->
       (* Bind the value expressions using a synthetic table meta *)
       let synth_meta = fts_as_table_meta fts_meta in
       let bind_value_expr (e : Ast.expr) : (bound_expr, error) result =
         match e with
         | Ast.E_lit _ | Ast.E_neg _ | Ast.E_param _ ->
           bind_expr ~param_counter ~named_params synth_meta e
         | _ ->
           Error (Unsupported "complex expression in INSERT VALUES")
       in
       let results = List.map bind_value_expr values in
       let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) results in
       (match errors with
        | e :: _ -> Lwt.return (Error e)
        | [] ->
          let col_values = List.filter_map (function Ok e -> Some e | Error _ -> None) results in
          let n_cols = List.length columns in
          let n_vals = List.length values in
          if n_cols <> n_vals then
            Lwt.return (Error (Arity_mismatch { expected = n_cols; got = n_vals }))
          else
            Lwt.return (Ok (BS_fts_insert {
              fts_meta;
              col_names  = columns;
              col_values;
            }))))

let bind_returning_exprs ~param_counter ~named_params (meta : Cat.table_meta) (exprs : Ast.expr list) =
  List.fold_left (fun acc re ->
    match acc with
    | Error _ -> acc
    | Ok bexprs ->
      (match bind_expr ~param_counter ~named_params meta re with
       | Error e -> Error e
       | Ok be   ->
         if expr_has_subquery be then
           Error (Unsupported "subqueries in RETURNING are not supported")
         else
           Ok (bexprs @ [be]))
  ) (Ok []) exprs

let bind_upsert_rhs_expr ~param_counter ~named_params (meta : Cat.table_meta) (e : Ast.expr) =
  match e with
  | Ast.E_tbl_col (tbl, col) when
      String.equal (String.uppercase_ascii tbl) "EXCLUDED" ->
    (match col_index meta.columns col with
     | None   -> Error (Unknown_column { table = "excluded"; column = col })
     | Some i -> Ok (BE_excluded_col i))
  | other -> bind_expr ~param_counter ~named_params meta other

let bind_upsert_assignments ~param_counter ~named_params (meta : Cat.table_meta)
    (assigns : (string * Ast.expr) list) =
  List.fold_left (fun acc (col_name, rhs_expr) ->
    match acc with
    | Error _ -> acc
    | Ok bound_list ->
      (match col_index meta.columns col_name with
       | None   -> Error (Unknown_column { table = meta.name; column = col_name })
       | Some i ->
         (match bind_upsert_rhs_expr ~param_counter ~named_params meta rhs_expr with
          | Error e -> Error e
          | Ok be   -> Ok (bound_list @ [(i, be)])))
  ) (Ok []) assigns

let bind_insert cat ~param_counter ~named_params ~table ~columns ~values ~on_conflict ~returning ~upsert_update =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None ->
    (* Not a regular table — check if it's an FTS table.
       For multi-row FTS INSERT (not tested), flatten all rows. *)
    bind_fts_insert cat ~param_counter ~named_params ~table ~columns ~values:(List.concat values)
  | Some meta ->
    let columns =
      if columns = [] then List.map (fun c -> c.Row.name) meta.columns
      else columns
    in
    (* bind_one_row: bind a single row of VALUES exprs.
       Returns Ok (ordinals, full_vals) or Error. *)
    let bind_one_row row_vals =
      let n_cols = List.length columns in
      let n_vals = List.length row_vals in
      if n_cols <> n_vals then
        Error (Arity_mismatch { expected = n_cols; got = n_vals })
      else
        (* 1. Bind each expr and build a map from column ordinal -> bound_expr. *)
        let bind_value_expr (e : Ast.expr) : (bound_expr, error) result =
          match e with
          | Ast.E_lit _ | Ast.E_neg _ | Ast.E_param _ ->
            (* Literals, negated literals, and params: bind without column context. *)
            bind_expr ~param_counter ~named_params meta e
          | _ ->
            (* Column references in VALUES make no sense — reject. *)
            Error (Unsupported "complex expression in INSERT VALUES")
        in
        let explicit_result =
          List.fold_left2 (fun acc col_name expr_ast ->
            match acc with
            | Error _ -> acc
            | Ok map ->
              (match col_index meta.columns col_name with
               | None ->
                 Error (Unknown_column { table; column = col_name })
               | Some i ->
                 let col = List.nth meta.columns i in
                 if col.Row.generated_as <> None then
                   Error (Unsupported (Printf.sprintf
                     "cannot INSERT into generated column '%s'"
                     col.Row.name))
                 else
                 (match bind_value_expr expr_ast with
                  | Error e -> Error e
                  | Ok bexpr ->
                    (* Skip type check for params (unknown at bind time). *)
                    (match bexpr with
                     | BE_param _ -> Ok (map @ [(i, bexpr)])
                     | BE_lit lit ->
                       let col = List.nth meta.columns i in
                       (match lit_ty lit with
                        | None   -> Ok (map @ [(i, bexpr)])   (* NULL: skip type check *)
                        | Some t ->
                          if ty_equal t col.ty then Ok (map @ [(i, bexpr)])
                          else Error (Type_mismatch { expected = col.ty; got = t }))
                     | _ -> Ok (map @ [(i, bexpr)]))))
          ) (Ok []) columns row_vals
        in
        (match explicit_result with
         | Error e -> Error e
         | Ok explicit_map ->
           (* 2. Build the full value list (one entry per table column),
                 applying DEFAULT for omitted columns. *)
           let n_table_cols = List.length meta.columns in
           let per_col_results =
             List.init n_table_cols (fun i ->
               let col = List.nth meta.columns i in
               match List.assoc_opt i explicit_map with
               | Some bexpr -> (i, bexpr)
               | None ->
                 (* Not explicitly supplied: use DEFAULT if present, else NULL. *)
                 let bexpr = match col.Row.default with
                   | Some dv -> dv_to_bound_expr dv
                   | None    -> BE_lit Ast.L_null
                 in
                 (i, bexpr))
           in
           let full_pairs = per_col_results in
           (* 3. NOT NULL enforcement: reject if any NOT NULL column has a NULL literal.
                 Params are unchecked at bind time (checked at runtime). *)
           let nn_result =
             List.fold_left (fun acc (i, bexpr) ->
               match acc with
               | Error _ -> acc
               | Ok () ->
                 let col = List.nth meta.columns i in
                 (match bexpr with
                  | BE_lit Ast.L_null when col.Row.not_null ->
                    Error (Not_null_violation col.Row.name)
                  | _ -> Ok ())
             ) (Ok ()) full_pairs
           in
           (match nn_result with
            | Error e -> Error e
            | Ok () ->
              let ordinals = List.map fst full_pairs in
              let full_vals = List.map snd full_pairs in
              Ok (ordinals, full_vals)))
    in
    (* Bind every row *)
    let rows_result = List.fold_left (fun acc row ->
      match acc with
      | Error e -> Error e
      | Ok bound_rows ->
        (match bind_one_row row with
         | Error e -> Error e
         | Ok (ords, vals) -> Ok (bound_rows @ [(ords, vals)]))
    ) (Ok []) values in
    (match rows_result with
     | Error e -> Lwt.return (Error e)
     | Ok [] -> Lwt.return (Error (Unsupported "INSERT with empty VALUES list"))
     | Ok ((ordinals, _) :: _ as bound_rows) ->
       let all_vals = List.map snd bound_rows in
       (match bind_returning_exprs ~param_counter ~named_params meta returning with
        | Error e -> Lwt.return (Error e)
        | Ok ret_bound ->
          let upsert_result =
            match upsert_update with
            | None -> Ok None
            | Some Ast.{ conflict_cols; assignments } ->
              (match bind_upsert_assignments ~param_counter ~named_params meta assignments with
               | Error e -> Error e
               | Ok bound_assigns -> Ok (Some (conflict_cols, bound_assigns)))
          in
          (match upsert_result with
           | Error e -> Lwt.return (Error e)
           | Ok bound_upsert ->
             Lwt.return (Ok (BS_insert {
               table_meta    = meta;
               ordinals;
               values        = all_vals;
               on_conflict;
               returning     = ret_bound;
               upsert_update = bound_upsert;
             })))))

(* ------------------------------------------------------------------ *)
(* SELECT                                                               *)
(* ------------------------------------------------------------------ *)

let bind_fts_seq_scan cat ~param_counter ~named_params ~table ~where ~proj =
  match Cat.find_fts cat table with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some fts_meta ->
    (match where with
     | Some (Ast.E_match (match_table, query_str)) ->
       (* Validate the table name matches *)
       if not (String.equal match_table fts_meta.Cat.fts_name) then
         Lwt.return (Error (Unknown_table match_table))
       else
         (match Fts_query.parse query_str with
          | Error msg -> Lwt.return (Error (Unsupported ("FTS query parse error: " ^ msg)))
          | Ok q ->
            (* Compute column ordinals and detect the virtual `rank` column.
               `rank` is not a real column — it triggers include_rank=true and
               is NOT added to proj (the executor appends it as the last value). *)
            let all_real_ords = List.mapi (fun i _ -> i) fts_meta.Cat.fts_columns in
            (match proj with
             | `All ->
               Lwt.return (Ok (BS_fts_match_scan {
                 fts_meta; query = q;
                 proj = all_real_ords; include_rank = false; snippets = [];
               }))
             | `Cols names ->
               let has_rank = List.exists (String.equal "rank") names in
               let real_ords = List.filter_map (fun name ->
                 if String.equal name "rank" then None
                 else
                   let rec find i = function
                     | [] -> None
                     | c :: _ when String.equal c name -> Some i
                     | _ :: rest -> find (i + 1) rest
                   in
                   find 0 fts_meta.Cat.fts_columns
               ) names in
               Lwt.return (Ok (BS_fts_match_scan {
                 fts_meta; query = q;
                 proj = real_ords; include_rank = has_rank; snippets = [];
               }))
             | `Exprs exprs ->
               let process_result =
                 List.fold_left (fun acc (e, _alias) ->
                   match acc with
                   | Error _ as err -> err
                   | Ok (ords, has_rank, snips) ->
                     (match e with
                      | Ast.E_col col_name ->
                        if String.equal (String.lowercase_ascii col_name) "rank" then
                          Ok (ords, true, snips)
                        else
                          (let rec find i = function
                             | [] -> None
                             | c :: _ when String.equal c col_name -> Some i
                             | _ :: rest -> find (i + 1) rest
                           in
                           match find 0 fts_meta.Cat.fts_columns with
                           | None ->
                             Error (Unknown_column {
                               table = fts_meta.Cat.fts_name; column = col_name
                             })
                           | Some i -> Ok (ords @ [i], has_rank, snips))
                      | Ast.E_fts_snippet { table; col_idx; start_tag; end_tag; ellipsis; n_tokens } ->
                        let tbl_lower = String.lowercase_ascii table in
                        let fts_lower = String.lowercase_ascii fts_meta.Cat.fts_name in
                        if not (String.equal tbl_lower fts_lower) then
                          Error (Unknown_table table)
                        else
                          let spec = Plan.{ col_idx; start_tag; end_tag; ellipsis; n_tokens } in
                          Ok (ords, has_rank, snips @ [spec])
                      | _ ->
                        Error (Unsupported
                          "only column references and snippet() are supported in FTS SELECT"))
                 ) (Ok ([], false, [])) exprs
               in
               (match process_result with
                | Error e -> Lwt.return (Error e)
                | Ok (col_ords, include_rank, snippets) ->
                  Lwt.return (Ok (BS_fts_match_scan {
                    fts_meta; query = q; proj = col_ords; include_rank; snippets;
                  })))))
     | _ ->
       let synth_meta = fts_as_table_meta fts_meta in
       let where_result =
         match where with
         | None   -> Ok None
         | Some e ->
           (match bind_expr ~param_counter ~named_params synth_meta e with
            | Ok be   -> Ok (Some be)
            | Error e -> Error e)
       in
       (match where_result with
        | Error e -> Lwt.return (Error e)
        | Ok bound_where ->
          Lwt.return (Ok (BS_fts_seq_scan {
            fts_meta;
            where = bound_where;
          }))))

let bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~table_alias ~joins ~where ~group_by ~having ~order ~limit ~offset =
  let* meta_opt =
    let tbl_lower = String.lowercase_ascii table in
    if String.equal tbl_lower "sqlite_master"
    || String.equal tbl_lower "sqlite_schema" then
      Lwt.return (Some sqlite_master_meta)
    else
      Cat.find_table cat ~name:table
  in
  match meta_opt with
  | None ->
    (* Not a regular table — check if it's an FTS table (only plain SELECT supported) *)
    (match joins, group_by, having, order, limit, offset with
     | [], [], None, [], None, None ->
       bind_fts_seq_scan cat ~param_counter ~named_params ~table ~where ~proj
     | _ ->
       (* FTS does not yet support JOINs, GROUP BY, HAVING, ORDER BY, LIMIT, OFFSET *)
       (match Cat.find_fts cat table with
        | None -> Lwt.return (Error (Unknown_table table))
        | Some _ -> Lwt.return (Error (Unsupported "FTS tables do not support this query form"))))
  | Some meta ->
    (* Resolve all join table metas in order *)
    let* joined_pairs_result =
      Lwt_list.fold_left_s (fun acc (jc : Ast.join_clause) ->
        match acc with
        | Error e -> Lwt.return (Error e)
        | Ok pairs ->
          let* rm_opt = Cat.find_table cat ~name:jc.table in
          (match rm_opt with
           | None    -> Lwt.return (Error (Unknown_table jc.table))
           | Some rm -> Lwt.return (Ok (pairs @ [(jc, rm)])))
      ) (Ok []) joins
    in
    (match joined_pairs_result with
     | Error e -> Lwt.return (Error e)
     | Ok joined_pairs ->
       let n_left = List.length meta.columns in
       (* tables: [(primary_meta, 0, alias); (rm0, n_left, alias0); ...] *)
       let (tables, _) =
         List.fold_left (fun (acc, off) ((jc : Ast.join_clause), rm) ->
           let n = List.length rm.Cat.columns in
           (acc @ [(rm, off, jc.Ast.alias)], off + n)
         ) ([(meta, 0, table_alias)], n_left) joined_pairs
       in
       (* Combined-row column lookup with full error reporting (Ambiguous,
          Unknown).  Used for proj and ORDER BY name resolution. *)
       let proj_lookup name : (int, error) result =
         let hits = List.filter_map (fun (tm, base, _alias) ->
           match col_index tm.Cat.columns name with
           | Some i -> Some (base + i) | None -> None
         ) tables in
         (match hits with
          | [i]    -> Ok i
          | []     -> Error (Unknown_column { table = meta.Cat.name; column = name })
          | _ :: _ -> Error (Ambiguous_column name))
       in
       let qual_lookup t c : (int, error) result =
         match List.find_opt (fun (tm, _, alias_opt) ->
           String.equal tm.Cat.name t ||
           (match alias_opt with Some a -> String.equal t a | None -> false)
         ) tables with
         | None -> Error (Unknown_table t)
         | Some (tm, base, _) ->
           (match col_index tm.Cat.columns c with
            | Some i -> Ok (base + i)
            | None   -> Error (Unknown_column { table = t; column = c }))
       in
       (* Detect whether this is an aggregated query: any aggregate in
          projection or HAVING, or GROUP BY present. *)
       let proj_has_agg =
         match proj with
         | `All | `Cols _ -> false
         | `Exprs es -> List.exists (fun (e, _) -> expr_has_agg e) es
       in
       let having_has_agg =
         match having with
         | None -> false
         | Some e -> expr_has_agg e
       in
       let group_by_present = group_by <> [] in
       let is_aggregated =
         proj_has_agg || having_has_agg || group_by_present
       in
       (* Bind GROUP BY columns — any number supported. *)
       let find_pos lst v =
         let rec go i = function
           | [] -> None
           | x :: _ when x = v -> Some i
           | _ :: rest -> go (i + 1) rest
         in go 0 lst
       in
       let group_cols_result : (int list, error) result =
         let result = List.fold_left (fun acc col_name ->
           match acc with
           | Error _ as e -> e
           | Ok indices ->
             (match proj_lookup col_name with
              | Ok i -> Ok (i :: indices)  (* prepend, reverse later *)
              | Error _ ->
                (* try qualified lookup across all tables *)
                let found = List.find_map (fun (tm, _, _) ->
                  match qual_lookup tm.Cat.name col_name with
                  | Ok i -> Some i
                  | Error _ -> None
                ) tables in
                (match found with
                 | Some i -> Ok (i :: indices)
                 | None -> Error (Unknown_column { table = ""; column = col_name })))
         ) (Ok []) group_by
         in
         (match result with Ok indices -> Ok (List.rev indices) | Error _ as e -> e)
       in
       (match group_cols_result with
        | Error e -> Lwt.return (Error e)
        | Ok group_cols ->
       let offset_for_aggs = List.length group_cols in
       (* Build proj/agg_proj.
          The result tuple is (col_ordinals, agg_proj, agg_specs, expr_proj, windows, agg_windows).
          [expr_proj] is non-empty only for scalar-function projections
          (Phase 5); [col_ordinals] is empty in that case.
          [windows] is non-empty only for window-function projections (Phase 14).
          [agg_windows] is non-empty only for window functions in aggregated context. *)
       let* proj_result :
           (int list * agg_proj_item list * agg_spec list * (bound_expr * string option) list
            * window_sema list * window_sema list,
            error) result =
         if not is_aggregated then
           (* Ordinary SELECT — keep behaviour identical to pre-Task-6,
              but also handle E_window via bind_ww. *)
           let bind_one e =
             if joined_pairs = [] then
               bind_expr ~param_counter ~named_params meta e
             else
               bind_expr_join ~param_counter ~named_params ~tables e
           in
           let windows_queue : window_sema Queue.t = Queue.create () in
           let rec bind_ww e =
             if not (expr_has_window e) then Lwt.return (bind_one e)
             else match e with
               | Ast.E_window { func; args; window } ->
                 (* Validate frame spec: only aggregate window functions support frames *)
                 (match window.Ast.frame with
                  | Some _ when (match func with Ast.WF_agg _ -> false | _ -> true) ->
                    Lwt.return (Error (Unsupported "ROWS/RANGE frame spec is only supported for aggregate window functions"))
                  | _ ->
                 let slot = Queue.length windows_queue in
                 let bind_list es =
                   List.fold_left (fun acc_r ex ->
                     match acc_r with
                     | Error _ as err -> err
                     | Ok acc ->
                       match bind_one ex with
                       | Error er -> Error er
                       | Ok be    -> Ok (acc @ [be])
                   ) (Ok []) es
                 in
                 let bind_ok_list es =
                   List.fold_left (fun acc_r (ok : Ast.order_key) ->
                     match acc_r with
                     | Error _ as err -> err
                     | Ok acc ->
                       match bind_one ok.Ast.expr with
                       | Error er -> Error er
                       | Ok be    -> Ok (acc @ [{ key = be; dir = ok.Ast.dir; nulls = ok.Ast.nulls }])
                   ) (Ok []) es
                 in
                 (match bind_list args with
                  | Error er -> Lwt.return (Error er)
                  | Ok bound_args ->
                    match bind_list window.Ast.partition_by with
                    | Error er -> Lwt.return (Error er)
                    | Ok bound_pb ->
                      match bind_ok_list window.Ast.order_by with
                      | Error er -> Lwt.return (Error er)
                      | Ok bound_ob ->
                        let ws = { func; args = bound_args;
                                   partition_by = bound_pb;
                                   order_by = bound_ob;
                                   frame = window.Ast.frame } in
                        Queue.push ws windows_queue;
                        Lwt.return (Ok (BE_window_slot slot))))
               | Ast.E_binop (op, a, b) ->
                 let* ba = bind_ww a in
                 let* bb = bind_ww b in
                 (match ba, bb with
                  | Ok ba', Ok bb' ->
                    Lwt.return (Ok (BE_binop (ast_binop_to_sema op, ba', bb')))
                  | Error er, _ | _, Error er -> Lwt.return (Error er))
               | Ast.E_not a ->
                 let* ba = bind_ww a in
                 Lwt.return (Result.map (fun x -> BE_not x) ba)
               | Ast.E_neg a ->
                 let* ba = bind_ww a in
                 Lwt.return (Result.map (fun x -> BE_neg x) ba)
               | Ast.E_is_null a ->
                 let* ba = bind_ww a in
                 Lwt.return (Result.map (fun x -> BE_is_null x) ba)
               | Ast.E_is_not_null a ->
                 let* ba = bind_ww a in
                 Lwt.return (Result.map (fun x -> BE_is_not_null x) ba)
               | Ast.E_bitnot a ->
                 let* ba = bind_ww a in
                 Lwt.return (Result.map (fun x -> BE_bitnot x) ba)
               | Ast.E_between (x, lo, hi) ->
                 let* bx  = bind_ww x  in
                 let* blo = bind_ww lo in
                 let* bhi = bind_ww hi in
                 (match bx, blo, bhi with
                  | Ok x', Ok lo', Ok hi' ->
                    Lwt.return (Ok (BE_between (x', lo', hi')))
                  | Error er, _, _ | _, Error er, _ | _, _, Error er ->
                    Lwt.return (Error er))
               | Ast.E_case { scrutinee; branches; else_ } ->
                 let* bscr = (match scrutinee with
                   | None   -> Lwt.return (Ok None)
                   | Some e ->
                     let* r = bind_ww e in
                     Lwt.return (Result.map Option.some r)) in
                 let* bbranches = Lwt_list.fold_left_s (fun acc (c, r) ->
                     match acc with
                     | Error er -> Lwt.return (Error er)
                     | Ok bs ->
                       let* bc = bind_ww c in
                       let* br = bind_ww r in
                       (match bc, br with
                        | Ok c', Ok r' -> Lwt.return (Ok (bs @ [(c', r')]))
                        | Error er, _ | _, Error er -> Lwt.return (Error er))
                   ) (Ok []) branches in
                 let* belse_ = (match else_ with
                   | None   -> Lwt.return (Ok None)
                   | Some e ->
                     let* r = bind_ww e in
                     Lwt.return (Result.map Option.some r)) in
                 (match bscr, bbranches, belse_ with
                  | Ok scr, Ok brs, Ok el ->
                    Lwt.return (Ok (BE_case { scrutinee = scr;
                                              branches = brs;
                                              else_ = el }))
                  | Error er, _, _ | _, Error er, _ | _, _, Error er ->
                    Lwt.return (Error er))
               | Ast.E_func (f, fargs) ->
                 let* bargs = Lwt_list.fold_left_s (fun acc a ->
                     match acc with
                     | Error er -> Lwt.return (Error er)
                     | Ok bs ->
                       let* r = bind_ww a in
                       Lwt.return (Result.map (fun b -> bs @ [b]) r)
                   ) (Ok []) fargs in
                 Lwt.return (Result.map (fun ba -> BE_func (f, ba)) bargs)
               | Ast.E_cast (e, ty) ->
                 let* be = bind_ww e in
                 Lwt.return (Result.map (fun x -> BE_cast (x, ty)) be)
               | Ast.E_collate (e, c) ->
                 let* be = bind_ww e in
                 Lwt.return (Result.map (fun x -> BE_collate (x, c)) be)
               | _ -> Lwt.return (bind_one e)
           in
           let ords_result_lwt =
             match proj with
             | `All ->
               let all_ords = List.concat_map (fun (tm, base, _alias) ->
                 List.mapi (fun i _ -> base + i) tm.Cat.columns
               ) tables in
               Lwt.return (Ok (`Ords all_ords))
             | `Cols names ->
               Lwt.return (List.fold_left (fun acc name ->
                 match acc with
                 | Error _ -> acc
                 | Ok (`Exprs _) -> acc
                 | Ok (`Ords ords) ->
                   (match proj_lookup name with
                    | Error e -> Error e
                    | Ok i    -> Ok (`Ords (ords @ [i])))
               ) (Ok (`Ords [])) names)
             | `Exprs es ->
               (* Phase 5 / Phase 11 / Phase 14: arbitrary expr projection.
                  Use bind_ww so that E_window nodes are handled. *)
               let* bound_list =
                 Lwt_list.map_s (fun (e, alias) ->
                   let* r = bind_ww e in
                   Lwt.return (match r with
                     | Ok be   -> Ok (be, alias)
                     | Error e -> Error e)
                 ) es
               in
               let errors = List.filter_map
                 (function Error e -> Some e | Ok _ -> None) bound_list in
               (match errors with
                | e :: _ -> Lwt.return (Error e)
                | [] ->
                  Lwt.return (Ok (`Exprs (List.filter_map
                    (function Ok p -> Some p | Error _ -> None) bound_list))))
           in
           let* ords_result = ords_result_lwt in
           let windows_list = Queue.fold (fun acc w -> acc @ [w]) [] windows_queue in
           (match ords_result with
            | Error e -> Lwt.return (Error e)
            | Ok (`Ords o)    -> Lwt.return (Ok (o, [], [], [], windows_list, []))
            | Ok (`Exprs bes) -> Lwt.return (Ok ([], [], [], bes, windows_list, [])))
         else begin
           (* Aggregated SELECT — build agg_proj and aggs list. *)
           (* Helper: walk an expression that is an explicit projection
              item.  For a bare column reference, produce a non-agg slot;
              for an aggregate, produce an AP_agg_slot. *)
           let acc_aggs = ref [] in
           let agg_windows_queue : window_sema Queue.t = Queue.create () in
           let add_agg spec =
             let idx = List.length !acc_aggs in
             acc_aggs := !acc_aggs @ [spec];
             idx
           in
           let project_one (e : Ast.expr) : (agg_proj_item, error) result =
             match e with
             | Ast.E_col name ->
               (match proj_lookup name with
                | Error e -> Error e
                | Ok i ->
                  (* Must appear in GROUP BY. *)
                  (match find_pos group_cols i with
                   | Some pos -> Ok (AP_group_col pos)
                   | None -> Error (Unsupported (Printf.sprintf
                                  "column '%s' must appear in GROUP BY clause" name))))
             | Ast.E_tbl_col (t, c) ->
               (match qual_lookup t c with
                | Error e -> Error e
                | Ok i ->
                  (match find_pos group_cols i with
                   | Some pos -> Ok (AP_group_col pos)
                   | None -> Error (Unsupported (Printf.sprintf
                                  "column '%s.%s' must appear in GROUP BY clause" t c))))
             | Ast.E_agg (func, arg_opt) ->
               (* SUM/AVG type check. *)
               let validate_numeric col_ord =
                 let cols = List.concat_map (fun (tm, _, _) -> tm.Cat.columns) tables in
                 let col = List.nth cols col_ord in
                 match col.Row.ty with
                 | Row.Integer | Row.Real -> Ok ()
                 | _ -> Error (Type_mismatch { expected = Row.Real; got = col.ty })
               in
               let col_ord_result : (int option, error) result =
                 match arg_opt with
                 | None ->
                   (match func with
                    | Ast.Agg_count -> Ok None
                    | _ -> Error (Unsupported "non-COUNT aggregate requires an argument"))
                 | Some (Ast.E_col name) ->
                   (match proj_lookup name with
                    | Error e -> Error e
                    | Ok i -> Ok (Some i))
                 | Some (Ast.E_tbl_col (t, c)) ->
                   (match qual_lookup t c with
                    | Error e -> Error e
                    | Ok i -> Ok (Some i))
                 | Some _ ->
                   Error (Unsupported "aggregate argument must be a column reference")
               in
               (match col_ord_result with
                | Error e -> Error e
                | Ok co ->
                  let type_check =
                    match func, co with
                    | (Ast.Agg_sum | Ast.Agg_avg), Some i -> validate_numeric i
                    | _ -> Ok ()
                  in
                  (match type_check with
                   | Error e -> Error e
                   | Ok () ->
                     let slot = add_agg { func; col_ord = co } in
                     Ok (AP_agg_slot slot)))
             | Ast.E_window { func; args; window } ->
               (* Window function in aggregated projection: bind args in
                  post-agg context (GROUP BY cols and agg slots allowed). *)
               let bind_post_agg e =
                 let rec go = function
                   | Ast.E_lit l -> Ok (BE_lit l)
                   | Ast.E_col name ->
                     (match proj_lookup name with
                      | Error e -> Error e
                      | Ok i ->
                        match find_pos group_cols i with
                        | Some pos -> Ok (BE_col pos)
                        | None -> Error (Unsupported (Printf.sprintf
                            "column '%s' must appear in GROUP BY to be referenced in a window function in this context" name)))
                   | Ast.E_tbl_col (t, c) ->
                     (match qual_lookup t c with
                      | Error e -> Error e
                      | Ok i ->
                        match find_pos group_cols i with
                        | Some pos -> Ok (BE_col pos)
                        | None -> Error (Unsupported (Printf.sprintf
                            "column '%s.%s' must appear in GROUP BY to be referenced in a window function in this context" t c)))
                   | Ast.E_agg (func, arg_opt) ->
                     let col_ord_result : (int option, error) result =
                       match arg_opt with
                       | None -> (match func with
                                  | Ast.Agg_count -> Ok None
                                  | _ -> Error (Unsupported "non-COUNT aggregate requires an argument"))
                       | Some (Ast.E_col name) ->
                         (match proj_lookup name with
                          | Error e -> Error e | Ok i -> Ok (Some i))
                       | Some (Ast.E_tbl_col (t, c)) ->
                         (match qual_lookup t c with
                          | Error e -> Error e | Ok i -> Ok (Some i))
                       | Some _ ->
                         Error (Unsupported "aggregate argument in window function must be a column reference")
                     in
                     (match col_ord_result with
                      | Error e -> Error e
                      | Ok co ->
                        let spec = { func; col_ord = co } in
                        let rec find_slot i = function
                          | [] ->
                            acc_aggs := !acc_aggs @ [spec];
                            List.length !acc_aggs - 1
                          | s :: _ when s.func = spec.func && s.col_ord = spec.col_ord -> i
                          | _ :: rest -> find_slot (i + 1) rest
                        in
                        let slot = find_slot 0 !acc_aggs in
                        Ok (BE_col (offset_for_aggs + slot)))
                   | Ast.E_neg e -> (match go e with Ok be -> Ok (BE_neg be) | Error e -> Error e)
                   | Ast.E_not e -> (match go e with Ok be -> Ok (BE_not be) | Error e -> Error e)
                   | Ast.E_binop (op, a, b) ->
                     (match go a, go b with
                      | Ok ba, Ok bb -> Ok (BE_binop (ast_binop_to_sema op, ba, bb))
                      | Error e, _ | _, Error e -> Error e)
                   | Ast.E_window _ -> Error (Unsupported "nested window functions not supported")
                   | _ -> Error (Unsupported "only GROUP BY columns and aggregate expressions are supported in window function arguments in this context")
                 in go e
               in
               let bind_list es =
                 List.fold_left (fun acc_r ex ->
                   match acc_r with
                   | Error _ as err -> err
                   | Ok acc ->
                     match bind_post_agg ex with
                     | Error er -> Error er
                     | Ok be    -> Ok (acc @ [be])
                 ) (Ok []) es
               in
               let bind_ok_list (oks : Ast.order_key list) =
                 List.fold_left (fun acc_r ok ->
                   match acc_r with
                   | Error _ as err -> err
                   | Ok acc ->
                     match bind_post_agg ok.Ast.expr with
                     | Error er -> Error er
                     | Ok be    ->
                       let nulls = ok.Ast.nulls in
                       Ok (acc @ [{ key = be; dir = ok.Ast.dir; nulls }])
                 ) (Ok []) oks
               in
               (match bind_list args with
                | Error er -> Error er
                | Ok bound_args ->
                  match bind_list window.Ast.partition_by with
                  | Error er -> Error er
                  | Ok bound_pb ->
                    match bind_ok_list window.Ast.order_by with
                    | Error er -> Error er
                    | Ok bound_ob ->
                      let slot = Queue.length agg_windows_queue in
                      Queue.push
                        { func; args = bound_args;
                          partition_by = bound_pb;
                          order_by = bound_ob;
                          frame = window.Ast.frame }
                        agg_windows_queue;
                      Ok (AP_window_slot slot))
             | _ ->
               Error (Unsupported "complex expression in aggregated projection not supported")
           in
           let exprs_to_project : Ast.expr list =
             match proj with
             | `All ->
               (* In aggregated context, `*` is interpreted as either:
                  - the group column alone (if GROUP BY present and no
                    aggregates in HAVING — rare), or
                  - error if there are no aggregates.
                  This is non-standard SQL behaviour but for Phase 2 we
                  only accept aggregated `*` if there's a GROUP BY. *)
               (match group_cols with
                | _ :: _ -> [] (* unused — won't reach here *)
                | [] -> [])
             | `Cols names -> List.map (fun n -> Ast.E_col n) names
             | `Exprs es -> List.map fst es
           in
           if exprs_to_project = [] && proj = `All && is_aggregated then
             Lwt.return (Error (Unsupported "SELECT * with aggregates requires explicit columns"))
           else
             let agg_proj_result =
               List.fold_left (fun acc e ->
                 match acc with
                 | Error _ -> acc
                 | Ok items ->
                   (match project_one e with
                    | Error e -> Error e
                    | Ok item -> Ok (items @ [item]))
               ) (Ok []) exprs_to_project
             in
             let agg_wins = Queue.fold (fun acc w -> acc @ [w]) [] agg_windows_queue in
             Lwt.return (match agg_proj_result with
              | Error e -> Error e
              | Ok items -> Ok ([], items, !acc_aggs, [], [], agg_wins))
         end
       in
       (match proj_result with
        | Error e -> Lwt.return (Error e)
        | Ok (proj_ords, agg_proj_items, proj_aggs, proj_exprs, proj_windows, agg_wins) ->
          (* Bind each join ON predicate against tables visible so far *)
          let bind_joins_result : (bound_join list, error) result =
            let rec go acc tbl_acc offset = function
              | [] -> Ok (List.rev acc)
              | ((jc : Ast.join_clause), rm) :: rest ->
                let tables_so_far = tbl_acc @ [(rm, offset, jc.Ast.alias)] in
                (match bind_expr_join ~param_counter ~named_params
                         ~tables:tables_so_far jc.Ast.on with
                 | Error e -> Error e
                 | Ok be   ->
                   let bj = { kind = jc.Ast.kind; right_meta = rm;
                              on = be; right_col_offset = offset } in
                   go (bj :: acc) tables_so_far (offset + List.length rm.Cat.columns) rest)
            in
            go [] [(meta, 0, table_alias)] n_left joined_pairs
          in
          (match bind_joins_result with
           | Error e -> Lwt.return (Error e)
           | Ok bound_joins ->
             let bind_combined e =
               if joined_pairs = [] then
                 bind_expr ~param_counter ~named_params meta e
               else
                 bind_expr_join ~param_counter ~named_params ~tables e
             in
             let where_result =
               match where with
               | None   -> Ok None
               | Some e ->
                 (match bind_combined e with
                  | Ok be   -> Ok (Some be)
                  | Error e -> Error e)
             in
             (match where_result with
              | Error e -> Lwt.return (Error e)
              | Ok bound_where ->
                (* HAVING is bound in the agg-output context.  Aggregates
                   inside HAVING collect into [having_aggs] which are
                   appended after [proj_aggs]. *)
                let having_result : (bound_expr option * agg_spec list, error) result =
                  match having with
                  | None -> Ok (None, [])
                  | Some e ->
                    if not is_aggregated then
                      Error (Unsupported "HAVING requires GROUP BY or aggregate")
                    else begin
                      (* Use a resolver that, for plain column refs,
                         requires the column to appear in GROUP BY
                         (resolves to its position in group_cols), else error. *)
                      let having_resolver_unqual name =
                        match proj_lookup name with
                        | Error e -> Error e
                        | Ok i ->
                          (match find_pos group_cols i with
                           | Some pos -> Ok pos
                           | None -> Error (Unsupported (Printf.sprintf
                                          "HAVING references non-grouped column '%s'" name)))
                      in
                      let having_resolver_qual t c =
                        match qual_lookup t c with
                        | Error e -> Error e
                        | Ok i ->
                          (match find_pos group_cols i with
                           | Some pos -> Ok pos
                           | None -> Error (Unsupported (Printf.sprintf
                                          "HAVING references non-grouped column '%s.%s'" t c)))
                      in
                      let having_resolver = {
                        resolve_unqual = having_resolver_unqual;
                        resolve_qual = having_resolver_qual;
                        (* Inside aggregate args in HAVING, any table column is allowed *)
                        resolve_agg_arg      = proj_lookup;
                        resolve_agg_arg_qual = qual_lookup;
                      } in
                      (* Append HAVING aggregates AFTER the projection
                         aggregates: in BE_col offset = offset_for_aggs +
                         (List.length proj_aggs). *)
                      let having_offset =
                        offset_for_aggs + List.length proj_aggs
                      in
                      match bind_expr_agg ~param_counter ~named_params ~resolver:having_resolver
                              ~offset:having_offset e with
                      | Error e -> Error e
                      | Ok (be, hagg) -> Ok (Some be, hagg)
                    end
                in
                (match having_result with
                 | Error e -> Lwt.return (Error e)
                 | Ok (bound_having, having_aggs) ->
                let all_aggs = proj_aggs @ having_aggs in
                (* alias_map: name → bound_expr for ORDER BY alias resolution *)
                let alias_map : (string * bound_expr) list =
                  List.filter_map (fun (be, alias_opt) ->
                    Option.map (fun a -> (a, be)) alias_opt
                  ) proj_exprs
                in
                let bind_order_expr e =
                  let base_result =
                    if joined_pairs = [] then
                      bind_expr ~param_counter ~named_params meta e
                    else
                      bind_expr_join ~param_counter ~named_params ~tables e
                  in
                  match base_result with
                  | Ok _ -> base_result
                  | Error _ ->
                    (match e with
                     | Ast.E_col name ->
                       (match List.assoc_opt name alias_map with
                        | Some be -> Ok be
                        | None    -> base_result)
                     | _ -> base_result)
                in
                let order_result =
                  List.fold_left (fun acc (ok : Ast.order_key) ->
                    match acc with
                    | Error _ -> acc
                    | Ok keys ->
                      (match bind_order_expr ok.Ast.expr with
                       | Error e -> Error e
                       | Ok key  -> Ok (keys @ [{ key; dir = ok.Ast.dir; nulls = ok.Ast.nulls }]))
                  ) (Ok []) order
                in
                (match order_result with
                 | Error e -> Lwt.return (Error e)
                 | Ok bound_order ->
                   let limit_result =
                     match limit with
                     | Some n when n < 0 ->
                       Error (Invalid_limit "LIMIT must be non-negative")
                     | _ -> Ok limit
                   in
                   (match limit_result with
                    | Error e -> Lwt.return (Error e)
                    | Ok valid_limit ->
                      let offset_result =
                        match offset with
                        | Some n when n < 0 ->
                          Error (Invalid_limit "OFFSET must be non-negative")
                        | _ -> Ok offset
                      in
                      (match offset_result with
                       | Error e -> Lwt.return (Error e)
                       | Ok valid_offset ->
                         Lwt.return (Ok (BS_select {
                           distinct;
                           table_meta = meta;
                           proj       = proj_ords;
                           expr_proj  = proj_exprs;
                           where      = bound_where;
                           order      = bound_order;
                           limit      = valid_limit;
                           offset     = valid_offset;
                           joins      = bound_joins;
                           group_by   = group_cols;
                           aggs       = all_aggs;
                           having     = bound_having;
                           agg_proj    = agg_proj_items;
                           windows     = proj_windows;
                           agg_windows = agg_wins;
                         })))))))))))

(* ------------------------------------------------------------------ *)
(* Best-effort type inference for bound expressions.                    *)
(* Returns [None] when the type is statically indeterminate (e.g. NULL  *)
(* literal, or mixed-type arithmetic where coercion may apply).         *)
(* ------------------------------------------------------------------ *)

let rec infer_type (cols : Row.column list) : bound_expr -> Row.ty option = function
  | BE_lit l -> lit_ty l
  | BE_col i ->
    (* col_idx is already validated; safe to access. *)
    Some (List.nth cols i).Row.ty
  | BE_not _ | BE_is_null _ | BE_is_not_null _ ->
    Some Row.Integer   (* boolean expressions encoded as INTEGER 0/1 *)
  | BE_binop (op, a, b) ->
    (match op with
     | Eq | Ne | Lt | Le | Gt | Ge | And | Or ->
       Some Row.Integer
     | Bit_and | Bit_or | Lshift | Rshift | Mod ->
       Some Row.Integer
     | Like | Glob ->
       Some Row.Integer
     | Concat -> Some Row.Text
     | Add | Sub | Mul | Div ->
       (match infer_type cols a, infer_type cols b with
        | Some Row.Integer, Some Row.Integer -> Some Row.Integer
        | Some Row.Real,    _
        | _,                Some Row.Real    -> Some Row.Real
        | Some Row.Integer, None
        | None,             Some Row.Integer -> None
        | _                                  -> None))
  | BE_neg e -> infer_type cols e
  | BE_bitnot _ -> Some Row.Integer
  | BE_between _ -> Some Row.Integer  (* BETWEEN returns boolean 0/1 *)
  | BE_in _ -> Some Row.Integer       (* IN returns boolean 0/1 *)
  | BE_func _ -> None   (* scalar functions return dynamic types *)
  | BE_param _ -> None  (* parameter type unknown at compile time *)
  | BE_match _ -> Some Row.Integer  (* MATCH returns boolean (0/1) *)
  | BE_subquery _ -> None           (* subquery type unknown at bind time *)
  | BE_exists _ -> Some Row.Integer (* EXISTS returns boolean 0/1 *)
  | BE_in_select _ -> Some Row.Integer (* IN (SELECT) returns boolean 0/1 *)
  | BE_case _ -> None               (* CASE result type depends on branches *)
  | BE_cast (_, ty) ->              (* CAST target type is statically known *)
    Some (match ty with
      | Ast.Ty_int  -> Row.Integer
      | Ast.Ty_text -> Row.Text
      | Ast.Ty_real -> Row.Real
      | Ast.Ty_blob -> Row.Blob)
  | BE_excluded_col _ -> None      (* type of excluded col unknown at bind time *)
  | BE_window_slot _ -> None       (* type of window func result unknown at bind time *)
  | BE_collate (e, _) -> infer_type cols e   (* collation doesn't change type *)

(* ------------------------------------------------------------------ *)
(* CREATE INDEX                                                         *)
(* ------------------------------------------------------------------ *)

let bind_create_index cat ~name ~table ~columns ~where_clause ~unique ~if_not_exists =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some meta ->
    let pc = ref 0 in
    let np = Hashtbl.create 0 in
    (* Bind each column expression to validate it; discard the bound forms —
       the SQL strings in col_sqls are sufficient for runtime eval. *)
    let col_results = List.map (fun col_ast ->
      match bind_expr ~param_counter:pc ~named_params:np meta col_ast with
      | Error e -> Error e
      | Ok _    -> Ok col_ast    (* keep AST for SQL serialization only *)
    ) columns in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) col_results in
    (match errors with
     | e :: _ -> Lwt.return (Error e)
     | [] ->
       (* Compute col_sqls and col_expr_flags from the original AST *)
       let col_sqls, col_expr_flags = List.split (List.map (fun col_ast ->
         match col_ast with
         | Ast.E_col cname | Ast.E_tbl_col (_, cname) -> (cname, false)
         | _ -> (Ast.expr_to_sql col_ast, true)
       ) columns) in
       (* Bind WHERE clause *)
       let where_result = match where_clause with
         | None -> Ok (None, None)
         | Some w_ast ->
           (match bind_expr ~param_counter:pc ~named_params:np meta w_ast with
            | Error e -> Error e
            | Ok bw -> Ok (Some bw, Some w_ast))
       in
       (match where_result with
        | Error e -> Lwt.return (Error e)
        | Ok (where_expr, where_ast) ->
          (match Cat.find_index cat ~name with
           | Some _ when not if_not_exists -> Lwt.return (Error (Already_exists name))
           | Some _ (* if_not_exists = true: silently succeed *) ->
             Lwt.return (Ok (BS_create_index {
               name;
               table_meta = meta;
               col_sqls;
               col_expr_flags;
               where_expr;
               where_ast;
               unique;
               if_not_exists = true;
             }))
           | None ->
             Lwt.return (Ok (BS_create_index {
               name;
               table_meta = meta;
               col_sqls;
               col_expr_flags;
               where_expr;
               where_ast;
               unique;
               if_not_exists;
             })))))

(* ------------------------------------------------------------------ *)
(* UPDATE                                                               *)
(* ------------------------------------------------------------------ *)

let bind_order_keys ~param_counter ~named_params (meta : Cat.table_meta) (oks : Ast.order_key list) =
  List.fold_left (fun acc_r ok ->
    match acc_r with
    | Error _ as e -> e
    | Ok acc ->
      (match bind_expr ~param_counter ~named_params meta ok.Ast.expr with
       | Error e -> Error e
       | Ok be   -> Ok (acc @ [{ key = be; dir = ok.Ast.dir; nulls = ok.Ast.nulls }]))
  ) (Ok []) oks

let bind_update cat ~param_counter ~named_params ~table ~assignments ~where ~order ~limit ~offset ~returning =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some meta ->
    (* Bind each assignment: resolve column ordinal, bind the expression,
       and check that the inferred expression type matches the column.
       Also reject SET col = NULL on a NOT NULL column (static check). *)
    let assign_result =
      List.fold_left (fun acc (col_name, expr_ast) ->
        match acc with
        | Error _ -> acc
        | Ok bound_list ->
          (match col_index meta.columns col_name with
           | None ->
             Error (Unknown_column { table; column = col_name })
           | Some i ->
             let col = List.nth meta.columns i in
             if col.Row.generated_as <> None then
               Error (Unsupported (Printf.sprintf
                 "cannot UPDATE generated column '%s'"
                 col.Row.name))
             else
             (* Static NOT NULL check for literal NULL assignments. *)
             if col.Row.not_null && expr_ast = Ast.E_lit Ast.L_null then
               Error (Not_null_violation col.Row.name)
             else
             (match bind_expr ~param_counter ~named_params meta expr_ast with
              | Error e -> Error e
              | Ok bexpr ->
                (match infer_type meta.columns bexpr with
                 | None    -> Ok (bound_list @ [(i, bexpr)])
                 | Some t  ->
                   if ty_equal t col.ty then Ok (bound_list @ [(i, bexpr)])
                   else Error (Type_mismatch { expected = col.ty; got = t }))))
      ) (Ok []) assignments
    in
    (match assign_result with
     | Error e -> Lwt.return (Error e)
     | Ok bound_assigns ->
       let where_result =
         match where with
         | None   -> Ok None
         | Some e ->
           (match bind_expr ~param_counter ~named_params meta e with
            | Ok be   -> Ok (Some be)
            | Error e -> Error e)
       in
       (match where_result with
        | Error e -> Lwt.return (Error e)
        | Ok bound_where ->
          (* Block subqueries in UPDATE WHERE/SET — not supported in Phase 9. *)
          let has_subquery_in_where = match bound_where with
            | Some e -> expr_has_subquery e
            | None   -> false
          in
          let has_subquery_in_assign =
            List.exists (fun (_, e) -> expr_has_subquery e) bound_assigns
          in
          if has_subquery_in_where || has_subquery_in_assign then
            Lwt.return (Error (Unsupported "subqueries in UPDATE WHERE/SET are not supported"))
          else
          let order_result = bind_order_keys ~param_counter ~named_params meta order in
          (match order_result with
           | Error e -> Lwt.return (Error e)
           | Ok bound_order ->
             (match bind_returning_exprs ~param_counter ~named_params meta returning with
              | Error e -> Lwt.return (Error e)
              | Ok ret_bound ->
                Lwt.return (Ok (BS_update {
                  table_meta  = meta;
                  assignments = bound_assigns;
                  where       = bound_where;
                  order       = bound_order;
                  limit;
                  offset;
                  returning   = ret_bound;
                }))))))

(* ------------------------------------------------------------------ *)
(* DELETE                                                               *)
(* ------------------------------------------------------------------ *)

let bind_fts_delete cat ~param_counter ~named_params ~table ~where =
  match Cat.find_fts cat table with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some fts_meta ->
    let synth_meta = fts_as_table_meta fts_meta in
    let where_result =
      match where with
      | None   -> Ok None
      | Some e ->
        (match bind_expr ~param_counter ~named_params synth_meta e with
         | Ok be   -> Ok (Some be)
         | Error e -> Error e)
    in
    (match where_result with
     | Error e -> Lwt.return (Error e)
     | Ok bound_where ->
       Lwt.return (Ok (BS_fts_delete {
         fts_meta;
         where = bound_where;
       })))

let bind_delete cat ~param_counter ~named_params ~table ~where ~order ~limit ~offset ~returning =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None ->
    (* Not a regular table — check if it's an FTS table *)
    bind_fts_delete cat ~param_counter ~named_params ~table ~where
  | Some meta ->
    let where_result =
      match where with
      | None   -> Ok None
      | Some e ->
        (match bind_expr ~param_counter ~named_params meta e with
         | Ok be   -> Ok (Some be)
         | Error e -> Error e)
    in
    (match where_result with
     | Error e -> Lwt.return (Error e)
     | Ok bound_where ->
       (* Block subqueries in DELETE WHERE — not supported in Phase 9. *)
       let has_subquery_in_where = match bound_where with
         | Some e -> expr_has_subquery e
         | None   -> false
       in
       if has_subquery_in_where then
         Lwt.return (Error (Unsupported "subqueries in DELETE WHERE are not supported"))
       else
         let order_result = bind_order_keys ~param_counter ~named_params meta order in
         (match order_result with
          | Error e -> Lwt.return (Error e)
          | Ok bound_order ->
            (match bind_returning_exprs ~param_counter ~named_params meta returning with
             | Error e -> Lwt.return (Error e)
             | Ok ret_bound ->
               Lwt.return (Ok (BS_delete {
                 table_meta = meta;
                 where      = bound_where;
                 order      = bound_order;
                 limit;
                 offset;
                 returning  = ret_bound;
               })))))

(* ------------------------------------------------------------------ *)
(* ALTER TABLE                                                          *)
(* ------------------------------------------------------------------ *)

let bind_alter_table cat ~table ~action =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some table_meta ->
    (match action with
     | Ast.AA_add_column col_def ->
       let col_name = col_def.Ast.name in
       let exists = List.exists (fun c -> String.equal c.Row.name col_name) table_meta.Cat.columns in
       if exists then Lwt.return (Error (Already_exists col_name))
       else if col_def.Ast.not_null && (col_def.Ast.default = None ||
                                         col_def.Ast.default = Some Ast.L_null) then
         Lwt.return (Error (Unsupported
           "ADD COLUMN with NOT NULL requires a non-NULL DEFAULT"))
       else Lwt.return (Ok (BS_alter_table { table_meta; action }))
     | Ast.AA_rename_table _ ->
       Lwt.return (Ok (BS_alter_table { table_meta; action }))
     | Ast.AA_rename_column (old_col, _new_col) ->
       let exists = List.exists (fun c -> String.equal c.Row.name old_col) table_meta.Cat.columns in
       if not exists then Lwt.return (Error (Unknown_column { table; column = old_col }))
       else Lwt.return (Ok (BS_alter_table { table_meta; action }))
     | Ast.AA_drop_column col_name ->
       let exists = List.exists (fun c -> String.equal c.Row.name col_name) table_meta.Cat.columns in
       if not exists then
         Lwt.return (Error (Unknown_column { table; column = col_name }))
       else if List.length table_meta.Cat.columns <= 1 then
         Lwt.return (Error (Unsupported "cannot drop the only column of a table"))
       else
         Lwt.return (Ok (BS_alter_table { table_meta; action })))

(* ------------------------------------------------------------------ *)
(* DROP TABLE                                                           *)
(* ------------------------------------------------------------------ *)

let bind_drop_table cat ~name ~if_exists =
  let* meta_opt = Cat.find_table cat ~name in
  match meta_opt with
  | None when if_exists -> Lwt.return (Ok BS_no_op)
  | None -> Lwt.return (Error (Unknown_table name))
  | Some table_meta ->
    Lwt.return (Ok (BS_drop_table { name; table_meta }))

(* ------------------------------------------------------------------ *)
(* DROP INDEX                                                           *)
(* ------------------------------------------------------------------ *)

let bind_drop_index cat ~name ~if_exists =
  match Cat.find_index cat ~name with
  | None when if_exists -> Lwt.return (Ok BS_no_op)
  | None -> Lwt.return (Error (Unknown_index name))
  | Some idx_info ->
    Lwt.return (Ok (BS_drop_index { name; idx_info }))

(* ------------------------------------------------------------------ *)
(* Error pretty-printer                                                 *)
(* ------------------------------------------------------------------ *)

let pp_error fmt = function
  | Unknown_table t ->
    Format.fprintf fmt "unknown table: %s" t
  | Unknown_column { table; column } ->
    Format.fprintf fmt "unknown column: %s.%s" table column
  | Ambiguous_column col ->
    Format.fprintf fmt "ambiguous column: %s" col
  | Type_mismatch { expected; got } ->
    let ty_str = function
      | Sqlocaml_encoding.Row.Integer -> "INTEGER"
      | Sqlocaml_encoding.Row.Text    -> "TEXT"
      | Sqlocaml_encoding.Row.Real    -> "REAL"
      | Sqlocaml_encoding.Row.Blob    -> "BLOB"
    in
    Format.fprintf fmt "type mismatch: expected %s, got %s" (ty_str expected) (ty_str got)
  | Arity_mismatch { expected; got } ->
    Format.fprintf fmt "arity mismatch: expected %d, got %d" expected got
  | Already_exists name ->
    Format.fprintf fmt "already exists: %s" name
  | Invalid_limit msg ->
    Format.fprintf fmt "invalid limit: %s" msg
  | Unsupported msg ->
    Format.fprintf fmt "unsupported: %s" msg
  | Not_null_violation col ->
    Format.fprintf fmt "NOT NULL violation: %s" col
  | Unknown_index name ->
    Format.fprintf fmt "unknown index: %s" name

(* ------------------------------------------------------------------ *)
(* Public entry point                                                   *)
(* ------------------------------------------------------------------ *)

(* Count output columns of a bound statement for compound-select validation. *)
let rec compound_col_count = function
  | BS_select { proj; expr_proj; aggs; _ } ->
    if aggs <> [] then List.length aggs
    else if expr_proj <> [] then List.length expr_proj
    else List.length proj
  | BS_compound { left; _ } -> compound_col_count left
  | BS_const_select { exprs } -> List.length exprs
  | BS_with_cte { query; recursive = _; _ } -> compound_col_count query
  | _ -> 0  (* non-select stmts in compound: don't validate *)

let rec col_names_of_bound_stmt bs =
  let n = compound_col_count bs in
  match bs with
  | BS_select { expr_proj; proj; table_meta; agg_proj; _ } ->
    if agg_proj <> [] then
      (* For aggregated queries, agg_proj contains the final projection order.
         agg_proj has the actual number of output columns.
         Return generic names — callers can override with AST aliases if needed. *)
      List.mapi (fun i _ -> Printf.sprintf "col_%d" (i + 1)) agg_proj
    else if expr_proj <> [] then
      List.mapi (fun i (_, alias_opt) ->
        Option.value alias_opt ~default:(Printf.sprintf "col_%d" (i + 1))
      ) expr_proj
    else
      List.filter_map (fun i ->
        if i < List.length table_meta.Cat.columns
        then Some (List.nth table_meta.Cat.columns i).Row.name
        else None
      ) proj
  | BS_compound { left; _ } -> col_names_of_bound_stmt left
  | BS_with_cte { query; recursive = _; _ } -> col_names_of_bound_stmt query
  | BS_const_select { exprs } ->
    List.mapi (fun i (_, alias_opt) ->
      Option.value alias_opt ~default:(Printf.sprintf "col_%d" (i + 1))
    ) exprs
  | _ -> List.init n (fun i -> Printf.sprintf "col_%d" (i + 1))

(** Extract output column names from an AST SELECT stmt (best-effort; used for CTEs). *)
let rec col_names_of_ast_stmt = function
  | Ast.S_select { proj; _ } ->
    (match proj with
     | `All -> []   (* unknown until resolved *)
     | `Cols names -> names
     | `Exprs items ->
       List.mapi (fun i (expr, alias_opt) ->
         match alias_opt with
         | Some a -> a
         | None ->
           (match expr with
            | Ast.E_col name -> name
            | Ast.E_tbl_col (_, name) -> name
            | _ -> Printf.sprintf "col_%d" (i + 1))
       ) items)
  | Ast.S_compound { left; _ } -> col_names_of_ast_stmt left
  | Ast.S_const_select { exprs } ->
    List.mapi (fun i (expr, alias_opt) ->
      match alias_opt with
      | Some a -> a
      | None ->
        (match expr with
         | Ast.E_col name -> name
         | Ast.E_tbl_col (_, name) -> name
         | _ -> Printf.sprintf "col_%d" (i + 1))
    ) exprs
  | _ -> []

let rec bind_internal ?(views = Hashtbl.create 0) ~named_params ~param_counter cat stmt =
  match stmt with
  | Ast.S_create_table { name; columns; constraints; if_not_exists } -> bind_create cat ~name ~columns ~constraints ~if_not_exists
  | Ast.S_insert { table; columns; values; on_conflict; returning; upsert_update } -> bind_insert cat ~param_counter ~named_params ~table ~columns ~values ~on_conflict ~returning ~upsert_update
  | Ast.S_insert_select { table; columns; on_conflict; select } ->
    let* table_meta_opt = Cat.find_table cat ~name:table in
    (match table_meta_opt with
     | None -> Lwt.return (Error (Unknown_table table))
     | Some table_meta ->
       let ordinals_result =
         if columns = [] then
           Ok (List.filter_map Fun.id
               (List.mapi (fun i (c : Row.column) ->
                 match c.generated_as with
                 | Some _ -> None
                 | None   -> Some i
               ) table_meta.Cat.columns))
         else
           List.fold_left (fun acc col_name ->
             match acc with
             | Error _ as e -> e
             | Ok ords ->
               let rec fi i = function
                 | [] -> Error (Unknown_column { table = table_meta.Cat.name; column = col_name })
                 | (c : Row.column) :: _ when String.equal c.name col_name ->
                   Ok (ords @ [i])
                 | _ :: rest -> fi (i + 1) rest
               in fi 0 table_meta.Cat.columns
           ) (Ok []) columns
       in
       (match ordinals_result with
        | Error e -> Lwt.return (Error e)
        | Ok ordinals ->
          let* source_result = bind_internal ~views ~named_params ~param_counter cat select in
          (match source_result with
           | Error e -> Lwt.return (Error e)
           | Ok source ->
             Lwt.return (Ok (BS_insert_select { table_meta; ordinals; source; on_conflict })))))
  | Ast.S_select { distinct; proj; table; table_alias; joins; where; group_by; having; order; limit; offset } as sel ->
    let* meta_opt = Cat.find_table cat ~name:table in
    (match meta_opt with
     | Some _ ->
       bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~table_alias
         ~joins ~where ~group_by ~having ~order ~limit ~offset
     | None ->
       (match Hashtbl.find_opt views table with
        | Some view_def ->
          bind_internal ~views ~named_params ~param_counter cat
            (Ast.S_with_cte { name = table; def = view_def; query = sel; recursive = false })
        | None ->
          bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~table_alias
            ~joins ~where ~group_by ~having ~order ~limit ~offset))
  | Ast.S_create_index { name; table; columns; where_clause; unique; if_not_exists } ->
    bind_create_index cat ~name ~table ~columns ~where_clause ~unique ~if_not_exists
  | Ast.S_update { table; assignments; where; order; limit; offset; returning } ->
    bind_update cat ~param_counter ~named_params ~table ~assignments ~where ~order ~limit ~offset ~returning
  | Ast.S_delete { table; where; order; limit; offset; returning } ->
    bind_delete cat ~param_counter ~named_params ~table ~where ~order ~limit ~offset ~returning
  | Ast.S_drop_table { name; if_exists } ->
    bind_drop_table cat ~name ~if_exists
  | Ast.S_drop_index { name; if_exists } ->
    bind_drop_index cat ~name ~if_exists
  | Ast.S_alter_table { table; action } ->
    bind_alter_table cat ~table ~action
  | Ast.S_begin    -> Lwt.return (Ok BS_begin)
  | Ast.S_commit   -> Lwt.return (Ok BS_commit)
  | Ast.S_rollback -> Lwt.return (Ok BS_rollback)
  | Ast.S_savepoint name   -> Lwt.return (Ok (BS_savepoint name))
  | Ast.S_release name     -> Lwt.return (Ok (BS_release name))
  | Ast.S_rollback_to name -> Lwt.return (Ok (BS_rollback_to name))
  | Ast.S_create_fts_table { name; columns } ->
    let* tbl = Cat.find_table cat ~name in
    let fts_existing = Cat.find_fts cat name in
    (match tbl, fts_existing with
     | Some _, _ | _, Some _ -> Lwt.return (Error (Already_exists name))
     | None, None ->
       Lwt.return (Ok (BS_create_fts_table { name; columns })))
  | Ast.S_pragma kind -> Lwt.return (Ok (BS_pragma { kind }))
  | Ast.S_const_select { exprs } ->
    (* FROM-less SELECT: bind each expression without any table context.
       We use a dummy empty table_meta for the resolver. *)
    let dummy_meta : Cat.table_meta = {
      Cat.name           = "__const__";
      Cat.tree_id        = 0;
      Cat.columns        = [];
      Cat.next_rowid     = 0L;
      Cat.fk_constraints = [];
    } in
    let bound = List.map (fun (expr, alias) ->
      match bind_expr ~param_counter ~named_params dummy_meta expr with
      | Error e -> Error e
      | Ok be   -> Ok (be, alias)
    ) exprs in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bound in
    (match errors with
     | e :: _ -> Lwt.return (Error e)
     | [] ->
       let ok_exprs = List.filter_map (function Ok e -> Some e | Error _ -> None) bound in
       Lwt.return (Ok (BS_const_select { exprs = ok_exprs })))
  | Ast.S_with_cte { name; def; query; recursive } ->
    (* For recursive CTEs the def is UNION ALL [base; recursive_arm].
       The recursive arm references the CTE by name, which must be in the
       catalog before we can bind the recursive arm.  Strategy:
         1. Bind just the base (left branch) to derive column names.
         2. Register the CTE as an ephemeral table.
         3. Bind the full def (now the recursive arm can resolve the name).
       For non-recursive CTEs the base-only pre-pass is not needed; we bind
       the full def in one shot as before. *)
    let base_ast = match recursive, def with
      | true, Ast.S_compound { left; _ } -> Some left
      | _ -> None
    in
    let col_source_ast = match base_ast with Some b -> b | None -> def in
    let* col_source_r = bind_internal ~views ~named_params ~param_counter cat col_source_ast in
    (match col_source_r with
     | Error e -> Lwt.return (Error e)
     | Ok col_source ->
       (* Derive column names: prefer AST-level names (which preserve aliases
          for aggregated projections) and fall back to bound-stmt names. *)
       let ast_names = col_names_of_ast_stmt col_source_ast in
       let bound_names = col_names_of_bound_stmt col_source in
       (* Use AST names when available (non-empty), filling gaps from bound names. *)
       let n_cols = List.length bound_names in
       let col_names = List.init n_cols (fun i ->
         if i < List.length ast_names then List.nth ast_names i
         else List.nth bound_names i)
       in
       let cte_cols = List.map (fun col_name ->
         { Row.name         = col_name;
           Row.ty           = Row.Integer;
           Row.not_null     = false;
           Row.primary_key  = false;
           Row.default      = None;
           Row.check_sql    = None;
           Row.generated_as = None;
         }) col_names in
       let cte_meta : Cat.table_meta = {
         Cat.name           = name;
         Cat.tree_id        = -1;
         Cat.columns        = cte_cols;
         Cat.next_rowid     = 0L;
         Cat.fk_constraints = [];
       } in
       Cat.register_ephemeral cat cte_meta;
       (* For non-recursive CTEs col_source already is the fully-bound def —
          reuse it directly.  For recursive CTEs we must bind the full def now
          that the CTE name is in scope (so the recursive arm can resolve it). *)
       let* def_r =
         if recursive then
           bind_internal ~views ~named_params ~param_counter cat def
         else
           Lwt.return (Ok col_source)
       in
       (match def_r with
        | Error e ->
          Cat.unregister_ephemeral cat ~name;
          Lwt.return (Error e)
        | Ok bound_def ->
          let* query_r = bind_internal ~views ~named_params ~param_counter cat query in
          Cat.unregister_ephemeral cat ~name;
          (match query_r with
           | Error e -> Lwt.return (Error e)
           | Ok bound_query ->
             Lwt.return (Ok (BS_with_cte { name; def = bound_def; query = bound_query; recursive })))))
  | Ast.S_create_view { name; query } ->
    let* bound_r = bind_internal ~views ~named_params ~param_counter cat query in
    (match bound_r with
     | Error e -> Lwt.return (Error e)
     | Ok _ -> Lwt.return (Ok (BS_create_view { name; query })))
  | Ast.S_drop_view { name; if_exists = _ } ->
    Lwt.return (Ok (BS_drop_view { name }))
  | Ast.S_create_trigger { name; timing; event; table; when_; body } ->
    Lwt.return (Ok (BS_create_trigger { name; timing; event; table; when_; body }))
  | Ast.S_drop_trigger { name; if_exists = _ } ->
    Lwt.return (Ok (BS_drop_trigger { name }))
  | Ast.S_explain { analyze; stmt = inner_ast } ->
    let* r = bind_internal ~views ~named_params ~param_counter cat inner_ast in
    (match r with
     | Error e -> Lwt.return (Error e)
     | Ok inner -> Lwt.return (Ok (BS_explain { analyze; inner })))
  | Ast.S_compound { op; left; right } ->
    let* left_r  = bind_internal ~views ~named_params ~param_counter cat left  in
    let* right_r = bind_internal ~views ~named_params ~param_counter cat right in
    (match left_r, right_r with
     | Ok l, Ok r   ->
       let n_left  = compound_col_count l in
       let n_right = compound_col_count r in
       if n_left <> n_right then
         Lwt.return (Error (Arity_mismatch { expected = n_left; got = n_right }))
       else
         Lwt.return (Ok (BS_compound { op; left = l; right = r }))
     | Error e, _
     | _, Error e   -> Lwt.return (Error e))

let bind ?(views : (string, Ast.stmt) Hashtbl.t = Hashtbl.create 0) cat ast =
  let named_params : (string, int) Hashtbl.t = Hashtbl.create 4 in
  let param_counter = ref 0 in
  bind_internal ~views ~named_params ~param_counter cat ast

let bind_returning_params ?(views : (string, Ast.stmt) Hashtbl.t = Hashtbl.create 0) cat ast =
  let named_params : (string, int) Hashtbl.t = Hashtbl.create 4 in
  let param_counter = ref 0 in
  let* result = bind_internal ~views ~named_params ~param_counter cat ast in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok bs   ->
    let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) named_params [] in
    Lwt.return (Ok (bs, pairs))
