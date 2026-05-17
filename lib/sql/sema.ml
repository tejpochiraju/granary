open Lwt.Syntax
module Row = Sqlocaml_encoding.Row
module Cat = Sqlocaml_catalog.Catalog

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

type bound_order_key = {
  key : bound_expr;
  dir : Ast.order_dir;
}

type agg_spec = {
  func    : Ast.agg_func;
  col_ord : int option;
}

type agg_proj_item =
  | AP_group_col
  | AP_agg_slot of int

(** A bound JOIN clause.  See sema.mli for layout details. *)
type bound_join = {
  kind             : Ast.join_kind;
  right_meta       : Cat.table_meta;
  on               : bound_expr;
  right_col_offset : int;
}

type bound_stmt =
  | BS_create_table of {
      name      : string;
      columns   : Row.column list;
      uniq_idxs : (string * string list) list;
    }
  | BS_insert of {
      table_meta  : Cat.table_meta;
      ordinals    : int list;
      values      : bound_expr list;
      on_conflict : Ast.conflict_action option;
      returning   : bound_expr list;
    }
  | BS_select of {
      distinct   : bool;
      table_meta : Cat.table_meta;
      proj       : int list;
      expr_proj  : bound_expr list;
        (** Non-empty when projection contains scalar functions (Phase 5).
            When non-empty, [proj] is empty and [expr_proj] governs the
            output columns. *)
      where      : bound_expr option;
      order      : bound_order_key list;
      limit      : int option;
      offset     : int option;
      join       : bound_join option;
      group_by   : int option;
      aggs       : agg_spec list;
      having     : bound_expr option;
      agg_proj   : agg_proj_item list;
    }
  | BS_create_index of {
      name       : string;
      table_meta : Cat.table_meta;
      col_idxs   : int list;
      unique     : bool;
    }
  | BS_update of {
      table_meta  : Cat.table_meta;
      assignments : (int * bound_expr) list;
      where       : bound_expr option;
      returning   : bound_expr list;
    }
  | BS_delete of {
      table_meta : Cat.table_meta;
      where      : bound_expr option;
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
          primary_key = false; default = None }
  ) m.Cat.fts_columns in
  { Cat.name       = m.Cat.fts_name;
    Cat.tree_id    = m.Cat.fts_content_tree;
    Cat.columns;
    Cat.next_rowid = 0L }

let lit_ty = function
  | Ast.L_int _  -> Some Row.Integer
  | Ast.L_text _ -> Some Row.Text
  | Ast.L_null   -> None   (* NULL is compatible with any column *)
  | Ast.L_real _ -> Some Row.Real
  | Ast.L_blob _ -> Some Row.Blob

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
       in
       if not arity_ok then
         Error (Arity_mismatch { expected = (match func with Ast.Fn_ifnull -> 2 | _ -> 1); got = n })
       else
         Ok (BE_func (func, ok_args)))
  | Ast.E_match _ ->
    Error (Unsupported "MATCH is only valid as a top-level WHERE clause on FTS tables")

(* ------------------------------------------------------------------ *)
(* Two-table column resolution used when a JOIN is present.            *)
(* The combined row layout is [left_cols ... right_cols].              *)
(* Right table ordinal i becomes absolute ordinal [right_offset + i].  *)
(* ------------------------------------------------------------------ *)

let rec bind_expr_join
    ~param_counter
    ~named_params
    ~(left_meta : Cat.table_meta)
    ~(right_meta : Cat.table_meta)
    ~(right_offset : int)
  = function
  | Ast.E_lit l -> Ok (BE_lit l)
  | Ast.E_col name ->
    let in_left  = col_index left_meta.columns  name in
    let in_right = col_index right_meta.columns name in
    (match in_left, in_right with
     | Some _, Some _ -> Error (Ambiguous_column name)
     | Some i, None   -> Ok (BE_col i)
     | None,   Some i -> Ok (BE_col (right_offset + i))
     | None,   None   ->
       (* Report unknown_column against the left table for consistency. *)
       Error (Unknown_column { table = left_meta.name; column = name }))
  | Ast.E_tbl_col (tbl, name) ->
    if String.equal tbl left_meta.name then
      (match col_index left_meta.columns name with
       | Some i -> Ok (BE_col i)
       | None   -> Error (Unknown_column { table = tbl; column = name }))
    else if String.equal tbl right_meta.name then
      (match col_index right_meta.columns name with
       | Some i -> Ok (BE_col (right_offset + i))
       | None   -> Error (Unknown_column { table = tbl; column = name }))
    else
      Error (Unknown_table tbl)
  | Ast.E_binop (op, a, b) ->
    (match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset a,
           bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset b with
     | Ok ba, Ok bb  -> Ok (BE_binop (ast_binop_to_sema op, ba, bb))
     | Error e, _    -> Error e
     | Ok _,  Error e -> Error e)
  | Ast.E_not e ->
    (match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_not be) | Error e -> Error e)
  | Ast.E_is_null e ->
    (match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_is_null be) | Error e -> Error e)
  | Ast.E_is_not_null e ->
    (match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_is_not_null be) | Error e -> Error e)
  | Ast.E_neg e ->
    (match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_neg be) | Error e -> Error e)
  | Ast.E_bitnot e ->
    (match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_bitnot be) | Error e -> Error e)
  | Ast.E_between (x, lo, hi) ->
    (match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset x,
           bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset lo,
           bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset hi with
     | Ok bx, Ok blo, Ok bhi -> Ok (BE_between (bx, blo, bhi))
     | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | Ast.E_in (x, vals) ->
    let bx = bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset x in
    let bvals = List.map (bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset) vals in
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
    let bound = List.map (bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset) args in
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
       in
       if not arity_ok then
         Error (Arity_mismatch { expected = (match func with Ast.Fn_ifnull -> 2 | _ -> 1); got = n })
       else
         Ok (BE_func (func, ok_args)))
  | Ast.E_match _ ->
    Error (Unsupported "MATCH in JOIN context")

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
         in
         if not arity_ok then
           Error (Arity_mismatch { expected = (match func with Ast.Fn_ifnull -> 2 | _ -> 1); got = n })
         else
           Ok (BE_func (func, ok_args)))
    | Ast.E_match _ ->
      Error (Unsupported "MATCH is only valid as a top-level WHERE clause on FTS tables")
  in
  match go e with
  | Error e -> Error e
  | Ok be   -> Ok (be, !aggs)

(** Check if any [E_agg] appears anywhere in an [expr]. *)
let rec expr_has_agg = function
  | Ast.E_agg _ -> true
  | Ast.E_lit _ | Ast.E_col _ | Ast.E_tbl_col _ | Ast.E_param _ | Ast.E_match _ -> false
  | Ast.E_binop (_, a, b) -> expr_has_agg a || expr_has_agg b
  | Ast.E_not e | Ast.E_is_null e | Ast.E_is_not_null e | Ast.E_neg e | Ast.E_bitnot e ->
    expr_has_agg e
  | Ast.E_between (x, lo, hi) -> expr_has_agg x || expr_has_agg lo || expr_has_agg hi
  | Ast.E_in (x, vals) -> expr_has_agg x || List.exists expr_has_agg vals
  | Ast.E_func (_, args) -> List.exists expr_has_agg args

(* ------------------------------------------------------------------ *)
(* CREATE TABLE                                                         *)
(* ------------------------------------------------------------------ *)

let bind_create cat ~name ~columns ~constraints =
  let* existing = Cat.find_table cat ~name in
  match existing with
  | Some _ -> Lwt.return (Error (Already_exists name))
  | None ->
    let ast_lit_to_dv : Ast.literal -> Row.default_value = function
      | Ast.L_int  n -> Row.DV_int n
      | Ast.L_text s -> Row.DV_text s
      | Ast.L_null   -> Row.DV_null
      | Ast.L_real f -> Row.DV_real f
      | Ast.L_blob b -> Row.DV_blob b
    in
    let row_cols = List.map (fun (c : Ast.column_def) ->
      Row.{ name        = c.name;
            ty          = (match c.ty with
                           | Ast.Ty_int  -> Row.Integer
                           | Ast.Ty_text -> Row.Text
                           | Ast.Ty_real -> Row.Real
                           | Ast.Ty_blob -> Row.Blob);
            not_null    = c.not_null;
            primary_key = c.primary_key;
            default     = Option.map ast_lit_to_dv c.default }
    ) columns in
    (* Generate auto-UNIQUE index specs for table-level constraints *)
    let uniq_idxs = List.mapi (fun i tc ->
      match tc with
      | Ast.TC_unique cols ->
        let idx_name = Printf.sprintf "__uniq_%s_%s_%d"
            name (String.concat "_" cols) i in
        (idx_name, cols)
      | Ast.TC_primary_key cols ->
        let idx_name = Printf.sprintf "__pk_%s_%s_%d"
            name (String.concat "_" cols) i in
        (idx_name, cols)
    ) constraints in
    Lwt.return (Ok (BS_create_table { name; columns = row_cols; uniq_idxs }))

(* ------------------------------------------------------------------ *)
(* INSERT                                                               *)
(* ------------------------------------------------------------------ *)

(** Convert a [Row.default_value] to an [Ast.literal]. *)
let dv_to_lit : Row.default_value -> Ast.literal = function
  | Row.DV_int  n -> Ast.L_int n
  | Row.DV_text s -> Ast.L_text s
  | Row.DV_null   -> Ast.L_null
  | Row.DV_real f -> Ast.L_real f
  | Row.DV_blob b -> Ast.L_blob b

let bind_fts_insert cat ~param_counter ~named_params ~table ~columns ~values =
  match Cat.find_fts cat table with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some fts_meta ->
    let fts_cols = fts_meta.Cat.fts_columns in
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
       | Ok be   -> Ok (bexprs @ [be]))
  ) (Ok []) exprs

let bind_insert cat ~param_counter ~named_params ~table ~columns ~values ~on_conflict ~returning =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None ->
    (* Not a regular table — check if it's an FTS table *)
    bind_fts_insert cat ~param_counter ~named_params ~table ~columns ~values
  | Some meta ->
    let columns =
      if columns = [] then List.map (fun c -> c.Row.name) meta.columns
      else columns
    in
    let n_cols = List.length columns in
    let n_vals = List.length values in
    if n_cols <> n_vals then
      Lwt.return (Error (Arity_mismatch { expected = n_cols; got = n_vals }))
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
        ) (Ok []) columns values
      in
      (match explicit_result with
       | Error e -> Lwt.return (Error e)
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
               let lit = match col.Row.default with
                 | Some dv -> dv_to_lit dv
                 | None    -> Ast.L_null
               in
               (i, BE_lit lit))
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
          | Error e -> Lwt.return (Error e)
          | Ok () ->
            let ordinals = List.map fst full_pairs in
            let full_vals = List.map snd full_pairs in
            (match bind_returning_exprs ~param_counter ~named_params meta returning with
             | Error e -> Lwt.return (Error e)
             | Ok ret_bound ->
               Lwt.return (Ok (BS_insert {
                 table_meta = meta;
                 ordinals;
                 values     = full_vals;
                 on_conflict;
                 returning  = ret_bound;
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
            let (col_ords, include_rank) = match proj with
              | `All ->
                (* SELECT * from FTS: no explicit rank requested *)
                (all_real_ords, false)
              | `Cols names ->
                let has_rank = List.exists (String.equal "rank") names in
                let real_ords = List.filter_map (fun name ->
                  if String.equal name "rank" then None
                  else
                    let rec find i = function
                      | [] -> None
                      | c :: _ when String.equal c name -> Some i
                      | _ :: rest -> find (i+1) rest
                    in
                    find 0 fts_meta.Cat.fts_columns
                ) names in
                (real_ords, has_rank)
              | `Exprs _ ->
                (* Expression projections: treat as SELECT * with no rank *)
                (all_real_ords, false)
            in
            Lwt.return (Ok (BS_fts_match_scan {
              fts_meta;
              query = q;
              proj  = col_ords;
              include_rank;
            })))
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

let bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~joins ~where ~group_by ~having ~order ~limit ~offset =
  let* meta_opt = Cat.find_table cat ~name:table in
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
    (* Phase 2: support a single JOIN clause. *)
    if List.length joins > 1 then
      Lwt.return (Error (Unsupported
        "more than one JOIN clause is not supported in Phase 2"))
    else
    let* join_meta_result =
      match joins with
      | [] -> Lwt.return (Ok None)
      | [ (jc : Ast.join_clause) ] ->
        let* rm_opt = Cat.find_table cat ~name:jc.table in
        (match rm_opt with
         | None -> Lwt.return (Error (Unknown_table jc.table))
         | Some rm -> Lwt.return (Ok (Some (jc, rm))))
      | _ -> Lwt.return (Ok None)   (* unreachable due to length check *)
    in
    (match join_meta_result with
     | Error e -> Lwt.return (Error e)
     | Ok join_info ->
       let n_left = List.length meta.columns in
       let right_offset = n_left in
       (* Combined-row column lookup with full error reporting (Ambiguous,
          Unknown).  Used for proj and ORDER BY name resolution. *)
       let proj_lookup name : (int, error) result =
         match join_info with
         | None ->
           (match col_index meta.columns name with
            | None   -> Error (Unknown_column { table; column = name })
            | Some i -> Ok i)
         | Some (_jc, rm) ->
           let in_left  = col_index meta.columns name in
           let in_right = col_index rm.columns    name in
           (match in_left, in_right with
            | Some _, Some _ -> Error (Ambiguous_column name)
            | Some i, None   -> Ok i
            | None,   Some i -> Ok (right_offset + i)
            | None,   None   -> Error (Unknown_column { table; column = name }))
       in
       let qual_lookup t c : (int, error) result =
         match join_info with
         | None ->
           if String.equal t meta.name then
             (match col_index meta.columns c with
              | Some i -> Ok i
              | None -> Error (Unknown_column { table = t; column = c }))
           else Error (Unknown_table t)
         | Some (_jc, rm) ->
           if String.equal t meta.name then
             (match col_index meta.columns c with
              | Some i -> Ok i
              | None -> Error (Unknown_column { table = t; column = c }))
           else if String.equal t rm.Cat.name then
             (match col_index rm.columns c with
              | Some i -> Ok (right_offset + i)
              | None -> Error (Unknown_column { table = t; column = c }))
           else Error (Unknown_table t)
       in
       (* Detect whether this is an aggregated query: any aggregate in
          projection or HAVING, or GROUP BY present. *)
       let proj_has_agg =
         match proj with
         | `All | `Cols _ -> false
         | `Exprs es -> List.exists expr_has_agg es
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
       (* Bind GROUP BY column (only first column supported in Phase 2). *)
       let group_col_result : (int option, error) result =
         match group_by with
         | [] -> Ok None
         | [name] ->
           (match proj_lookup name with
            | Error e -> Error e
            | Ok i -> Ok (Some i))
         | _ -> Error (Unsupported "GROUP BY with more than one column is not supported in Phase 2")
       in
       (match group_col_result with
        | Error e -> Lwt.return (Error e)
        | Ok group_col ->
       let offset_for_aggs = match group_col with Some _ -> 1 | None -> 0 in
       (* Build proj/agg_proj.
          The result tuple is (col_ordinals, agg_proj, agg_specs, expr_proj).
          [expr_proj] is non-empty only for scalar-function projections
          (Phase 5); [col_ordinals] is empty in that case. *)
       let proj_result :
           (int list * agg_proj_item list * agg_spec list * bound_expr list,
            error) result =
         if not is_aggregated then
           (* Ordinary SELECT — keep behaviour identical to pre-Task-6. *)
           let bind_one e =
             match join_info with
             | None -> bind_expr ~param_counter ~named_params meta e
             | Some (_jc, rm) ->
               bind_expr_join ~param_counter ~named_params ~left_meta:meta ~right_meta:rm ~right_offset e
           in
           let ords_result =
             match proj with
             | `All ->
               let left_ords = List.mapi (fun i _ -> i) meta.columns in
               (match join_info with
                | None -> Ok (`Ords left_ords)
                | Some (_jc, rm) ->
                  let n_right = List.length rm.columns in
                  let right_ords = List.init n_right (fun i -> right_offset + i) in
                  Ok (`Ords (left_ords @ right_ords)))
             | `Cols names ->
               List.fold_left (fun acc name ->
                 match acc with
                 | Error _ -> acc
                 | Ok (`Exprs _) -> acc  (* shouldn't reach here *)
                 | Ok (`Ords ords) ->
                   (match proj_lookup name with
                    | Error e -> Error e
                    | Ok i    -> Ok (`Ords (ords @ [i])))
               ) (Ok (`Ords [])) names
             | `Exprs es ->
               (* Phase 5: scalar function (or general expr) projection.
                  Bind each expression; return as BE list. *)
               let bound_list = List.map bind_one es in
               let errors = List.filter_map
                 (function Error e -> Some e | Ok _ -> None) bound_list in
               (match errors with
                | e :: _ -> Error e
                | [] ->
                  Ok (`Exprs (List.filter_map
                    (function Ok e -> Some e | Error _ -> None) bound_list)))
           in
           (match ords_result with
            | Error e -> Error e
            | Ok (`Ords o)    -> Ok (o, [], [], [])
            | Ok (`Exprs bes) -> Ok ([], [], [], bes))
         else begin
           (* Aggregated SELECT — build agg_proj and aggs list. *)
           (* Helper: walk an expression that is an explicit projection
              item.  For a bare column reference, produce a non-agg slot;
              for an aggregate, produce an AP_agg_slot. *)
           let acc_aggs = ref [] in
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
                  (* Must match the GROUP BY column. *)
                  (match group_col with
                   | Some gc when gc = i -> Ok AP_group_col
                   | _ -> Error (Unsupported (Printf.sprintf
                                  "column '%s' must appear in GROUP BY clause" name))))
             | Ast.E_tbl_col (t, c) ->
               (match qual_lookup t c with
                | Error e -> Error e
                | Ok i ->
                  (match group_col with
                   | Some gc when gc = i -> Ok AP_group_col
                   | _ -> Error (Unsupported (Printf.sprintf
                                  "column '%s.%s' must appear in GROUP BY clause" t c))))
             | Ast.E_agg (func, arg_opt) ->
               (* SUM/AVG type check. *)
               let validate_numeric col_ord =
                 let cols =
                   match join_info with
                   | None -> meta.columns
                   | Some (_jc, rm) -> meta.columns @ rm.Cat.columns
                 in
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
               (match group_col with
                | Some _ -> [] (* unused — won't reach here *)
                | None -> [])
             | `Cols names -> List.map (fun n -> Ast.E_col n) names
             | `Exprs es -> es
           in
           if exprs_to_project = [] && proj = `All && is_aggregated then
             Error (Unsupported "SELECT * with aggregates requires explicit columns")
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
             (match agg_proj_result with
              | Error e -> Error e
              | Ok items -> Ok ([], items, !acc_aggs, []))
         end
       in
       (match proj_result with
        | Error e -> Lwt.return (Error e)
        | Ok (proj_ords, agg_proj_items, proj_aggs, proj_exprs) ->
          (* Bind the JOIN ON predicate (must use two-table resolution). *)
          let bound_join_result : (bound_join option, error) result =
            match join_info with
            | None -> Ok None
            | Some (jc, rm) ->
              (match bind_expr_join
                       ~param_counter
                       ~named_params
                       ~left_meta:meta
                       ~right_meta:rm
                       ~right_offset jc.Ast.on with
               | Error e -> Error e
               | Ok be ->
                 Ok (Some { kind = jc.Ast.kind;
                            right_meta = rm;
                            on = be;
                            right_col_offset = right_offset }))
          in
          (match bound_join_result with
           | Error e -> Lwt.return (Error e)
           | Ok bound_join ->
             let bind_combined e =
               match join_info with
               | None -> bind_expr ~param_counter ~named_params meta e
               | Some (_jc, rm) ->
                 bind_expr_join ~param_counter ~named_params ~left_meta:meta ~right_meta:rm ~right_offset e
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
                         requires the column to be the GROUP BY column
                         (resolves to slot 0 = group col), else error. *)
                      let having_resolver_unqual name =
                        match proj_lookup name with
                        | Error e -> Error e
                        | Ok i ->
                          (match group_col with
                           | Some gc when gc = i -> Ok 0
                           | _ -> Error (Unsupported (Printf.sprintf
                                          "HAVING references non-grouped column '%s'" name)))
                      in
                      let having_resolver_qual t c =
                        match qual_lookup t c with
                        | Error e -> Error e
                        | Ok i ->
                          (match group_col with
                           | Some gc when gc = i -> Ok 0
                           | _ -> Error (Unsupported (Printf.sprintf
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
                let order_result =
                  List.fold_left (fun acc (ok : Ast.order_key) ->
                    match acc with
                    | Error _ -> acc
                    | Ok keys ->
                      let bound_e =
                        match join_info with
                        | None ->
                          bind_expr ~param_counter ~named_params meta ok.Ast.expr
                        | Some (_jc, rm) ->
                          bind_expr_join ~param_counter ~named_params ~left_meta:meta
                            ~right_meta:rm ~right_offset ok.Ast.expr
                      in
                      (match bound_e with
                       | Error e -> Error e
                       | Ok key  -> Ok (keys @ [{ key; dir = ok.Ast.dir }]))
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
                           join       = bound_join;
                           group_by   = group_col;
                           aggs       = all_aggs;
                           having     = bound_having;
                           agg_proj   = agg_proj_items;
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

(* ------------------------------------------------------------------ *)
(* CREATE INDEX                                                         *)
(* ------------------------------------------------------------------ *)

let bind_create_index cat ~name ~table ~columns ~unique =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some meta ->
    let col_idxs_r = List.map (fun col ->
      match col_index meta.columns col with
      | None -> Error (Unknown_column { table; column = col })
      | Some i -> Ok i
    ) columns in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) col_idxs_r in
    (match errors with
     | e :: _ -> Lwt.return (Error e)
     | [] ->
       let col_idxs = List.filter_map (function Ok i -> Some i | Error _ -> None) col_idxs_r in
       (match Cat.find_index cat ~name with
        | Some _ -> Lwt.return (Error (Already_exists name))
        | None ->
          Lwt.return (Ok (BS_create_index {
            name;
            table_meta = meta;
            col_idxs;
            unique;
          }))))

(* ------------------------------------------------------------------ *)
(* UPDATE                                                               *)
(* ------------------------------------------------------------------ *)

let bind_update cat ~param_counter ~named_params ~table ~assignments ~where ~returning =
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
          (match bind_returning_exprs ~param_counter ~named_params meta returning with
           | Error e -> Lwt.return (Error e)
           | Ok ret_bound ->
             Lwt.return (Ok (BS_update {
               table_meta  = meta;
               assignments = bound_assigns;
               where       = bound_where;
               returning   = ret_bound;
             })))))

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

let bind_delete cat ~param_counter ~named_params ~table ~where ~returning =
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
       (match bind_returning_exprs ~param_counter ~named_params meta returning with
        | Error e -> Lwt.return (Error e)
        | Ok ret_bound ->
          Lwt.return (Ok (BS_delete {
            table_meta = meta;
            where      = bound_where;
            returning  = ret_bound;
          }))))

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
       else Lwt.return (Ok (BS_alter_table { table_meta; action })))

(* ------------------------------------------------------------------ *)
(* DROP TABLE                                                           *)
(* ------------------------------------------------------------------ *)

let bind_drop_table cat ~name =
  let* meta_opt = Cat.find_table cat ~name in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table name))
  | Some table_meta ->
    Lwt.return (Ok (BS_drop_table { name; table_meta }))

(* ------------------------------------------------------------------ *)
(* DROP INDEX                                                           *)
(* ------------------------------------------------------------------ *)

let bind_drop_index cat ~name =
  match Cat.find_index cat ~name with
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
  | _ -> 0  (* non-select stmts in compound: don't validate *)

let rec bind_internal ~named_params ~param_counter cat stmt =
  match stmt with
  | Ast.S_create_table { name; columns; constraints }        -> bind_create cat ~name ~columns ~constraints
  | Ast.S_insert { table; columns; values; on_conflict; returning } -> bind_insert cat ~param_counter ~named_params ~table ~columns ~values ~on_conflict ~returning
  | Ast.S_select { distinct; proj; table; joins; where; group_by; having; order; limit; offset } ->
    bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~joins ~where ~group_by ~having ~order ~limit ~offset
  | Ast.S_create_index { name; table; columns; unique } ->
    bind_create_index cat ~name ~table ~columns ~unique
  | Ast.S_update { table; assignments; where; returning } ->
    bind_update cat ~param_counter ~named_params ~table ~assignments ~where ~returning
  | Ast.S_delete { table; where; returning } ->
    bind_delete cat ~param_counter ~named_params ~table ~where ~returning
  | Ast.S_drop_table { name } ->
    bind_drop_table cat ~name
  | Ast.S_drop_index { name } ->
    bind_drop_index cat ~name
  | Ast.S_alter_table { table; action } ->
    bind_alter_table cat ~table ~action
  | Ast.S_begin    -> Lwt.return (Ok BS_begin)
  | Ast.S_commit   -> Lwt.return (Ok BS_commit)
  | Ast.S_rollback -> Lwt.return (Ok BS_rollback)
  | Ast.S_create_fts_table { name; columns } ->
    let* tbl = Cat.find_table cat ~name in
    let fts_existing = Cat.find_fts cat name in
    (match tbl, fts_existing with
     | Some _, _ | _, Some _ -> Lwt.return (Error (Already_exists name))
     | None, None ->
       Lwt.return (Ok (BS_create_fts_table { name; columns })))
  | Ast.S_pragma kind -> Lwt.return (Ok (BS_pragma { kind }))
  | Ast.S_compound { op; left; right } ->
    let* left_r  = bind_internal ~named_params ~param_counter cat left  in
    let* right_r = bind_internal ~named_params ~param_counter cat right in
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

let bind cat ast =
  let named_params : (string, int) Hashtbl.t = Hashtbl.create 4 in
  let param_counter = ref 0 in
  bind_internal ~named_params ~param_counter cat ast

let bind_returning_params cat ast =
  let named_params : (string, int) Hashtbl.t = Hashtbl.create 4 in
  let param_counter = ref 0 in
  let* result = bind_internal ~named_params ~param_counter cat ast in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok bs   ->
    let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) named_params [] in
    Lwt.return (Ok (bs, pairs))
