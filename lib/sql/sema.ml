open Lwt.Syntax
module Row = Sqlocaml_encoding.Row
module Cat = Sqlocaml_catalog.Catalog

type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or

type bound_expr =
  | BE_lit         of Ast.literal
  | BE_col         of int
  | BE_binop       of binop * bound_expr * bound_expr
  | BE_not         of bound_expr
  | BE_is_null     of bound_expr
  | BE_is_not_null of bound_expr
  | BE_neg         of bound_expr

type bound_order_key = {
  col_idx : int;
  dir     : Ast.order_dir;
}

(** A bound JOIN clause.  See sema.mli for layout details. *)
type bound_join = {
  kind             : Ast.join_kind;
  right_meta       : Cat.table_meta;
  on               : bound_expr;
  right_col_offset : int;
}

type bound_stmt =
  | BS_create_table of {
      name    : string;
      columns : Row.column list;
    }
  | BS_insert of {
      table_meta : Cat.table_meta;
      ordinals   : int list;
      values     : Ast.literal list;
    }
  | BS_select of {
      table_meta : Cat.table_meta;
      proj       : int list;
      where      : bound_expr option;
      order      : bound_order_key list;
      limit      : int option;
      offset     : int option;
      join       : bound_join option;
    }
  | BS_create_index of {
      name       : string;
      table_meta : Cat.table_meta;
      col_idx    : int;
      unique     : bool;
    }
  | BS_update of {
      table_meta  : Cat.table_meta;
      assignments : (int * bound_expr) list;
      where       : bound_expr option;
    }
  | BS_delete of {
      table_meta : Cat.table_meta;
      where      : bound_expr option;
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

let rec bind_expr (meta : Cat.table_meta) = function
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
    (match bind_expr meta a, bind_expr meta b with
     | Ok ba, Ok bb  -> Ok (BE_binop (ast_binop_to_sema op, ba, bb))
     | Error e, _    -> Error e
     | Ok _,  Error e -> Error e)
  | Ast.E_not e ->
    (match bind_expr meta e with
     | Ok be   -> Ok (BE_not be)
     | Error e -> Error e)
  | Ast.E_is_null e ->
    (match bind_expr meta e with
     | Ok be   -> Ok (BE_is_null be)
     | Error e -> Error e)
  | Ast.E_is_not_null e ->
    (match bind_expr meta e with
     | Ok be   -> Ok (BE_is_not_null be)
     | Error e -> Error e)
  | Ast.E_neg e ->
    (match bind_expr meta e with
     | Ok be   -> Ok (BE_neg be)
     | Error e -> Error e)

(* ------------------------------------------------------------------ *)
(* Two-table column resolution used when a JOIN is present.            *)
(* The combined row layout is [left_cols ... right_cols].              *)
(* Right table ordinal i becomes absolute ordinal [right_offset + i].  *)
(* ------------------------------------------------------------------ *)

let rec bind_expr_join
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
    (match bind_expr_join ~left_meta ~right_meta ~right_offset a,
           bind_expr_join ~left_meta ~right_meta ~right_offset b with
     | Ok ba, Ok bb  -> Ok (BE_binop (ast_binop_to_sema op, ba, bb))
     | Error e, _    -> Error e
     | Ok _,  Error e -> Error e)
  | Ast.E_not e ->
    (match bind_expr_join ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_not be) | Error e -> Error e)
  | Ast.E_is_null e ->
    (match bind_expr_join ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_is_null be) | Error e -> Error e)
  | Ast.E_is_not_null e ->
    (match bind_expr_join ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_is_not_null be) | Error e -> Error e)
  | Ast.E_neg e ->
    (match bind_expr_join ~left_meta ~right_meta ~right_offset e with
     | Ok be -> Ok (BE_neg be) | Error e -> Error e)

(* ------------------------------------------------------------------ *)
(* CREATE TABLE                                                         *)
(* ------------------------------------------------------------------ *)

let bind_create cat ~name ~columns =
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
    Lwt.return (Ok (BS_create_table { name; columns = row_cols }))

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

let bind_insert cat ~table ~columns ~values =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some meta ->
    let n_cols = List.length columns in
    let n_vals = List.length values in
    if n_cols <> n_vals then
      Lwt.return (Error (Arity_mismatch { expected = n_cols; got = n_vals }))
    else
      (* 1. Validate the explicitly-provided columns and build a map
            from column ordinal -> supplied literal. *)
      let explicit_result =
        List.fold_left2 (fun acc col_name lit ->
          match acc with
          | Error _ -> acc
          | Ok map ->
            (match col_index meta.columns col_name with
             | None ->
               Error (Unknown_column { table; column = col_name })
             | Some i ->
               let col = List.nth meta.columns i in
               (match lit_ty lit with
                | None   -> Ok (map @ [(i, lit)])   (* NULL: skip type check *)
                | Some t ->
                  if ty_equal t col.ty then Ok (map @ [(i, lit)])
                  else Error (Type_mismatch { expected = col.ty; got = t })))
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
             | Some lit -> (i, lit)
             | None ->
               (* Not explicitly supplied: use DEFAULT if present, else NULL. *)
               let lit = match col.Row.default with
                 | Some dv -> dv_to_lit dv
                 | None    -> Ast.L_null
               in
               (i, lit))
         in
         let full_values_result : ((int * Ast.literal) list, error) result =
           Ok per_col_results
         in
         (match full_values_result with
          | Error e -> Lwt.return (Error e)
          | Ok full_pairs ->
            (* 3. NOT NULL enforcement: reject if any NOT NULL column has NULL. *)
            let nn_result =
              List.fold_left (fun acc (i, lit) ->
                match acc with
                | Error _ -> acc
                | Ok () ->
                  let col = List.nth meta.columns i in
                  if col.Row.not_null && lit = Ast.L_null then
                    Error (Not_null_violation col.Row.name)
                  else
                    Ok ()
              ) (Ok ()) full_pairs
            in
            (match nn_result with
             | Error e -> Lwt.return (Error e)
             | Ok () ->
               let ordinals = List.map fst full_pairs in
               let full_vals = List.map snd full_pairs in
               Lwt.return (Ok (BS_insert {
                 table_meta = meta;
                 ordinals;
                 values     = full_vals;
               }))))) (* closes Ok, Lwt.return, nn_result match, full_values_result match, explicit_result match *)

(* ------------------------------------------------------------------ *)
(* SELECT                                                               *)
(* ------------------------------------------------------------------ *)

let bind_select cat ~proj ~table ~joins ~where ~order ~limit ~offset =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
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
       let proj_result =
         match proj with
         | `All ->
           (* All columns from both tables, left ++ right. *)
           let left_ords = List.mapi (fun i _ -> i) meta.columns in
           (match join_info with
            | None -> Ok left_ords
            | Some (_jc, rm) ->
              let n_right = List.length rm.columns in
              let right_ords =
                List.init n_right (fun i -> right_offset + i)
              in
              Ok (left_ords @ right_ords))
         | `Cols names ->
           List.fold_left (fun acc name ->
             match acc with
             | Error _ -> acc
             | Ok ords ->
               (match proj_lookup name with
                | Error e -> Error e
                | Ok i    -> Ok (ords @ [i]))
           ) (Ok []) names
       in
       (match proj_result with
        | Error e -> Lwt.return (Error e)
        | Ok proj_ords ->
          (* Bind the JOIN ON predicate (must use two-table resolution). *)
          let bound_join_result : (bound_join option, error) result =
            match join_info with
            | None -> Ok None
            | Some (jc, rm) ->
              (match bind_expr_join
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
               | None -> bind_expr meta e
               | Some (_jc, rm) ->
                 bind_expr_join ~left_meta:meta ~right_meta:rm ~right_offset e
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
                if List.length order > 1 then
                  Lwt.return (Error (Unsupported
                    "ORDER BY with more than one key is not supported in Phase 1"))
                else
                let order_result =
                  List.fold_left (fun acc (ok : Ast.order_key) ->
                    match acc with
                    | Error _ -> acc
                    | Ok keys ->
                      (match proj_lookup ok.col with
                       | Error e -> Error e
                       | Ok i    -> Ok (keys @ [{ col_idx = i; dir = ok.dir }]))
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
                           table_meta = meta;
                           proj       = proj_ords;
                           where      = bound_where;
                           order      = bound_order;
                           limit      = valid_limit;
                           offset     = valid_offset;
                           join       = bound_join;
                         })))))))))

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
     | Add | Sub | Mul | Div ->
       (match infer_type cols a, infer_type cols b with
        | Some Row.Integer, Some Row.Integer -> Some Row.Integer
        | Some Row.Real,    _
        | _,                Some Row.Real    -> Some Row.Real
        | Some Row.Integer, None
        | None,             Some Row.Integer -> None
        | _                                  -> None))
  | BE_neg e -> infer_type cols e

(* ------------------------------------------------------------------ *)
(* CREATE INDEX                                                         *)
(* ------------------------------------------------------------------ *)

let bind_create_index cat ~name ~table ~column ~unique =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some meta ->
    (match col_index meta.columns column with
     | None ->
       Lwt.return (Error (Unknown_column { table; column }))
     | Some i ->
       (match Cat.find_index cat ~name with
        | Some _ -> Lwt.return (Error (Already_exists name))
        | None ->
          Lwt.return (Ok (BS_create_index {
            name;
            table_meta = meta;
            col_idx    = i;
            unique;
          }))))

(* ------------------------------------------------------------------ *)
(* UPDATE                                                               *)
(* ------------------------------------------------------------------ *)

let bind_update cat ~table ~assignments ~where =
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
             (match bind_expr meta expr_ast with
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
           (match bind_expr meta e with
            | Ok be   -> Ok (Some be)
            | Error e -> Error e)
       in
       (match where_result with
        | Error e -> Lwt.return (Error e)
        | Ok bound_where ->
          Lwt.return (Ok (BS_update {
            table_meta  = meta;
            assignments = bound_assigns;
            where       = bound_where;
          }))))

(* ------------------------------------------------------------------ *)
(* DELETE                                                               *)
(* ------------------------------------------------------------------ *)

let bind_delete cat ~table ~where =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some meta ->
    let where_result =
      match where with
      | None   -> Ok None
      | Some e ->
        (match bind_expr meta e with
         | Ok be   -> Ok (Some be)
         | Error e -> Error e)
    in
    (match where_result with
     | Error e -> Lwt.return (Error e)
     | Ok bound_where ->
       Lwt.return (Ok (BS_delete {
         table_meta = meta;
         where      = bound_where;
       })))

(* ------------------------------------------------------------------ *)
(* Public entry point                                                   *)
(* ------------------------------------------------------------------ *)

let bind cat = function
  | Ast.S_create_table { name; columns }                     -> bind_create cat ~name ~columns
  | Ast.S_insert { table; columns; values }                  -> bind_insert cat ~table ~columns ~values
  | Ast.S_select { proj; table; joins; where; order; limit; offset } ->
    bind_select cat ~proj ~table ~joins ~where ~order ~limit ~offset
  | Ast.S_create_index { name; table; column; unique } ->
    bind_create_index cat ~name ~table ~column ~unique
  | Ast.S_update { table; assignments; where } ->
    bind_update cat ~table ~assignments ~where
  | Ast.S_delete { table; where } ->
    bind_delete cat ~table ~where
