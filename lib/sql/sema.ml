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

type error =
  | Unknown_table  of string
  | Unknown_column of { table : string; column : string }
  | Type_mismatch  of { expected : Row.ty; got : Row.ty }
  | Arity_mismatch of { expected : int; got : int }
  | Already_exists of string
  | Invalid_limit  of string
  | Unsupported    of string

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
(* CREATE TABLE                                                         *)
(* ------------------------------------------------------------------ *)

let bind_create cat ~name ~columns =
  let* existing = Cat.find_table cat ~name in
  match existing with
  | Some _ -> Lwt.return (Error (Already_exists name))
  | None ->
    let row_cols = List.map (fun (c : Ast.column_def) ->
      Row.{ name = c.name;
            ty   = (match c.ty with
                    | Ast.Ty_int  -> Row.Integer
                    | Ast.Ty_text -> Row.Text
                    | Ast.Ty_real -> Row.Real
                    | Ast.Ty_blob -> Row.Blob) }
    ) columns in
    Lwt.return (Ok (BS_create_table { name; columns = row_cols }))

(* ------------------------------------------------------------------ *)
(* INSERT                                                               *)
(* ------------------------------------------------------------------ *)

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
      let result =
        List.fold_left2 (fun acc col_name lit ->
          match acc with
          | Error _ -> acc
          | Ok ords ->
            (match col_index meta.columns col_name with
             | None ->
               Error (Unknown_column { table; column = col_name })
             | Some i ->
               let col = List.nth meta.columns i in
               (match lit_ty lit with
                | None   -> Ok (ords @ [i])   (* NULL: skip type check *)
                | Some t ->
                  if ty_equal t col.ty then Ok (ords @ [i])
                  else Error (Type_mismatch { expected = col.ty; got = t })))
        ) (Ok []) columns values
      in
      (match result with
       | Error e    -> Lwt.return (Error e)
       | Ok ordinals ->
         Lwt.return (Ok (BS_insert { table_meta = meta; ordinals; values })))

(* ------------------------------------------------------------------ *)
(* SELECT                                                               *)
(* ------------------------------------------------------------------ *)

let bind_select cat ~proj ~table ~where ~order ~limit ~offset =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some meta ->
    let proj_result =
      match proj with
      | `All ->
        Ok (List.mapi (fun i _ -> i) meta.columns)
      | `Cols names ->
        List.fold_left (fun acc name ->
          match acc with
          | Error _ -> acc
          | Ok ords ->
            (match col_index meta.columns name with
             | None   -> Error (Unknown_column { table; column = name })
             | Some i -> Ok (ords @ [i]))
        ) (Ok []) names
    in
    (match proj_result with
     | Error e -> Lwt.return (Error e)
     | Ok proj_ords ->
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
          (* Phase 1: single-column ORDER BY only *)
          if List.length order > 1 then
            Lwt.return (Error (Unsupported
              "ORDER BY with more than one key is not supported in Phase 1"))
          else
          (* Validate and bind ORDER BY columns *)
          let order_result =
            List.fold_left (fun acc (ok : Ast.order_key) ->
              match acc with
              | Error _ -> acc
              | Ok keys ->
                (match col_index meta.columns ok.col with
                 | None   -> Error (Unknown_column { table; column = ok.col })
                 | Some i -> Ok (keys @ [{ col_idx = i; dir = ok.dir }]))
            ) (Ok []) order
          in
          (match order_result with
           | Error e -> Lwt.return (Error e)
           | Ok bound_order ->
             (* Validate LIMIT/OFFSET are non-negative *)
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
                   })))))))

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
       and check that the inferred expression type matches the column. *)
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
(* Public entry point                                                   *)
(* ------------------------------------------------------------------ *)

let bind cat = function
  | Ast.S_create_table { name; columns }                     -> bind_create cat ~name ~columns
  | Ast.S_insert { table; columns; values }                  -> bind_insert cat ~table ~columns ~values
  | Ast.S_select { proj; table; where; order; limit; offset } ->
    bind_select cat ~proj ~table ~where ~order ~limit ~offset
  | Ast.S_create_index { name; table; column; unique } ->
    bind_create_index cat ~name ~table ~column ~unique
  | Ast.S_update { table; assignments; where } ->
    bind_update cat ~table ~assignments ~where
