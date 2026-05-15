open Lwt.Syntax
module Row = Sqlocaml_encoding.Row
module Cat = Sqlocaml_catalog.Catalog

type bound_expr =
  | BE_lit of Ast.literal
  | BE_col of int
  | BE_eq  of bound_expr * bound_expr

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
    }

type error =
  | Unknown_table  of string
  | Unknown_column of { table : string; column : string }
  | Type_mismatch  of { expected : Row.ty; got : Row.ty }
  | Arity_mismatch of { expected : int; got : int }
  | Already_exists of string

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

let rec bind_expr (meta : Cat.table_meta) = function
  | Ast.E_lit l -> Ok (BE_lit l)
  | Ast.E_col name ->
    (match col_index meta.columns name with
     | None   -> Error (Unknown_column { table = meta.name; column = name })
     | Some i -> Ok (BE_col i))
  | Ast.E_eq (a, b) ->
    (match bind_expr meta a, bind_expr meta b with
     | Ok ba, Ok bb  -> Ok (BE_eq (ba, bb))
     | Error e, _    -> Error e
     | Ok _,  Error e -> Error e)

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

let bind_select cat ~proj ~table ~where =
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
          Lwt.return (Ok (BS_select {
            table_meta = meta;
            proj       = proj_ords;
            where      = bound_where;
          }))))

(* ------------------------------------------------------------------ *)
(* Public entry point                                                   *)
(* ------------------------------------------------------------------ *)

let bind cat = function
  | Ast.S_create_table { name; columns }         -> bind_create cat ~name ~columns
  | Ast.S_insert { table; columns; values }       -> bind_insert cat ~table ~columns ~values
  | Ast.S_select { proj; table; where }           -> bind_select cat ~proj ~table ~where
