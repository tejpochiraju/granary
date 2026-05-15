open Lwt.Syntax
module S      = Sqlocaml_store.Store
module Cat    = Sqlocaml_catalog.Catalog
module Sql    = Sqlocaml_sql
module Row    = Sqlocaml_encoding.Row

type t = {
  store   : S.t;
  catalog : Cat.t;
}

type value = Row.value =
  | V_int  of int64
  | V_text of string
  | V_null
  | V_real of float
  | V_blob of bytes

type row = Row.t

type error =
  | Parse   of string
  | Sema    of Sql.Sema.error
  | Runtime of string

let open_in_memory () =
  let store = S.create () in
  let* catalog = Cat.open_ store in
  Lwt.return { store; catalog }

let open_file ~path =
  let* result = S.open_file ~path in
  match result with
  | Error e ->
    let msg = Format.asprintf "%a" S.pp_error e in
    Lwt.return (Error (Runtime msg))
  | Ok store ->
    let* catalog = Cat.open_ store in
    Lwt.return (Ok { store; catalog })

let close t = S.close t.store

let parse sql =
  match
    let lexbuf = Lexing.from_string sql in
    Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
  with
  | stmt        -> Ok stmt
  | exception Sql.Parser.Error -> Error (Parse "syntax error")
  | exception Failure msg      -> Error (Parse msg)

let prepare t sql =
  match parse sql with
  | Error e -> Lwt.return (Error e)
  | Ok ast  ->
    let* bound = Sql.Sema.bind t.catalog ast in
    match bound with
    | Error e -> Lwt.return (Error (Sema e))
    | Ok b    -> Lwt.return (Ok (Sql.Planner.plan ~cat:t.catalog b))

let execute t sql =
  let* op = prepare t sql in
  match op with
  | Error e -> Lwt.return (Error e)
  | Ok op   ->
    (match Sql.Exec.execute t.store t.catalog op with
     | exception Failure msg -> Lwt.return (Error (Runtime msg))
     | lwt_op ->
       Lwt.catch
         (fun () ->
           let* () = lwt_op in
           Lwt.return (Ok ()))
         (function
          | Failure msg -> Lwt.return (Error (Runtime msg))
          | exn         -> Lwt.fail exn))

let execute_change_count t sql =
  let* op = prepare t sql in
  match op with
  | Error e -> Lwt.return (Error e)
  | Ok op   ->
    (match Sql.Exec.execute_with_count t.store t.catalog op with
     | exception Failure msg -> Lwt.return (Error (Runtime msg))
     | lwt_op ->
       Lwt.catch
         (fun () ->
           let* n = lwt_op in
           Lwt.return (Ok n))
         (function
          | Failure msg -> Lwt.return (Error (Runtime msg))
          | exn         -> Lwt.fail exn))

let query t sql =
  let* op = prepare t sql in
  match op with
  | Error e -> Lwt.return (Error e)
  | Ok op   ->
    (match Sql.Exec.query t.store t.catalog op with
     | exception Failure msg -> Lwt.return (Error (Runtime msg))
     | lwt_stream ->
       let* stream = lwt_stream in
       Lwt.return (Ok stream))
