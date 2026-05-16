open Lwt.Syntax
module S      = Sqlocaml_store.Store
module Cat    = Sqlocaml_catalog.Catalog
module Sql    = Sqlocaml_sql
module Row    = Sqlocaml_encoding.Row

type t = {
  store            : S.t;
  catalog          : Cat.t;
  mutable explicit_txn : S.rw S.txn option;
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
  Lwt.return { store; catalog; explicit_txn = None }

let open_file ~path =
  let* result = S.open_file ~path in
  match result with
  | Error e ->
    let msg = Format.asprintf "%a" S.pp_error e in
    Lwt.return (Error (Runtime msg))
  | Ok store ->
    let* catalog = Cat.open_ store in
    Lwt.return (Ok { store; catalog; explicit_txn = None })

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

(* ------------------------------------------------------------------ *)
(* Explicit transaction management                                      *)
(* ------------------------------------------------------------------ *)

let begin_txn t =
  match t.explicit_txn with
  | Some _ -> Lwt.return (Error (Runtime "transaction already active"))
  | None ->
    let* tx = S.rw_begin t.store in
    t.explicit_txn <- Some tx;
    Lwt.return (Ok ())

let commit_txn t =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction"))
  | Some tx ->
    let* () = S.commit tx in
    t.explicit_txn <- None;
    Lwt.return (Ok ())

let rollback_txn t =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction"))
  | Some tx ->
    let* () = S.rollback tx in
    t.explicit_txn <- None;
    Lwt.return (Ok ())

(* ------------------------------------------------------------------ *)
(* Public execute / query API                                           *)
(* ------------------------------------------------------------------ *)

let execute t sql =
  let* op = prepare t sql in
  match op with
  | Error e -> Lwt.return (Error e)
  | Ok Sql.Plan.Op_begin    -> begin_txn t
  | Ok Sql.Plan.Op_commit   -> commit_txn t
  | Ok Sql.Plan.Op_rollback -> rollback_txn t
  | Ok op ->
    (* SELECT always uses snapshot reads inside exec.ml (ro_begin/ro_end),
       so it reads committed state regardless of an active explicit txn.
       For DML, pass In_txn when an explicit transaction is open so all
       writes join the same atomic context.
       Note on SELECT within explicit txn: SELECTs always read the last committed
       state (snapshot isolation), not in-progress writes from the current txn.
       This is a known Phase 3 limitation — read-your-own-writes deferred to Phase 4. *)
    let mode = match t.explicit_txn with
      | None    -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    (match Sql.Exec.execute ~mode t.store t.catalog op with
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
  | Ok Sql.Plan.Op_begin    ->
    let* r = begin_txn t in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  | Ok Sql.Plan.Op_commit   ->
    let* r = commit_txn t in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  | Ok Sql.Plan.Op_rollback ->
    let* r = rollback_txn t in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  | Ok op ->
    let mode = match t.explicit_txn with
      | None    -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    (match Sql.Exec.execute_with_count ~mode t.store t.catalog op with
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
