open Lwt.Syntax
module S      = Sqlocaml_store.Store
module Cat    = Sqlocaml_catalog.Catalog
module Sql    = Sqlocaml_sql
module Row    = Sqlocaml_encoding.Row

type t = {
  store            : S.t;
  catalog          : Cat.t;
  clock            : (unit -> float) option;
  mutable explicit_txn    : S.rw S.txn option;
  views            : (string, Sql.Ast.stmt) Hashtbl.t;
  triggers         : (string, Sql.Ast.stmt) Hashtbl.t;  (* trigger_name -> S_create_trigger AST *)
  mutable savepoint_names : string list;  (* active savepoints, newest first *)
  mutable auto_began      : bool;         (* txn started implicitly by SAVEPOINT *)
}

type value = Row.value =
  | V_int  of int64
  | V_text of string
  | V_null
  | V_real of float
  | V_blob of bytes

type row = Row.t

(** In-memory representation of a trigger, extracted from S_create_trigger AST. *)
type trigger_meta = {
  trig_table  : string;
  trig_timing : [ `Before | `After | `Instead_of ];
  trig_event  : [ `Insert | `Update | `Delete ];
  trig_when   : Sql.Ast.expr option;
  trig_body   : Sql.Ast.stmt list;
}

type error =
  | Parse   of string
  | Sema    of Sql.Sema.error
  | Runtime of string

let open_in_memory ?clock () =
  let store = S.create () in
  let* catalog = Cat.open_ store in
  Lwt.return { store; catalog; clock; explicit_txn = None; views = Hashtbl.create 4;
             triggers = Hashtbl.create 4;
             savepoint_names = []; auto_began = false }

let load_views_into_hashtbl store views_tbl =
  let* pairs = Cat.load_all_views store in
  List.iter (fun (name, sql) ->
    (match
      let lexbuf = Lexing.from_string sql in
      Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
    with
    | Sql.Ast.S_create_view { query; _ } ->
      Hashtbl.replace views_tbl name query
    | _ ->
      Printf.eprintf "warning: skipping non-view SQL in sys_views (name: %s)\n%!" name
    | exception _ -> ())
  ) pairs;
  Lwt.return_unit

let load_triggers_into_hashtbl store trig_tbl =
  let* pairs = Cat.load_all_triggers store in
  List.iter (fun (name, sql) ->
    (match
       let lexbuf = Lexing.from_string sql in
       Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
     with
     | Sql.Ast.S_create_trigger _ as ast ->
       Hashtbl.replace trig_tbl name ast
     | _ ->
       Printf.eprintf "warning: skipping non-trigger SQL in sys_triggers (name: %s)\n%!" name
     | exception exn ->
       Printf.eprintf "warning: failed to parse trigger SQL for '%s': %s\n%!" name
         (Printexc.to_string exn))
  ) pairs;
  Lwt.return_unit

let open_file ~path =
  let* result = S.open_file ~path in
  match result with
  | Error e ->
    let msg = Format.asprintf "%a" S.pp_error e in
    Lwt.return (Error (Runtime msg))
  | Ok store ->
    let* catalog = Cat.open_ store in
    let views = Hashtbl.create 4 in
    let* () = load_views_into_hashtbl store views in
    let triggers = Hashtbl.create 4 in
    let* () = load_triggers_into_hashtbl store triggers in
    Lwt.return (Ok { store; catalog; clock = None; explicit_txn = None; views;
                 triggers; savepoint_names = []; auto_began = false })

let open_block
    ~read_page ~write_page ~sync ~resize ~n_pages ~close
    : (t, error) result Lwt.t =
  let* result = S.open_block ~read_page ~write_page ~sync ~resize ~n_pages ~close in
  match result with
  | Error e ->
    let msg = Format.asprintf "%a" S.pp_error e in
    Lwt.return (Error (Runtime msg))
  | Ok store ->
    let* catalog = Cat.open_ store in
    let views = Hashtbl.create 4 in
    let* () = load_views_into_hashtbl store views in
    let triggers = Hashtbl.create 4 in
    let* () = load_triggers_into_hashtbl store triggers in
    Lwt.return (Ok { store; catalog; clock = None; explicit_txn = None; views;
                 triggers; savepoint_names = []; auto_began = false })

let close t = S.close t.store

let parse sql =
  match
    let lexbuf = Lexing.from_string sql in
    Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
  with
  | stmt        -> Ok stmt
  | exception Sql.Parser.Error -> Error (Parse "syntax error")
  | exception Failure msg      -> Error (Parse msg)

let compile t sql =
  match parse sql with
  | Error e -> Lwt.return (Error e)
  | Ok ast  ->
    let* bound = Sql.Sema.bind ~views:t.views t.catalog ast in
    match bound with
    | Error e -> Lwt.return (Error (Sema e))
    | Ok b    -> Lwt.return (Ok (Sql.Planner.plan ~cat:t.catalog b))

(* Prepared statement: holds a compiled plan for repeated execution. *)
type stmt = {
  db_ref             : t;
  plan               : Sql.Plan.op;
  param_names        : (string * int) list;
  mutable finalized  : bool;
}

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
    t.savepoint_names <- [];
    t.auto_began <- false;
    Lwt.return (Ok ())

let rollback_txn t =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction"))
  | Some tx ->
    let* () = S.rollback tx in
    t.explicit_txn <- None;
    t.savepoint_names <- [];
    t.auto_began <- false;
    Lwt.return (Ok ())

let savepoint_txn t name =
  let* tx = match t.explicit_txn with
    | Some tx -> Lwt.return tx
    | None ->
      let* tx = S.rw_begin t.store in
      t.explicit_txn <- Some tx;
      t.auto_began <- true;
      Lwt.return tx
  in
  let* () = S.savepoint_begin tx name in
  t.savepoint_names <- name :: t.savepoint_names;
  Lwt.return (Ok ())

let release_savepoint t name =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction for RELEASE"))
  | Some tx ->
    let* () = S.savepoint_release tx name in
    if List.mem name t.savepoint_names then begin
      let rec drop = function
        | [] -> []
        | n :: rest when String.equal n name -> rest
        | _ :: rest -> drop rest
      in
      t.savepoint_names <- drop t.savepoint_names
    end;
    if t.auto_began && t.savepoint_names = [] then begin
      let* () = S.commit tx in
      t.explicit_txn <- None;
      t.auto_began <- false;
      Lwt.return (Ok ())
    end else
      Lwt.return (Ok ())

let rollback_to_savepoint t name =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction for ROLLBACK TO"))
  | Some tx ->
    let* () = S.savepoint_rollback tx name in
    if List.mem name t.savepoint_names then begin
      let rec trim = function
        | [] -> []
        | n :: _ as rest when String.equal n name -> rest
        | _ :: rest -> trim rest
      in
      t.savepoint_names <- trim t.savepoint_names
    end;
    Lwt.return (Ok ())

(* ------------------------------------------------------------------ *)
(* Trigger firing helpers                                               *)
(* ------------------------------------------------------------------ *)

let trigger_meta_of_ast _name = function
  | Sql.Ast.S_create_trigger { timing; event; table; when_; body; _ } ->
    let trig_timing = (match timing with
      | Sql.Ast.TT_before     -> `Before
      | Sql.Ast.TT_after      -> `After
      | Sql.Ast.TT_instead_of -> `Instead_of) in
    let trig_event = (match event with
      | Sql.Ast.TE_insert -> `Insert
      | Sql.Ast.TE_update -> `Update
      | Sql.Ast.TE_delete -> `Delete) in
    Some { trig_table = table; trig_timing; trig_event;
           trig_when = when_; trig_body = body }
  | _ -> None

let value_to_literal = function
  | Row.V_int n  -> Sql.Ast.L_int n
  | Row.V_text s -> Sql.Ast.L_text s
  | Row.V_real f -> Sql.Ast.L_real f
  | Row.V_blob b -> Sql.Ast.L_blob b
  | Row.V_null   -> Sql.Ast.L_null

(** Walk an Ast.expr, applying [f tbl col] to each E_tbl_col.
    If f returns Some lit, substitute E_lit; otherwise keep the original. *)
let rec map_expr f e =
  let go = map_expr f in
  match e with
  | Sql.Ast.E_tbl_col (tbl, col) ->
    (match f tbl col with Some lit -> Sql.Ast.E_lit lit | None -> e)
  | Sql.Ast.E_binop (op, a, b) -> Sql.Ast.E_binop (op, go a, go b)
  | Sql.Ast.E_not x            -> Sql.Ast.E_not (go x)
  | Sql.Ast.E_is_null x        -> Sql.Ast.E_is_null (go x)
  | Sql.Ast.E_is_not_null x    -> Sql.Ast.E_is_not_null (go x)
  | Sql.Ast.E_neg x            -> Sql.Ast.E_neg (go x)
  | Sql.Ast.E_bitnot x         -> Sql.Ast.E_bitnot (go x)
  | Sql.Ast.E_between (x, lo, hi) -> Sql.Ast.E_between (go x, go lo, go hi)
  | Sql.Ast.E_in (x, vs)       -> Sql.Ast.E_in (go x, List.map go vs)
  | Sql.Ast.E_func (fn, args)  -> Sql.Ast.E_func (fn, List.map go args)
  | Sql.Ast.E_agg (fn, arg)    -> Sql.Ast.E_agg (fn, Option.map go arg)
  | Sql.Ast.E_case { scrutinee; branches; else_ } ->
    Sql.Ast.E_case {
      scrutinee = Option.map go scrutinee;
      branches  = List.map (fun (c, r) -> (go c, go r)) branches;
      else_     = Option.map go else_;
    }
  | Sql.Ast.E_cast (x, ty)    -> Sql.Ast.E_cast (go x, ty)
  | Sql.Ast.E_collate (x, c)  -> Sql.Ast.E_collate (go x, c)
  (* NEW/OLD references inside subquery expressions (E_subquery, E_exists,
     E_in_select, E_window) are not substituted. Trigger bodies should not
     reference NEW/OLD inside subqueries. *)
  | other -> other

let make_subst_fn ~schema ~(new_row : Row.t option) ~(old_row : Row.t option) =
  let col_idx name =
    let rec fi i = function
      | [] -> None
      | (c : Row.column) :: _ when String.equal c.name name -> Some i
      | _ :: rest -> fi (i + 1) rest
    in fi 0 schema
  in
  fun tbl col ->
    match String.uppercase_ascii tbl with
    | "NEW" -> (match new_row, col_idx col with
      | Some r, Some i -> Some (value_to_literal r.(i))
      | _ -> None)
    | "OLD" -> (match old_row, col_idx col with
      | Some r, Some i -> Some (value_to_literal r.(i))
      | _ -> None)
    | _ -> None

(** Substitute NEW.col / OLD.col references in an Ast.stmt with literal values. *)
let subst_new_old ~schema ~new_row ~old_row stmt =
  let f = make_subst_fn ~schema ~new_row ~old_row in
  let ge = map_expr f in
  match stmt with
  | Sql.Ast.S_insert { table; columns; values; on_conflict; returning; upsert_update } ->
    Sql.Ast.S_insert { table; columns;
      values = List.map (List.map ge) values;
      on_conflict;
      returning = List.map ge returning;
      upsert_update = Option.map (fun u ->
        { u with Sql.Ast.assignments =
            List.map (fun (c, e) -> (c, ge e)) u.Sql.Ast.assignments }
      ) upsert_update;
    }
  | Sql.Ast.S_update { table; assignments; where; order; limit; offset; returning } ->
    Sql.Ast.S_update { table;
      assignments = List.map (fun (c, e) -> (c, ge e)) assignments;
      where = Option.map ge where;
      order = List.map (fun ok -> { ok with Sql.Ast.expr = ge ok.Sql.Ast.expr }) order;
      limit;
      offset;
      returning = List.map ge returning;
    }
  | Sql.Ast.S_delete { table; where; order; limit; offset; returning } ->
    Sql.Ast.S_delete { table; where = Option.map ge where;
      order = List.map (fun ok -> { ok with Sql.Ast.expr = ge ok.Sql.Ast.expr }) order;
      limit;
      offset;
      returning = List.map ge returning }
  | Sql.Ast.S_select { distinct; proj; table; table_alias; joins;
                        where; group_by; having; order; limit; offset } ->
    Sql.Ast.S_select { distinct;
      proj = (match proj with
        | `Exprs es -> `Exprs (List.map (fun (e, a) -> (ge e, a)) es)
        | other -> other);
      table; table_alias;
      joins = List.map (fun j ->
        { j with Sql.Ast.on = ge j.Sql.Ast.on }) joins;
      where = Option.map ge where;
      group_by;
      having = Option.map ge having;
      order = List.map (fun ok ->
        { ok with Sql.Ast.expr = ge ok.Sql.Ast.expr }) order;
      limit; offset;
    }
  | other -> other

(** Compile and execute one pre-substituted trigger body statement within the db context.
    Note: triggers fired here do NOT recursively fire further triggers (nested trigger
    firing is not supported in Phase 22). *)
let fire_trigger_stmt t stmt =
  let* bound = Sql.Sema.bind ~views:t.views t.catalog stmt in
  match bound with
  | Error e -> Lwt.fail_with (Format.asprintf "trigger sema: %a" Sql.Sema.pp_error e)
  | Ok b ->
    let op = Sql.Planner.plan ~cat:t.catalog b in
    let mode = match t.explicit_txn with
      | None    -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    Sql.Exec.execute ~mode ~clock:t.clock t.store t.catalog op

(** Build a DML hook for exec.ml that fires triggers.
    Returns None if no triggers exist for the given table/timing/event (fast path).
    Scans the trigger hashtable exactly once to collect matching triggers. *)
(* Atomicity note: AFTER triggers fire after the DML transaction commits.
   If an AFTER trigger body fails, the committed DML row is NOT rolled back.
   This differs from SQLite's semantics where all-or-nothing applies. *)
let make_trigger_hook t table_meta ~timing ~event =
  let matching =
    Hashtbl.fold (fun _name ast acc ->
      match trigger_meta_of_ast _name ast with
      | Some m when
          String.equal m.trig_table table_meta.Cat.name &&
          m.trig_timing = timing && m.trig_event = event ->
        m :: acc
      | _ -> acc
    ) t.triggers []
  in
  if matching = [] then None
  else Some (fun ~new_row ~old_row ->
    let schema = table_meta.Cat.columns in
    Lwt_list.iter_s (fun m ->
      let* should_fire = match m.trig_when with
        | None -> Lwt.return true
        | Some when_expr ->
          let subst = map_expr (make_subst_fn ~schema ~new_row ~old_row) when_expr in
          let* bound = Sql.Sema.bind ~views:t.views t.catalog
            (Sql.Ast.S_const_select { exprs = [(subst, None)] }) in
          (match bound with
           | Error e ->
             Lwt.fail_with (Format.asprintf
               "trigger WHEN clause binding error: %a" Sql.Sema.pp_error e)
           | Ok bw ->
             let op = Sql.Planner.plan ~cat:t.catalog bw in
             let mode = match t.explicit_txn with
               | None -> Sql.Exec.Auto | Some tx -> Sql.Exec.In_txn tx in
             let* stream = Sql.Exec.query ~mode ~clock:t.clock t.store t.catalog op in
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (match rows with
               | row :: _ when Array.length row > 0 ->
                 (match row.(0) with Row.V_int 0L | Row.V_null -> false | _ -> true)
               | _ -> true))
      in
      if not should_fire then Lwt.return_unit
      else
        Lwt_list.iter_s (fun stmt ->
          let substituted = subst_new_old ~schema ~new_row ~old_row stmt in
          fire_trigger_stmt t substituted
        ) m.trig_body
    ) matching
  )

(** Extract column names from a view query's projection, in order.
    Returns [] if the projection cannot be resolved to simple column names. *)
let view_col_names view_query =
  let extract_from_select = function
    | Sql.Ast.S_select { proj = `Cols cols; _ } -> cols
    | Sql.Ast.S_select { proj = `Exprs exprs; _ } ->
      List.filter_map (fun (expr, alias) ->
        match alias with
        | Some a -> Some a
        | None   -> (match expr with
          | Sql.Ast.E_col name         -> Some name
          | Sql.Ast.E_tbl_col (_, col) -> Some col
          | _                          -> None)
      ) exprs
    | _ -> []
  in
  match view_query with
  | Sql.Ast.S_select _ -> extract_from_select view_query
  | Sql.Ast.S_compound { left; _ } -> extract_from_select left
  | _ -> []

(** Build a Row.column schema list from a list of column name strings. *)
let make_col_schema names =
  List.map (fun col_name ->
    { Row.name = col_name;
      Row.ty = Row.Text;
      Row.not_null = false;
      Row.primary_key = false;
      Row.default = None;
      Row.check_sql = None;
      Row.generated_as = None }
  ) names

(** Execute INSTEAD OF triggers for a view write operation. *)
let execute_instead_of t view_name ast =
  let find_instead_of event =
    Hashtbl.fold (fun _name trig_ast acc ->
      match trigger_meta_of_ast _name trig_ast with
      | Some m when
          String.equal m.trig_table view_name &&
          m.trig_timing = `Instead_of &&
          m.trig_event = event -> m :: acc
      | _ -> acc
    ) t.triggers []
  in
  let eval_insert_ast_value = function
    | Sql.Ast.E_lit (Sql.Ast.L_int n)  -> Row.V_int n
    | Sql.Ast.E_lit (Sql.Ast.L_text s) -> Row.V_text s
    | Sql.Ast.E_lit (Sql.Ast.L_real f) -> Row.V_real f
    | Sql.Ast.E_lit (Sql.Ast.L_blob b) -> Row.V_blob b
    | Sql.Ast.E_lit Sql.Ast.L_null     -> Row.V_null
    | Sql.Ast.E_neg (Sql.Ast.E_lit (Sql.Ast.L_int n)) ->
      Row.V_int (Int64.neg n)
    | _ -> Row.V_null
  in
  match ast with
  | Sql.Ast.S_insert { columns; values; _ } ->
    let matching = find_instead_of `Insert in
    if matching = [] then
      Lwt.return (Error (Sema (Sql.Sema.Unsupported
        (Printf.sprintf "view '%s' is not directly modifiable (no INSTEAD OF INSERT trigger)"
           view_name))))
    else begin
      (* When INSERT has no explicit column list, derive column names from the view's SELECT. *)
      let effective_cols =
        if columns <> [] then columns
        else
          match Hashtbl.find_opt t.views view_name with
          | Some view_query -> view_col_names view_query
          | None -> []
      in
      let* () = Lwt_list.iter_s (fun value_exprs ->
        let schema = make_col_schema effective_cols in
        let new_vals = List.map eval_insert_ast_value value_exprs in
        let new_row = Some (Array.of_list new_vals) in
        Lwt_list.iter_s (fun m ->
          let substituted_body = List.map (fun stmt ->
            subst_new_old ~schema ~new_row ~old_row:None stmt
          ) m.trig_body in
          Lwt_list.iter_s (fire_trigger_stmt t) substituted_body
        ) matching
      ) values in
      Lwt.return (Ok ())
    end
  | Sql.Ast.S_delete _ ->
    let matching = find_instead_of `Delete in
    if matching = [] then
      Lwt.return (Error (Sema (Sql.Sema.Unsupported
        (Printf.sprintf "view '%s' is not directly modifiable (no INSTEAD OF DELETE trigger)"
           view_name))))
    else begin
      let schema = [] in
      let* () = Lwt_list.iter_s (fun m ->
        let substituted_body = List.map (fun stmt ->
          subst_new_old ~schema ~new_row:None ~old_row:None stmt
        ) m.trig_body in
        Lwt_list.iter_s (fire_trigger_stmt t) substituted_body
      ) matching in
      Lwt.return (Ok ())
    end
  | _ ->
    Lwt.return (Error (Sema (Sql.Sema.Unsupported
      (Printf.sprintf "view '%s' is not directly modifiable" view_name))))

(* ------------------------------------------------------------------ *)
(* Public execute / query API                                           *)
(* ------------------------------------------------------------------ *)

let execute t sql =
  let* op = compile t sql in
  match op with
  | Error (Sema (Sql.Sema.Unknown_table view_name))
    when Hashtbl.mem t.views view_name ->
    (match parse sql with
     | Error _ -> Lwt.return (Error (Parse "syntax error"))
     | Ok ast  -> execute_instead_of t view_name ast)
  | Error e -> Lwt.return (Error e)
  | Ok Sql.Plan.Op_begin    -> begin_txn t
  | Ok Sql.Plan.Op_commit   -> commit_txn t
  | Ok Sql.Plan.Op_rollback -> rollback_txn t
  | Ok Sql.Plan.Op_savepoint name   -> savepoint_txn t name
  | Ok Sql.Plan.Op_release name     -> release_savepoint t name
  | Ok Sql.Plan.Op_rollback_to name -> rollback_to_savepoint t name
  | Ok Sql.Plan.Op_create_view { name; query } ->
    (* DDL is not transactional — persist_view commits immediately regardless of any open explicit txn *)
    Hashtbl.replace t.views name query;
    let* () = Cat.persist_view t.store ~name ~sql in
    Lwt.return (Ok ())
  | Ok Sql.Plan.Op_drop_view { name } ->
    (* DDL is not transactional — persist_view commits immediately regardless of any open explicit txn *)
    Hashtbl.remove t.views name;
    let* () = Cat.remove_view t.store ~name in
    Lwt.return (Ok ())
  | Ok Sql.Plan.Op_create_trigger { name; timing; event; table; when_; body } ->
    let ast = Sql.Ast.S_create_trigger { name; timing; event; table; when_; body } in
    Hashtbl.replace t.triggers name ast;
    let* () = Cat.persist_trigger t.store ~name ~sql in
    Lwt.return (Ok ())
  | Ok Sql.Plan.Op_drop_trigger { name } ->
    Hashtbl.remove t.triggers name;
    let* () = Cat.remove_trigger t.store ~name in
    Lwt.return (Ok ())
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
    let (before_hook, after_hook) = match op with
      | Sql.Plan.Op_insert { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Insert,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Insert)
      | Sql.Plan.Op_insert_select { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Insert,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Insert)
      | Sql.Plan.Op_update { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Update,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Update)
      | Sql.Plan.Op_delete { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Delete,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Delete)
      | _ -> (None, None)
    in
    (match Sql.Exec.execute ~mode ~clock:t.clock
             ~before_hook ~after_hook t.store t.catalog op with
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
  let* op = compile t sql in
  match op with
  | Error (Sema (Sql.Sema.Unknown_table view_name))
    when Hashtbl.mem t.views view_name ->
    (match parse sql with
     | Error _ -> Lwt.return (Error (Parse "syntax error"))
     | Ok ast  ->
       let* r = execute_instead_of t view_name ast in
       (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e)))
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
  | Ok Sql.Plan.Op_savepoint name ->
    let* r = savepoint_txn t name in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  | Ok Sql.Plan.Op_release name ->
    let* r = release_savepoint t name in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  | Ok Sql.Plan.Op_rollback_to name ->
    let* r = rollback_to_savepoint t name in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  | Ok Sql.Plan.Op_create_view { name; query } ->
    (* DDL is not transactional — persist_view commits immediately regardless of any open explicit txn *)
    Hashtbl.replace t.views name query;
    let* () = Cat.persist_view t.store ~name ~sql in
    Lwt.return (Ok 0)
  | Ok Sql.Plan.Op_drop_view { name } ->
    (* DDL is not transactional — persist_view commits immediately regardless of any open explicit txn *)
    Hashtbl.remove t.views name;
    let* () = Cat.remove_view t.store ~name in
    Lwt.return (Ok 0)
  | Ok Sql.Plan.Op_create_trigger { name; timing; event; table; when_; body } ->
    let ast = Sql.Ast.S_create_trigger { name; timing; event; table; when_; body } in
    Hashtbl.replace t.triggers name ast;
    let* () = Cat.persist_trigger t.store ~name ~sql in
    Lwt.return (Ok 0)
  | Ok Sql.Plan.Op_drop_trigger { name } ->
    Hashtbl.remove t.triggers name;
    let* () = Cat.remove_trigger t.store ~name in
    Lwt.return (Ok 0)
  | Ok op ->
    let mode = match t.explicit_txn with
      | None    -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    let (before_hook, after_hook) = match op with
      | Sql.Plan.Op_insert { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Insert,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Insert)
      | Sql.Plan.Op_insert_select { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Insert,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Insert)
      | Sql.Plan.Op_update { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Update,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Update)
      | Sql.Plan.Op_delete { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Delete,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Delete)
      | _ -> (None, None)
    in
    (match Sql.Exec.execute_with_count ~mode ~clock:t.clock
             ~before_hook ~after_hook t.store t.catalog op with
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
  let* op = compile t sql in
  match op with
  | Error e -> Lwt.return (Error e)
  | Ok op   ->
    let mode = match t.explicit_txn with
      | None    -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    (match Sql.Exec.query ~mode ~clock:t.clock t.store t.catalog op with
     | exception Failure msg -> Lwt.return (Error (Runtime msg))
     | lwt_stream ->
       let* stream = lwt_stream in
       Lwt.return (Ok stream))

(* ------------------------------------------------------------------ *)
(* Prepared statement API                                               *)
(* ------------------------------------------------------------------ *)

let prepare t sql =
  match parse sql with
  | Error e -> Lwt.return (Error e)
  | Ok ast  ->
    let* bound = Sql.Sema.bind_returning_params ~views:t.views t.catalog ast in
    (match bound with
     | Error e         -> Lwt.return (Error (Sema e))
     | Ok (b, names)  ->
       let plan = Sql.Planner.plan ~cat:t.catalog b in
       Lwt.return (Ok { db_ref = t; plan; param_names = names; finalized = false }))

let param_slot st name = List.assoc_opt name st.param_names

let params_of_named st named =
  let n = List.fold_left (fun acc (_, i) -> max acc (i + 1)) 0 st.param_names in
  let arr = Array.make n Row.V_null in
  List.iter (fun (name, v) ->
    match List.assoc_opt name st.param_names with
    | Some i -> arr.(i) <- v
    | None   -> ()
  ) named;
  arr

let run st ~params =
  if st.finalized then Lwt.return (Error (Runtime "statement already finalized"))
  else
  let params_arr = Array.of_list params in
  let t = st.db_ref in
  let mode = match t.explicit_txn with
    | None    -> Sql.Exec.Auto
    | Some tx -> Sql.Exec.In_txn tx
  in
  let (before_hook, after_hook) = match st.plan with
    | Sql.Plan.Op_insert { table_meta; _ } ->
      (make_trigger_hook t table_meta ~timing:`Before ~event:`Insert,
       make_trigger_hook t table_meta ~timing:`After  ~event:`Insert)
    | Sql.Plan.Op_insert_select { table_meta; _ } ->
      (make_trigger_hook t table_meta ~timing:`Before ~event:`Insert,
       make_trigger_hook t table_meta ~timing:`After  ~event:`Insert)
    | Sql.Plan.Op_update { table_meta; _ } ->
      (make_trigger_hook t table_meta ~timing:`Before ~event:`Update,
       make_trigger_hook t table_meta ~timing:`After  ~event:`Update)
    | Sql.Plan.Op_delete { table_meta; _ } ->
      (make_trigger_hook t table_meta ~timing:`Before ~event:`Delete,
       make_trigger_hook t table_meta ~timing:`After  ~event:`Delete)
    | _ -> (None, None)
  in
  Lwt.catch
    (fun () ->
      let* n = Sql.Exec.execute_with_count ~mode ~clock:t.clock ~params:params_arr
                 ~before_hook ~after_hook t.store t.catalog st.plan in
      Lwt.return (Ok n))
    (function
     | Failure msg -> Lwt.return (Error (Runtime msg))
     | exn         -> Lwt.fail exn)

let iter st ~params =
  if st.finalized then Lwt.return (Error (Runtime "statement already finalized"))
  else
  let params_arr = Array.of_list params in
  let t = st.db_ref in
  Lwt.catch
    (fun () ->
      let* stream = Sql.Exec.query ~clock:t.clock ~params:params_arr t.store t.catalog st.plan in
      Lwt.return (Ok stream))
    (function
     | Failure msg -> Lwt.return (Error (Runtime msg))
     | exn         -> Lwt.fail exn)

let finalize st =
  st.finalized <- true;
  Lwt.return_unit

let pp_error fmt = function
  | Parse msg -> Format.fprintf fmt "parse error: %s" msg
  | Sema  e   -> Format.fprintf fmt "sema error: %a" Sql.Sema.pp_error e
  | Runtime m -> Format.fprintf fmt "runtime error: %s" m
