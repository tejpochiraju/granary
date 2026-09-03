(** #572: identifier quoting in the SQL text the engine stores about itself.

    [Ast.expr_to_sql] used to render identifiers verbatim, so a column whose
    name needed quoting lost its delimiters on the way into the catalog. The
    table was created successfully and was then permanently un-insertable: the
    executor re-parses [check_sql] on every write, and

    {v CREATE TABLE t ("my col" INTEGER, b INTEGER CHECK ("my col" > 0)) v}

    stored the CHECK as the un-parseable text [(my col > 0)]. The same emitter
    builds a GENERATED column's [generated_as], a partial index's
    [idx_where_sql] and an expression index's [idx_columns] entry, so all four
    carried the same defect.

    The oracle here is the round trip: whatever the engine writes down about a
    name it must be able to read back. Three layers of it —

    - [quote_ident] itself, and [parse -> expr_to_sql -> parse] on the AST;
    - end to end, the shape the issue asked for: for a generated column name,
      CREATE TABLE with a CHECK naming it, then INSERT (and prove the CHECK is
      live by watching it reject a bad row, not merely by watching a good row
      succeed);
    - a drift guard that re-derives the reserved-word list from [lexer.mll],
      since a keyword added there and not here reopens the whole class.

    #619: that drift guard reads the keyword list from
    {!Granary_encoding.Sql_ident} directly. It used to go through aliases in
    [Granary_sql.Ast], which had no consumer in [lib/] at all and which a
    [dead_code_analyzer] pruning pass would therefore have deleted — taking the
    only binding between the keyword list and the lexer with them, silently.
    Naming the owning module here keeps the "do not prune" note (at the top of
    [sql_ident.mli]) next to the values it is about. *)

open Lwt.Syntax
open Granary_sql
module Db = Granary.Db
module Sql_ident = Granary_encoding.Sql_ident

let run = Lwt_main.run

(* ── Layer 1: the rule, and the AST round trip ─────────────────────── *)

let parse_stmt s =
  let lexbuf = Lexing.from_string s in
  Parser.stmt_eof Lexer.token lexbuf
;;

(* Parse [sql] as a column CHECK expression and hand back the expression. *)
let parse_check sql =
  match parse_stmt (Printf.sprintf "CREATE TABLE t (x INTEGER CHECK (%s));" sql) with
  | Ast.S_create_table { columns = [ { check = Some e; _ } ]; _ } -> e
  | _ -> Alcotest.failf "could not parse %S back as a CHECK expression" sql
;;

let test_quote_ident_rule () =
  let bare = [ "a"; "_x"; "col1"; "MixedCase"; "_"; "a_1_b" ] in
  List.iter
    (fun s ->
       Alcotest.(check string) (Printf.sprintf "%S stays bare" s) s (Ast.quote_ident s))
    bare;
  let quoted =
    [ "", "\"\""
    ; "my col", "\"my col\""
    ; "1col", "\"1col\""
    ; "col-1", "\"col-1\""
    ; "a.b", "\"a.b\""
    ; "a\"b", "\"a\"\"b\""
    ; "a'b", "\"a'b\""
      (* reserved words are plain words, and still cannot be written bare *)
    ; "select", "\"select\""
    ; "SELECT", "\"SELECT\""
    ; "SeLeCt", "\"SeLeCt\""
    ; "key", "\"key\""
    ; "order", "\"order\""
    ; "log2", "\"log2\""
    ]
  in
  List.iter
    (fun (s, want) ->
       Alcotest.(check string) (Printf.sprintf "quote %S" s) want (Ast.quote_ident s))
    quoted
;;

(* [Exec.quote_ident], which renders DDL for sqlite_master and [Db.dump], and
   [Ast.expr_to_sql], which renders stored constraint text, write into the same
   catalog.  Two different rules there is the bug, so they are one function. *)
let test_emitters_share_one_rule () =
  List.iter
    (fun s ->
       Alcotest.(check string)
         (Printf.sprintf "Exec.quote_ident %S = Ast.quote_ident" s)
         (Ast.quote_ident s)
         (Exec.quote_ident s))
    [ "a"; ""; "my col"; "1col"; "a\"b"; "select"; "KEY"; "order" ]
;;

(* The closure property: for any name the lexer accepts as a quoted
   identifier, [parse -> expr_to_sql -> parse] returns the same expression. *)
let roundtrip_col name =
  let sql = Printf.sprintf "%s > 0" (Ast.quote_ident name) in
  let e = parse_check sql in
  (match e with
   | Ast.E_binop (_, Ast.E_col got, _) ->
     Alcotest.(check string) "parsed back to the same name" name got
   | _ -> Alcotest.failf "unexpected AST for %S" sql);
  let reprinted = Ast.expr_to_sql e in
  let e' = parse_check reprinted in
  Alcotest.(check bool)
    (Printf.sprintf "expr_to_sql is closed on %S (%S)" name reprinted)
    true
    (e = e')
;;

let interesting_names =
  [ "my col"
  ; "1col"
  ; "col-1"
  ; "a.b"
  ; "a\"b"
  ; "a'b"
  ; "a`b"
  ; "a[b]"
  ; "a(b)"
  ; "a,b"
  ; "a;b"
  ; "a b c"
  ; " leading"
  ; "trailing "
  ; "a\nb"
  ; "naïve"
  ; "select"
  ; "FROM"
  ; "key"
  ; "order"
  ; "table"
  ; "check"
  ; "default"
  ; "and"
  ; "group"
  ; "index"
  ; "log2"
  ; "plain"
  ]
;;

let test_roundtrip_interesting () = List.iter roundtrip_col interesting_names

(* Every reserved word, not just a hand-picked handful. *)
let test_roundtrip_every_keyword () = List.iter roundtrip_col Sql_ident.sql_keywords

let test_roundtrip_tbl_col () =
  let e = parse_check "\"my tbl\".\"my col\" > 0" in
  (match e with
   | Ast.E_binop (_, Ast.E_tbl_col ("my tbl", "my col"), _) -> ()
   | _ -> Alcotest.fail "expected E_tbl_col (\"my tbl\", \"my col\")");
  Alcotest.(check bool) "closed" true (e = parse_check (Ast.expr_to_sql e))
;;

(* A text literal containing a quote round-trips too — the same emitter. *)
let test_roundtrip_text_literal () =
  let e = parse_check "\"my col\" = 'it''s'" in
  Alcotest.(check bool) "closed" true (e = parse_check (Ast.expr_to_sql e))
;;

(* ── QCheck: the property over generated names ─────────────────────── *)

let name_gen =
  let open QCheck.Gen in
  let chars =
    [ 'a'
    ; 'b'
    ; 'Z'
    ; '_'
    ; '0'
    ; '9'
    ; ' '
    ; '-'
    ; '.'
    ; '"'
    ; '\''
    ; '('
    ; ')'
    ; '*'
    ; '/'
    ; '+'
    ; ','
    ; ';'
    ; '['
    ; ']'
    ; '`'
    ; '!'
    ; '?'
    ; ':'
    ; '%'
    ; '&'
    ; '|'
    ; '<'
    ; '>'
    ; '='
    ; '~'
    ; '\t'
    ]
  in
  let* n = int_range 1 8 in
  let+ cs = list_size (return n) (oneof_list chars) in
  String.init n (List.nth cs)
;;

let arb_name = QCheck.make ~print:(Printf.sprintf "%S") name_gen

let prop_roundtrip =
  QCheck.Test.make
    ~count:400
    ~name:"#572 parse -> expr_to_sql -> parse is closed over generated names"
    arb_name
    (fun name ->
       roundtrip_col name;
       true)
;;

(* ── Layer 2: end to end through a real database ───────────────────── *)

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> None
  | Error e -> Some (Format.asprintf "%a" Db.pp_error e)
;;

let rows db sql =
  run
    (let* s = Db.query db sql in
     match s with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok s -> Lwt_stream.to_list s)
;;

let one_int db sql =
  match rows db sql with
  | [ [| Db.V_int i |] ] -> i
  | _ -> Alcotest.failf "expected one integer row from %S" sql
;;

(* The shape the issue asked for: CREATE TABLE with a CHECK naming the column,
   then INSERT.  A bad row must still be rejected — a CHECK that silently
   stopped being enforced would pass an insert-only test. *)
let check_survives_create name =
  with_db (fun db ->
    let q = Ast.quote_ident name in
    exec db (Printf.sprintf "CREATE TABLE t (%s INTEGER, b INTEGER CHECK (%s > 0))" q q);
    exec db "INSERT INTO t VALUES (1, 1)";
    Alcotest.(check int64) "row landed" 1L (one_int db "SELECT COUNT(*) FROM t");
    match exec_err db "INSERT INTO t VALUES (0, 1)" with
    | None -> Alcotest.failf "CHECK on %S did not reject a violating row" name
    | Some msg ->
      (* and it must fail as a constraint violation, not as a parse error *)
      let lower = String.lowercase_ascii msg in
      let mentions s =
        let ls = String.length s
        and ll = String.length lower in
        let rec go i = i + ls <= ll && (String.sub lower i ls = s || go (i + 1)) in
        go 0
      in
      if mentions "parse"
      then Alcotest.failf "CHECK on %S failed to parse rather than firing: %s" name msg)
;;

let test_check_issue_repro () = check_survives_create "my col"

let test_check_interesting_names () =
  List.iter check_survives_create [ "my col"; "1col"; "col-1"; "a'b"; "select"; "order" ]
;;

let prop_check_e2e =
  QCheck.Test.make
    ~count:40
    ~name:"#572 CREATE TABLE with a CHECK naming the column, then INSERT"
    arb_name
    (fun name ->
       check_survives_create name;
       true)
;;

let test_generated_column () =
  with_db (fun db ->
    let q = Ast.quote_ident "my col" in
    exec
      db
      (Printf.sprintf
         "CREATE TABLE g (%s INTEGER, d INTEGER GENERATED ALWAYS AS (%s * 2) VIRTUAL)"
         q
         q);
    exec db (Printf.sprintf "INSERT INTO g (%s) VALUES (21)" q);
    Alcotest.(check int64) "generated value" 42L (one_int db "SELECT d FROM g"))
;;

let test_generated_column_stored () =
  with_db (fun db ->
    let q = Ast.quote_ident "order" in
    exec
      db
      (Printf.sprintf
         "CREATE TABLE g (%s INTEGER, d INTEGER GENERATED ALWAYS AS (%s + 1) STORED)"
         q
         q);
    exec db (Printf.sprintf "INSERT INTO g (%s) VALUES (7)" q);
    Alcotest.(check int64) "generated value" 8L (one_int db "SELECT d FROM g"))
;;

let test_partial_index () =
  with_db (fun db ->
    let q = Ast.quote_ident "my col" in
    exec db (Printf.sprintf "CREATE TABLE p (%s INTEGER, v INTEGER)" q);
    exec db (Printf.sprintf "CREATE INDEX pi ON p (v) WHERE %s > 0" q);
    exec db "INSERT INTO p VALUES (1, 5)";
    exec db "INSERT INTO p VALUES (0, 6)";
    Alcotest.(check int64) "both rows" 2L (one_int db "SELECT COUNT(*) FROM p");
    Alcotest.(check int64)
      "indexed row readable"
      5L
      (one_int db "SELECT v FROM p WHERE v = 5"))
;;

let test_expression_index () =
  with_db (fun db ->
    let q = Ast.quote_ident "my col" in
    exec db (Printf.sprintf "CREATE TABLE e (%s INTEGER, v INTEGER)" q);
    exec db (Printf.sprintf "CREATE INDEX ei ON e (%s + 1)" q);
    exec db "INSERT INTO e VALUES (1, 5)";
    Alcotest.(check int64) "row landed" 1L (one_int db "SELECT COUNT(*) FROM e"))
;;

(* The DDL the catalog reports must itself be re-executable — this is the
   [Exec.quote_ident] half of the same rule. *)
let test_sqlite_master_ddl_replays () =
  let ddl =
    with_db (fun db ->
      let q = Ast.quote_ident "my col" in
      exec db (Printf.sprintf "CREATE TABLE t (%s INTEGER, b INTEGER CHECK (%s > 0))" q q);
      match rows db "SELECT sql FROM sqlite_master WHERE name = 't'" with
      | [ [| Db.V_text s |] ] -> s
      | _ -> Alcotest.fail "no sqlite_master row for t")
  in
  with_db (fun db ->
    exec db ddl;
    exec db "INSERT INTO t VALUES (1, 1)";
    Alcotest.(check int64)
      "replayed DDL is usable"
      1L
      (one_int db "SELECT COUNT(*) FROM t");
    match exec_err db "INSERT INTO t VALUES (0, 1)" with
    | None -> Alcotest.fail "replayed CHECK does not fire"
    | Some _ -> ())
;;

(* #553 / PR #564 added [Catalog.rewrite_ident_in_sql], whose delimited-
   identifier branch this bug made unreachable: no delimited identifier could
   ever appear in stored SQL text.  Now it can, so the path is testable. *)
let test_rename_column_delimited () =
  with_db (fun db ->
    exec db "CREATE TABLE r (\"my col\" INTEGER, b INTEGER CHECK (\"my col\" > 0))";
    exec db "ALTER TABLE r RENAME COLUMN \"my col\" TO \"your col\"";
    exec db "INSERT INTO r VALUES (1, 1)";
    Alcotest.(check int64) "row landed" 1L (one_int db "SELECT COUNT(*) FROM r");
    Alcotest.(check int64)
      "readable under the new name"
      1L
      (one_int db "SELECT \"your col\" FROM r");
    match exec_err db "INSERT INTO r VALUES (0, 1)" with
    | None -> Alcotest.fail "CHECK stopped firing after RENAME COLUMN"
    | Some _ -> ())
;;

(* The other direction: an ordinary name renamed INTO one that needs quoting.
   The rewriter used to substitute [new_name] verbatim, so this produced the
   same un-parseable [(my col > 0)] at ALTER time.

   Renaming to a RESERVED WORD was the one case left open here — a keyword is a
   plain word, and the catalog could not see the lexer's keyword table from
   below the parser.  #577 closed it by hoisting the rule into
   [Granary_encoding.Sql_ident]; [test_rename_reserved_577.ml] covers it. *)
let test_rename_column_to_delimited () =
  with_db (fun db ->
    exec db "CREATE TABLE r (a INTEGER, b INTEGER CHECK (a > 0))";
    exec db "ALTER TABLE r RENAME COLUMN a TO \"my col\"";
    exec db "INSERT INTO r VALUES (1, 1)";
    Alcotest.(check int64) "row landed" 1L (one_int db "SELECT COUNT(*) FROM r");
    match exec_err db "INSERT INTO r VALUES (0, 1)" with
    | None -> Alcotest.fail "CHECK stopped firing after RENAME COLUMN to a delimited name"
    | Some _ -> ())
;;

let test_rename_column_partial_index () =
  with_db (fun db ->
    exec db "CREATE TABLE p (\"my col\" INTEGER, v INTEGER)";
    exec db "CREATE INDEX pi ON p (v) WHERE \"my col\" > 0";
    exec db "ALTER TABLE p RENAME COLUMN \"my col\" TO \"your col\"";
    exec db "INSERT INTO p VALUES (1, 5)";
    Alcotest.(check int64) "row landed" 1L (one_int db "SELECT COUNT(*) FROM p"))
;;

(* ── Layer 3: drift guard against lexer.mll ────────────────────────── *)

(* Read [lexer.mll] and collect every quoted upper-case word on a keyword
   mapping line ([| "CREATE" -> CREATE], [| "CEIL" | "CEILING" -> CEIL]).
   Skipped rather than failed when the source is not reachable from the test's
   working directory, so an unusual build layout cannot turn this into a
   spurious failure — but when it IS reachable it is the only thing standing
   between a newly added keyword and a silent return of #572. *)
let lexer_keywords () =
  let candidates =
    [ "../lib/sql/lexer.mll"; "lib/sql/lexer.mll"; "../../lib/sql/lexer.mll" ]
  in
  match List.find_opt Sys.file_exists candidates with
  | None -> None
  | Some path ->
    let ic = open_in path in
    let out = ref [] in
    let is_upper_word s =
      s <> ""
      && String.for_all
           (fun c -> (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_')
           s
      &&
      let c = s.[0] in
      (c >= 'A' && c <= 'Z') || c = '_'
    in
    (* every "..." on the line, left of the arrow *)
    let scan line =
      let n = String.length line in
      let rec go i =
        if i >= n
        then ()
        else if line.[i] = '"'
        then (
          match String.index_from_opt line (i + 1) '"' with
          | None -> ()
          | Some j ->
            let w = String.sub line (i + 1) (j - i - 1) in
            if is_upper_word w then out := w :: !out;
            go (j + 1))
        else go (i + 1)
      in
      go 0
    in
    (try
       while true do
         let line = input_line ic in
         let t = String.trim line in
         if String.length t > 2 && String.sub t 0 3 = "| \"" && String.length t > 3
         then (
           match String.index_opt t '>' with
           | Some k when k > 0 && t.[k - 1] = '-' -> scan (String.sub t 0 (k - 1))
           | _ -> ())
       done
     with
     | End_of_file -> ());
    close_in ic;
    Some (List.sort_uniq compare !out)
;;

let test_keyword_list_matches_lexer () =
  match lexer_keywords () with
  | None -> Alcotest.skip ()
  | Some kws ->
    Alcotest.(check bool) "lexer.mll yielded keywords" true (List.length kws > 100);
    let missing = List.filter (fun k -> not (Sql_ident.is_sql_keyword k)) kws in
    if missing <> []
    then
      Alcotest.failf
        "lexer.mll has keywords Sql_ident.sql_keywords does not: %s — add them, or a \
         column with one of those names silently loses its quoting again (#572)"
        (String.concat ", " missing);
    (* and nothing in our list that the lexer does not actually reserve: a
       stale entry only costs cosmetic over-quoting, so this is a warning-grade
       check kept honest by the same source of truth. *)
    let extra = List.filter (fun k -> not (List.mem k kws)) Sql_ident.sql_keywords in
    Alcotest.(check (list string)) "no stale entries in Sql_ident.sql_keywords" [] extra
;;

let () =
  Alcotest.run
    "ident_quoting_572"
    [ ( "rule"
      , [ Alcotest.test_case "quote_ident" `Quick test_quote_ident_rule
        ; Alcotest.test_case "Ast and Exec agree" `Quick test_emitters_share_one_rule
        ; Alcotest.test_case
            "keyword list matches lexer.mll"
            `Quick
            test_keyword_list_matches_lexer
        ] )
    ; ( "round trip"
      , [ Alcotest.test_case "interesting names" `Quick test_roundtrip_interesting
        ; Alcotest.test_case "every keyword" `Quick test_roundtrip_every_keyword
        ; Alcotest.test_case "qualified column" `Quick test_roundtrip_tbl_col
        ; Alcotest.test_case "text literal" `Quick test_roundtrip_text_literal
        ; QCheck_alcotest.to_alcotest prop_roundtrip
        ] )
    ; ( "end to end"
      , [ Alcotest.test_case
            "issue repro: CHECK on \"my col\""
            `Quick
            test_check_issue_repro
        ; Alcotest.test_case
            "CHECK, interesting names"
            `Quick
            test_check_interesting_names
        ; Alcotest.test_case "GENERATED VIRTUAL" `Quick test_generated_column
        ; Alcotest.test_case "GENERATED STORED" `Quick test_generated_column_stored
        ; Alcotest.test_case "partial index WHERE" `Quick test_partial_index
        ; Alcotest.test_case "expression index" `Quick test_expression_index
        ; Alcotest.test_case
            "sqlite_master DDL replays"
            `Quick
            test_sqlite_master_ddl_replays
        ; QCheck_alcotest.to_alcotest prop_check_e2e
        ] )
    ; ( "rename column (#553/#564)"
      , [ Alcotest.test_case "delimited name" `Quick test_rename_column_delimited
        ; Alcotest.test_case
            "renamed to a delimited name"
            `Quick
            test_rename_column_to_delimited
        ; Alcotest.test_case "partial index WHERE" `Quick test_rename_column_partial_index
        ] )
    ]
;;
