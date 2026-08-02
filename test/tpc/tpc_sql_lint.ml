(* #510 — enforcement for the #485 workaround.

   #485: granary does not bind an UNQUALIFIED outer-column reference inside a
   correlated subquery to the outer row; it silently evaluates to NULL instead
   of erroring.  The workaround is to qualify every outer reference
   ([orders.o_orderkey], not [o_orderkey]), and it is applied throughout
   test/tpc/.  Until this lint existed nothing enforced it: the discipline lived
   in comments, so tidying a query back to the unqualified form silently
   re-disabled whatever it checked, with no failure.

   It had already gone wrong twice.  TPC-H Q4 returned 0 rows instead of 5.
   Worse, TPC-C's clause-3.3 consistency conditions 1 and 2 were written as
   "zero rows means the invariant holds" queries whose correlated predicate
   became [x <> NULL] — never true — so both reported a clean database over any
   input, including a deliberately corrupted one.  A vacuous oracle that looks
   healthy is not a wrong answer anyone reads; it was found only because a
   review insisted the conditions be tested for their ability to FAIL.

   THIS LINT IS CHEAP INSURANCE UNTIL #485 IS FIXED, AND SHOULD BE DELETED
   WITH IT.  Once the planner binds unqualified outer references correctly,
   the unqualified form is no longer a trap and there is nothing to enforce.
   Do not grow this into a general SQL analyzer in the meantime — it is a
   deliberately crude heuristic over the harness's own SQL, which is all the
   two incidents needed.

   How crude, stated so the limits are not mistaken for coverage: it maps a
   bare column name to its table by looking the name up in the generators' own
   {!Tpch_gen.column_names} / {!Tpcc_gen.column_names}, which the TPC schemas
   make unambiguous because every column carries its table's prefix.  A name
   the generators do not declare — a projection alias, a view column — is
   ignored rather than guessed at.  It sees FROM/JOIN table names, not view
   definitions or CTEs, so a subquery selecting from a setup view contributes
   no tables and its bare references are judged against the outer query alone.
   That direction is the safe one: it can report a reference the engine would
   in fact resolve, never miss one it would not. *)

type finding =
  { column : string
  ; table : string
  ; subquery_tables : string list
  }

let pp_finding fmt { column; table; subquery_tables } =
  Format.fprintf
    fmt
    "unqualified %s resolves to outer table %s from a subquery over [%s] — qualify it as \
     %s.%s (#485)"
    column
    table
    (String.concat "; " subquery_tables)
    table
    column
;;

(* --- which table each column name belongs to -------------------------- *)

let columns_of ~tables ~column_names =
  List.concat_map
    (fun table -> List.map (fun column -> column, table) (column_names ~table))
    tables
;;

let tpch_columns = columns_of ~tables:Tpch_gen.tables ~column_names:Tpch_gen.column_names
let tpcc_columns = columns_of ~tables:Tpcc_gen.tables ~column_names:Tpcc_gen.column_names

(* --- tokens ------------------------------------------------------------ *)

(* A qualified reference lexes as one word including its dot, so it never
   matches a bare column name and is skipped for free. *)
type token =
  | Lparen
  | Rparen
  | Word of string
  | Punct

let is_word_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' -> true
  | _ -> false
;;

let rec word_end s i =
  if i < String.length s && is_word_char s.[i] then word_end s (i + 1) else i
;;

(* Past the closing quote.  Doubled quotes inside a literal do not need
   special handling: the pair closes and reopens, landing in the same place. *)
let rec literal_end s i =
  if i >= String.length s
  then i
  else if Char.equal s.[i] '\''
  then i + 1
  else literal_end s (i + 1)
;;

let rec tokenize s i acc =
  if i >= String.length s
  then List.rev acc
  else if Char.equal s.[i] '('
  then tokenize s (i + 1) (Lparen :: acc)
  else if Char.equal s.[i] ')'
  then tokenize s (i + 1) (Rparen :: acc)
  else if Char.equal s.[i] '\''
  then tokenize s (literal_end s (i + 1)) (Punct :: acc)
  else if is_word_char s.[i]
  then (
    let j = word_end s i in
    let w = String.lowercase_ascii (String.sub s i (j - i)) in
    tokenize s j (Word w :: acc))
  else tokenize s (i + 1) (Punct :: acc)
;;

(* --- parenthesis tree -------------------------------------------------- *)

type node =
  | Tok of token
  | Group of node list

let rec parse toks acc =
  match toks with
  | [] -> List.rev acc, []
  | Rparen :: rest -> List.rev acc, rest
  | Lparen :: rest ->
    let inner, rest = parse rest [] in
    parse rest (Group inner :: acc)
  | t :: rest -> parse rest (Tok t :: acc)
;;

(* Only a parenthesised group whose first WORD is SELECT is a query of its own.
   [(1 - l_discount)] and [SUM(l_quantity)] belong to the level around them.
   The leading skip matters: whitespace lexes to [Punct], so an opening paren
   followed by a newline before SELECT — the shape most of the harness is
   written in — would otherwise not be recognised as a subquery at all. *)
let rec starts_select = function
  | Tok (Word "select") :: _ -> true
  | Tok Punct :: rest -> starts_select rest
  | _ -> false
;;

(* The words belonging to this query level: its own, plus those of any nested
   group that is not itself a SELECT. *)
let rec level_words nodes acc =
  match nodes with
  | [] -> List.rev acc
  | Tok (Word w) :: rest -> level_words rest (w :: acc)
  | Tok _ :: rest -> level_words rest acc
  | Group g :: rest when starts_select g -> level_words rest acc
  | Group g :: rest -> level_words rest (List.rev_append (level_words g []) acc)
;;

(* The SELECT groups directly under this level, reaching through non-SELECT
   parentheses so that [EXISTS ((SELECT ...))] is still found. *)
let rec sub_selects nodes acc =
  match nodes with
  | [] -> List.rev acc
  | Tok _ :: rest -> sub_selects rest acc
  | Group g :: rest when starts_select g -> sub_selects rest (g :: acc)
  | Group g :: rest -> sub_selects rest (List.rev_append (sub_selects g []) acc)
;;

(* The word after FROM or JOIN is the table.  An alias following it is a
   separate word and is not a column name, so it needs no special case. *)
let rec tables_named words acc =
  match words with
  | ("from" | "join") :: name :: rest -> tables_named rest (name :: acc)
  | _ :: rest -> tables_named rest acc
  | [] -> List.rev acc
;;

(* --- the check --------------------------------------------------------- *)

let refs_to_outer ~columns ~words ~here ~outer =
  let step acc w =
    match List.assoc_opt w columns with
    | Some table when (not (List.mem table here)) && List.mem table outer ->
      { column = w; table; subquery_tables = here } :: acc
    | Some _ | None -> acc
  in
  List.rev (List.fold_left step [] words)
;;

(* [outer] is empty at the top level, where by definition no reference can be
   an outer one; every nested SELECT sees its ancestors' tables. *)
let rec walk nodes ~columns ~outer acc =
  let words = level_words nodes [] in
  let here = tables_named words [] in
  let acc =
    if outer = [] then acc else acc @ refs_to_outer ~columns ~words ~here ~outer
  in
  let visible = here @ outer in
  List.fold_left
    (fun acc g -> walk g ~columns ~outer:visible acc)
    acc
    (sub_selects nodes [])
;;

type source =
  { label : string
  ; columns : (string * string) list
  ; sql : string
  }

let check { label = _; columns; sql } =
  let nodes, _ = parse (tokenize sql 0 []) [] in
  walk nodes ~columns ~outer:[] []
;;

(* --- the harness SQL this runs over ------------------------------------ *)

(* Register new harness SQL here.  Tpcc_txn's transaction statements are not
   listed: the spec's five profiles are flat single-table statements with no
   subquery at all, so there is no correlated reference for them to get wrong.
   If that changes, add them. *)
let tpch_sources =
  List.concat_map
    (fun (q : Tpch_queries.query) ->
       let of_sql tag sql =
         { label = Printf.sprintf "tpch q%d %s" q.number tag
         ; columns = tpch_columns
         ; sql
         }
       in
       of_sql "sql" q.sql :: List.map (of_sql "setup") q.setup)
    Tpch_queries.all
;;

let tpcc_sources =
  List.concat_map
    (fun (c : Tpcc_check.condition) ->
       List.mapi
         (fun i sql ->
            { label = Printf.sprintf "tpcc condition %d query %d" c.number i
            ; columns = tpcc_columns
            ; sql
            })
         c.queries)
    Tpcc_check.conditions
;;

let harness = tpch_sources @ tpcc_sources
