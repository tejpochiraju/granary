open Lwt.Syntax
module Db = Sqlocaml.Db

let value_to_string = function
  | Db.V_null -> "NULL"
  | Db.V_int n -> Int64.to_string n
  | Db.V_real f -> Printf.sprintf "%.17g" f
  | Db.V_text s -> s
  | Db.V_blob b -> Printf.sprintf "<blob:%d>" (Bytes.length b)
;;

let is_query_stmt sql =
  let s = String.trim sql in
  if s = ""
  then false
  else (
    let upper = String.uppercase_ascii s in
    let stop c = c = ' ' || c = '\n' || c = '\t' in
    let len = String.length upper in
    let rec end_of_word i =
      if i >= len || stop upper.[i] then i else end_of_word (i + 1)
    in
    let i = end_of_word 0 in
    match String.sub upper 0 i with
    | "SELECT" | "WITH" | "EXPLAIN" | "VALUES" | "PRAGMA" -> true
    | _ -> false)
;;

(* Lexical state of the statement splitter.  A [;] only terminates a statement
   in [Code]; inside string literals, quoted identifiers or comments it is
   ordinary text (#389).  [In_sq]/[In_dq]/[In_backtick] all open and close with
   the same character, so a doubled quote (['']/[""]/[``]) is handled by parity
   alone: the closing quote returns us to [Code] and the immediately following
   quote re-enters, and no [;] can sit between two adjacent quote characters, so
   the in-string state at any [;] is correct without a dedicated escape state.
   [In_bracket] is asymmetric ([ opens, ] closes), so its []]] escape can NOT be
   recovered by re-entry and is handled explicitly. *)
type scan_state =
  | Code
  | In_sq (* inside a '...' string literal *)
  | In_dq (* inside a "..." quoted identifier *)
  | In_backtick (* inside a `...` quoted identifier *)
  | In_bracket (* inside a [...] quoted identifier *)
  | Line_comment (* after -- , until end of line *)
  | Block_comment (* inside /* ... */ *)

(* The single lexer both splitters share, so their state machines can never
   drift (PR #394 review).  Walks [s] left to right threading [acc]: [on_char]
   sees every character that belongs to the current statement (comment and
   quote text included; the two-character [--]/[/*]/[*/] delimiters arrive as
   two calls); [on_terminator] sees each [;] that ends a statement and is the
   only place a [;] is consumed rather than emitted. *)
let lex s ~init ~on_char ~on_terminator =
  let n = String.length s in
  let peek i = if i + 1 < n then s.[i + 1] else '\000' in
  let add2 acc a b = on_char (on_char acc a) b in
  let rec code i acc c =
    match c, peek i with
    | '\'', _ -> go (i + 1) (on_char acc c) In_sq
    | '"', _ -> go (i + 1) (on_char acc c) In_dq
    | '`', _ -> go (i + 1) (on_char acc c) In_backtick
    | '[', _ -> go (i + 1) (on_char acc c) In_bracket
    | '-', '-' -> go (i + 2) (add2 acc '-' '-') Line_comment
    | '/', '*' -> go (i + 2) (add2 acc '/' '*') Block_comment
    | ';', _ -> go (i + 1) (on_terminator acc) Code
    | _ -> go (i + 1) (on_char acc c) Code
  and go i acc state =
    if i >= n
    then acc
    else (
      let c = s.[i] in
      match state with
      | Code -> code i acc c
      | In_sq -> go (i + 1) (on_char acc c) (if c = '\'' then Code else In_sq)
      | In_dq -> go (i + 1) (on_char acc c) (if c = '"' then Code else In_dq)
      | In_backtick -> go (i + 1) (on_char acc c) (if c = '`' then Code else In_backtick)
      | In_bracket ->
        if c = ']' && peek i = ']'
        then go (i + 2) (add2 acc ']' ']') In_bracket
        else go (i + 1) (on_char acc c) (if c = ']' then Code else In_bracket)
      | Line_comment ->
        go (i + 1) (on_char acc c) (if c = '\n' then Code else Line_comment)
      | Block_comment ->
        if c = '*' && peek i = '/'
        then go (i + 2) (add2 acc '*' '/') Code
        else go (i + 1) (on_char acc c) Block_comment)
  in
  go 0 init Code
;;

let has_terminator buf =
  lex
    (Buffer.contents buf)
    ~init:false
    ~on_char:(fun acc _ -> acc)
    ~on_terminator:(fun _ -> true)
;;

let split_stmts text =
  let cur = Buffer.create 64 in
  let flush acc =
    let s = String.trim (Buffer.contents cur) in
    Buffer.clear cur;
    if s = "" then acc else s :: acc
  in
  let acc =
    lex
      text
      ~init:[]
      ~on_char:(fun acc c ->
        Buffer.add_char cur c;
        acc)
      ~on_terminator:flush
  in
  List.rev (flush acc)
;;

(* #91: import a SQLite [.dump] script. *)

(* Uppercased leading keyword of [s] (up to the first whitespace). *)
let first_word_upper s =
  let s = String.trim s in
  let len = String.length s in
  let stop c = c = ' ' || c = '\n' || c = '\t' in
  let rec end_of_word i = if i >= len || stop s.[i] then i else end_of_word (i + 1) in
  String.uppercase_ascii (String.sub s 0 (end_of_word 0))
;;

(* True iff [sub] occurs in [s] (naive search; statements are short). *)
let contains_sub s sub =
  let n = String.length s
  and m = String.length sub in
  let rec at i = (i + m <= n && String.sub s i m = sub) || (i + m < n && at (i + 1)) in
  m = 0 || at 0
;;

(* A SQLite [.dump] wraps its DDL/INSERTs in [PRAGMA foreign_keys=OFF;],
   [BEGIN TRANSACTION;] / [COMMIT;] and — for AUTOINCREMENT tables —
   maintenance of the internal [sqlite_sequence] table.  Our engine owns
   transaction control, pragmas and the [sqlite_sequence] view (#312/#314), so
   those statements are dropped; everything else is replayed verbatim. *)
let skip_dump_stmt s =
  match first_word_upper s with
  | "PRAGMA" | "BEGIN" | "COMMIT" | "END" -> true
  | ("DELETE" | "INSERT") as _kw ->
    contains_sub (String.uppercase_ascii s) "SQLITE_SEQUENCE"
  | _ -> false
;;

let sqlite_dump_stmts text =
  split_stmts text |> List.filter (fun s -> not (skip_dump_stmt s))
;;

(* Run one statement, converting both [Db] errors and any raised exception into
   a human-readable message so a single bad statement cannot abort the import. *)
let run_stmt db stmt =
  Lwt.catch
    (fun () ->
       let+ r = Db.execute db stmt in
       match r with
       | Ok () -> Ok ()
       | Error e -> Error (Format.asprintf "%a" Db.pp_error e))
    (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
;;

let import_sqlite_dump db dump =
  let stmts = sqlite_dump_stmts dump in
  let+ applied, failures =
    Lwt_list.fold_left_s
      (fun (applied, failures) stmt ->
         let+ r = run_stmt db stmt in
         match r with
         | Ok () -> applied + 1, failures
         | Error msg -> applied, (stmt, msg) :: failures)
      (0, [])
      stmts
  in
  applied, List.rev failures
;;

let open_db ~path =
  if path = ":memory:"
  then
    let* d = Db.open_in_memory () in
    Lwt.return (Ok d)
  else Sqlocaml_unix.open_file ~path ()
;;

let column_widths rows =
  match rows with
  | [] -> [||]
  | first :: _ ->
    let n_cols = Array.length first in
    let widths = Array.make n_cols 0 in
    List.iter
      (fun row ->
         Array.iteri
           (fun i v ->
              let len = String.length (value_to_string v) in
              if len > widths.(i) then widths.(i) <- len)
           row)
      rows;
    widths
;;
