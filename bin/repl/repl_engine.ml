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
   in [Code]; inside string literals or comments it is ordinary text.  Doubled
   string quotes (['']/[""]) are handled naturally: the closing quote returns us
   to [Code] and the immediately following quote re-enters the string, so the
   in-string parity at any [;] is correct without a dedicated escape state
   (#389). *)
type scan_state =
  | Code
  | In_sq (* inside a '...' string literal *)
  | In_dq (* inside a "..." quoted identifier *)
  | Line_comment (* after -- , until end of line *)
  | Block_comment (* inside /* ... */ *)

let has_terminator buf =
  let s = Buffer.contents buf in
  let n = String.length s in
  let peek i = if i + 1 < n then s.[i + 1] else '\000' in
  let rec scan i state =
    if i >= n
    then false
    else (
      let c = s.[i] in
      match state with
      | Code ->
        (match c, peek i with
         | '\'', _ -> scan (i + 1) In_sq
         | '"', _ -> scan (i + 1) In_dq
         | '-', '-' -> scan (i + 2) Line_comment
         | '/', '*' -> scan (i + 2) Block_comment
         | ';', _ -> true
         | _ -> scan (i + 1) Code)
      | In_sq -> scan (i + 1) (if c = '\'' then Code else In_sq)
      | In_dq -> scan (i + 1) (if c = '"' then Code else In_dq)
      | Line_comment -> scan (i + 1) (if c = '\n' then Code else Line_comment)
      | Block_comment ->
        if c = '*' && peek i = '/' then scan (i + 2) Code else scan (i + 1) Block_comment)
  in
  scan 0 Code
;;

let split_stmts text =
  let n = String.length text in
  let cur = Buffer.create 64 in
  let add c = Buffer.add_char cur c in
  let peek i = if i + 1 < n then text.[i + 1] else '\000' in
  let flush acc =
    let s = String.trim (Buffer.contents cur) in
    Buffer.clear cur;
    if s = "" then acc else s :: acc
  in
  let code i acc c scan =
    match c, peek i with
    | '\'', _ ->
      add c;
      scan (i + 1) acc In_sq
    | '"', _ ->
      add c;
      scan (i + 1) acc In_dq
    | '-', '-' ->
      add '-';
      add '-';
      scan (i + 2) acc Line_comment
    | '/', '*' ->
      add '/';
      add '*';
      scan (i + 2) acc Block_comment
    | ';', _ -> scan (i + 1) (flush acc) Code
    | _ ->
      add c;
      scan (i + 1) acc Code
  in
  let rec scan i acc state =
    if i >= n
    then List.rev (flush acc)
    else (
      let c = text.[i] in
      match state with
      | Code -> code i acc c scan
      | In_sq ->
        add c;
        scan (i + 1) acc (if c = '\'' then Code else In_sq)
      | In_dq ->
        add c;
        scan (i + 1) acc (if c = '"' then Code else In_dq)
      | Line_comment ->
        add c;
        scan (i + 1) acc (if c = '\n' then Code else Line_comment)
      | Block_comment ->
        if c = '*' && peek i = '/'
        then (
          add '*';
          add '/';
          scan (i + 2) acc Code)
        else (
          add c;
          scan (i + 1) acc Block_comment))
  in
  scan 0 [] Code
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
