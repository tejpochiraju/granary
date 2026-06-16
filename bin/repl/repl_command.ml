type dot =
  | Help
  | Quit
  | Tables
  | Schema of string option
  | Databases
  | Open of string
  | Dump of string option
  | Unknown of string

type prompt =
  | No_prompt
  | Txn_prompt
  | Table_prompt

type filter_input =
  | Filter_txn of int64 option
  | Filter_table of string

type action =
  | Empty
  | Filter of filter_input
  | Dot of dot
  | Sql of string list

let parse_dot line =
  let trimmed = String.trim line in
  let parts = String.split_on_char ' ' trimmed |> List.filter (fun s -> s <> "") in
  match parts with
  | [ ".help" ] -> Help
  | [ ".quit" ] | [ ".exit" ] -> Quit
  | [ ".tables" ] -> Tables
  | [ ".schema" ] -> Schema None
  | [ ".schema"; name ] -> Schema (Some name)
  | [ ".databases" ] -> Databases
  | [ ".open"; path ] -> Open path
  | [ ".dump" ] -> Dump None
  | [ ".dump"; path ] -> Dump (Some path)
  | _ -> Unknown trimmed
;;

let classify ~prompt input =
  match prompt with
  | Txn_prompt -> Filter (Filter_txn (Int64.of_string_opt (String.trim input)))
  | Table_prompt -> Filter (Filter_table (String.trim input))
  | No_prompt ->
    let t = String.trim input in
    if t = ""
    then Empty
    else if t.[0] = '.'
    then Dot (parse_dot t)
    else Sql (Repl_engine.split_stmts input)
;;

let tables_sql = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
let databases_sql = "PRAGMA database_list"

let schema_sql arg_opt =
  match arg_opt with
  | None -> "SELECT sql FROM sqlite_master WHERE sql IS NOT NULL ORDER BY name"
  | Some name ->
    Printf.sprintf
      "SELECT sql FROM sqlite_master WHERE name = '%s' AND sql IS NOT NULL"
      (String.concat "''" (String.split_on_char '\'' name))
;;
