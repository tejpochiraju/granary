(** sqlocaml interactive shell (phase 40 / #70).

    A modest sqlite3-style REPL.  Goals:
    - Multi-line input accumulated until a terminating semicolon (outside
      string literals).
    - Dot commands: [.help], [.quit] / [.exit], [.tables], [.schema [name]],
      [.open <path>], [.databases].
    - Per-statement error recovery: parse / sema / runtime errors print a
      message but do not exit the loop.
    - Column-header output for query results, separator-aligned for narrow
      tables.

    Non-goals: history, completion, syntax highlighting, line editing
    beyond what the host terminal provides.  Users wanting readline can
    pipe through [rlwrap]. *)

open Lwt.Syntax
module Db = Sqlocaml.Db

(* ------------------------------------------------------------------ *)
(* Output formatting                                                    *)
(* ------------------------------------------------------------------ *)

let value_to_string = function
  | Db.V_null   -> "NULL"
  | Db.V_int n  -> Int64.to_string n
  | Db.V_real f -> Printf.sprintf "%.17g" f
  | Db.V_text s -> s
  | Db.V_blob b -> Printf.sprintf "<blob:%d>" (Bytes.length b)

(* Pretty-print a list of result rows in a pipe-separated table.  Each
   column is padded to its widest entry.  Without headers (we have no
   way to read column names back from the engine for a generic SELECT)
   the output prints just the data rows aligned by their own widths. *)
let print_rows rows =
  match rows with
  | [] -> ()
  | first :: _ ->
    let n_cols = Array.length first in
    let widths = Array.make n_cols 0 in
    List.iter (fun row ->
      Array.iteri (fun i v ->
        let len = String.length (value_to_string v) in
        if len > widths.(i) then widths.(i) <- len
      ) row
    ) rows;
    List.iter (fun row ->
      Array.iteri (fun i v ->
        if i > 0 then print_string " | ";
        let s = value_to_string v in
        let pad = widths.(i) - String.length s in
        print_string s;
        if pad > 0 then print_string (String.make pad ' ')
      ) row;
      print_newline ()
    ) rows

(* ------------------------------------------------------------------ *)
(* Statement classification                                             *)
(* ------------------------------------------------------------------ *)

let is_query_stmt sql =
  let s = String.trim sql in
  if s = "" then false
  else
    let upper = String.uppercase_ascii s in
    let stop c = c = ' ' || c = '\n' || c = '\t' in
    let len = String.length upper in
    let rec end_of_word i =
      if i >= len || stop upper.[i] then i else end_of_word (i + 1)
    in
    let i = end_of_word 0 in
    let first = String.sub upper 0 i in
    match first with
    | "SELECT" | "WITH" | "EXPLAIN" | "VALUES" | "PRAGMA" -> true
    | _ -> false

(* ------------------------------------------------------------------ *)
(* Multi-line input — accumulate until terminating ';' outside strings. *)
(* ------------------------------------------------------------------ *)

(* Returns true if [buf] contains a terminating semicolon outside any
   single-quoted string.  We scan character-by-character; double-quoted
   identifiers behave the same as single-quoted strings for this
   purpose. *)
let has_terminator buf =
  let s = Buffer.contents buf in
  let n = String.length s in
  let rec loop i in_squote in_dquote =
    if i >= n then false
    else
      let c = s.[i] in
      if c = '\'' && not in_dquote then loop (i + 1) (not in_squote) in_dquote
      else if c = '"' && not in_squote then loop (i + 1) in_squote (not in_dquote)
      else if c = ';' && (not in_squote) && (not in_dquote) then true
      else loop (i + 1) in_squote in_dquote
  in
  loop 0 false false

(* ------------------------------------------------------------------ *)
(* Database open helpers                                                *)
(* ------------------------------------------------------------------ *)

let open_db ~path =
  if path = ":memory:" then
    let* d = Db.open_in_memory () in
    Lwt.return (Ok d)
  else
    Db.open_file ~path

(* ------------------------------------------------------------------ *)
(* Dot commands                                                         *)
(* ------------------------------------------------------------------ *)

let dot_help () =
  print_endline ".help                       this list";
  print_endline ".quit | .exit               leave the shell";
  print_endline ".tables                     list tables in the active schema";
  print_endline ".schema [name]              show CREATE statements (optionally for one table)";
  print_endline ".open <path>                close current db and open the given path";
  print_endline ".databases                  list attached databases";
  print_endline "";
  print_endline "Multi-line SQL: type until a terminating ';' on its own or trailing a line."

let dot_tables db =
  match Lwt_main.run (Db.query db "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name") with
  | Error e -> Format.printf "Error: %a\n%!" Db.pp_error e
  | Ok stream ->
    let rows = Lwt_main.run (Lwt_stream.to_list stream) in
    List.iter (fun row ->
      match row.(0) with
      | Db.V_text s -> print_endline s
      | _ -> ()
    ) rows

let dot_schema db arg_opt =
  let sql = match arg_opt with
    | None       -> "SELECT sql FROM sqlite_master WHERE sql IS NOT NULL ORDER BY name"
    | Some name  -> Printf.sprintf
        "SELECT sql FROM sqlite_master WHERE name = '%s' AND sql IS NOT NULL"
        (String.concat "''" (String.split_on_char '\'' name))
  in
  match Lwt_main.run (Db.query db sql) with
  | Error e -> Format.printf "Error: %a\n%!" Db.pp_error e
  | Ok stream ->
    let rows = Lwt_main.run (Lwt_stream.to_list stream) in
    List.iter (fun row ->
      match row.(0) with
      | Db.V_text s -> print_string s; print_endline ";"
      | _ -> ()
    ) rows

let dot_databases db =
  match Lwt_main.run (Db.query db "PRAGMA database_list") with
  | Error e -> Format.printf "Error: %a\n%!" Db.pp_error e
  | Ok stream ->
    let rows = Lwt_main.run (Lwt_stream.to_list stream) in
    print_rows rows

(* Returns [Some new_db] when the command opened a new db (caller swaps);
   [None] otherwise.  Always prints any feedback. *)
let dispatch_dot ~db line =
  let trimmed = String.trim line in
  let parts = String.split_on_char ' ' trimmed |> List.filter (fun s -> s <> "") in
  match parts with
  | [".help"] -> dot_help (); None
  | [".quit"] | [".exit"] -> exit 0
  | [".tables"] -> dot_tables db; None
  | [".schema"] -> dot_schema db None; None
  | [".schema"; name] -> dot_schema db (Some name); None
  | [".databases"] -> dot_databases db; None
  | [".open"; path] ->
    (match Lwt_main.run (open_db ~path) with
     | Ok new_db ->
       Lwt_main.run (Db.close db);
       Printf.printf "Opened %s\n%!" path;
       Some new_db
     | Error e ->
       Format.printf "Error opening '%s': %a\n%!" path Db.pp_error e;
       None)
  | _ ->
    Printf.printf "Unknown dot command: %s (try .help)\n%!" trimmed;
    None

(* ------------------------------------------------------------------ *)
(* Statement execution                                                  *)
(* ------------------------------------------------------------------ *)

let run_one_stmt db stmt =
  let s = String.trim stmt in
  if s = "" then ()
  else if is_query_stmt s then begin
    match Lwt_main.run (Db.query db s) with
    | Error e -> Format.printf "Error: %a\n%!" Db.pp_error e
    | Ok stream ->
      let rows = Lwt_main.run (Lwt_stream.to_list stream) in
      print_rows rows
  end else begin
    match Lwt_main.run (Db.execute_change_count db s) with
    | Error e -> Format.printf "Error: %a\n%!" Db.pp_error e
    | Ok n ->
      if n > 0 then Printf.printf "%d row(s) affected\n%!" n
  end

(** Split a complete buffer (containing one or more semicolons) into
    individual statements.  This is a coarse split: it respects quoting
    so SQL with semicolons inside strings is not torn. *)
let split_stmts text =
  let n = String.length text in
  let rec scan i acc cur in_squote in_dquote =
    if i >= n then
      let last = String.trim cur in
      if last = "" then List.rev acc else List.rev (last :: acc)
    else
      let c = text.[i] in
      if c = '\'' && not in_dquote then
        scan (i + 1) acc (cur ^ String.make 1 c) (not in_squote) in_dquote
      else if c = '"' && not in_squote then
        scan (i + 1) acc (cur ^ String.make 1 c) in_squote (not in_dquote)
      else if c = ';' && not in_squote && not in_dquote then begin
        let s = String.trim cur in
        let acc' = if s = "" then acc else s :: acc in
        scan (i + 1) acc' "" in_squote in_dquote
      end else
        scan (i + 1) acc (cur ^ String.make 1 c) in_squote in_dquote
  in
  scan 0 [] "" false false

(* ------------------------------------------------------------------ *)
(* REPL loop                                                            *)
(* ------------------------------------------------------------------ *)

let primary_prompt   = "sqlocaml> "
let continue_prompt  = "      ...> "

let banner () =
  print_endline "sqlocaml interactive shell.";
  print_endline "Enter SQL terminated with ';'.  Type .help for shell commands."

(* Read one logical command (either a dot command or a complete
   SQL statement up to ';').  Returns [None] on EOF. *)
let read_one () =
  let buf = Buffer.create 256 in
  let rec loop primary =
    print_string (if primary then primary_prompt else continue_prompt);
    let () = try flush stdout with Sys_error _ -> () in
    match input_line stdin with
    | exception End_of_file ->
      if Buffer.length buf = 0 then None
      else Some (Buffer.contents buf)
    | line ->
      let stripped = String.trim line in
      if primary && String.length stripped > 0 && stripped.[0] = '.' then
        Some stripped
      else begin
        if Buffer.length buf > 0 then Buffer.add_char buf '\n';
        Buffer.add_string buf line;
        if has_terminator buf then Some (Buffer.contents buf)
        else loop false
      end
  in
  loop true

let main () =
  let path = match Array.to_list Sys.argv |> List.tl with
    | []         -> ":memory:"
    | path :: _  -> path
  in
  let db_ref = ref (
    match Lwt_main.run (open_db ~path) with
    | Ok d -> d
    | Error e ->
      Format.eprintf "Cannot open '%s': %a\n%!" path Db.pp_error e;
      exit 1
  ) in
  banner ();
  Printf.printf "Open: %s\n%!" path;
  let rec loop () =
    match read_one () with
    | None -> print_newline (); exit 0
    | Some s ->
      let trimmed = String.trim s in
      if trimmed = "" then loop ()
      else if String.length trimmed > 0 && trimmed.[0] = '.' then begin
        (match dispatch_dot ~db:!db_ref trimmed with
         | Some new_db -> db_ref := new_db
         | None        -> ());
        loop ()
      end else begin
        List.iter (run_one_stmt !db_ref) (split_stmts s);
        loop ()
      end
  in
  loop ()

let () = main ()
