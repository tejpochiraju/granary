open Lwt.Syntax
module Db = Granary.Db

let value_to_string = function
  | Db.V_null -> "NULL"
  | Db.V_int n -> Int64.to_string n
  | Db.V_real f -> Printf.sprintf "%.17g" f
  | Db.V_text s -> s
  | Db.V_blob b -> Printf.sprintf "<blob:%d>" (Bytes.length b)
;;

let print_row row =
  Array.iteri
    (fun i v ->
       if i > 0 then print_char '|';
       print_string (value_to_string v))
    row;
  print_newline ()
;;

let is_query_stmt sql =
  let s = String.trim sql in
  if s = ""
  then false
  else (
    let upper = String.uppercase_ascii s in
    let first_word =
      match
        ( String.index_opt upper ' '
        , String.index_opt upper '\n'
        , String.index_opt upper '\t' )
      with
      | None, None, None -> upper
      | Some i, None, None -> String.sub upper 0 i
      | None, Some i, None -> String.sub upper 0 i
      | None, None, Some i -> String.sub upper 0 i
      | Some a, Some b, None -> String.sub upper 0 (min a b)
      | Some a, None, Some b -> String.sub upper 0 (min a b)
      | None, Some a, Some b -> String.sub upper 0 (min a b)
      | Some a, Some b, Some c -> String.sub upper 0 (min a (min b c))
    in
    (* Note: writeable CTEs (WITH ... INSERT/UPDATE/DELETE) are mis-routed to
       Db.query and will fail at runtime rather than execute. *)
    match first_word with
    | "SELECT" | "WITH" | "EXPLAIN" | "VALUES" | "PRAGMA" -> true
    | _ -> false)
;;

let run_sql db sql =
  let stmts =
    (* Note: splitting on ';' does not handle semicolons inside string literals.
       Such inputs produce parse errors rather than silent corruption. *)
    String.split_on_char ';' sql |> List.map String.trim |> List.filter (fun s -> s <> "")
  in
  Lwt_list.iter_s
    (fun stmt ->
       if is_query_stmt stmt
       then
         let* result = Db.query db stmt in
         match result with
         | Error e ->
           Format.eprintf "Error: %a\n%!" Db.pp_error e;
           Lwt.return_unit
         | Ok stream -> Lwt_stream.iter print_row stream
       else
         let* result = Db.execute_change_count db stmt in
         match result with
         | Error e ->
           Format.eprintf "Error: %a\n%!" Db.pp_error e;
           Lwt.return_unit
         | Ok n ->
           if n > 0 then Printf.printf "%d row(s) affected\n%!" n;
           Lwt.return_unit)
    stmts
;;

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s
;;

let read_stdin () =
  let buf = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    let n = input stdin chunk 0 4096 in
    if n = 0
    then Buffer.contents buf
    else (
      Buffer.add_subbytes buf chunk 0 n;
      loop ())
  in
  loop ()
;;

let () =
  (* Enable Unix file operations (ATTACH / VACUUM) for this process. *)
  Granary_unix.install ();
  let args = Array.to_list Sys.argv |> List.tl in
  let db_path, sql_source =
    match args with
    | [] -> ":memory:", `Stdin
    | [ path ] -> path, `Stdin
    | path :: f :: _ -> path, `File f
  in
  let sql =
    match sql_source with
    | `Stdin -> read_stdin ()
    | `File f -> read_file f
  in
  Lwt_main.run
    (let* db =
       if db_path = ":memory:"
       then Db.open_in_memory ()
       else
         let* result = Granary_unix.open_file ~path:db_path () in
         match result with
         | Ok db -> Lwt.return db
         | Error e ->
           Format.eprintf "Cannot open '%s': %a\n%!" db_path Db.pp_error e;
           exit 1
     in
     let* () = run_sql db sql in
     Db.close db)
;;
