(** Mined-SQLite-corpus runner. Replays each .sql file against Mem and Btree
    backends, comparing observed SELECT rows to inline `-- expect:` markers. *)

open Lwt.Syntax

module Db = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
end

(* ------------------------------------------------------------------ *)
(* Parser for the .sql + -- expect: format                               *)
(* ------------------------------------------------------------------ *)

type step =
  | Exec of string (* SQL statement, no result expected *)
  | Query of string * string list (* SELECT + expected rows *)

let split_rows s =
  (* "1|2||3|4" => ["1|2"; "3|4"]. Empty input yields []. *)
  if s = ""
  then []
  else (
    let rec aux acc start i =
      if i >= String.length s
      then List.rev (String.sub s start (i - start) :: acc)
      else if i + 1 < String.length s && s.[i] = '|' && s.[i + 1] = '|'
      then aux (String.sub s start (i - start) :: acc) (i + 2) (i + 2)
      else aux acc start (i + 1)
    in
    aux [] 0 0)
;;

let trim_stmt s =
  let s = String.trim s in
  if String.length s > 0 && s.[String.length s - 1] = ';'
  then String.trim (String.sub s 0 (String.length s - 1))
  else s
;;

(* A SELECT seen with no "-- expect:" before the next statement is run as Exec. *)
let flush_pending ~steps ~pending_select () =
  match !pending_select with
  | None -> ()
  | Some q ->
    steps := Exec q :: !steps;
    pending_select := None
;;

let handle_stmt ~steps ~pending_select stmt =
  let stmt = trim_stmt stmt in
  if stmt = ""
  then ()
  else (
    flush_pending ~steps ~pending_select ();
    let upper = String.uppercase_ascii (String.sub stmt 0 (min 6 (String.length stmt))) in
    if
      String.length upper >= 6
      && (String.sub upper 0 6 = "SELECT" || String.sub upper 0 4 = "WITH")
    then pending_select := Some stmt
    else steps := Exec stmt :: !steps)
;;

let handle_line ~buf ~steps ~pending_select ~path line =
  let line = String.trim line in
  if line = ""
  then ()
  else if String.length line >= 9 && String.sub line 0 9 = "-- expect"
  then (
    let colon =
      try String.index line ':' with
      | Not_found -> -1
    in
    let expected_str =
      if colon = -1
      then ""
      else String.trim (String.sub line (colon + 1) (String.length line - colon - 1))
    in
    let rows = split_rows expected_str in
    (match !pending_select with
     | None -> failwith ("expect: with no preceding SELECT in " ^ path)
     | Some q -> steps := Query (q, rows) :: !steps);
    pending_select := None)
  else if String.length line >= 2 && String.sub line 0 2 = "--"
  then ()
  else (
    Buffer.add_string buf line;
    Buffer.add_char buf ' ';
    if String.length line > 0 && line.[String.length line - 1] = ';'
    then (
      handle_stmt ~steps ~pending_select (Buffer.contents buf);
      Buffer.clear buf))
;;

let parse_file path =
  let ic = open_in path in
  let buf = Buffer.create 128 in
  let steps = ref [] in
  let pending_select = ref None in
  (* the most recent SELECT awaiting expect: *)
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with
   | End_of_file -> ());
  close_in ic;
  let lines = List.rev !lines in
  List.iter (handle_line ~buf ~steps ~pending_select ~path) lines;
  if Buffer.length buf > 0 then handle_stmt ~steps ~pending_select (Buffer.contents buf);
  flush_pending ~steps ~pending_select ();
  List.rev !steps
;;

let row_to_string row =
  Array.to_list row
  |> List.map (function
    | Db.V_int n -> Int64.to_string n
    | Db.V_text s -> s
    | Db.V_null -> "NULL"
    (* NaN/Inf fall through to %g and print as "nan"/"inf"; corpus avoids
       non-finite arithmetic so this is unreachable. *)
    | Db.V_real f ->
      (* Match sqlite default formatting: trim trailing zeros where possible *)
      if Float.is_integer f then Printf.sprintf "%.1f" f else Printf.sprintf "%g" f
    | Db.V_blob b -> Printf.sprintf "blob(%d)" (Bytes.length b))
  |> String.concat "|"
;;

let exec_stmt db sql =
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error _ -> Alcotest.failf "exec failed: %s" sql
;;

let query_rows db sql =
  let* r = Db.query db sql in
  match r with
  | Error _ -> Alcotest.failf "query failed: %s" sql
  | Ok stream ->
    let* rows = Lwt_stream.to_list stream in
    Lwt.return (List.map row_to_string rows)
;;

let run_file open_db close_db path =
  Lwt_main.run
    (let steps = parse_file path in
     let* db = open_db () in
     Lwt.finalize
       (fun () ->
          Lwt_list.iter_s
            (fun step ->
               match step with
               | Exec sql -> exec_stmt db sql
               | Query (sql, expected) ->
                 let* actual = query_rows db sql in
                 Alcotest.(check (list string))
                   (Printf.sprintf "%s :: %s" (Filename.basename path) sql)
                   expected
                   actual;
                 Lwt.return_unit)
            steps)
       (fun () -> close_db db))
;;

let open_mem () = Db.open_in_memory ()
let close_mem = Db.close

let open_file path () =
  let* r = Db.open_file ~path () in
  match r with
  | Ok db -> Lwt.return db
  | Error e ->
    let msg =
      match e with
      | Db.Parse s -> "Parse: " ^ s
      | Db.Runtime s -> "Runtime: " ^ s
      | Db.Sema _ -> "Sema error"
      | e -> Format.asprintf "%a" Db.pp_error e
    in
    Alcotest.failf "open_file failed: %s" msg
;;

let close_file path db =
  let* () = Db.close db in
  (try Unix.unlink path with
   | _ -> ());
  Lwt.return_unit
;;

let corpus_dir = "sqlite_corpus"

let corpus_files () =
  Sys.readdir corpus_dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".sql")
  |> List.sort compare
  |> List.map (Filename.concat corpus_dir)
;;

let () =
  let files = corpus_files () in
  let mem_cases =
    List.map
      (fun f ->
         Alcotest.test_case (Filename.basename f) `Quick (fun () ->
           run_file open_mem close_mem f))
      files
  in
  let file_cases =
    List.map
      (fun f ->
         Alcotest.test_case (Filename.basename f) `Quick (fun () ->
           let path = Filename.temp_file "granary_corpus_" ".db" in
           Fun.protect
             ~finally:(fun () ->
               try Unix.unlink path with
               | _ -> ())
             (fun () -> run_file (open_file path) (close_file path) f)))
      files
  in
  Alcotest.run "sqlite_corpus" [ "mem", mem_cases; "btree", file_cases ]
;;
