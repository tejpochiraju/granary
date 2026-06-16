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

let has_terminator buf =
  let s = Buffer.contents buf in
  let n = String.length s in
  let rec loop i in_sq in_dq =
    if i >= n
    then false
    else (
      let c = s.[i] in
      if c = '\'' && not in_dq
      then loop (i + 1) (not in_sq) in_dq
      else if c = '"' && not in_sq
      then loop (i + 1) in_sq (not in_dq)
      else if c = ';' && (not in_sq) && not in_dq
      then true
      else loop (i + 1) in_sq in_dq)
  in
  loop 0 false false
;;

let split_stmts text =
  let n = String.length text in
  let cur = Buffer.create 64 in
  let flush acc =
    let s = String.trim (Buffer.contents cur) in
    Buffer.clear cur;
    if s = "" then acc else s :: acc
  in
  let rec scan i acc in_sq in_dq =
    if i >= n
    then List.rev (flush acc)
    else (
      let c = text.[i] in
      if c = '\'' && not in_dq
      then (
        Buffer.add_char cur c;
        scan (i + 1) acc (not in_sq) in_dq)
      else if c = '"' && not in_sq
      then (
        Buffer.add_char cur c;
        scan (i + 1) acc in_sq (not in_dq))
      else if c = ';' && (not in_sq) && not in_dq
      then scan (i + 1) (flush acc) in_sq in_dq
      else (
        Buffer.add_char cur c;
        scan (i + 1) acc in_sq in_dq))
  in
  scan 0 [] false false
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
