open Lwt.Syntax
module Db = Sqlocaml.Db
module Row = Sqlocaml_encoding.Row

let ok_or_fail = function
  | Ok x -> x
  | Error e -> failwith (Format.asprintf "%a" Db.pp_error e)
;;

let make_db () =
  let db = Lwt_main.run (Db.open_in_memory ()) in
  db
;;

let exec db sql =
  let* result = Db.execute db sql in
  let () = ok_or_fail result in
  Lwt.return_unit
;;

let query db sql =
  let* result = Db.query db sql in
  let stream = ok_or_fail result in
  Lwt_stream.to_list stream
;;

let test_create_columnar () =
  let db = make_db () in
  let* () = exec db "CREATE TABLE t (region TEXT, amount REAL) USING COLUMNSTORE" in
  let* rows = query db "SELECT name FROM sqlite_master WHERE type='table'" in
  let names =
    List.filter_map
      (function
        | [| Row.V_text n |] -> Some n
        | _ -> None)
      rows
  in
  assert (List.mem "t" names);
  Lwt.return_unit
;;

let () =
  Printf.printf "%-45s" "create_columnar...";
  flush stdout;
  Lwt_main.run (test_create_columnar ());
  print_endline "PASS"
;;
