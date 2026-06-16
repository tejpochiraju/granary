(** Db.set_event_callback passthrough (#382). *)

module D = struct
  include Sqlocaml.Db

  let open_file_wal = Sqlocaml_unix.open_file_wal
end

module L = Event_log
module Ev = Sqlocaml.Db.Event

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_dbevent_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let test_passthrough_fires () =
  let path = fresh_path () in
  cleanup path;
  let seen = ref 0 in
  let n =
    Lwt.finalize
      (fun () ->
         let open Lwt.Syntax in
         let* db = D.open_file_wal ~path () in
         let db = Result.get_ok db in
         D.set_event_callback db (Some (fun _ -> incr seen));
         let* _ = D.execute db "CREATE TABLE t(x INTEGER)" in
         let* _ = D.execute db "INSERT INTO t VALUES (1)" in
         let* () = D.close db in
         Lwt.return !seen)
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool) "events fired through Db" true (n > 0)
;;

let test_tree_of_table () =
  let path = fresh_path () in
  cleanup path;
  let open Lwt.Syntax in
  let found, missing =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file_wal ~path () in
         let db = Result.get_ok db in
         let* _ = D.execute db "CREATE TABLE t(x INTEGER)" in
         let found = D.tree_of_table db "t" in
         let missing = D.tree_of_table db "nope" in
         let* () = D.close db in
         Lwt.return (found, missing))
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool) "known table resolves" true (found <> None);
  Alcotest.(check (option int)) "unknown table is None" None missing
;;

let test_table_filter_resolution () =
  let path = fresh_path () in
  cleanup path;
  let open Lwt.Syntax in
  let visible_trees, a_tree, b_tree =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file_wal ~path () in
         let db = Result.get_ok db in
         let* _ = D.execute db "CREATE TABLE a(x INTEGER)" in
         let* _ = D.execute db "CREATE TABLE b(x INTEGER)" in
         let log = L.create ~capacity:5000 in
         D.set_event_callback db (Some (fun ev -> L.push log ev));
         (* writes to both tables produce page events stamped with each tree *)
         let* _ = D.execute db "INSERT INTO a VALUES (1)" in
         let* _ = D.execute db "INSERT INTO b VALUES (2)" in
         let a_tree = D.tree_of_table db "a" in
         let b_tree = D.tree_of_table db "b" in
         (match a_tree with
          | Some tree -> L.set_filter log (L.By_table { name = "a"; tree })
          | None -> ());
         let visible_trees =
           List.filter_map Ev.tree_id_of (L.visible log) |> List.sort_uniq compare
         in
         let* () = D.close db in
         Lwt.return (visible_trees, a_tree, b_tree))
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool) "table a resolved" true (a_tree <> None);
  Alcotest.(check bool) "table b resolved" true (b_tree <> None);
  let a = Option.get a_tree in
  let b = Option.get b_tree in
  Alcotest.(check bool) "a and b are distinct trees" true (a <> b);
  (* under the By_table=a filter, every visible page event belongs to tree a;
     tree b never appears *)
  Alcotest.(check bool)
    "only table a's tree visible"
    true
    (List.for_all (fun t -> t = a) visible_trees);
  Alcotest.(check bool) "table b's tree filtered out" false (List.mem b visible_trees);
  Alcotest.(check bool) "table a actually had page events" true (List.mem a visible_trees)
;;

let () =
  Sqlocaml_unix.install ();
  Alcotest.run
    "db_event"
    [ "passthrough", [ Alcotest.test_case "fires" `Quick test_passthrough_fires ]
    ; "tree_of_table", [ Alcotest.test_case "resolves" `Quick test_tree_of_table ]
    ; ( "table_filter"
      , [ Alcotest.test_case "resolution + filter" `Quick test_table_filter_resolution ] )
    ]
;;
