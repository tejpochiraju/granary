(** Db.set_event_callback passthrough (#382). *)

module D = struct
  include Sqlocaml.Db

  let open_file_wal = Sqlocaml_unix.open_file_wal
end

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

let () =
  Sqlocaml_unix.install ();
  Alcotest.run
    "db_event"
    [ "passthrough", [ Alcotest.test_case "fires" `Quick test_passthrough_fires ] ]
;;
