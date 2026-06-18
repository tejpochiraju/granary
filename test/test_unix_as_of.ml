module U = Sqlocaml_unix.Store
module H = Sqlocaml_store.History
open Lwt.Syntax

let with_temp f =
  let path = Filename.temp_file "aslog" ".log" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () -> f path)
;;

let test_file_roundtrip () =
  with_temp (fun path ->
    Lwt_main.run
      (let sink = U.file_history_sink ~path in
       let* () = sink.H.append { txn_id = 1L; timestamp = 10L; root_page = 100L } in
       let* () = sink.H.append { txn_id = 2L; timestamp = 20L; root_page = 200L } in
       let sink2 = U.file_history_sink ~path in
       let* recs = sink2.H.load () in
       assert (
         recs
         = [ { H.txn_id = 1L; timestamp = 10L; root_page = 100L }
           ; { H.txn_id = 2L; timestamp = 20L; root_page = 200L }
           ]);
       Lwt.return_unit))
;;

let test_torn_tail_dropped () =
  with_temp (fun path ->
    Lwt_main.run
      (let sink = U.file_history_sink ~path in
       let* () = sink.H.append { txn_id = 1L; timestamp = 10L; root_page = 100L } in
       let* () = sink.H.append { txn_id = 2L; timestamp = 20L; root_page = 200L } in
       Lwt.return_unit);
    (* truncate the file by 3 bytes to corrupt the last record's tail *)
    let len = (Unix.stat path).st_size in
    let fd = Unix.openfile path [ Unix.O_WRONLY ] 0 in
    Unix.ftruncate fd (len - 3);
    Unix.close fd;
    Lwt_main.run
      (let sink = U.file_history_sink ~path in
       let* recs = sink.H.load () in
       assert (recs = [ { H.txn_id = 1L; timestamp = 10L; root_page = 100L } ]);
       Lwt.return_unit))
;;

let test_missing_file () =
  let path = Filename.temp_file "aslog" ".log" in
  Sys.remove path;
  Lwt_main.run
    (let sink = U.file_history_sink ~path in
     let* recs = sink.H.load () in
     assert (recs = []);
     Lwt.return_unit)
;;

let () =
  test_file_roundtrip ();
  test_torn_tail_dropped ();
  test_missing_file ();
  print_endline "test_unix_as_of: OK"
;;
