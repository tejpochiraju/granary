(** Tests for #382 — Store_event type + the Store on_event seam. *)

module Ev = Sqlocaml_store.Store_event

let test_label_and_txn_id () =
  let c = Ev.Txn_commit { txn_id = 7L; frames = 3 } in
  Alcotest.(check string) "label" "COMMIT" (Ev.label c);
  Alcotest.(check (option int64)) "txn_id" (Some 7L) (Ev.txn_id c);
  Alcotest.(check (option int64))
    "wal_reset has no txn"
    None
    (Ev.txn_id (Ev.Wal_reset { epoch = 2L }))
;;

let test_pp_roundtrip_nonempty () =
  let s = Format.asprintf "%a" Ev.pp (Ev.Txn_begin { txn_id = 1L }) in
  Alcotest.(check bool) "pp non-empty" true (String.length s > 0)
;;

module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_event_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

(* Collect event labels into a ref while [f] runs against a fresh store. *)
let with_recorder ~f =
  let path = fresh_path () in
  cleanup path;
  let seen = ref [] in
  Lwt.finalize
    (fun () ->
       let open Lwt.Syntax in
       let* st = S.open_file_wal ~path () in
       let st = Result.get_ok st in
       S.set_event_callback st (Some (fun ev -> seen := Ev.label ev :: !seen));
       let* () = f st in
       let* () = S.close st in
       Lwt.return (List.rev !seen))
    (fun () ->
       cleanup path;
       Lwt.return_unit)
  |> run
;;

let bs = Bytes.of_string

let test_commit_emits_begin_and_commit () =
  let labels =
    with_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      S.commit txn)
  in
  Alcotest.(check bool) "has BEGIN" true (List.mem "BEGIN" labels);
  Alcotest.(check bool) "has COMMIT" true (List.mem "COMMIT" labels)
;;

let test_rollback_emits_rollback () =
  let labels =
    with_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      S.rollback txn)
  in
  Alcotest.(check bool) "has ROLLBACK" true (List.mem "ROLLBACK" labels);
  Alcotest.(check bool) "no COMMIT" false (List.mem "COMMIT" labels)
;;

let () =
  Alcotest.run
    "store_event"
    [ ( "type"
      , [ Alcotest.test_case "label + txn_id" `Quick test_label_and_txn_id
        ; Alcotest.test_case "pp non-empty" `Quick test_pp_roundtrip_nonempty
        ] )
    ; ( "seam"
      , [ Alcotest.test_case
            "commit emits begin+commit"
            `Quick
            test_commit_emits_begin_and_commit
        ; Alcotest.test_case "rollback emits rollback" `Quick test_rollback_emits_rollback
        ] )
    ]
;;
