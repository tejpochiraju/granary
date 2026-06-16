(** Tests for #382 — Store_event type + the Store on_event seam. *)

module Ev = Sqlocaml_store.Store_event

let test_label_and_txn_id () =
  let c = Ev.Txn_commit { txn_id = 7L; frames = 3 } in
  Alcotest.(check string) "label" "COMMIT" (Ev.label c);
  Alcotest.(check (option int64)) "txn_id" (Some 7L) (Ev.txn_id c);
  Alcotest.(check (option int64))
    "wal_reset has no txn"
    None
    (Ev.txn_id (Ev.Wal_reset { epoch = 2L }));
  Alcotest.(check string)
    "page_read label"
    "PAGE_READ"
    (Ev.label (Ev.Page_read { txn_id = 5L; page = 1L }));
  Alcotest.(check (option int64))
    "page_read txn"
    (Some 5L)
    (Ev.txn_id (Ev.Page_read { txn_id = 5L; page = 1L }))
;;

let test_pp_roundtrip_nonempty () =
  let s = Format.asprintf "%a" Ev.pp (Ev.Txn_begin { txn_id = 1L }) in
  Alcotest.(check bool) "pp non-empty" true (String.length s > 0)
;;

let test_pp_all_constructors () =
  let check expected ev =
    Alcotest.(check string) expected expected (Format.asprintf "%a" Ev.pp ev)
  in
  check "BEGIN txn=1" (Ev.Txn_begin { txn_id = 1L });
  check "ROLLBACK txn=4" (Ev.Txn_rollback { txn_id = 4L });
  check "COMMIT txn=2 frames=3" (Ev.Txn_commit { txn_id = 2L; frames = 3 });
  check "SP_BEGIN txn=5 name=a" (Ev.Savepoint_begin { txn_id = 5L; name = "a" });
  check "SP_RELEASE txn=6 name=b" (Ev.Savepoint_release { txn_id = 6L; name = "b" });
  check "SP_ROLLBACK txn=7 name=c" (Ev.Savepoint_rollback { txn_id = 7L; name = "c" });
  check
    "WAL_APPEND txn=8 base=9 count=10"
    (Ev.Wal_append { txn_id = 8L; base_idx = 9; count = 10 });
  check "WAL_RESET epoch=11" (Ev.Wal_reset { epoch = 11L });
  check "CKPT_BEGIN target=12" (Ev.Checkpoint_begin { target_frames = 12 });
  check "CKPT_END migrated=13" (Ev.Checkpoint_end { pages_migrated = 13 });
  check "PAGE_READ txn=1 page=7" (Ev.Page_read { txn_id = 1L; page = 7L });
  check "PAGE_WRITE txn=2 page=8" (Ev.Page_write { txn_id = 2L; page = 8L });
  check
    "PAGE_ALLOC txn=3 page=9 reused=true"
    (Ev.Page_alloc { txn_id = 3L; page = 9L; reused = true });
  check
    "PAGE_ALLOC txn=3 page=9 reused=false"
    (Ev.Page_alloc { txn_id = 3L; page = 9L; reused = false });
  check "PAGE_FREE txn=4 page=10" (Ev.Page_free { txn_id = 4L; page = 10L })
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

let test_savepoint_events () =
  let labels =
    with_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.savepoint_begin txn "sp1" in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      let* () = S.savepoint_rollback txn "sp1" in
      let* () = S.savepoint_release txn "sp1" in
      S.commit txn)
  in
  Alcotest.(check bool) "SP_BEGIN" true (List.mem "SP_BEGIN" labels);
  Alcotest.(check bool) "SP_ROLLBACK" true (List.mem "SP_ROLLBACK" labels);
  Alcotest.(check bool) "SP_RELEASE" true (List.mem "SP_RELEASE" labels)
;;

let test_wal_append_and_checkpoint () =
  let labels =
    with_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      let* () = S.commit txn in
      S.checkpoint st)
  in
  Alcotest.(check bool) "WAL_APPEND" true (List.mem "WAL_APPEND" labels);
  Alcotest.(check bool) "CKPT_BEGIN" true (List.mem "CKPT_BEGIN" labels);
  Alcotest.(check bool) "CKPT_END" true (List.mem "CKPT_END" labels);
  Alcotest.(check bool) "WAL_RESET" true (List.mem "WAL_RESET" labels)
;;

(* Collect full events (not just labels) while [f] runs. *)
let with_event_recorder ~f =
  let path = fresh_path () in
  cleanup path;
  let seen = ref [] in
  Lwt.finalize
    (fun () ->
       let open Lwt.Syntax in
       let* st = S.open_file_wal ~path () in
       let st = Result.get_ok st in
       S.set_event_callback st (Some (fun ev -> seen := ev :: !seen));
       let* () = f st in
       let* () = S.close st in
       Lwt.return (List.rev !seen))
    (fun () ->
       cleanup path;
       Lwt.return_unit)
  |> run
;;

let test_insert_emits_page_events () =
  let evs =
    with_event_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      S.commit txn)
  in
  let has p = List.exists p evs in
  Alcotest.(check bool)
    "has PAGE_ALLOC"
    true
    (has (function
       | Ev.Page_alloc _ -> true
       | _ -> false));
  Alcotest.(check bool)
    "has PAGE_WRITE"
    true
    (has (function
       | Ev.Page_write _ -> true
       | _ -> false));
  Alcotest.(check bool)
    "page write txn > 0"
    true
    (has (function
       | Ev.Page_write { txn_id; _ } -> txn_id > 0L
       | _ -> false))
;;

(* #384: [Page_read] fires only when a page is loaded from the main file (cache
   miss, not served from the WAL index).  In WAL mode, a reopen re-populates the
   WAL index via recover_index so reads go through the WAL path.  Reopening with
   the non-WAL [open_file] forces every read through the main-file path so
   [load_main_page] / [emit_read] reliably fires on a cache miss. *)
let test_checkpoint_then_read_emits_page_read () =
  let path = fresh_path () in
  cleanup path;
  let evs =
    Lwt.finalize
      (fun () ->
         let open Lwt.Syntax in
         (* Step 1: write + commit + checkpoint so data lands in the main file. *)
         let* st = S.open_file_wal ~path () in
         let st = Result.get_ok st in
         let* txn = S.rw_begin st in
         let* () = S.put txn 16 (bs "k") (bs "v") in
         let* () = S.commit txn in
         let* () = S.checkpoint st in
         let* () = S.close st in
         (* Step 2: reopen with the non-WAL open_file so the pager has no WAL
            index.  Every read is a main-file lookup; cache is cold on open. *)
         let seen = ref [] in
         let* st2 = Sqlocaml_unix.Store.open_file ~path () in
         let st2 = Result.get_ok st2 in
         S.set_event_callback st2 (Some (fun ev -> seen := ev :: !seen));
         let* txn2 = S.rw_begin st2 in
         let* _ = S.get txn2 16 (bs "k") in
         let* () = S.commit txn2 in
         let* () = S.close st2 in
         Lwt.return (List.rev !seen))
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool)
    "has PAGE_READ"
    true
    (List.exists
       (function
         | Ev.Page_read _ -> true
         | _ -> false)
       evs)
;;

(* Defensive: a raising callback must not break the commit. *)
let test_raising_callback_is_swallowed () =
  let path = fresh_path () in
  cleanup path;
  let ok =
    Lwt.finalize
      (fun () ->
         let open Lwt.Syntax in
         let* st = S.open_file_wal ~path () in
         let st = Result.get_ok st in
         S.set_event_callback st (Some (fun _ -> failwith "boom"));
         let* txn = S.rw_begin st in
         let* () = S.put txn 16 (bs "k") (bs "v") in
         let* () = S.commit txn in
         let* () = S.close st in
         Lwt.return true)
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool) "commit survived a raising observer" true ok
;;

let prop_commit_count =
  QCheck.Test.make
    ~count:50
    ~name:"commit count matches"
    QCheck.(list bool)
    (fun outcomes ->
       let path = fresh_path () in
       cleanup path;
       let commits = ref 0 in
       Lwt.finalize
         (fun () ->
            let open Lwt.Syntax in
            let* st = S.open_file_wal ~path () in
            let st = Result.get_ok st in
            S.set_event_callback
              st
              (Some
                 (fun ev ->
                   match ev with
                   | Ev.Txn_commit _ -> incr commits
                   | _ -> ()));
            let* () =
              Lwt_list.iter_s
                (fun commit ->
                   let open Lwt.Syntax in
                   let* txn = S.rw_begin st in
                   let* () = S.put txn 16 (bs "k") (bs "v") in
                   if commit then S.commit txn else S.rollback txn)
                outcomes
            in
            let* () = S.close st in
            Lwt.return_unit)
         (fun () ->
            cleanup path;
            Lwt.return_unit)
       |> run;
       !commits = List.length (List.filter Fun.id outcomes))
;;

(* #382 (Fix 1 regression): forcing an auto-checkpoint to fire during commits
   must never produce a negative [Wal_append] count or base_idx.  A concurrent
   [Wal.reset] (zeroing committed_frames) during the group-commit yields would,
   if the count were re-read after lock release, go negative. *)
let test_wal_append_count_nonneg_under_autockpt () =
  let path = fresh_path () in
  cleanup path;
  let bad = ref [] in
  Lwt.finalize
    (fun () ->
       let open Lwt.Syntax in
       let* st = S.open_file_wal ~path () in
       let st = Result.get_ok st in
       S.set_wal_autocheckpoint st 1;
       (* force auto-checkpoint to fire between/within commits *)
       S.set_event_callback
         st
         (Some
            (fun ev ->
              match ev with
              | Ev.Wal_append { base_idx; count; _ } ->
                if count < 0 || base_idx < 0 then bad := (base_idx, count) :: !bad
              | _ -> ()));
       let* () =
         Lwt_list.iter_s
           (fun i ->
              let open Lwt.Syntax in
              let* txn = S.rw_begin st in
              let* () = S.put txn 16 (bs (Printf.sprintf "k%d" i)) (bs "v") in
              S.commit txn)
           (List.init 20 Fun.id)
       in
       let* () = S.close st in
       Lwt.return_unit)
    (fun () ->
       cleanup path;
       Lwt.return_unit)
  |> run;
  Alcotest.(check (list (pair int int))) "no negative Wal_append counts/base" [] !bad
;;

let test_txn_commit_frames_matches_wal_append () =
  let evs =
    with_event_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      S.commit txn)
  in
  let commit_frames =
    List.find_map
      (function
        | Ev.Txn_commit { frames; _ } -> Some frames
        | _ -> None)
      evs
  in
  let append_count =
    List.find_map
      (function
        | Ev.Wal_append { count; _ } -> Some count
        | _ -> None)
      evs
  in
  Alcotest.(check (option int))
    "commit frames present"
    (Some (Option.get append_count))
    commit_frames;
  Alcotest.(check bool)
    "commit frames > 0 for a real write"
    true
    (match commit_frames with
     | Some n -> n > 0
     | None -> false)
;;

let test_writes_emit_page_free () =
  let evs =
    with_event_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () =
        Lwt_list.iter_s
          (fun i -> S.put txn 16 (bs (Printf.sprintf "k%04d" i)) (bs "v"))
          (List.init 1000 Fun.id)
      in
      S.commit txn)
  in
  Alcotest.(check bool)
    "has PAGE_FREE"
    true
    (List.exists
       (function
         | Ev.Page_free _ -> true
         | _ -> false)
       evs)
;;

let () =
  Alcotest.run
    "store_event"
    [ ( "type"
      , [ Alcotest.test_case "label + txn_id" `Quick test_label_and_txn_id
        ; Alcotest.test_case "pp non-empty" `Quick test_pp_roundtrip_nonempty
        ; Alcotest.test_case "pp all constructors" `Quick test_pp_all_constructors
        ] )
    ; ( "seam"
      , [ Alcotest.test_case
            "commit emits begin+commit"
            `Quick
            test_commit_emits_begin_and_commit
        ; Alcotest.test_case "rollback emits rollback" `Quick test_rollback_emits_rollback
        ; Alcotest.test_case "savepoint events" `Quick test_savepoint_events
        ; Alcotest.test_case
            "wal append + checkpoint"
            `Quick
            test_wal_append_and_checkpoint
        ; Alcotest.test_case
            "raising callback swallowed"
            `Quick
            test_raising_callback_is_swallowed
        ; Alcotest.test_case
            "wal_append count nonneg under autockpt"
            `Quick
            test_wal_append_count_nonneg_under_autockpt
        ; Alcotest.test_case
            "insert emits page events"
            `Quick
            test_insert_emits_page_events
        ; Alcotest.test_case
            "checkpoint then read emits page read"
            `Quick
            test_checkpoint_then_read_emits_page_read
        ; Alcotest.test_case
            "txn commit frames matches wal append"
            `Quick
            test_txn_commit_frames_matches_wal_append
        ; Alcotest.test_case "writes emit page free" `Quick test_writes_emit_page_free
        ] )
    ; "props", [ QCheck_alcotest.to_alcotest prop_commit_count ]
    ]
;;
