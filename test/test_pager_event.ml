(** Tests for the Pager page-event seam (#384). *)

open Granary_storage

(* Keep cache assertions deterministic across this exe. *)
let () = Unix.putenv "GRANARY_PAGE_CACHE" "64"

type mock_block =
  { store : (int64, Bytes.t) Hashtbl.t
  ; mutable n_pages : int64 [@warning "-69"]
  }

let make_mock () = { store = Hashtbl.create 16; n_pages = 0L }

let mock_callbacks mb =
  let read_page ~page_id buf =
    (match Hashtbl.find_opt mb.store page_id with
     | None -> Cstruct.memset buf 0
     | Some bytes -> Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size);
    Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    let bytes = Bytes.create Page.page_size in
    Cstruct.blit_to_bytes buf 0 bytes 0 Page.page_size;
    Hashtbl.replace mb.store page_id bytes;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages =
    mb.n_pages <- n_pages;
    Lwt.return_ok ()
  in
  read_page, write_page, sync, resize
;;

let make_pager ?(n_pages = 0L) ?(freelist = Freelist.empty) () =
  let mb = make_mock () in
  let read_page, write_page, sync, resize = mock_callbacks mb in
  let pager = Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist in
  pager, mb
;;

let run = Lwt_main.run

(* Attach a recorder; returns the event-list ref (newest-first). *)
let recorder pager =
  let seen = ref [] in
  Pager.set_page_event_callback pager (Some (fun ev -> seen := ev :: !seen));
  seen
;;

let events seen = List.rev !seen

let test_no_ops_no_events () =
  let p, _ = make_pager () in
  let seen = recorder p in
  Alcotest.(check int) "no events before any op" 0 (List.length (events seen))
;;

let test_set_none_clears () =
  let p, _ = make_pager () in
  let seen = recorder p in
  Pager.set_page_event_callback p None;
  (* a read that would emit (Task 2) must produce nothing once cleared *)
  let _ = run (Pager.read p 0L) in
  Alcotest.(check int) "no events after clearing callback" 0 (List.length (events seen))
;;

let is_read = function
  | Pager_event.Page_read _ -> true
  | _ -> false
;;

let test_cache_miss_emits_read () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.read p 2L) in
  let reads = List.filter is_read (events seen) in
  Alcotest.(check int) "one Page_read on cache miss" 1 (List.length reads);
  match reads with
  | [ Pager_event.Page_read { page_id } ] -> Alcotest.(check int64) "page id" 2L page_id
  | _ -> Alcotest.fail "expected exactly one Page_read"
;;

let test_cache_hit_emits_nothing () =
  let p, _ = make_pager ~n_pages:4L () in
  (* prime cache — recorder not yet attached *)
  let _ = run (Pager.read p 2L) in
  let seen = recorder p in
  let _ = run (Pager.read p 2L) in
  Alcotest.(check int) "no Page_read on cache hit" 0 (List.length (events seen))
;;

let test_borrow_miss_emits_read () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.read_borrow p 3L (fun _ -> Lwt.return_unit)) in
  let reads = List.filter is_read (events seen) in
  Alcotest.(check int) "one Page_read on borrow cache miss" 1 (List.length reads);
  match reads with
  | [ Pager_event.Page_read { page_id } ] -> Alcotest.(check int64) "page id" 3L page_id
  | _ -> Alcotest.fail "expected exactly one Page_read"
;;

let fill_page byte =
  let buf = Cstruct.create Page.page_size in
  Cstruct.memset buf byte;
  buf
;;

let is_wal_read = function
  | Pager_event.Wal_read _ -> true
  | _ -> false
;;

(* Attach a mock WAL that resolves [page_id] to a frame and serves a page from
   [wal_read_frame] (a physical frame read, from the pager's perspective). *)
let attach_mock_wal p ~page_id ~frame_idx =
  let wal_read_frame idx =
    if idx = frame_idx
    then Lwt.return_ok (fill_page 7)
    else Lwt.return_error "no such frame"
  in
  let unused_commit _ = Lwt.return_ok () in
  Pager.set_wal
    p
    (Some
       { Pager.wal_find_page = (fun pid -> if pid = page_id then Some frame_idx else None)
       ; wal_find_page_at =
           (fun pid ~max_frame:_ -> if pid = page_id then Some frame_idx else None)
       ; wal_read_frame
       ; wal_append_commit = unused_commit
       ; wal_append_commit_no_sync = unused_commit
       ; wal_sync = (fun () -> Lwt.return_ok ())
       })
;;

(* #392: a WAL-resident page read produces a [Wal_read] event with the page id —
   not a [Page_read] (that variant stays main-file-only). *)
let test_wal_served_read_emits_wal_read () =
  let p, _ = make_pager ~n_pages:4L () in
  attach_mock_wal p ~page_id:2L ~frame_idx:5;
  let seen = recorder p in
  let _ = run (Pager.read p 2L) |> Result.get_ok in
  Alcotest.(check int)
    "no main-file Page_read on WAL-served read"
    0
    (List.length (List.filter is_read (events seen)));
  let wreads = List.filter is_wal_read (events seen) in
  Alcotest.(check int) "one Wal_read on WAL-served read" 1 (List.length wreads);
  match wreads with
  | [ Pager_event.Wal_read { page_id } ] -> Alcotest.(check int64) "page id" 2L page_id
  | _ -> Alcotest.fail "expected exactly one Wal_read"
;;

let test_wal_served_borrow_read_emits_wal_read () =
  let p, _ = make_pager ~n_pages:4L () in
  attach_mock_wal p ~page_id:3L ~frame_idx:1;
  let seen = recorder p in
  let _ = run (Pager.read_borrow p 3L (fun _ -> Lwt.return_unit)) |> Result.get_ok in
  Alcotest.(check int)
    "no main-file Page_read on WAL-served borrow read"
    0
    (List.length (List.filter is_read (events seen)));
  let wreads = List.filter is_wal_read (events seen) in
  Alcotest.(check int) "one Wal_read on WAL-served borrow read" 1 (List.length wreads);
  match wreads with
  | [ Pager_event.Wal_read { page_id } ] -> Alcotest.(check int64) "page id" 3L page_id
  | _ -> Alcotest.fail "expected exactly one Wal_read"
;;

(* A page absent from the WAL falls through to the main file: main-file
   [Page_read] only, no [Wal_read]. *)
let test_wal_miss_falls_through_to_main_read () =
  let p, _ = make_pager ~n_pages:4L () in
  attach_mock_wal p ~page_id:2L ~frame_idx:5;
  let seen = recorder p in
  let _ = run (Pager.read p 3L) |> Result.get_ok in
  Alcotest.(check int)
    "no Wal_read when page absent from WAL"
    0
    (List.length (List.filter is_wal_read (events seen)));
  Alcotest.(check int)
    "one main-file Page_read on WAL miss"
    1
    (List.length (List.filter is_read (events seen)))
;;

let allocs seen =
  List.filter_map
    (function
      | Pager_event.Page_alloc { page_id; reused } -> Some (page_id, reused)
      | _ -> None)
    (events seen)
;;

let frees seen =
  List.filter_map
    (function
      | Pager_event.Page_free { page_id } -> Some page_id
      | _ -> None)
    (events seen)
;;

let writes seen =
  List.filter_map
    (function
      | Pager_event.Page_write { page_id } -> Some page_id
      | _ -> None)
    (events seen)
;;

(* Non-WAL pager: [flush] writes each dirty page to main. *)
let test_flush_emits_one_write_per_dirty () =
  let p, _ = make_pager ~n_pages:4L () in
  Pager.write p 0L (fill_page 1);
  Pager.write p 1L (fill_page 2);
  Pager.write p 2L (fill_page 3);
  let seen = recorder p in
  let _ = run (Pager.flush p) |> Result.get_ok in
  let ws = List.sort compare (writes seen) in
  Alcotest.(check (list int64)) "one Page_write per dirty page" [ 0L; 1L; 2L ] ws
;;

let test_flush_one_to_main_emits_write () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ =
    run (Pager.flush_one_to_main p ~page_id:1L ~buf:(fill_page 9)) |> Result.get_ok
  in
  Alcotest.(check (list int64)) "single write" [ 1L ] (writes seen)
;;

let test_flush_empty_no_writes () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.flush p) |> Result.get_ok in
  Alcotest.(check (list int64)) "no dirty pages -> no writes" [] (writes seen)
;;

let test_flush_no_sync_emits_writes () =
  let p, _ = make_pager ~n_pages:4L () in
  Pager.write p 0L (fill_page 1);
  Pager.write p 1L (fill_page 2);
  let seen = recorder p in
  let _ = run (Pager.flush_no_sync p) |> Result.get_ok in
  let ws = List.sort compare (writes seen) in
  Alcotest.(check (list int64)) "flush_no_sync emits per dirty page" [ 0L; 1L ] ws
;;

(* QCheck: every reused page-id was previously freed in the same session. *)
let prop_reuse_was_freed =
  QCheck.Test.make
    ~count:100
    ~name:"alloc reused=true page was previously freed"
    QCheck.(int_range 1 30)
    (fun n ->
       let p, _ = make_pager ~n_pages:0L () in
       (* force the main freelist, not the txn-owned pool *)
       Pager.set_n_pages_at_rw_begin p 1_000_000L;
       let freed = Hashtbl.create 16 in
       let alloc1 () = run (Pager.alloc p) |> Result.get_ok in
       let pids = List.init n (fun _ -> alloc1 ()) in
       List.iter
         (fun pid ->
            Hashtbl.replace freed pid ();
            Pager.free p ~page_id:pid ~freed_at_txn_id:1L)
         pids;
       Pager.set_alloc_min_safe p 1_000_000L;
       let seen = ref [] in
       Pager.set_page_event_callback p (Some (fun ev -> seen := ev :: !seen));
       let _ = List.init n (fun _ -> alloc1 ()) in
       List.for_all
         (function
           | Pager_event.Page_alloc { page_id; reused = true } ->
             Hashtbl.mem freed page_id
           | _ -> true)
         !seen)
;;

let test_alloc_extend_not_reused () =
  let p, _ = make_pager ~n_pages:0L () in
  let seen = recorder p in
  let pid = run (Pager.alloc p) |> Result.get_ok in
  Alcotest.(check (list (pair int64 bool)))
    "alloc by file-extend, reused=false"
    [ pid, false ]
    (allocs seen)
;;

let test_alloc_from_freelist_reused () =
  let fl = Freelist.add Freelist.empty ~page_id:1l ~freed_at_txn_id:1L in
  let p, _ = make_pager ~n_pages:8L ~freelist:fl () in
  (* min_safe must exceed freed_at_txn_id (1L) so the page is poppable *)
  Pager.set_alloc_min_safe p 5L;
  let seen = recorder p in
  let pid = run (Pager.alloc p) |> Result.get_ok in
  Alcotest.(check (list (pair int64 bool)))
    "alloc from freelist, reused=true"
    [ pid, true ]
    (allocs seen);
  Alcotest.(check int64) "reused page id is 1" 1L pid
;;

let test_alloc_from_txn_pool_reused () =
  let p, _ = make_pager ~n_pages:8L () in
  Pager.set_n_pages_at_rw_begin p 4L;
  Pager.free p ~page_id:6L ~freed_at_txn_id:1L;
  (* attach recorder AFTER the free, so only the alloc is recorded *)
  let seen = recorder p in
  let pid = run (Pager.alloc p) |> Result.get_ok in
  Alcotest.(check (list (pair int64 bool)))
    "alloc from txn pool, reused=true"
    [ pid, true ]
    (allocs seen);
  Alcotest.(check int64) "txn-pool page id is 6" 6L pid
;;

let test_free_below_threshold_emits_free () =
  let p, _ = make_pager ~n_pages:8L () in
  Pager.set_n_pages_at_rw_begin p 8L;
  let seen = recorder p in
  Pager.free p ~page_id:2L ~freed_at_txn_id:3L;
  Alcotest.(check (list int64)) "Page_free for freelist push" [ 2L ] (frees seen)
;;

let test_free_txn_owned_emits_free () =
  let p, _ = make_pager ~n_pages:8L () in
  Pager.set_n_pages_at_rw_begin p 4L;
  let seen = recorder p in
  Pager.free p ~page_id:6L ~freed_at_txn_id:3L;
  Alcotest.(check (list int64)) "Page_free for txn-owned push" [ 6L ] (frees seen)
;;

let test_alloc_free_none_no_events () =
  let p, _ = make_pager ~n_pages:8L () in
  let seen = ref [] in
  Pager.set_page_event_callback p (Some (fun ev -> seen := ev :: !seen));
  Pager.set_page_event_callback p None;
  let _ = run (Pager.alloc p) |> Result.get_ok in
  Pager.free p ~page_id:2L ~freed_at_txn_id:1L;
  Alcotest.(check int) "no events when callback is None" 0 (List.length !seen)
;;

let test_pp_all_variants () =
  let check expected ev =
    Alcotest.(check string) expected expected (Format.asprintf "%a" Pager_event.pp ev)
  in
  check "PAGE_READ page=7" (Pager_event.Page_read { page_id = 7L });
  check "WAL_READ page=11" (Pager_event.Wal_read { page_id = 11L });
  check "PAGE_WRITE page=8" (Pager_event.Page_write { page_id = 8L });
  check
    "PAGE_ALLOC page=9 reused=true"
    (Pager_event.Page_alloc { page_id = 9L; reused = true });
  check
    "PAGE_ALLOC page=9 reused=false"
    (Pager_event.Page_alloc { page_id = 9L; reused = false });
  check "PAGE_FREE page=10" (Pager_event.Page_free { page_id = 10L })
;;

let () =
  Alcotest.run
    "pager_event"
    [ "pp", [ Alcotest.test_case "pp all variants" `Quick test_pp_all_variants ]
    ; ( "plumbing"
      , [ Alcotest.test_case "no ops no events" `Quick test_no_ops_no_events
        ; Alcotest.test_case "set None clears" `Quick test_set_none_clears
        ; Alcotest.test_case
            "alloc/free None no events"
            `Quick
            test_alloc_free_none_no_events
        ] )
    ; ( "read"
      , [ Alcotest.test_case "cache miss emits read" `Quick test_cache_miss_emits_read
        ; Alcotest.test_case "cache hit emits nothing" `Quick test_cache_hit_emits_nothing
        ; Alcotest.test_case "borrow miss emits read" `Quick test_borrow_miss_emits_read
        ; Alcotest.test_case
            "WAL-served read emits Wal_read"
            `Quick
            test_wal_served_read_emits_wal_read
        ; Alcotest.test_case
            "WAL-served borrow read emits Wal_read"
            `Quick
            test_wal_served_borrow_read_emits_wal_read
        ; Alcotest.test_case
            "WAL miss falls through to main read"
            `Quick
            test_wal_miss_falls_through_to_main_read
        ] )
    ; ( "alloc/free"
      , [ Alcotest.test_case "alloc extend not reused" `Quick test_alloc_extend_not_reused
        ; Alcotest.test_case
            "alloc from freelist reused"
            `Quick
            test_alloc_from_freelist_reused
        ; Alcotest.test_case
            "alloc from txn pool reused"
            `Quick
            test_alloc_from_txn_pool_reused
        ; Alcotest.test_case
            "free below threshold"
            `Quick
            test_free_below_threshold_emits_free
        ; Alcotest.test_case "free txn-owned" `Quick test_free_txn_owned_emits_free
        ] )
    ; ( "write"
      , [ Alcotest.test_case
            "flush one write per dirty"
            `Quick
            test_flush_emits_one_write_per_dirty
        ; Alcotest.test_case
            "flush_one_to_main emits write"
            `Quick
            test_flush_one_to_main_emits_write
        ; Alcotest.test_case "flush empty no writes" `Quick test_flush_empty_no_writes
        ; Alcotest.test_case
            "flush_no_sync emits writes"
            `Quick
            test_flush_no_sync_emits_writes
        ] )
    ; "props", [ QCheck_alcotest.to_alcotest prop_reuse_was_freed ]
    ]
;;
