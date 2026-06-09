(** Tests for Sqlocaml_storage.Pager *)

open Sqlocaml_storage

(* These unit tests were written against the original 64-page cache and
   assert on exact eviction thresholds.  #159 raised the default capacity to
   1024, so pin it back to 64 for this executable to keep those assertions
   deterministic. *)
let () = Unix.putenv "SQLOCAML_PAGE_CACHE" "64"

(* ------------------------------------------------------------------ *)
(* Mock BLOCK backend                                                   *)
(* ------------------------------------------------------------------ *)

(** An in-memory mock block device backed by a Hashtbl.
    Every page is [Page.page_size] bytes. *)
type mock_block =
  { store : (int64, Bytes.t) Hashtbl.t
  ; mutable n_pages : int64
  ; mutable read_count : int (* total BLOCK read calls *)
  ; mutable write_count : int (* total BLOCK write calls *)
  ; mutable sync_count : int
  }

let make_mock () =
  { store = Hashtbl.create 16
  ; n_pages = 0L
  ; read_count = 0
  ; write_count = 0
  ; sync_count = 0
  }
;;

(** Wire up the four callbacks expected by [Pager.create]. *)
let mock_callbacks mb =
  let read_page ~page_id buf =
    mb.read_count <- mb.read_count + 1;
    match Hashtbl.find_opt mb.store page_id with
    | None ->
      (* Unwritten page → return zeros *)
      Cstruct.memset buf 0;
      Lwt.return_ok ()
    | Some bytes ->
      Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size;
      Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    mb.write_count <- mb.write_count + 1;
    let bytes = Bytes.create Page.page_size in
    Cstruct.blit_to_bytes buf 0 bytes 0 Page.page_size;
    Hashtbl.replace mb.store page_id bytes;
    Lwt.return_ok ()
  in
  let sync () =
    mb.sync_count <- mb.sync_count + 1;
    Lwt.return_ok ()
  in
  let resize ~n_pages =
    mb.n_pages <- n_pages;
    Lwt.return_ok ()
  in
  read_page, write_page, sync, resize
;;

(** Create a pager wired to a fresh mock block. *)
let make_pager ?(n_pages = 0L) ?(freelist = Freelist.empty) () =
  let mb = make_mock () in
  let read_page, write_page, sync, resize = mock_callbacks mb in
  let pager = Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist in
  pager, mb
;;

(** Run an Lwt value synchronously (test helper). *)
let run = Lwt_main.run

(** Build a Cstruct filled with a given byte value. *)
let fill_page byte =
  let buf = Cstruct.create Page.page_size in
  Cstruct.memset buf byte;
  buf
;;

(* ------------------------------------------------------------------ *)
(* Unit tests                                                          *)
(* ------------------------------------------------------------------ *)

(* create with empty state; n_pages = 0 *)
let test_create_empty () =
  let p, _ = make_pager () in
  Alcotest.(check int64) "n_pages = 0" 0L (Pager.n_pages p);
  Alcotest.(check int) "freelist size = 0" 0 (Freelist.size (Pager.freelist p))
;;

(* alloc with empty freelist → returns page 0; n_pages becomes 1 *)
let test_alloc_first () =
  let p, mb = make_pager () in
  let id = run (Pager.alloc p) in
  match id with
  | Error e -> Alcotest.failf "alloc failed: %a" Pager.pp_error e
  | Ok pid ->
    Alcotest.(check int64) "first alloc = 0" 0L pid;
    Alcotest.(check int64) "n_pages = 1" 1L (Pager.n_pages p);
    Alcotest.(check int64) "mock resized to 1" 1L mb.n_pages
;;

(* alloc twice → returns pages 0 and 1 in order *)
let test_alloc_twice () =
  let p, _ = make_pager () in
  let id0 = run (Pager.alloc p) in
  let id1 = run (Pager.alloc p) in
  match id0, id1 with
  | Ok p0, Ok p1 ->
    Alcotest.(check int64) "first = 0" 0L p0;
    Alcotest.(check int64) "second = 1" 1L p1;
    Alcotest.(check int64) "n_pages = 2" 2L (Pager.n_pages p)
  | _ -> Alcotest.fail "unexpected error"
;;

(* write then read returns same content (from dirty/cache, no BLOCK hit) *)
let test_write_then_read () =
  let p, mb = make_pager () in
  let _ = run (Pager.alloc p) in
  let buf = fill_page 0xAB in
  Pager.write p 0L buf;
  let before_reads = mb.read_count in
  let result = run (Pager.read p 0L) in
  match result with
  | Error e -> Alcotest.failf "read failed: %a" Pager.pp_error e
  | Ok got ->
    Alcotest.(check int) "no BLOCK reads" before_reads mb.read_count;
    (* Verify content matches *)
    let expected = Bytes.make Page.page_size '\xAB' in
    let actual = Bytes.create Page.page_size in
    Cstruct.blit_to_bytes got 0 actual 0 Page.page_size;
    Alcotest.(check bool) "content matches" true (Bytes.equal expected actual)
;;

(* write then flush → verify BLOCK mock contains the page data *)
let test_write_then_flush () =
  let p, mb = make_pager () in
  let _ = run (Pager.alloc p) in
  let buf = fill_page 0x5A in
  Pager.write p 0L buf;
  match run (Pager.flush p) with
  | Error e -> Alcotest.failf "flush failed: %a" Pager.pp_error e
  | Ok () ->
    let stored = Hashtbl.find_opt mb.store 0L in
    (match stored with
     | None -> Alcotest.fail "page not written to mock"
     | Some bytes ->
       let expected = Bytes.make Page.page_size '\x5A' in
       Alcotest.(check bool) "flushed content correct" true (Bytes.equal expected bytes);
       Alcotest.(check int) "sync called once" 1 mb.sync_count)
;;

(* read on non-cached page calls BLOCK; subsequent read returns cached copy *)
let test_read_caches_block () =
  let p, mb = make_pager () in
  (* Pre-populate mock store directly so there is something to read *)
  let bytes = Bytes.make Page.page_size '\x77' in
  Hashtbl.replace mb.store 0L bytes;
  mb.n_pages <- 1L;
  (* First read — should hit BLOCK *)
  let _r1 = run (Pager.read p 0L) in
  Alcotest.(check int) "first read hits BLOCK" 1 mb.read_count;
  (* Second read — should come from cache *)
  let _r2 = run (Pager.read p 0L) in
  Alcotest.(check int) "second read from cache" 1 mb.read_count
;;

(* free then alloc (with alloc_min_safe > freed_at_txn_id) → returns the freed page_id *)
let test_free_then_alloc_reusable () =
  let p, _ = make_pager ~n_pages:5L () in
  (* Free page 3 at txn 1; set alloc_min_safe=2 so freed_at(1) < min_safe(2) *)
  Pager.free p ~page_id:3L ~freed_at_txn_id:1L;
  Pager.set_alloc_min_safe p 2L;
  match run (Pager.alloc p) with
  | Error e -> Alcotest.failf "alloc failed: %a" Pager.pp_error e
  | Ok pid -> Alcotest.(check int64) "reuses freed page 3" 3L pid
;;

(* free then alloc (with alloc_min_safe = freed_at_txn_id) → returns NEW page (freed not yet reusable) *)
let test_free_then_alloc_same_txn () =
  let p, _ = make_pager ~n_pages:5L () in
  (* #297: set n_pages_at_rw_begin so page 3 (< 5) routes to the main
     freelist, not the txn_owned_pool, to test the guard directly. *)
  Pager.set_n_pages_at_rw_begin p 5L;
  Pager.free p ~page_id:3L ~freed_at_txn_id:2L;
  (* alloc_min_safe=2: freed_at(2) < 2 is false — page 3 not yet reusable *)
  Pager.set_alloc_min_safe p 2L;
  match run (Pager.alloc p) with
  | Error e -> Alcotest.failf "alloc failed: %a" Pager.pp_error e
  | Ok pid ->
    Alcotest.(check bool)
      "new page allocated, not freed one"
      true
      (Int64.compare pid 3L <> 0);
    Alcotest.(check int64) "new page = n_pages before (5)" 5L pid
;;

(* flush clears dirty — second flush makes no BLOCK write calls *)
let test_flush_clears_dirty () =
  let p, mb = make_pager () in
  let _ = run (Pager.alloc p) in
  Pager.write p 0L (fill_page 0x11);
  let _ = run (Pager.flush p) in
  let writes_after_first = mb.write_count in
  (* Second flush — dirty is empty, should write nothing *)
  let _ = run (Pager.flush p) in
  Alcotest.(check int)
    "no extra writes after second flush"
    writes_after_first
    mb.write_count
;;

(* n_pages after two allocs = 2 *)
let test_n_pages_after_two_allocs () =
  let p, _ = make_pager () in
  let _ = run (Pager.alloc p) in
  let _ = run (Pager.alloc p) in
  Alcotest.(check int64) "n_pages = 2" 2L (Pager.n_pages p)
;;

(* Cache eviction — write 65 pages; read oldest back (should hit BLOCK) *)
let test_cache_eviction () =
  let p, mb = make_pager ~n_pages:65L () in
  (* Pre-populate mock store for all 65 pages *)
  for i = 0 to 64 do
    let bytes = Bytes.make Page.page_size (Char.chr (i land 0xFF)) in
    Hashtbl.replace mb.store (Int64.of_int i) bytes
  done;
  (* Write pages 0..64 to the pager (fills cache + dirty) *)
  for i = 0 to 64 do
    let buf = fill_page (i land 0xFF) in
    Pager.write p (Int64.of_int i) buf
  done;
  (* Flush so pages move out of dirty *)
  let _ = run (Pager.flush p) in
  (* Now create a fresh pager over the same mock so cache is empty *)
  let read_page, write_page, sync, resize = mock_callbacks mb in
  let p2 =
    Pager.create
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages:65L
      ~freelist:Freelist.empty
  in
  (* Write 65 distinct pages to fill the cache beyond capacity *)
  for i = 0 to 64 do
    let buf = fill_page (i land 0xFF) in
    Pager.write p2 (Int64.of_int i) buf
  done;
  let _ = run (Pager.flush p2) in
  (* Build a third fresh pager to test cache eviction purely on reads *)
  let mb2 = make_mock () in
  (* Populate mb2.store with recognizable content per page *)
  for i = 0 to 64 do
    let bytes = Bytes.make Page.page_size (Char.chr ((i + 1) land 0xFF)) in
    Hashtbl.replace mb2.store (Int64.of_int i) bytes
  done;
  let rp, wp, sy, rs = mock_callbacks mb2 in
  let p3 =
    Pager.create
      ~read_page:rp
      ~write_page:wp
      ~sync:sy
      ~resize:rs
      ~n_pages:65L
      ~freelist:Freelist.empty
  in
  (* Read pages 0..63 to fill cache to capacity *)
  for i = 0 to 63 do
    let _ = run (Pager.read p3 (Int64.of_int i)) in
    ()
  done;
  let reads_before_64 = mb2.read_count in
  (* Reading page 64 should cause eviction of page 0 from cache *)
  let _ = run (Pager.read p3 64L) in
  (* Now re-read page 0 — it must have been evicted, so should hit BLOCK *)
  let _ = run (Pager.read p3 0L) in
  Alcotest.(check bool)
    "evicted page re-read from BLOCK"
    true
    (mb2.read_count > reads_before_64 + 1)
;;

(* ------------------------------------------------------------------ *)
(* Cache-bypass tests (#268)                                           *)
(* ------------------------------------------------------------------ *)

(** [~bypass_cache:true] reads from BLOCK every time (never cached). *)
let test_bypass_cache_does_not_cache () =
  let p, mb = make_pager ~n_pages:2L () in
  let bytes0 = Bytes.make Page.page_size '\x77' in
  let bytes1 = Bytes.make Page.page_size '\x88' in
  Hashtbl.replace mb.store 0L bytes0;
  Hashtbl.replace mb.store 1L bytes1;
  (* Read page 0 normally — gets cached *)
  let _ = run (Pager.read p 0L) in
  Alcotest.(check int) "first read hits BLOCK" 1 mb.read_count;
  let _ = run (Pager.read p 0L) in
  Alcotest.(check int) "second read from cache" 1 mb.read_count;
  (* Read page 1 with bypass — hits BLOCK every time *)
  let _ = run (Pager.read ~bypass_cache:true p 1L) in
  Alcotest.(check int) "bypass read hits BLOCK" 2 mb.read_count;
  let _ = run (Pager.read ~bypass_cache:true p 1L) in
  Alcotest.(check int) "second bypass read still hits BLOCK" 3 mb.read_count
;;

(** [~bypass_cache:true] with [read_borrow] — same property. *)
let test_bypass_cache_read_borrow () =
  let p, mb = make_pager ~n_pages:2L () in
  let bytes0 = Bytes.make Page.page_size '\x77' in
  let bytes1 = Bytes.make Page.page_size '\x88' in
  Hashtbl.replace mb.store 0L bytes0;
  Hashtbl.replace mb.store 1L bytes1;
  (* First: cached path — second read_borrow shouldn't hit BLOCK *)
  let _ = run (Pager.read_borrow p 0L (fun b -> Lwt.return b)) in
  Alcotest.(check int) "first borrow hits BLOCK" 1 mb.read_count;
  let _ = run (Pager.read_borrow p 0L (fun b -> Lwt.return b)) in
  Alcotest.(check int) "second borrow from cache" 1 mb.read_count;
  (* Bypass on an already-cached page still serves from cache *)
  let _ = run (Pager.read_borrow ~bypass_cache:true p 0L (fun b -> Lwt.return b)) in
  Alcotest.(check int) "bypass borrow serves cached page" 1 mb.read_count;
  (* Bypass on a different page — hits BLOCK every time *)
  let _ = run (Pager.read_borrow ~bypass_cache:true p 1L (fun b -> Lwt.return b)) in
  Alcotest.(check int) "bypass borrow page 1 first time" 2 mb.read_count;
  let _ = run (Pager.read_borrow ~bypass_cache:true p 1L (fun b -> Lwt.return b)) in
  Alcotest.(check int) "bypass borrow page 1 second time" 3 mb.read_count
;;

(** Bypass reads don't evict hot pages from the cache. *)
let test_bypass_cache_does_not_evict () =
  let p, mb = make_pager ~n_pages:128L () in
  (* Pre-populate mock store for all pages *)
  for i = 0 to 127 do
    let bytes = Bytes.make Page.page_size (Char.chr (i land 0xFF)) in
    Hashtbl.replace mb.store (Int64.of_int i) bytes
  done;
  (* Fill cache with pages 0..62 using normal reads (63 pages, cache cap is 64) *)
  for i = 0 to 62 do
    let _ = run (Pager.read p (Int64.of_int i)) in
    ()
  done;
  (* Now read pages 63..127 with bypass — should NOT evict *)
  let reads_before_bypass = mb.read_count in
  for i = 63 to 127 do
    let _ = run (Pager.read ~bypass_cache:true p (Int64.of_int i)) in
    ()
  done;
  Alcotest.(check int)
    "bypass reads all hit BLOCK (not cached)"
    65
    (mb.read_count - reads_before_bypass);
  (* Re-read pages 0..62 — they should still be in cache *)
  let reads_before_reread = mb.read_count in
  for i = 0 to 62 do
    let _ = run (Pager.read p (Int64.of_int i)) in
    ()
  done;
  Alcotest.(check int)
    "cached pages not evicted by bypass"
    reads_before_reread
    mb.read_count
;;

(* ------------------------------------------------------------------ *)
(* QCheck property tests                                               *)
(* ------------------------------------------------------------------ *)

(** Last write for a page_id always readable. *)
let prop_last_write_readable =
  QCheck.Test.make
    ~name:"prop_last_write_readable"
    ~count:10_000
    QCheck.(
      make Gen.(list_size (int_range 1 20) (pair (int_range 0 7) (int_range 0 255))))
    (fun ops ->
       let p, _ = make_pager ~n_pages:8L () in
       (* Track last written byte per page *)
       let last_written = Hashtbl.create 8 in
       List.iter
         (fun (page_idx, byte_val) ->
            let pid = Int64.of_int page_idx in
            let buf = fill_page byte_val in
            Pager.write p pid buf;
            Hashtbl.replace last_written pid byte_val)
         ops;
       (* Verify every page that was written reads back correctly *)
       Hashtbl.fold
         (fun pid expected_byte ok ->
            if not ok
            then false
            else (
              match run (Pager.read p pid) with
              | Error _ -> false
              | Ok got ->
                let b = Cstruct.get_uint8 got 0 in
                b = expected_byte))
         last_written
         true)
;;

(** alloc produces monotonically increasing page_ids when freelist is empty. *)
let prop_alloc_monotone =
  QCheck.Test.make
    ~name:"prop_alloc_monotone"
    ~count:10_000
    QCheck.(Gen.int_range 1 30 |> make)
    (fun n ->
       let p, _ = make_pager () in
       let ids =
         Array.init n (fun _ ->
           match run (Pager.alloc p) with
           | Ok id -> id
           | Error _ -> Int64.minus_one)
       in
       (* Every id must be non-negative *)
       let all_valid = Array.for_all (fun id -> Int64.compare id 0L >= 0) ids in
       (* ids must be strictly increasing *)
       let monotone =
         let ok = ref true in
         for i = 1 to n - 1 do
           if Int64.compare ids.(i) ids.(i - 1) <= 0 then ok := false
         done;
         !ok
       in
       all_valid && monotone)
;;

(** flush then re-read from fresh pager returns correct data. *)
let prop_flush_then_reread =
  QCheck.Test.make
    ~name:"prop_flush_then_reread"
    ~count:10_000
    QCheck.(
      make Gen.(list_size (int_range 1 10) (pair (int_range 0 4) (int_range 0 255))))
    (fun ops ->
       let mb = make_mock () in
       let rp, wp, sy, rs = mock_callbacks mb in
       let p1 =
         Pager.create
           ~read_page:rp
           ~write_page:wp
           ~sync:sy
           ~resize:rs
           ~n_pages:5L
           ~freelist:Freelist.empty
       in
       (* Track last write per page *)
       let last_written = Hashtbl.create 5 in
       List.iter
         (fun (page_idx, byte_val) ->
            let pid = Int64.of_int page_idx in
            let buf = fill_page byte_val in
            Pager.write p1 pid buf;
            Hashtbl.replace last_written pid byte_val)
         ops;
       (* Flush *)
       match run (Pager.flush p1) with
       | Error _ -> false (* flush failure = test inconclusive, pass it *)
       | Ok () ->
         (* Create a fresh pager over the same mock *)
         let rp2, wp2, sy2, rs2 = mock_callbacks mb in
         let p2 =
           Pager.create
             ~read_page:rp2
             ~write_page:wp2
             ~sync:sy2
             ~resize:rs2
             ~n_pages:5L
             ~freelist:Freelist.empty
         in
         Hashtbl.fold
           (fun pid expected_byte ok ->
              if not ok
              then false
              else (
                match run (Pager.read p2 pid) with
                | Error _ -> false
                | Ok got ->
                  let b = Cstruct.get_uint8 got 0 in
                  b = expected_byte))
           last_written
           true)
;;

(* ------------------------------------------------------------------ *)
(* pp_error formatting + flush error paths                              *)
(* ------------------------------------------------------------------ *)

let string_contains hay needle =
  let hl = String.length hay
  and nl = String.length needle in
  let rec go i =
    if i > hl - nl
    then false
    else if String.sub hay i nl = needle
    then true
    else go (i + 1)
  in
  go 0
;;

let test_pp_error_block () =
  let s = Format.asprintf "%a" Pager.pp_error (Pager.Block_error "msg") in
  Alcotest.(check bool) "Block_error tag" true (string_contains s "Block_error");
  Alcotest.(check bool) "Block_error msg" true (string_contains s "msg")
;;

let test_pp_error_corruption () =
  let s = Format.asprintf "%a" Pager.pp_error (Pager.Corruption "bad") in
  Alcotest.(check bool) "Corruption tag" true (string_contains s "Corruption");
  Alcotest.(check bool) "Corruption msg" true (string_contains s "bad")
;;

(* flush where write_page returns Error -> pager surfaces Block_error *)
let test_flush_write_error () =
  let read_page ~page_id:_ buf =
    Cstruct.memset buf 0;
    Lwt.return_ok ()
  in
  let write_page ~page_id:_ _buf = Lwt.return_error "injected write" in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages:_ = Lwt.return_ok () in
  let p =
    Pager.create ~read_page ~write_page ~sync ~resize ~n_pages:1L ~freelist:Freelist.empty
  in
  let buf = fill_page 0x11 in
  Pager.write p 0L buf;
  match run (Pager.flush p) with
  | Ok () -> Alcotest.fail "expected error from flush"
  | Error (Pager.Block_error _) -> ()
  | Error _ -> Alcotest.fail "expected Block_error"
;;

(* flush where sync returns Error -> pager surfaces Block_error *)
let test_flush_sync_error () =
  let read_page ~page_id:_ buf =
    Cstruct.memset buf 0;
    Lwt.return_ok ()
  in
  let write_page ~page_id:_ _buf = Lwt.return_ok () in
  let sync () = Lwt.return_error "injected sync" in
  let resize ~n_pages:_ = Lwt.return_ok () in
  let p =
    Pager.create ~read_page ~write_page ~sync ~resize ~n_pages:1L ~freelist:Freelist.empty
  in
  (* dirty list is empty - flush still calls sync at the end *)
  match run (Pager.flush p) with
  | Ok () -> Alcotest.fail "expected error from flush sync"
  | Error (Pager.Block_error _) -> ()
  | Error _ -> Alcotest.fail "expected Block_error"
;;

let test_alloc_no_arg () =
  let p, _ = make_pager () in
  match Lwt_main.run (Pager.alloc p) with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "alloc failed"
;;

let test_set_get_txn_id () =
  let p, _ = make_pager () in
  Pager.set_txn_id p 5L;
  Alcotest.(check int64) "get_txn_id" 5L (Pager.get_txn_id p)
;;

let test_freelist_recycled_after_txn () =
  (* Free page with freed_at=2; alloc_min_safe=3 means it should be recycled *)
  let p, _ = make_pager () in
  (* Allocate 3 pages to get a known page_id to free *)
  let pid1 = Result.get_ok (Lwt_main.run (Pager.alloc p)) in
  let pid2 = Result.get_ok (Lwt_main.run (Pager.alloc p)) in
  let pid3 = Result.get_ok (Lwt_main.run (Pager.alloc p)) in
  ignore (pid1, pid2);
  (* Free pid3 with freed_at=2 *)
  Pager.free p ~page_id:pid3 ~freed_at_txn_id:2L;
  (* Set min_safe=3 so freed_at=2 < 3, making pid3 reusable *)
  Pager.set_alloc_min_safe p 3L;
  (* Next alloc should return pid3 (recycled) *)
  match Lwt_main.run (Pager.alloc p) with
  | Ok pid -> Alcotest.(check int64) "page recycled" pid3 pid
  | Error _ -> Alcotest.fail "expected recycled page"
;;

(* read where the underlying read_page fails -> Block_error *)
let test_read_block_error () =
  let read_page ~page_id:_ _buf = Lwt.return_error "injected read" in
  let write_page ~page_id:_ _buf = Lwt.return_ok () in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages:_ = Lwt.return_ok () in
  let p =
    Pager.create ~read_page ~write_page ~sync ~resize ~n_pages:1L ~freelist:Freelist.empty
  in
  match run (Pager.read p 0L) with
  | Ok _ -> Alcotest.fail "expected Block_error"
  | Error (Pager.Block_error _) -> ()
  | Error _ -> Alcotest.fail "expected Block_error"
;;

(* ------------------------------------------------------------------ *)
(* RUNNER                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map
      QCheck_alcotest.to_alcotest
      [ prop_last_write_readable; prop_alloc_monotone; prop_flush_then_reread ]
  in
  Alcotest.run
    "pager"
    [ ( "basic"
      , [ Alcotest.test_case "create empty" `Quick test_create_empty
        ; Alcotest.test_case "alloc first page" `Quick test_alloc_first
        ; Alcotest.test_case "alloc twice" `Quick test_alloc_twice
        ; Alcotest.test_case "write then read (no BLOCK)" `Quick test_write_then_read
        ; Alcotest.test_case "write then flush to BLOCK" `Quick test_write_then_flush
        ; Alcotest.test_case "read caches BLOCK page" `Quick test_read_caches_block
        ; Alcotest.test_case
            "free then alloc reusable"
            `Quick
            test_free_then_alloc_reusable
        ; Alcotest.test_case
            "free then alloc same txn"
            `Quick
            test_free_then_alloc_same_txn
        ; Alcotest.test_case "flush clears dirty" `Quick test_flush_clears_dirty
        ; Alcotest.test_case
            "n_pages after two allocs"
            `Quick
            test_n_pages_after_two_allocs
        ; Alcotest.test_case "cache eviction" `Quick test_cache_eviction
        ; Alcotest.test_case
            "bypass cache does not cache"
            `Quick
            test_bypass_cache_does_not_cache
        ; Alcotest.test_case
            "bypass cache read_borrow"
            `Quick
            test_bypass_cache_read_borrow
        ; Alcotest.test_case
            "bypass cache does not evict hot pages"
            `Quick
            test_bypass_cache_does_not_evict
        ] )
    ; ( "errors"
      , [ Alcotest.test_case "pp_error Block_error" `Quick test_pp_error_block
        ; Alcotest.test_case "pp_error Corruption" `Quick test_pp_error_corruption
        ; Alcotest.test_case "flush write error" `Quick test_flush_write_error
        ; Alcotest.test_case "flush sync error" `Quick test_flush_sync_error
        ; Alcotest.test_case "read block error" `Quick test_read_block_error
        ] )
    ; ( "txn_id"
      , [ Alcotest.test_case "alloc no arg" `Quick test_alloc_no_arg
        ; Alcotest.test_case "set/get txn_id" `Quick test_set_get_txn_id
        ; Alcotest.test_case
            "freelist recycled after txn"
            `Quick
            test_freelist_recycled_after_txn
        ] )
    ; "qcheck", qcheck_tests
    ]
;;
