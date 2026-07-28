(** Phase 42 / #159 — Snapshot page-pinning regression test.

    A long-lived RO snapshot pins the pages it materialises so a
    concurrent writer's CoW churn cannot FIFO them out of the bounded
    page cache.  Property under test: once a reader has warmed its
    working set, repeated cursor walks on the same snapshot do *zero*
    device reads, no matter how much the writer commits in between
    (no growing re-fetch tail).

    Deterministic, non-timing: we count [read_page] device calls and
    attribute them to the reader by bracketing each re-walk.  Before the
    pinning fix, the writer's CoW page allocations evict the reader's
    pages and each re-walk re-reads its whole working set; the per-walk
    read count grows without bound. *)

open Lwt.Syntax
module S = Granary_store.Store

let bs = Bytes.of_string
let run = Lwt_main.run

(* In-memory block device with a [read_page] counter and a growable
   backing buffer (so [alloc]'s [resize] can extend it). *)
let make_dev () =
  let page = 4096 in
  let buf = ref (Bytes.make (page * 4) '\x00') in
  let reads = ref 0 in
  let ensure n_pages =
    let need = n_pages * page in
    if Bytes.length !buf < need
    then (
      (* Grow geometrically: a long-lived reader pins [alloc_min_safe] so
         freed pages can't be reused, and the writer's CoW keeps allocating
         fresh ones — the file grows steadily.  Doubling keeps the backing
         buffer amortised O(1) per growth instead of O(n^2). *)
      let newcap = max need (2 * Bytes.length !buf) in
      let nb = Bytes.make newcap '\x00' in
      Bytes.blit !buf 0 nb 0 (Bytes.length !buf);
      buf := nb)
  in
  let read_page ~page_id b =
    incr reads;
    ensure (Int64.to_int page_id + 1);
    Cstruct.blit_from_bytes !buf (Int64.to_int page_id * page) b 0 page;
    Lwt.return_ok ()
  in
  let write_page ~page_id b =
    ensure (Int64.to_int page_id + 1);
    Cstruct.blit_to_bytes b 0 !buf (Int64.to_int page_id * page) page;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages =
    ensure (Int64.to_int n_pages);
    Lwt.return_ok ()
  in
  read_page, write_page, sync, resize, reads
;;

let open_store () =
  let read_page, write_page, sync, resize, reads = make_dev () in
  let* r =
    S.open_block
      ~init_if_corrupt:true
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages:0L
      ~close:(fun () -> Lwt.return_unit)
      ()
  in
  match r with
  | Ok st -> Lwt.return (st, reads)
  | Error e -> Alcotest.failf "open_block: %a" S.pp_error e
;;

let tid = 7

(* Full forward walk of [tid]; returns number of entries seen. *)
let walk : type a. a S.txn -> int Lwt.t =
  fun tx ->
  let* cur = S.cursor_open tx tid in
  let _ = S.cursor_first cur in
  let rec loop n =
    match S.cursor_next cur with
    | None -> n
    | Some _ -> loop (n + 1)
  in
  let n = loop 0 in
  S.cursor_close cur;
  Lwt.return n
;;

let seed st n =
  let* tx = S.rw_begin st in
  let rec loop i =
    if i >= n
    then Lwt.return_unit
    else
      let* () =
        S.put tx tid (bs (Printf.sprintf "k%06d" i)) (bs (Printf.sprintf "v%06d" i))
      in
      loop (i + 1)
  in
  let* () = loop 0 in
  S.commit tx
;;

(* One writer commit: append a fresh row to [tid] (CoW-paths root->leaf,
   allocating new page ids that pressure the cache). *)
let writer_commit st i =
  let* tx = S.rw_begin st in
  let* () =
    S.put tx tid (bs (Printf.sprintf "w%08d" i)) (bs (Printf.sprintf "wv%08d" i))
  in
  S.commit tx
;;

let test_pinned_snapshot_no_growing_tail () =
  (* Small cache so writer churn comfortably exceeds it. *)
  Unix.putenv "GRANARY_PAGE_CACHE" "64";
  run
    (let* st, reads = open_store () in
     (* Seed enough rows to span a genuine multi-page working set (root +
       interior + several leaves), so pinning has to protect more than a
       single page. *)
     let* () = seed st 2000 in
     (* Hold one snapshot for the whole test. *)
     let* ro = S.ro_begin st in
     (* Warm walk: materialises (and, post-fix, pins) the working set. *)
     let* _ = walk ro in
     (* Each cycle commits well over [GRANARY_PAGE_CACHE] fresh CoW pages,
       so without pinning the reader's whole working set is evicted every
       cycle; the re-fetch count then grows linearly with [cycles]. *)
     let cycles = 8 in
     let commits_per_cycle = 40 in
     let wcount = ref 0 in
     let rewalk_reads = ref 0 in
     let rec cycle c =
       if c >= cycles
       then Lwt.return_unit
       else (
         (* Writer churn into the same tree. *)
         let rec churn k =
           if k >= commits_per_cycle
           then Lwt.return_unit
           else
             let* () = writer_commit st !wcount in
             incr wcount;
             churn (k + 1)
         in
         let* () = churn 0 in
         (* Reader re-walk: count only the device reads it triggers. *)
         let before = !reads in
         let* _ = walk ro in
         rewalk_reads := !rewalk_reads + (!reads - before);
         cycle (c + 1))
     in
     let* () = cycle 0 in
     let* () = S.ro_end ro in
     let* () = S.close st in
     Alcotest.(check int)
       "pinned snapshot re-walks do zero device reads under writer CoW"
       0
       !rewalk_reads;
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "pager_snapshot_pin"
    [ ( "pin"
      , [ Alcotest.test_case
            "held snapshot has no growing re-fetch tail"
            `Quick
            test_pinned_snapshot_no_growing_tail
        ] )
    ]
;;
