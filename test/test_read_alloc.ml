(** #244 regression: the scoped zero-copy read path must not copy the page.

    Root cause (pre-fix), in [lib/storage/pager.ml]: [Pager.read] returned a
    fresh [cstruct_dup] of the full ~4 KB page on EVERY call — including pure
    cache hits.  A read-only B+-tree descent touches O(log n) pages, decodes a
    handful of entries and discards each buffer, so every level paid a ~4 KB
    page-sized alloc+memcpy for nothing.  [Pager.read_borrow] (#244) hands the
    decode a borrowed view of the cached buffer instead — no copy.

    The invariant is pinned by {b physical buffer identity}, NOT by
    [Gc.allocated_bytes]: Cstruct buffers are Bigarray-backed, so the page
    bytes live in external (malloc) memory the OCaml minor/major-heap counter
    does not see — [cstruct_dup]'s real cost is wall-clock + RSS, invisible to
    [Gc.allocated_bytes].  Instead the test proves the borrow shares storage: a
    cache-hit [read_borrow] hands back the very Bigarray the cache holds (so two
    borrows of the same page expose a physically-equal buffer), whereas
    [Pager.read] copies (two reads expose distinct buffers).  Deterministic and
    machine-independent — reintroducing a copy in the borrow path breaks the
    identity and fails the test.  (The end-to-end speedup is a wall-clock
    benchmark concern; see the PR's point_lookup numbers.) *)

open Sqlocaml_storage

(* In-memory mock block device (same pattern as test_write_alloc). *)
type mock_block =
  { store : (int64, Bytes.t) Hashtbl.t
  ; mutable n_pages : int64 [@warning "-69"]
  }
[@@warning "-69"]

let make_mock () = { store = Hashtbl.create 64; n_pages = 0L }

let mock_callbacks mb =
  let read_page ~page_id buf =
    match Hashtbl.find_opt mb.store page_id with
    | None ->
      Cstruct.memset buf 0;
      Lwt.return_ok ()
    | Some bytes ->
      Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size;
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

let make_pager () =
  let mb = make_mock () in
  let read_page, write_page, sync, resize = mock_callbacks mb in
  ( Pager.create ~read_page ~write_page ~sync ~resize ~n_pages:2L ~freelist:Freelist.empty
  , mb )
;;

let run = Lwt_main.run

let ok = function
  | Ok x -> x
  | Error e -> Alcotest.failf "btree error: %a" Btree.pp_error e
;;

let key_of i =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int i);
  b
;;

let payload i = Bytes.of_string (Printf.sprintf "payload-row-%d" i)

(* Seed an [n]-row tree and return both the handle and its pager. *)
let seed n =
  let p, _ = make_pager () in
  let t = ref (Btree.create p ~root_page:0L) in
  for i = 0 to n - 1 do
    t := ok (run (Btree.put !t (key_of i) (payload i)))
  done;
  !t, p
;;

let okp = function
  | Ok x -> x
  | Error e -> Alcotest.failf "pager error: %a" Pager.pp_error e
;;

(* Flush a seeded tree's writes out of the dirty set so subsequent reads resolve
   through the read CACHE (the headline borrow path). *)
let flush_to_cache p =
  match run (Pager.flush p) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "flush: %a" Pager.pp_error e
;;

(* A cache-hit [read_borrow] hands back the cache's own [Cstruct.t]: two borrows
   of the same page see the physically-identical buffer object — no copy made.
   (Capturing [buf] out of the callback would violate the borrow contract in
   production; here it is only used for an identity check, never dereferenced
   after the scope.) *)
let test_borrow_shares_cached_buffer () =
  let t, p = seed 5000 in
  let root = Btree.root_page t in
  flush_to_cache p;
  (* First borrow reads-through and caches; the next two are pure cache hits. *)
  ignore (okp (run (Pager.read_borrow p root (fun b -> Lwt.return b))));
  let b1 = okp (run (Pager.read_borrow p root (fun b -> Lwt.return b))) in
  let b2 = okp (run (Pager.read_borrow p root (fun b -> Lwt.return b))) in
  Alcotest.(check bool) "two cache-hit borrows share one buffer (no copy)" true (b1 == b2)
;;

(* Foil: the copying [Pager.read] returns a fresh buffer each call — two reads
   of the same cached page yield physically-distinct [Cstruct.t]s.  Confirms the
   identity gate above is actually distinguishing copy from no-copy. *)
let test_read_copies_buffer () =
  let t, p = seed 5000 in
  let root = Btree.root_page t in
  flush_to_cache p;
  let r1 = okp (run (Pager.read p root)) in
  let r2 = okp (run (Pager.read p root)) in
  Alcotest.(check bool) "two reads return distinct buffers (each a copy)" false (r1 == r2)
;;

(* Correctness companion: borrowed-buffer descents must still read every key
   back exactly, including missing-key and overflow-valued lookups. *)
let test_get_readback () =
  let n = 3000 in
  let t, _ = seed n in
  for i = 0 to n - 1 do
    match run (Btree.get t (key_of i)) with
    | Ok (Some v) when Bytes.equal v (payload i) -> ()
    | Ok got ->
      Alcotest.failf
        "key %d: wrong value (got %s)"
        i
        (match got with
         | Some b -> Bytes.to_string b
         | None -> "<none>")
    | Error e -> Alcotest.failf "key %d: get error %a" i Btree.pp_error e
  done;
  (* Absent keys return None through the borrowed leaf decode. *)
  match run (Btree.get t (key_of (n + 1))) with
  | Ok None -> ()
  | _ -> Alcotest.fail "absent key should read back None"
;;

(* Overflow values are decoded OUTSIDE the leaf borrow (the marker bytes are
   copied out first); a big value must round-trip through borrowed descents. *)
let test_get_overflow_readback () =
  let p, _ = make_pager () in
  let big k = Bytes.of_string (String.make 5000 (Char.chr (65 + (k mod 26)))) in
  let t = ref (Btree.create p ~root_page:0L) in
  for i = 0 to 49 do
    t := ok (run (Btree.put !t (key_of i) (payload i)))
  done;
  t := ok (run (Btree.put !t (key_of 100) (big 7)));
  match run (Btree.get !t (key_of 100)) with
  | Ok (Some v) when Bytes.equal v (big 7) -> ()
  | _ -> Alcotest.fail "overflow value did not round-trip through borrowed descent"
;;

let () =
  Alcotest.run
    "read_alloc"
    [ ( "borrow"
      , [ Alcotest.test_case
            "read_borrow shares the cached buffer"
            `Quick
            test_borrow_shares_cached_buffer
        ; Alcotest.test_case
            "read copies the buffer (foil)"
            `Quick
            test_read_copies_buffer
        ] )
    ; ( "get"
      , [ Alcotest.test_case "gets read back exactly" `Quick test_get_readback
        ; Alcotest.test_case
            "overflow value round-trips"
            `Quick
            test_get_overflow_readback
        ] )
    ]
;;
