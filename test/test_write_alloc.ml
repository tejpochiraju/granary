(** #231 regression: a single COW B+-tree insert must not pay for two full
    root-to-leaf descents, nor copy each rewritten page twice.

    Root cause (pre-fix), both in [lib/storage/btree.ml]:

    1. [put] called [get_raw] — a full O(log n) tree descent — purely to detect
       and free a stale overflow chain under the key, then [put_into_leaf] did a
       SECOND full descent ([find_leaf]) to the same leaf.  The B+-tree is
       sorted, so the target leaf [find_leaf] reaches already contains the old
       entry: the [get_raw] descent was pure duplication (~25% of per-insert
       allocation, measured).  It is now folded into [put_into_leaf].

    2. Every rewritten page (leaf + each branch up the path) was built into a
       fresh page-sized buffer and then copied AGAIN by [Pager.write]'s
       defensive [cstruct_dup].  Those buffers are never reused, so the second
       copy was pure waste; [Pager.write_owned] now transfers ownership.

    This assertion is deterministic and machine-independent: it measures bytes
    allocated by one steady-state insert via [Gc.allocated_bytes] (an exact
    counter, not a timer), at the B+-tree level so there is no SQL/Store noise.
    Measured over a 5000-row tree: pre-fix ~92.2 KB/insert, post-fix ~59.7 KB.
    The ceiling sits between the two so reintroducing either redundancy fails
    the test while genuine drift keeps wide headroom. *)

open Sqlocaml_storage

(* In-memory mock block device (same pattern as test_btree). *)
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

(* Build a tree of [n] rows; return the final handle. *)
let seed n =
  let p, _ = make_pager () in
  let t = ref (Btree.create p ~root_page:0L) in
  for i = 0 to n - 1 do
    t := ok (run (Btree.put !t (key_of i) (payload i)))
  done;
  !t
;;

let test_insert_allocation_bounded () =
  let n = 5000 in
  let t = ref (seed n) in
  (* Warm: do one insert outside the measured window (first-touch of a new leaf
     buffer etc.), then measure the average of a batch of steady inserts. *)
  t := ok (run (Btree.put !t (key_of n) (payload n)));
  let batch = 16 in
  let before = Gc.allocated_bytes () in
  for j = 1 to batch do
    t := ok (run (Btree.put !t (key_of (n + j)) (payload (n + j))))
  done;
  let after = Gc.allocated_bytes () in
  let per_insert = (after -. before) /. float_of_int batch in
  Printf.eprintf "WRITE-ALLOC: %.0f bytes/insert over %d-row tree\n%!" per_insert n;
  (* Pre-fix ~92.2 KB; post-fix ~59.7 KB.  Ceiling at 75 KB. *)
  Alcotest.(check bool)
    (Printf.sprintf "per-insert alloc %.0f < 75000" per_insert)
    true
    (per_insert < 75000.0)
;;

(* Correctness: every inserted key reads back exactly, including ones written
   after the redundant descent was removed (replace path still consistent). *)
let test_insert_readback () =
  let n = 2000 in
  let t = seed n in
  (* Overwrite half the keys (exercises the in-leaf replace + the folded
     overflow-free path), then verify all read back the latest value. *)
  let t =
    let r = ref t in
    for i = 0 to (n / 2) - 1 do
      r := ok (run (Btree.put !r (key_of i) (payload (i + 1_000_000))))
    done;
    !r
  in
  for i = 0 to n - 1 do
    let expect = if i < n / 2 then payload (i + 1_000_000) else payload i in
    match run (Btree.get t (key_of i)) with
    | Ok (Some v) when Bytes.equal v expect -> ()
    | Ok got ->
      Alcotest.failf
        "key %d: wrong value (got %s)"
        i
        (match got with
         | Some b -> Bytes.to_string b
         | None -> "<none>")
    | Error e -> Alcotest.failf "key %d: get error %a" i Btree.pp_error e
  done
;;

(* Overwriting a key whose value is overflow-sized must free the OLD chain (the
   logic relocated from [get_raw] into [put_into_leaf]).  If it leaked, repeated
   overwrites of one key would grow the backing store without bound. *)
let test_overflow_overwrite_frees () =
  let p, mb = make_pager () in
  let big k = Bytes.of_string (String.make 5000 (Char.chr (65 + (k mod 26)))) in
  let t = ref (Btree.create p ~root_page:0L) in
  (* Seed a few other keys so the tree isn't trivial. *)
  for i = 0 to 9 do
    t := ok (run (Btree.put !t (key_of i) (payload i)))
  done;
  t := ok (run (Btree.put !t (key_of 100) (big 0)));
  let pages_after_first = Hashtbl.length mb.store in
  (* Overwrite the same key with a fresh overflow value many times; freed chains
     must be reused so the page count stays bounded. *)
  for k = 1 to 30 do
    t := ok (run (Btree.put !t (key_of 100) (big k)))
  done;
  let pages_after_many = Hashtbl.length mb.store in
  (* Latest value is intact. *)
  (match run (Btree.get !t (key_of 100)) with
   | Ok (Some v) when Bytes.equal v (big 30) -> ()
   | _ -> Alcotest.fail "overflow key 100: wrong/absent value after overwrites");
  Printf.eprintf
    "WRITE-ALLOC: overflow store pages %d -> %d over 30 overwrites\n%!"
    pages_after_first
    pages_after_many;
  (* Without freeing, 30 overwrites of a ~2-page chain would add ~60 pages.
     With freeing+reuse the growth is small; allow generous slack for the
     committed-txn reuse threshold. *)
  Alcotest.(check bool)
    (Printf.sprintf "page growth bounded (%d -> %d)" pages_after_first pages_after_many)
    true
    (pages_after_many - pages_after_first < 20)
;;

let () =
  Alcotest.run
    "write_alloc"
    [ ( "insert"
      , [ Alcotest.test_case
            "per-insert allocation bounded"
            `Slow
            test_insert_allocation_bounded
        ; Alcotest.test_case
            "inserts and overwrites read back"
            `Quick
            test_insert_readback
        ; Alcotest.test_case
            "overflow overwrite frees old chain"
            `Quick
            test_overflow_overwrite_frees
        ] )
    ]
;;
