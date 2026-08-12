(** #716: [Btree.max_key] / [Store.max_key] — O(log n) rightmost descent.

    The measured defect: [Cat.recover_next_rowid] found a table's maximum rowid
    by draining the WHOLE data tree through [Store.cursor_open] (which
    materialises every key AND value into a list, [store.ml:2902]) and walking
    it. At TPC-C W=1 that is ~300k rows per rolled-back counter, three counters
    per NewOrder, and 358.9 ms per ROLLBACK — 25.72% of NewOrder service time
    in four calls (#714).

    The hazard this file exists to pin: [Btree.rightmost_append_cursor] already
    descends the rightmost spine, and reusing it would be WRONG. It answers
    [None] both for a genuinely empty tree ([btree.ml:1252]) and for an empty
    rightmost LEAF ([btree.ml:1267]), and the second is reachable — [btree.ml]
    has no merge/rebalance, and [del_from_leaf] collapses to [root_page = 0L]
    only for a single empty ROOT leaf ([:1358]), otherwise rewriting the leaf in
    place at zero keys and leaving it linked in its parent ([:1363]). Read as
    "empty tree", that returns [empty_next_rowid], the next insert seeds at
    rowid 1, and live rows are overwritten — #589's symptom by a new route. *)

open Granary_storage

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

(* Reserve pages 0 and 1 as real on-disk usage does, so the first alloc returns
   page 2 and "empty tree (root_page = 0L)" is never confused with "root is
   page 0" — the same reason test_btree.ml does it. *)
let make_pager () =
  let mb = make_mock () in
  let read_page, write_page, sync, resize = mock_callbacks mb in
  Pager.create ~read_page ~write_page ~sync ~resize ~n_pages:2L ~freelist:Freelist.empty
;;

let empty_tree () = Btree.create (make_pager ()) ~root_page:0L
let run = Lwt_main.run
let b s = Bytes.of_string s

let ok_btree : type a. (a, Btree.error) result -> a = function
  | Ok x -> x
  | Error e -> Alcotest.failf "unexpected error: %a" Btree.pp_error e
;;

let max_key t = ok_btree (run (Btree.max_key t))

let check_max msg expected t =
  Alcotest.(check (option string))
    msg
    (Option.map Bytes.to_string expected)
    (Option.map Bytes.to_string (max_key t))
;;

let test_empty_tree () = check_max "empty tree has no max key" None (empty_tree ())

let test_single_root_leaf () =
  let t = ref (empty_tree ()) in
  List.iter (fun k -> t := ok_btree (run (Btree.put !t (b k) (b "v")))) [ "c"; "a"; "b" ];
  check_max "max of a single root leaf" (Some (b "c")) !t
;;

let test_all_keys_deleted () =
  let t = ref (empty_tree ()) in
  List.iter (fun k -> t := ok_btree (run (Btree.put !t (b k) (b "v")))) [ "a"; "b" ];
  List.iter (fun k -> t := ok_btree (run (Btree.del !t (b k)))) [ "a"; "b" ];
  check_max "no keys left" None !t
;;

(* THE REGRESSION. 60 entries with a 100-byte value split the root leaf
   (test_btree.ml:235 establishes ~40 entries fill a page), so the tree has a
   branch over at least two leaves. Deleting the highest key repeatedly must
   walk the max down one key at a time; at whichever deletion empties the
   rightmost leaf, a descent built on [rightmost_append_cursor] answers None
   and this loop fails. Deleting the whole suffix rather than guessing the leaf
   boundary is what makes the test independent of the split point. *)
let test_empty_rightmost_leaf () =
  let t = ref (empty_tree ()) in
  let value = Bytes.make 100 'x' in
  let n = 60 in
  let key i = b (Printf.sprintf "k%04d" i) in
  for i = 0 to n - 1 do
    t := ok_btree (run (Btree.put !t (key i) value))
  done;
  check_max "max before any deletion" (Some (key (n - 1))) !t;
  for i = n - 1 downto 0 do
    t := ok_btree (run (Btree.del !t (key i)));
    let expected = if i = 0 then None else Some (key (i - 1)) in
    check_max (Printf.sprintf "max after deleting the top %d keys" (n - i)) expected !t
  done
;;

(* Same shape one level deeper: 200 entries with a 500-byte value builds a
   branch over many leaves (test_btree.ml:256), so the suffix deletion empties
   several consecutive rightmost leaves and the descent must skip past all of
   them, not just one. *)
let test_multiple_empty_rightmost_leaves () =
  let t = ref (empty_tree ()) in
  let value = Bytes.make 500 'y' in
  let n = 200 in
  let key i = b (Printf.sprintf "k%04d" i) in
  for i = 0 to n - 1 do
    t := ok_btree (run (Btree.put !t (key i) value))
  done;
  for i = n - 1 downto n / 2 do
    t := ok_btree (run (Btree.del !t (key i)))
  done;
  check_max "max after deleting the top half" (Some (key ((n / 2) - 1))) !t
;;

(* Rowid keys are [Rowid.encode]'s offset-binary form, whose byte order IS
   rowid order across the sign boundary. A tree of only negative rowids must
   still return the greatest of them. *)
let test_negative_rowid_keys () =
  let t = ref (empty_tree ()) in
  List.iter
    (fun r ->
       t := ok_btree (run (Btree.put !t (Granary_encoding.Rowid.encode r) (b "v"))))
    [ -100L; -5L; -50L ];
  match max_key !t with
  | None -> Alcotest.fail "expected a max key"
  | Some k ->
    Alcotest.(check int64)
      "max of negative rowids"
      (-5L)
      (Granary_encoding.Rowid.decode k)
;;

module S = Granary_store.Store

let lwt f = Lwt_main.run (f ())
let tid = 7
let put_keys tx ks = Lwt_list.iter_s (fun k -> S.put tx tid (b k) (b "v")) ks

(* Mem backend, RW txn: reads the per-txn shadow, so keys written in this
   transaction are visible to it. *)
let test_store_mem_rw () =
  lwt (fun () ->
    let s = S.create () in
    let%lwt tx = S.rw_begin s in
    let%lwt () = put_keys tx [ "b"; "a"; "c" ] in
    let%lwt mk = S.max_key tx tid in
    Alcotest.(check (option string))
      "max over the RW shadow"
      (Some "c")
      (Option.map Bytes.to_string mk);
    S.rollback tx)
;;

(* Mem backend, RO txn: must read the snapshot captured at ro_begin (#178),
   NOT the live tree — otherwise a concurrent writer's committed-after-the-
   snapshot keys leak in.

   tx2's "z" is COMMITTED (not just written-and-rolled-back) before the
   [max_key ro] read, on purpose: an RW [put] on the Mem backend never
   touches the live tree at all (it writes only to [mem_rw_shadow]), so if
   tx2's write were left uncommitted, an implementation that wrongly reads
   the live tree in the [Ro]+[Mem] arm would still see [a;b] and this test
   would pass either way. Committing makes "z" visible on the live tree,
   which is the only way a live-tree read and a snapshot read can be told
   apart. *)
let test_store_mem_ro_ignores_uncommitted () =
  lwt (fun () ->
    let s = S.create () in
    let%lwt tx = S.rw_begin s in
    let%lwt () = put_keys tx [ "a"; "b" ] in
    let%lwt () = S.commit tx in
    let%lwt ro = S.ro_begin s in
    let%lwt tx2 = S.rw_begin s in
    let%lwt () = put_keys tx2 [ "z" ] in
    let%lwt () = S.commit tx2 in
    let%lwt mk = S.max_key ro tid in
    Alcotest.(check (option string))
      "RO snapshot does not see the writer's post-snapshot commit of 'z'"
      (Some "b")
      (Option.map Bytes.to_string mk);
    S.ro_end ro)
;;

let test_store_mem_empty () =
  lwt (fun () ->
    let s = S.create () in
    S.with_ro s (fun tx ->
      let%lwt mk = S.max_key tx tid in
      Alcotest.(check (option string)) "empty tree" None (Option.map Bytes.to_string mk);
      Lwt.return_unit))
;;

(* Btree backend on disk, through the same suffix-deletion shape as the btree
   test — this is the arm the catalog actually uses. *)
let test_store_btree_empty_rightmost_leaf () =
  let path = Printf.sprintf "/tmp/granary_max_key_716_%d.db" (Unix.getpid ()) in
  let cleanup () =
    try Sys.remove path with
    | Sys_error _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    lwt (fun () ->
      let%lwt s =
        match%lwt Granary_unix.Store.open_file ~path () with
        | Ok s -> Lwt.return s
        | Error e -> Alcotest.failf "open_file: %a" S.pp_error e
      in
      let value = Bytes.make 100 'x' in
      let key i = b (Printf.sprintf "k%04d" i) in
      let n = 60 in
      let%lwt tx = S.rw_begin s in
      let%lwt () =
        Lwt_list.iter_s (fun i -> S.put tx tid (key i) value) (List.init n Fun.id)
      in
      let%lwt () = S.commit tx in
      let%lwt tx = S.rw_begin s in
      let%lwt () =
        Lwt_list.iter_s
          (fun i -> S.del tx tid (key i))
          (List.init (n / 2) (fun i -> n - 1 - i))
      in
      let%lwt () = S.commit tx in
      let%lwt mk = S.with_ro s (fun tx -> S.max_key tx tid) in
      Alcotest.(check (option string))
        "btree backend skips the emptied rightmost leaves"
        (Some (Bytes.to_string (key ((n / 2) - 1))))
        (Option.map Bytes.to_string mk);
      S.close s))
;;

(* Btree backend, RW txn: [Cat.max_rowid_in_txn] (catalog.ml:2851) calls
   [max_key] on the caller's OPEN txn and must see writes that txn has made
   but not yet committed — read-your-own-writes on the on-disk backend. This
   is the one arm ([Rw]+[Btree]) none of the other store cases exercise:
   [test_store_mem_rw] covers [Rw]+[Mem], and
   [test_store_btree_empty_rightmost_leaf] only ever reads through a
   SEPARATE, later RO txn after committing. Deletes run inside the still-open
   RW txn here, and [max_key] is read BEFORE [S.commit]. *)
let test_store_btree_rw_sees_own_writes () =
  let path = Printf.sprintf "/tmp/granary_max_key_716_rw_%d.db" (Unix.getpid ()) in
  let cleanup () =
    try Sys.remove path with
    | Sys_error _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    lwt (fun () ->
      let%lwt s =
        match%lwt Granary_unix.Store.open_file ~path () with
        | Ok s -> Lwt.return s
        | Error e -> Alcotest.failf "open_file: %a" S.pp_error e
      in
      let value = Bytes.make 100 'x' in
      let key i = b (Printf.sprintf "k%04d" i) in
      let n = 60 in
      let%lwt tx = S.rw_begin s in
      let%lwt () =
        Lwt_list.iter_s (fun i -> S.put tx tid (key i) value) (List.init n Fun.id)
      in
      let%lwt () = S.commit tx in
      let%lwt tx = S.rw_begin s in
      let%lwt () =
        Lwt_list.iter_s
          (fun i -> S.del tx tid (key i))
          (List.init (n / 2) (fun i -> n - 1 - i))
      in
      (* Read within the still-open txn, BEFORE commit. *)
      let%lwt mk = S.max_key tx tid in
      Alcotest.(check (option string))
        "RW txn sees its own uncommitted deletes"
        (Some (Bytes.to_string (key ((n / 2) - 1))))
        (Option.map Bytes.to_string mk);
      let%lwt () = S.commit tx in
      S.close s))
;;

(* The new path must agree with the old one on arbitrary insert/delete
   sequences: max_key = the last key a full forward cursor scan yields.

   This Mem-backend version is close to tautological: [cursor_open]'s Rw+Mem
   arm computes [Bytes_map.bindings map] and [max_key]'s Rw+Mem arm computes
   [Bytes_map.max_binding_opt map] from the SAME map, via byte-identical arm
   selection — so this really asserts [Map.max_binding_opt = List.last
   (Map.bindings)], a stdlib property, and can never catch a bug in
   [Btree.max_key] itself. Kept anyway as a cheap regression net over the Mem
   arms; [prop_agrees_with_full_scan_btree] below is the one that exercises
   the risky code. *)
let prop_agrees_with_full_scan_mem =
  QCheck.Test.make
    ~count:200
    ~name:"max_key agrees with a full cursor scan (mem backend)"
    QCheck.(list (pair (int_bound 200) bool))
    (fun ops ->
       lwt (fun () ->
         let s = S.create () in
         let%lwt tx = S.rw_begin s in
         let k i = b (Printf.sprintf "k%04d" i) in
         let%lwt () =
           Lwt_list.iter_s
             (fun (i, insert) ->
                if insert then S.put tx tid (k i) (b "v") else S.del tx tid (k i))
             ops
         in
         let%lwt cur = S.cursor_open tx tid in
         let _sr = S.cursor_first cur in
         let last = ref None in
         let rec walk () =
           match S.cursor_next cur with
           | None -> ()
           | Some (key, _) ->
             last := Some key;
             walk ()
         in
         walk ();
         S.cursor_close cur;
         let%lwt mk = S.max_key tx tid in
         let%lwt () = S.rollback tx in
         Lwt.return (Option.map Bytes.to_string mk = Option.map Bytes.to_string !last)))
;;

(* The B-tree-backed version of the same property: a genuinely independent
   oracle (a full forward cursor scan through [Btree.cursor_next]) checked
   against [Btree.max_key]'s own rightmost descent, on the real disk-backed
   structure the catalog actually uses. A small key space (0-19) with a
   raised op count (100-300 ops/case) means deletes routinely hit live keys,
   so the maximum actually moves across the run instead of only ever growing.

   Each case opens and closes its own temp file (unique per case via an
   incrementing counter, since QCheck may run many cases and shrinks). *)
let prop_agrees_with_full_scan_btree =
  let case_no = ref 0 in
  QCheck.Test.make
    ~count:100
    ~name:"max_key agrees with a full cursor scan (btree backend)"
    QCheck.(list_size (Gen.int_range 100 300) (pair (int_bound 19) bool))
    (fun ops ->
       lwt (fun () ->
         incr case_no;
         let path =
           Printf.sprintf
             "/tmp/granary_max_key_716_prop_%d_%d.db"
             (Unix.getpid ())
             !case_no
         in
         let cleanup () =
           try Sys.remove path with
           | Sys_error _ -> ()
         in
         cleanup ();
         Lwt.finalize
           (fun () ->
              let%lwt s =
                match%lwt Granary_unix.Store.open_file ~path () with
                | Ok s -> Lwt.return s
                | Error e -> Alcotest.failf "open_file: %a" S.pp_error e
              in
              let%lwt tx = S.rw_begin s in
              let k i = b (Printf.sprintf "k%04d" i) in
              let%lwt () =
                Lwt_list.iter_s
                  (fun (i, insert) ->
                     if insert then S.put tx tid (k i) (b "v") else S.del tx tid (k i))
                  ops
              in
              let%lwt cur = S.cursor_open tx tid in
              let _sr = S.cursor_first cur in
              let last = ref None in
              let rec walk () =
                match S.cursor_next cur with
                | None -> ()
                | Some (key, _) ->
                  last := Some key;
                  walk ()
              in
              walk ();
              S.cursor_close cur;
              let%lwt mk = S.max_key tx tid in
              let%lwt () = S.commit tx in
              let%lwt () = S.close s in
              Lwt.return (Option.map Bytes.to_string mk = Option.map Bytes.to_string !last))
           (fun () ->
              cleanup ();
              Lwt.return_unit)))
;;

module Db = Granary.Db

let exec db sql =
  match Lwt_main.run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let render (v : Db.value) =
  match v with
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%g" f
  | Db.V_null -> "NULL"
  | Db.V_blob b -> Bytes.to_string b
;;

let rows db sql =
  match Lwt_main.run (Db.query db sql) with
  | Error e -> Alcotest.failf "query error in %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun row -> String.concat "|" (Array.to_list (Array.map render row)))
      (Lwt_main.run (Lwt_stream.to_list stream))
;;

(* End-to-end form of the empty-rightmost-leaf hazard: a table whose highest
   rowids were deleted and COMMITTED, then a rolled-back INSERT. The rollback
   recompute must restore max(rowid)+1 over the surviving rows, not collapse
   the counter to 1 and start overwriting them.

   This is a BEHAVIOUR-PRESERVATION test: it must pass before the
   [S.cursor_open] -> [S.max_key] conversion in [recover_next_rowid] just as
   much as after. *)
let test_rollback_recompute_after_tail_delete () =
  let db = Lwt_main.run (Db.open_in_memory ()) in
  (* [id] is the INTEGER PRIMARY KEY rowid alias — this engine has no bare
     [SELECT rowid] / [WHERE rowid > ...] over an ordinary table (see
     test_e2e.ml's note near "this engine has no [SELECT rowid]"), so the
     alias column is how the rowid is observed and filtered. *)
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
  List.iter
    (fun i ->
       ignore i;
       exec db (Printf.sprintf "INSERT INTO t (v) VALUES ('%s')" (String.make 100 'x')))
    (List.init 200 Fun.id);
  exec db "DELETE FROM t WHERE id > 100";
  exec db "BEGIN";
  exec db "INSERT INTO t (v) VALUES ('doomed')";
  exec db "ROLLBACK";
  exec db "INSERT INTO t (v) VALUES ('after')";
  (match rows db "SELECT id FROM t WHERE v = 'after'" with
   | [ r ] ->
     Alcotest.(check string) "the post-rollback insert reuses rowid 101, never 1" "101" r
   | _ -> Alcotest.fail "expected exactly one 'after' row");
  (match rows db "SELECT COUNT(*) FROM t" with
   | [ c ] -> Alcotest.(check string) "no row was overwritten" "101" c
   | _ -> Alcotest.fail "expected one count row");
  Lwt_main.run (Db.close db)
;;

let () =
  Alcotest.run
    "max_key_716"
    [ ( "btree"
      , [ Alcotest.test_case "empty tree" `Quick test_empty_tree
        ; Alcotest.test_case "single root leaf" `Quick test_single_root_leaf
        ; Alcotest.test_case "all keys deleted" `Quick test_all_keys_deleted
        ; Alcotest.test_case "empty rightmost leaf" `Quick test_empty_rightmost_leaf
        ; Alcotest.test_case
            "multiple empty rightmost leaves"
            `Quick
            test_multiple_empty_rightmost_leaves
        ; Alcotest.test_case "negative rowid keys" `Quick test_negative_rowid_keys
        ] )
    ; ( "store"
      , [ Alcotest.test_case "mem rw shadow" `Quick test_store_mem_rw
        ; Alcotest.test_case
            "mem ro snapshot ignores uncommitted"
            `Quick
            test_store_mem_ro_ignores_uncommitted
        ; Alcotest.test_case "mem empty" `Quick test_store_mem_empty
        ; Alcotest.test_case
            "btree empty rightmost leaf"
            `Quick
            test_store_btree_empty_rightmost_leaf
        ; Alcotest.test_case
            "btree rw sees own writes"
            `Quick
            test_store_btree_rw_sees_own_writes
        ] )
    ; ( "property"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_agrees_with_full_scan_mem; prop_agrees_with_full_scan_btree ] )
    ; ( "catalog"
      , [ Alcotest.test_case
            "rollback recompute after a committed tail delete"
            `Quick
            test_rollback_recompute_after_tail_delete
        ] )
    ]
;;
