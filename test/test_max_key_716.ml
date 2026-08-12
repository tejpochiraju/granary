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
    ]
;;
