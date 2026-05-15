(** Tests for Sqlocaml_storage.Btree (CoW B+-tree). *)

open Sqlocaml_storage

(* ------------------------------------------------------------------ *)
(* Mock in-memory pager (same pattern as test_pager.ml)                 *)
(* ------------------------------------------------------------------ *)

type mock_block = {
  store           : (int64, Bytes.t) Hashtbl.t;
  mutable n_pages : int64; [@warning "-69"]
}
[@@warning "-69"]

let make_mock () =
  { store = Hashtbl.create 64; n_pages = 0L; }

let mock_callbacks mb =
  let read_page ~page_id buf =
    (match Hashtbl.find_opt mb.store page_id with
     | None ->
       Cstruct.memset buf 0;
       Lwt.return_ok ()
     | Some bytes ->
       Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size;
       Lwt.return_ok ())
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
  (read_page, write_page, sync, resize)

let make_pager () =
  let mb = make_mock () in
  let (read_page, write_page, sync, resize) = mock_callbacks mb in
  (* Reserve pages 0 and 1 (as real on-disk usage does for the alternating
     headers) so the B+-tree's first alloc returns page 2.  This avoids
     conflating "empty tree (root_page = 0L)" with "root is page 0". *)
  let pager =
    Pager.create ~read_page ~write_page ~sync ~resize
      ~n_pages:2L ~freelist:Freelist.empty
  in
  (pager, mb)

let run = Lwt_main.run

(* Convenience constructors. *)
let b s = Bytes.of_string s

(* Build an empty tree over a fresh pager. *)
let empty_tree () =
  let (p, _) = make_pager () in
  (Btree.create p ~root_page:0L, p)

(* Unwrap a [result] for use in tests; on Error, fail the test. *)
let ok_btree : type a. (a, Btree.error) result -> a = function
  | Ok x -> x
  | Error e ->
    Alcotest.failf "unexpected error: %a" Btree.pp_error e

(* ------------------------------------------------------------------ *)
(* Unit tests                                                          *)
(* ------------------------------------------------------------------ *)

let test_get_empty () =
  let (t, _) = empty_tree () in
  let r = run (Btree.get t (b "x")) in
  Alcotest.(check bool) "get on empty tree returns Ok None"
    true (r = Ok None)

let test_put_then_get () =
  let (t, _) = empty_tree () in
  let t = ok_btree (run (Btree.put t (b "k") (b "v"))) in
  let r = run (Btree.get t (b "k")) in
  Alcotest.(check bool) "get returns Some v"
    true (r = Ok (Some (b "v")))

let test_put_update () =
  let (t, _) = empty_tree () in
  let t = ok_btree (run (Btree.put t (b "k") (b "v1"))) in
  let t = ok_btree (run (Btree.put t (b "k") (b "v2"))) in
  let r = run (Btree.get t (b "k")) in
  Alcotest.(check bool) "get returns updated value"
    true (r = Ok (Some (b "v2")))

let test_del_missing () =
  let (t, _) = empty_tree () in
  let t = ok_btree (run (Btree.put t (b "k") (b "v"))) in
  let root_before = Btree.root_page t in
  let t' = ok_btree (run (Btree.del t (b "z"))) in
  Alcotest.(check int64) "root unchanged after no-op del"
    root_before (Btree.root_page t');
  let r = run (Btree.get t' (b "k")) in
  Alcotest.(check bool) "k still present" true (r = Ok (Some (b "v")))

let test_put_then_del () =
  let (t, _) = empty_tree () in
  let t = ok_btree (run (Btree.put t (b "k") (b "v"))) in
  let t = ok_btree (run (Btree.del t (b "k"))) in
  let r = run (Btree.get t (b "k")) in
  Alcotest.(check bool) "deleted key gone" true (r = Ok None);
  Alcotest.(check int64) "root_page becomes 0L after deleting last key"
    0L (Btree.root_page t)

let test_insert_10_random () =
  let (t, _) = empty_tree () in
  let pairs = [
    "delta", "4"; "alpha", "1"; "echo", "5"; "bravo", "2";
    "golf", "7"; "charlie", "3"; "foxtrot", "6"; "hotel", "8";
    "india", "9"; "juliet", "10";
  ] in
  let t = List.fold_left
      (fun t (k, v) -> ok_btree (run (Btree.put t (b k) (b v))))
      t pairs
  in
  List.iter (fun (k, v) ->
      let r = run (Btree.get t (b k)) in
      Alcotest.(check bool)
        (Printf.sprintf "lookup %s = %s" k v)
        true (r = Ok (Some (b v))))
    pairs

let test_cursor_empty () =
  let (t, _) = empty_tree () in
  let c = ok_btree (run (Btree.cursor_open t)) in
  let r = run (Btree.cursor_next c) in
  Alcotest.(check bool) "empty cursor returns None" true (r = Ok None)

let test_cursor_5_keys_sorted () =
  let (t, _) = empty_tree () in
  (* Insert out of order. *)
  let keys = ["c"; "a"; "e"; "b"; "d"] in
  let t = List.fold_left
      (fun t k -> ok_btree (run (Btree.put t (b k) (b (k ^ "v")))))
      t keys
  in
  let c = ok_btree (run (Btree.cursor_open t)) in
  let rec collect acc =
    match run (Btree.cursor_next c) with
    | Ok None -> List.rev acc
    | Ok (Some (k, _)) -> collect (Bytes.to_string k :: acc)
    | Error e -> Alcotest.failf "cursor error: %a" Btree.pp_error e
  in
  let got = collect [] in
  Alcotest.(check (list string)) "cursor visits in sorted order"
    ["a"; "b"; "c"; "d"; "e"] got;
  let extra = run (Btree.cursor_next c) in
  Alcotest.(check bool) "next after end returns None" true (extra = Ok None)

let test_cursor_seek_found () =
  let (t, _) = empty_tree () in
  let t = List.fold_left
      (fun t k -> ok_btree (run (Btree.put t (b k) (b (k ^ "v")))))
      t ["a"; "b"; "c"; "d"; "e"]
  in
  let c = ok_btree (run (Btree.cursor_open t)) in
  let r = run (Btree.cursor_seek c (b "c")) in
  Alcotest.(check bool) "seek finds exact match" true (r = Ok `Found);
  match run (Btree.cursor_next c) with
  | Ok (Some (k, v)) ->
    Alcotest.(check string) "first next returns matched key"
      "c" (Bytes.to_string k);
    Alcotest.(check string) "first next returns its value"
      "cv" (Bytes.to_string v)
  | _ -> Alcotest.fail "expected (c, cv)"

let test_cursor_seek_between () =
  let (t, _) = empty_tree () in
  let t = List.fold_left
      (fun t k -> ok_btree (run (Btree.put t (b k) (b (k ^ "v")))))
      t ["a"; "c"; "e"]
  in
  let c = ok_btree (run (Btree.cursor_open t)) in
  let r = run (Btree.cursor_seek c (b "b")) in
  Alcotest.(check bool) "seek between keys returns Not_found_after"
    true (match r with Ok (`Not_found_after _) -> true | _ -> false);
  match run (Btree.cursor_next c) with
  | Ok (Some (k, _)) ->
    Alcotest.(check string) "first next returns next key (c)"
      "c" (Bytes.to_string k)
  | _ -> Alcotest.fail "expected next = c"

let test_cursor_seek_past_end () =
  let (t, _) = empty_tree () in
  let t = List.fold_left
      (fun t k -> ok_btree (run (Btree.put t (b k) (b (k ^ "v")))))
      t ["a"; "b"; "c"]
  in
  let c = ok_btree (run (Btree.cursor_open t)) in
  let r = run (Btree.cursor_seek c (b "z")) in
  Alcotest.(check bool) "seek past end returns Not_found_after"
    true (match r with Ok (`Not_found_after _) -> true | _ -> false);
  let r2 = run (Btree.cursor_next c) in
  Alcotest.(check bool) "cursor_next at end returns None"
    true (r2 = Ok None)

(* Force a leaf split: each entry is 4 + key_len + val_len bytes; with a
   100-byte value and ~10-byte key, ~40 entries fill 4080 bytes. *)
let test_leaf_split () =
  let (t, _) = empty_tree () in
  let value = Bytes.make 100 'x' in
  let n = 60 in
  let t = ref t in
  for i = 0 to n - 1 do
    let k = b (Printf.sprintf "k%04d" i) in
    t := ok_btree (run (Btree.put !t k value))
  done;
  for i = 0 to n - 1 do
    let k = b (Printf.sprintf "k%04d" i) in
    let r = run (Btree.get !t k) in
    Alcotest.(check bool)
      (Printf.sprintf "after split, key %d present" i)
      true (r = Ok (Some value))
  done

(* Build a deeper tree (branch + leaf) and walk the cursor over it. *)
let test_multilevel_cursor () =
  let (t, _) = empty_tree () in
  let value = Bytes.make 500 'y' in
  let n = 200 in
  (* Insert in a non-sequential order to stress traversal. *)
  let t = ref t in
  for i = 0 to n - 1 do
    let perm = ((i * 17) + 3) mod n in
    let k = b (Printf.sprintf "k%05d" perm) in
    t := ok_btree (run (Btree.put !t k value))
  done;
  (* All keys must be retrievable. *)
  for i = 0 to n - 1 do
    let k = b (Printf.sprintf "k%05d" i) in
    let r = run (Btree.get !t k) in
    Alcotest.(check bool)
      (Printf.sprintf "deep tree: key %d present" i)
      true (r = Ok (Some value))
  done;
  (* Cursor visits in sorted order. *)
  let c = ok_btree (run (Btree.cursor_open !t)) in
  let rec collect acc =
    match run (Btree.cursor_next c) with
    | Ok None -> List.rev acc
    | Ok (Some (k, _)) -> collect (Bytes.to_string k :: acc)
    | Error e -> Alcotest.failf "cursor: %a" Btree.pp_error e
  in
  let got = collect [] in
  Alcotest.(check int) "cursor visits all entries" n (List.length got);
  let sorted = List.sort String.compare got in
  Alcotest.(check (list string)) "cursor order is sorted" sorted got

let test_root_page_changes () =
  let (t, _) = empty_tree () in
  let t1 = ok_btree (run (Btree.put t (b "a") (b "1"))) in
  let r1 = Btree.root_page t1 in
  (* The pager starts with pages 0 and 1 pre-allocated, so the first alloc
     returns page 2 — i.e. root must not be 0L after a successful put. *)
  Alcotest.(check bool) "root_page non-zero after first put"
    true (r1 <> 0L);
  (* Each put on a distinct key creates a new CoW page, so the root changes. *)
  let t2 = ok_btree (run (Btree.put t1 (b "b") (b "2"))) in
  let r2 = Btree.root_page t2 in
  Alcotest.(check bool) "root_page changed after second put (CoW)"
    true (r1 <> r2)

let test_key_too_large () =
  let (t, _) = empty_tree () in
  let big_key = Bytes.make 513 'k' in
  match run (Btree.put t big_key (b "v")) with
  | Error (Btree.Key_too_large 513) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" Btree.pp_error e
  | Ok _ -> Alcotest.fail "expected Key_too_large"

let test_value_too_large () =
  let (t, _) = empty_tree () in
  let big_value = Bytes.make 1025 'v' in
  match run (Btree.put t (b "k") big_value) with
  | Error (Btree.Value_too_large 1025) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" Btree.pp_error e
  | Ok _ -> Alcotest.fail "expected Value_too_large"

(* Inject a Header page (kind != Leaf/Branch) as the root, forcing Tree_corrupt *)
let test_tree_corrupt_bad_page_kind () =
  let mb = make_mock () in
  let (read_page, write_page, sync, resize) = mock_callbacks mb in
  let pager =
    Pager.create ~read_page ~write_page ~sync ~resize
      ~n_pages:3L ~freelist:Freelist.empty
  in
  (* Write a Header-kind page at page 2 (our designated root) *)
  let buf = Cstruct.create Page.page_size in
  Page.write_common buf Page.{
    kind = Page.Header; flags = 0; n_keys = 0; right_page = 0l; crc32 = 0l
  };
  Page.seal buf;
  Pager.write pager 2L buf;
  let t = Btree.create pager ~root_page:2L in
  (* GET on a root that is a Header page should yield Tree_corrupt *)
  (match run (Btree.get t (b "k")) with
   | Error (Btree.Tree_corrupt _) -> ()
   | Error e -> Alcotest.failf "expected Tree_corrupt, got: %a" Btree.pp_error e
   | Ok _ -> Alcotest.fail "expected Tree_corrupt error");
  (* PUT should also hit Tree_corrupt *)
  let _ = Btree.create pager ~root_page:2L in
  (match run (Btree.put t (b "k") (b "v")) with
   | Error (Btree.Tree_corrupt _) -> ()
   | Error e -> Alcotest.failf "expected Tree_corrupt on put, got: %a" Btree.pp_error e
   | Ok _ -> Alcotest.fail "expected Tree_corrupt error on put");
  (* DEL should also hit Tree_corrupt — but del starts by reading the leaf *)
  (match run (Btree.del t (b "k")) with
   | Error (Btree.Tree_corrupt _) -> ()
   | Error e -> Alcotest.failf "expected Tree_corrupt on del, got: %a" Btree.pp_error e
   | Ok _ -> Alcotest.fail "expected Tree_corrupt error on del")

(* Test del on a key that's too large (>max_key_size): should return t unchanged *)
let test_del_key_too_large () =
  let (t, _) = empty_tree () in
  let t = ok_btree (run (Btree.put t (b "k") (b "v"))) in
  let big_key = Bytes.make 513 'k' in
  (* del with oversized key is a no-op *)
  let t' = ok_btree (run (Btree.del t big_key)) in
  (* original key must still be present *)
  let r = run (Btree.get t' (b "k")) in
  Alcotest.(check bool) "key still present after no-op del" true (r = Ok (Some (b "v")))

(* Inject a branch page with n_keys=0 (empty branch) as root.
   This exercises the `entries = []` arm in leftmost_leaf_with_path
   that uses right_page directly. *)
let test_empty_branch_cursor () =
  let mb = make_mock () in
  let (read_page, write_page, sync, resize) = mock_callbacks mb in
  let pager =
    Pager.create ~read_page ~write_page ~sync ~resize
      ~n_pages:4L ~freelist:Freelist.empty
  in
  (* page 2: empty branch (n_keys=0) pointing right_page → page 3 *)
  let branch_buf = Cstruct.create Page.page_size in
  Page.write_common branch_buf Page.{
    kind = Page.Branch; flags = 0; n_keys = 0;
    right_page = 3l;  (* right_page = page 3 *)
    crc32 = 0l;
  };
  Page.seal branch_buf;
  Pager.write pager 2L branch_buf;
  (* page 3: leaf with one entry *)
  let leaf_buf = Cstruct.create Page.page_size in
  let _next = Page.leaf_append_entry leaf_buf
    ~offset:Page.data_offset ~key:(b "mykey") ~value:(b "myval") in
  Page.write_common leaf_buf Page.{
    kind = Page.Leaf; flags = 0; n_keys = 1;
    right_page = 0l; crc32 = 0l;
  };
  Page.seal leaf_buf;
  Pager.write pager 3L leaf_buf;
  let t = Btree.create pager ~root_page:2L in
  (* Open cursor: should descend through the empty branch → leaf *)
  let c = ok_btree (run (Btree.cursor_open t)) in
  (match run (Btree.cursor_next c) with
   | Ok (Some (k, v)) ->
     Alcotest.(check bool) "key is mykey" true (Bytes.equal k (b "mykey"));
     Alcotest.(check bool) "val is myval" true (Bytes.equal v (b "myval"))
   | Ok None -> Alcotest.fail "expected entry from leaf under empty branch"
   | Error e -> Alcotest.failf "cursor error: %a" Btree.pp_error e)

let string_contains hay needle =
  let hl = String.length hay and nl = String.length needle in
  let rec go i =
    if i > hl - nl then false
    else if String.sub hay i nl = needle then true
    else go (i + 1)
  in
  go 0

(* Exercise every constructor of Btree.error in pp_error. *)
let test_pp_error_all_variants () =
  let pe1 = Btree.pp_error in
  let s_pager_block = Format.asprintf "%a" pe1
                        (Btree.Pager_error (Pager.Block_error "x")) in
  let s_pager_corr  = Format.asprintf "%a" pe1
                        (Btree.Pager_error (Pager.Corruption "c")) in
  let s_keytl = Format.asprintf "%a" pe1 (Btree.Key_too_large 99) in
  let s_valtl = Format.asprintf "%a" pe1 (Btree.Value_too_large 7) in
  let s_corr  = Format.asprintf "%a" pe1 (Btree.Tree_corrupt "broken") in
  Alcotest.(check bool) "Pager_error/Block" true
    (string_contains s_pager_block "Block_error");
  Alcotest.(check bool) "Pager_error/Corr" true
    (string_contains s_pager_corr "Corruption");
  Alcotest.(check bool) "Key_too_large" true (string_contains s_keytl "99");
  Alcotest.(check bool) "Value_too_large" true (string_contains s_valtl "7");
  Alcotest.(check bool) "Tree_corrupt" true (string_contains s_corr "broken")

(* Force a BRANCH split by inserting enough keys that the root branch
   itself exceeds Page.max_data_bytes.  With ~1000-byte values and ~10-byte
   keys, each leaf holds 3-4 entries; an internal branch with 10-byte keys
   (16 bytes each) holds ~250 entries before splitting, so ~800-1000 leaf
   entries should overflow the root branch. *)
let test_branch_split () =
  let (t, _) = empty_tree () in
  let value = Bytes.make 1000 'q' in
  let n = 1200 in
  let t = ref t in
  for i = 0 to n - 1 do
    let k = b (Printf.sprintf "k%05d" i) in
    t := ok_btree (run (Btree.put !t k value))
  done;
  (* Spot-check retrieval at a few positions *)
  let positions = [0; 1; 100; 500; n - 1] in
  List.iter (fun i ->
    let k = b (Printf.sprintf "k%05d" i) in
    match run (Btree.get !t k) with
    | Ok (Some v) -> Alcotest.(check int) (Printf.sprintf "len(v%d)" i)
                       (Bytes.length value) (Bytes.length v)
    | _ -> Alcotest.failf "missing key at position %d" i
  ) positions;
  (* Cursor should still visit all entries in sorted order *)
  let c = ok_btree (run (Btree.cursor_open !t)) in
  let rec collect acc =
    match run (Btree.cursor_next c) with
    | Ok None -> List.rev acc
    | Ok (Some (k, _)) -> collect (Bytes.to_string k :: acc)
    | Error e -> Alcotest.failf "cursor error: %a" Btree.pp_error e
  in
  let got = collect [] in
  Alcotest.(check int) "cursor visits all entries" n (List.length got)

(* Mock pager whose write_page can be set to fail on demand --- used to
   exercise B-tree's error-propagation arms. *)
type failable_mock = {
  store : (int64, Bytes.t) Hashtbl.t;
  mutable n_pages : int64; [@warning "-69"]
  mutable fail_writes : bool;
  mutable fail_reads  : bool;
  mutable fail_resize : bool;
}
[@@warning "-69"]

let make_failable_mock () =
  { store = Hashtbl.create 64; n_pages = 0L;
    fail_writes = false; fail_reads = false; fail_resize = false }

let failable_pager mb =
  let read_page ~page_id buf =
    if mb.fail_reads then Lwt.return_error "injected read error"
    else
      (match Hashtbl.find_opt mb.store page_id with
       | None ->
         Cstruct.memset buf 0;
         Lwt.return_ok ()
       | Some bytes ->
         Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size;
         Lwt.return_ok ())
  in
  let write_page ~page_id buf =
    if mb.fail_writes then Lwt.return_error "injected write error"
    else begin
      let bytes = Bytes.create Page.page_size in
      Cstruct.blit_to_bytes buf 0 bytes 0 Page.page_size;
      Hashtbl.replace mb.store page_id bytes;
      Lwt.return_ok ()
    end
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages =
    if mb.fail_resize then Lwt.return_error "injected resize error"
    else begin mb.n_pages <- n_pages; Lwt.return_ok () end
  in
  Pager.create ~read_page ~write_page ~sync ~resize
    ~n_pages:2L ~freelist:Freelist.empty

(* Btree.put with a resize-failure pager -> Pager.alloc fails ->
   Btree.put surfaces a Pager_error (Block_error _). *)
let test_btree_put_alloc_error () =
  let mb = make_failable_mock () in
  let pager = failable_pager mb in
  let t = Btree.create pager ~root_page:0L in
  mb.fail_resize <- true;
  match run (Btree.put t (b "k") (b "v")) with
  | Error (Btree.Pager_error (Pager.Block_error _)) -> ()
  | Error e -> Alcotest.failf "expected Pager_error Block_error, got: %a"
                 Btree.pp_error e
  | Ok _ -> Alcotest.fail "expected error from failing resize"

(* Btree.get on a tree whose root page can't be read should surface a
   Pager_error.  We construct a Btree with a non-zero root_page that
   points to a page that doesn't exist in the mock's store, then make
   reads fail. *)
let test_btree_get_read_error () =
  let mb = make_failable_mock () in
  let pager = failable_pager mb in
  let t = Btree.create pager ~root_page:0L in
  (* Build a real tree first via a working pager. *)
  let t = match run (Btree.put t (b "k") (b "v")) with
    | Ok t -> t
    | Error e -> Alcotest.failf "setup put: %a" Btree.pp_error e
  in
  (* Now flip read failure on the same mock and create a fresh Btree
     against the same root page, but use a fresh pager (no cache).  *)
  let pager2 =
    let read_page ~page_id:_ _buf = Lwt.return_error "injected" in
    let write_page ~page_id:_ _buf = Lwt.return_ok () in
    let sync () = Lwt.return_ok () in
    let resize ~n_pages:_ = Lwt.return_ok () in
    Pager.create ~read_page ~write_page ~sync ~resize
      ~n_pages:2L ~freelist:Freelist.empty
  in
  let t2 = Btree.create pager2 ~root_page:(Btree.root_page t) in
  match run (Btree.get t2 (b "k")) with
  | Error (Btree.Pager_error _) -> ()
  | Error e -> Alcotest.failf "expected Pager_error, got: %a"
                 Btree.pp_error e
  | Ok _ -> Alcotest.fail "expected error from failing reads"

(* ------------------------------------------------------------------ *)
(* QCheck properties                                                   *)
(* ------------------------------------------------------------------ *)

(* Random non-empty bytes of bounded length. *)
let key_gen =
  let open QCheck in
  Gen.(map (fun s -> Bytes.of_string s)
         (string_size (int_range 1 16)))

let val_gen =
  let open QCheck in
  Gen.(map (fun s -> Bytes.of_string s)
         (string_size (int_range 0 32)))

let kv_list_gen =
  let open QCheck in
  Gen.list_size (Gen.int_range 0 100) (Gen.pair key_gen val_gen)

(* Build the expected map: last-write-wins semantics. *)
let map_of_list kvs =
  let h = Hashtbl.create 32 in
  List.iter (fun (k, v) -> Hashtbl.replace h k v) kvs;
  h

(* Property: every key in the inserted list (with last-write-wins) is
   retrievable via [get]. *)
let prop_put_get =
  QCheck.Test.make
    ~name:"prop_put_get"
    ~count:10_000
    (QCheck.make kv_list_gen)
    (fun kvs ->
       let (t, _) = empty_tree () in
       let t = List.fold_left
           (fun t (k, v) ->
              match run (Btree.put t k v) with
              | Ok t -> t
              | Error _ -> t)
           t kvs
       in
       let m = map_of_list kvs in
       Hashtbl.fold (fun k v ok ->
           if not ok then false
           else match run (Btree.get t k) with
             | Ok (Some v') -> Bytes.equal v v'
             | _ -> false)
         m true)

(* Property: cursor walks keys in ascending lexicographic order. *)
let prop_cursor_sorted =
  QCheck.Test.make
    ~name:"prop_cursor_sorted"
    ~count:10_000
    (QCheck.make kv_list_gen)
    (fun kvs ->
       let (t, _) = empty_tree () in
       let t = List.fold_left
           (fun t (k, v) ->
              match run (Btree.put t k v) with
              | Ok t -> t
              | Error _ -> t)
           t kvs
       in
       match run (Btree.cursor_open t) with
       | Error _ -> false
       | Ok c ->
         let rec collect acc =
           match run (Btree.cursor_next c) with
           | Ok None -> Some (List.rev acc)
           | Ok (Some (k, _)) -> collect (k :: acc)
           | Error _ -> None
         in
         (match collect [] with
          | None -> false
          | Some keys ->
            let rec is_sorted = function
              | [] | [_] -> true
              | a :: (b :: _ as rest) ->
                Bytes.compare a b < 0 && is_sorted rest
            in
            is_sorted keys))

(* Property: put then del; key gone, other keys still present. *)
let prop_put_del =
  QCheck.Test.make
    ~name:"prop_put_del"
    ~count:10_000
    QCheck.(make (Gen.triple kv_list_gen key_gen val_gen))
    (fun (kvs, target_k, target_v) ->
       (* Insert all kvs then put target_k=target_v then delete target_k. *)
       let (t, _) = empty_tree () in
       let t = List.fold_left
           (fun t (k, v) ->
              match run (Btree.put t k v) with
              | Ok t -> t
              | Error _ -> t)
           t kvs
       in
       let t = match run (Btree.put t target_k target_v) with
         | Ok t -> t | Error _ -> t
       in
       let t = match run (Btree.del t target_k) with
         | Ok t -> t | Error _ -> t
       in
       (* target_k must be gone. *)
       let target_gone =
         match run (Btree.get t target_k) with
         | Ok None -> true
         | _ -> false
       in
       (* Other keys with NO overlap with target_k must still be present
          (last-write-wins). *)
       let m = map_of_list kvs in
       Hashtbl.remove m target_k;
       let others_ok = Hashtbl.fold (fun k v ok ->
           if not ok then false
           else match run (Btree.get t k) with
             | Ok (Some v') -> Bytes.equal v v'
             | _ -> false)
           m true
       in
       target_gone && others_ok)

(* Property: after any sequence of successful puts, root_page is non-zero
   (if at least one put succeeded). *)
let prop_root_nonzero_after_put =
  QCheck.Test.make
    ~name:"prop_root_nonzero_after_put"
    ~count:10_000
    (QCheck.make kv_list_gen)
    (fun kvs ->
       let (t, _) = empty_tree () in
       let (t, any) = List.fold_left
           (fun (t, any) (k, v) ->
              match run (Btree.put t k v) with
              | Ok t -> (t, true)
              | Error _ -> (t, any))
           (t, false) kvs
       in
       if not any then Btree.root_page t = 0L
       else
         (* The test pager pre-allocates pages 0 and 1, so the btree's first
            alloc returns page 2 or higher.  After any successful put the
            root_page must therefore be > 0L. *)
         Int64.compare (Btree.root_page t) 0L > 0)

(* ------------------------------------------------------------------ *)
(* Runner                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_put_get;
      prop_cursor_sorted;
      prop_put_del;
      prop_root_nonzero_after_put;
    ]
  in
  Alcotest.run "btree" [
    "basic", [
      Alcotest.test_case "get on empty tree"           `Quick test_get_empty;
      Alcotest.test_case "put then get"                `Quick test_put_then_get;
      Alcotest.test_case "put then update"             `Quick test_put_update;
      Alcotest.test_case "del missing key no-op"       `Quick test_del_missing;
      Alcotest.test_case "put then del"                `Quick test_put_then_del;
      Alcotest.test_case "insert 10 random keys"       `Quick test_insert_10_random;
      Alcotest.test_case "cursor over empty tree"      `Quick test_cursor_empty;
      Alcotest.test_case "cursor 5 keys sorted"        `Quick test_cursor_5_keys_sorted;
      Alcotest.test_case "cursor_seek found"           `Quick test_cursor_seek_found;
      Alcotest.test_case "cursor_seek between"         `Quick test_cursor_seek_between;
      Alcotest.test_case "cursor_seek past end"        `Quick test_cursor_seek_past_end;
      Alcotest.test_case "leaf split + retrieval"      `Quick test_leaf_split;
      Alcotest.test_case "multilevel tree + cursor"    `Quick test_multilevel_cursor;
      Alcotest.test_case "root_page changes (CoW)"     `Quick test_root_page_changes;
      Alcotest.test_case "key too large"               `Quick test_key_too_large;
      Alcotest.test_case "value too large"             `Quick test_value_too_large;
      Alcotest.test_case "tree_corrupt bad page kind"  `Quick test_tree_corrupt_bad_page_kind;
      Alcotest.test_case "del key too large no-op"     `Quick test_del_key_too_large;
      Alcotest.test_case "empty branch cursor"         `Quick test_empty_branch_cursor;
      Alcotest.test_case "pp_error all variants"       `Quick test_pp_error_all_variants;
      Alcotest.test_case "branch split"                `Slow  test_branch_split;
      Alcotest.test_case "put alloc error"             `Quick test_btree_put_alloc_error;
      Alcotest.test_case "get read error"              `Quick test_btree_get_read_error;
    ];
    "qcheck", qcheck_tests;
  ]
