(** #245 regression: the point-lookup in-page search must not materialise a
    per-page entry list.

    Root cause (pre-fix), in [lib/storage/btree.ml]: [Btree.get] called
    [decode_leaf_entries] / [decode_branch_entries] on every visited page,
    building a full [leaf_entry list] (a fresh [Bytes] for the key AND the value
    of EVERY entry on the page, plus the cons cells) just to find one key — then
    discarded all of it.  On a ~full leaf (~145 entries for 8-byte keys + small
    values) that is hundreds of boxed allocations, measured at ~24 KB/get on a
    5000-row tree.  [Page.leaf_lookup] / [Page.branch_pick] (#245) scan the page
    in place and allocate ONLY the matched value.

    Unlike the #244 page copy (Bigarray-backed, invisible to the GC counter),
    these are ordinary OCaml-heap allocations, so [Gc.allocated_bytes] {b is}
    the right instrument.  But it is applied at the PAGE level, not around the
    full [Btree.get]: a get drags in the Lwt promise chain of the descent (~8 KB
    of scheduler machinery), which would swamp an absolute ceiling and make the
    gate insensitive to the very list it is meant to catch.  Measuring
    [Page.leaf_lookup] directly isolates the search-path allocation.

    The gate is a {b foil}: the same lookup done the OLD way (decode the whole
    list, then scan) allocates O(n_keys); the in-place lookup allocates only the
    matched value.  Asserting the in-place path stays far below both an absolute
    bound and the foil makes reintroducing a per-page list fail the test, and
    confirms the measurement actually distinguishes the two. *)

module P = Granary_storage.Page

let key_of i =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int i);
  b
;;

let value_of i = Bytes.of_string (Printf.sprintf "v%d" i)

(* Build a densely-packed sorted leaf (sequential int64 keys → ascending), and
   return the buffer and its entry count.  ~145 entries for this geometry. *)
let build_dense_leaf () =
  let buf = Cstruct.create P.page_size in
  Cstruct.memset buf 0;
  let rec fill off i =
    let key = key_of i in
    let value = value_of i in
    let sz = 2 + Bytes.length key + 2 + Bytes.length value in
    if off + sz > P.page_size
    then i
    else fill (P.leaf_append_entry buf ~offset:off ~key ~value) (i + 1)
  in
  let n = fill P.data_offset 0 in
  buf, n
;;

(* The OLD path: decode the ENTIRE entry list, then linear-scan it — the
   allocation [Page.leaf_lookup] replaces.  O(n_keys) boxed allocations per
   call, independent of where the key sits. *)
let foil_decode_then_lookup buf n_keys key : bytes option =
  let rec collect offset i acc =
    if i >= n_keys
    then List.rev acc
    else (
      match P.leaf_entry_at buf ~offset with
      | `End -> List.rev acc
      | `Entry (e : P.leaf_entry) -> collect e.next_offset (i + 1) (e :: acc))
  in
  let entries = collect P.data_offset 0 [] in
  let rec scan = function
    | [] -> None
    | (e : P.leaf_entry) :: rest ->
      let c = Bytes.compare key e.key in
      if c = 0 then Some e.value else if c < 0 then None else scan rest
  in
  scan entries
;;

(* Average per-call OCaml-heap allocation over [k] iterations. *)
let alloc_per_call k f =
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to k do
    ignore (Sys.opaque_identity (f ()))
  done;
  let after = Gc.allocated_bytes () in
  (after -. before) /. float_of_int k
;;

let test_inplace_allocates_no_list () =
  let buf, n = build_dense_leaf () in
  Alcotest.(check bool) "dense leaf has many entries" true (n >= 50);
  (* Probe the LAST key: worst case for a linear scan, and the case where the
     old full-list decode and the in-place scan touch the same number of
     entries — so any allocation gap is purely the per-entry list, not scan
     depth. *)
  let key = key_of (n - 1) in
  (* Sanity: identical results (the QCheck suite proves this exhaustively). *)
  (match P.leaf_lookup buf ~n_keys:n ~key, foil_decode_then_lookup buf n key with
   | Some a, Some b when Bytes.equal a b -> ()
   | _ -> Alcotest.fail "in-place and foil disagree on the last key");
  let inplace = alloc_per_call 1000 (fun () -> P.leaf_lookup buf ~n_keys:n ~key) in
  let foil = alloc_per_call 1000 (fun () -> foil_decode_then_lookup buf n key) in
  (* Foil must really allocate a lot (gate has teeth). *)
  Alcotest.(check bool)
    (Printf.sprintf "foil (list) allocates O(n): %.0f B > 4096" foil)
    true
    (foil > 4096.0);
  (* In-place allocates only the matched value: a few dozen bytes, and orders of
     magnitude below the list path. *)
  Alcotest.(check bool)
    (Printf.sprintf "in-place %.0f B < 512 (no per-page list)" inplace)
    true
    (inplace < 512.0);
  Alcotest.(check bool)
    (Printf.sprintf "in-place %.0f B < foil/8 %.0f B" inplace (foil /. 8.0))
    true
    (inplace < foil /. 8.0)
;;

(* Branch pick allocates nothing at all (no value copy). *)
let test_branch_pick_allocates_nothing () =
  let buf = Cstruct.create P.page_size in
  Cstruct.memset buf 0;
  let rec fill off i =
    let key = key_of i in
    let sz = 2 + Bytes.length key + 4 in
    if off + sz > P.page_size
    then i
    else
      fill
        (P.branch_append_entry buf ~offset:off ~key ~left_child:(Int32.of_int (i + 1)))
        (i + 1)
  in
  let n = fill P.data_offset 0 in
  let key = key_of (n - 1) in
  let per =
    alloc_per_call 1000 (fun () -> P.branch_pick buf ~n_keys:n ~right_page:0l ~key)
  in
  Alcotest.(check bool)
    (Printf.sprintf "branch_pick %.0f B < 64 (no allocation)" per)
    true
    (per < 64.0)
;;

let () =
  Alcotest.run
    "get_alloc"
    [ ( "leaf"
      , [ Alcotest.test_case
            "in-place lookup allocates no entry list"
            `Quick
            test_inplace_allocates_no_list
        ] )
    ; ( "branch"
      , [ Alcotest.test_case
            "branch pick allocates nothing"
            `Quick
            test_branch_pick_allocates_nothing
        ] )
    ]
;;
