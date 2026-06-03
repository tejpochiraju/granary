(** #230 regression: a full B+-tree scan must not re-decode the whole leaf into
    a fresh OCaml list on every [cursor_next].

    Root cause (pre-fix): [Btree.cursor_next] / [cursor_scan_for_key] called
    [decode_leaf_entries] on every invocation purely to recover the leaf's
    end-offset, then discarded the list.  With a leaf of K entries, scanning that
    leaf called [cursor_next] K times and each call decoded all K entries — K^2
    [Page.leaf_entry_at] calls per leaf (16.6% perf self-time in the #230
    profile) and a full K-entry list allocation per row.  The total scan stays
    O(n) but carries a ~K constant factor in CPU and allocation.

    The fix tracks a per-leaf consumed-entry index ([cursor.leaf_idx]) against
    the leaf header's [n_keys], so end-of-leaf is detected in O(1) with no list
    built per row.

    This assertion is deterministic and machine-independent: it measures bytes
    allocated by a full streaming drain via [Gc.allocated_bytes] (an exact
    counter, not a timer).  Measured at the Store/B+-tree level (no SQL pipeline
    noise) over 4000 rows: pre-fix ~12.1 KB/row, post-fix ~1.5 KB/row — a ~7.8x
    reduction.  The ceiling below sits comfortably between the two so a
    reintroduction of the per-row re-decode fails the test, while genuine
    pipeline drift has wide headroom. *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file = Sqlocaml_unix.Store.open_file
end

let run = Lwt_main.run
let tid = 0

(* Fixed-width ascending keys so [seek_ge ""] streams all rows in order and so
   leaves pack densely (high fan-out ⇒ a large pre-fix K^2 penalty to detect). *)
let key_of i = Bytes.of_string (Printf.sprintf "%012d" i)
let val_of i = Bytes.of_string (Printf.sprintf "payload-row-%d" i)

let ok_store : (S.t, S.error) result -> S.t = function
  | Ok s -> s
  | Error e -> Alcotest.failf "open_file error: %a" S.pp_error e
;;

let with_store ~f =
  let path = Filename.temp_file "sqlocaml_scan_alloc" ".db" in
  (try Unix.unlink path with
   | _ -> ());
  Lwt.finalize
    (fun () ->
       let* r = S.open_file ~path () in
       let s = ok_store r in
       Lwt.finalize (fun () -> f s) (fun () -> S.close s))
    (fun () ->
       (try Unix.unlink path with
        | _ -> ());
       Lwt.return_unit)
;;

let populate s ~n =
  let* tx = S.rw_begin s in
  let rec ins i =
    if i >= n
    then Lwt.return_unit
    else
      let* () = S.put tx tid (key_of i) (val_of i) in
      ins (i + 1)
  in
  let* () = ins 0 in
  S.commit tx
;;

(* Stream from [from_key] to the end, returning (count, first_key, last_key). *)
let drain s ~from_key =
  let* tx = S.ro_begin s in
  let* cur = S.seek_ge tx tid from_key in
  let first = ref None
  and last = ref None
  and count = ref 0 in
  let rec g () =
    let* kv = S.seek_next cur in
    match kv with
    | None -> Lwt.return_unit
    | Some (k, _) ->
      if !first = None then first := Some k;
      last := Some k;
      incr count;
      g ()
  in
  let* () = g () in
  S.seek_close cur;
  let* () = S.ro_end tx in
  Lwt.return (!count, !first, !last)
;;

let n = 4000

let test_scan_allocation_bounded () =
  run
    (with_store ~f:(fun s ->
       let* () = populate s ~n in
       (* Warm: first drain touches the page cache and lets the GC settle so the
          measured drain reflects steady-state per-row allocation. *)
       let* warm, _, _ = drain s ~from_key:(Bytes.of_string "") in
       Alcotest.(check int) "warm drain streamed every row" n warm;
       let a0 = Gc.allocated_bytes () in
       let* count, _, _ = drain s ~from_key:(Bytes.of_string "") in
       let a1 = Gc.allocated_bytes () in
       Alcotest.(check int) "measured drain streamed every row" n count;
       let per_row = (a1 -. a0) /. float_of_int count in
       Printf.eprintf "SCAN-ALLOC: %.0f bytes/row over %d rows\n%!" per_row count;
       (* Pre-fix ~12074 bytes/row; post-fix ~1547.  4000 is ~2.6x above the
          fixed cost and ~3x below the buggy cost. *)
       Alcotest.(check bool)
         (Printf.sprintf "scan allocates < 4000 bytes/row (got %.0f)" per_row)
         true
         (per_row < 4000.0);
       Lwt.return_unit))
;;

(* Guards the [leaf_idx] bookkeeping in BOTH read paths: a mid-tree [seek_ge]
   (which sets [leaf_idx] via [cursor_scan_for_key]) must stream exactly the
   tail [from..n), in order, with no drops or duplicates across leaf
   boundaries. *)
let test_scan_correct_after_seek () =
  run
    (with_store ~f:(fun s ->
       let* () = populate s ~n in
       let from = n / 3 in
       let* count, first, last = drain s ~from_key:(key_of from) in
       Alcotest.(check int) "tail count = n - from" (n - from) count;
       Alcotest.(check bool)
         "first streamed key is the seek target"
         true
         (first = Some (key_of from));
       Alcotest.(check bool)
         "last streamed key is the final row"
         true
         (last = Some (key_of (n - 1)));
       (* Full scan likewise returns the whole range in order. *)
       let* c2, f2, l2 = drain s ~from_key:(Bytes.of_string "") in
       Alcotest.(check int) "full scan count = n" n c2;
       Alcotest.(check bool) "full scan starts at row 0" true (f2 = Some (key_of 0));
       Alcotest.(check bool)
         "full scan ends at row n-1"
         true
         (l2 = Some (key_of (n - 1)));
       Lwt.return_unit))
;;

let () =
  Alcotest.run
    "scan_alloc"
    [ ( "scan"
      , [ Alcotest.test_case "full scan allocation is bounded" `Slow test_scan_allocation_bounded
        ; Alcotest.test_case "scan correct after mid-tree seek" `Quick test_scan_correct_after_seek
        ] )
    ]
;;
