module H = Granary_store.History

let r txn_id timestamp root_page = { H.txn_id; timestamp; root_page }

let test_roundtrip () =
  let rec_ = r 7L 1700000000000L 42L in
  match H.decode (H.encode rec_) with
  | Some got -> assert (got = rec_)
  | None -> assert false
;;

let test_crc_rejects_corruption () =
  let buf = H.encode (r 1L 2L 3L) in
  Cstruct.set_uint8 buf 0 (Cstruct.get_uint8 buf 0 lxor 0xff);
  assert (H.decode buf = None)
;;

let test_decode_all_two_full () =
  let a = H.encode (r 1L 10L 100L) in
  let b = H.encode (r 2L 20L 200L) in
  let buf = Cstruct.concat [ a; b ] in
  assert (H.decode_all buf = [ r 1L 10L 100L; r 2L 20L 200L ])
;;

let test_decode_all_drops_torn_tail () =
  let a = H.encode (r 1L 10L 100L) in
  let b = H.encode (r 2L 20L 200L) in
  let torn = Cstruct.sub b 0 (H.record_size - 3) in
  let buf = Cstruct.concat [ a; torn ] in
  assert (H.decode_all buf = [ r 1L 10L 100L ])
;;

let test_resolve_txn_le () =
  let recs = [ r 1L 10L 100L; r 3L 30L 300L; r 5L 50L 500L ] in
  assert (H.resolve recs (`Txn 4L) = Some (r 3L 30L 300L));
  assert (H.resolve recs (`Txn 5L) = Some (r 5L 50L 500L));
  assert (H.resolve recs (`Txn 0L) = None)
;;

let test_resolve_ts_le () =
  let recs = [ r 1L 10L 100L; r 3L 30L 300L ] in
  assert (H.resolve recs (`Ts 25L) = Some (r 1L 10L 100L));
  assert (H.resolve recs (`Ts 30L) = Some (r 3L 30L 300L))
;;

(* resolve: under a non-monotonic `Ts log, must return the record with the
   genuine MAXIMUM timestamp <= bound, not merely the last-in-list one. Here t3
   (ts=150) is later in txn order than t2 (ts=200) but has a smaller ts; a
   `Ts 250 query must resolve to t2, not t3. (#266 review) *)
let test_resolve_ts_non_monotonic () =
  let recs = [ r 1L 100L 100L; r 2L 200L 200L; r 3L 150L 150L; r 4L 300L 300L ] in
  assert (H.resolve recs (`Ts 250L) = Some (r 2L 200L 200L));
  (* `Txn behaviour is unchanged: unique, sorted, exact. *)
  assert (H.resolve recs (`Txn 3L) = Some (r 3L 150L 150L))
;;

(* decode: short buffer (< record_size) must return None *)
let test_decode_short_buffer () =
  let short = Cstruct.create (H.record_size - 1) in
  assert (H.decode short = None)
;;

(* decode_all: empty buffer yields empty list *)
let test_decode_all_empty () = assert (H.decode_all (Cstruct.create 0) = [])

(* resolve: empty list yields None *)
let test_resolve_empty () =
  assert (H.resolve [] (`Txn 100L) = None);
  assert (H.resolve [] (`Ts 100L) = None)
;;

(* resolve: all records newer than target yields None *)
let test_resolve_all_newer () =
  let recs = [ r 5L 50L 500L; r 10L 100L 1000L ] in
  assert (H.resolve recs (`Txn 3L) = None);
  assert (H.resolve recs (`Ts 30L) = None)
;;

let test_qcheck_roundtrip () =
  let gen = QCheck.(triple int64 int64 int64) in
  let prop =
    QCheck.Test.make
      ~count:1000
      ~name:"history encode/decode roundtrip"
      gen
      (fun (a, b, c) ->
         let rec_ = r a b c in
         H.decode (H.encode rec_) = Some rec_)
  in
  QCheck_base_runner.run_tests_main [ prop ] |> ignore
;;

let () =
  test_roundtrip ();
  test_crc_rejects_corruption ();
  test_decode_all_two_full ();
  test_decode_all_drops_torn_tail ();
  test_resolve_txn_le ();
  test_resolve_ts_le ();
  test_resolve_ts_non_monotonic ();
  test_decode_short_buffer ();
  test_decode_all_empty ();
  test_resolve_empty ();
  test_resolve_all_newer ();
  print_endline "test_history: OK";
  test_qcheck_roundtrip ()
;;
