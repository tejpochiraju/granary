module H = Sqlocaml_store.History

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
  print_endline "test_history: OK";
  test_qcheck_roundtrip ()
;;
