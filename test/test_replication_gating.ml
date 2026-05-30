(** Tests for replication reader gating in the Store. *)

open Lwt.Syntax
module Store = Sqlocaml_store.Store

(* ------------------------------------------------------------------ *)
(* WAL-backed in-memory store helpers                                  *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev size = { buf = Bytes.make size '\x00' }

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (let nb = Bytes.make (max need (cur * 2)) '\x00' in
        Bytes.blit d.buf 0 nb 0 cur; d.buf <- nb)
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  if off + len > Bytes.length d.buf
  then Lwt.return (Error "read past EOF")
  else (Cstruct.blit_from_bytes d.buf off out 0 len; Lwt.return (Ok ()))
;;

let write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  dev_grow d (off + len);
  Cstruct.blit_to_bytes src 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())


(* ------------------------------------------------------------------ *)
(* Open a WAL store in memory                                          *)
(* ------------------------------------------------------------------ *)

let open_test_store () =
  let main_dev = mk_dev (1024 * 4096) in
  let wal_dev = mk_dev 65536 in
  let main_n_pages = Int64.of_int (Bytes.length main_dev.buf / 4096) in
  let read_page ~page_id buf =
    let off = Int64.to_int (Int64.mul page_id 4096L) in
    let len = Cstruct.length buf in
    if off + len > Bytes.length main_dev.buf
    then Lwt.return (Error "read past EOF")
    else (Cstruct.blit_from_bytes main_dev.buf off buf 0 len; Lwt.return (Ok ()))
  in
  let write_page ~page_id buf =
    let off = Int64.to_int (Int64.mul page_id 4096L) in
    let len = Cstruct.length buf in
    dev_grow main_dev (off + len);
    Cstruct.blit_to_bytes buf 0 main_dev.buf off len;
    Lwt.return (Ok ())
  in
  let resize ~n_pages =
    dev_grow main_dev ((Int64.to_int n_pages) * 4096);
    Lwt.return (Ok ())
  in
  Store.open_block_wal
    ~read_page ~write_page ~sync:sync_ok ~resize ~n_pages:main_n_pages
    ~wal_read_at:(read_at wal_dev)
    ~wal_write_at:(write_at wal_dev)
    ~wal_sync:sync_ok
    ~wal_size_bytes:(Int64.of_int (Bytes.length wal_dev.buf))
    ~close:(fun () -> Lwt.return_unit)
    ~wal_close:(fun () -> Lwt.return_unit)
    ()
;;


(* ------------------------------------------------------------------ *)
(* Test: replication position blocks checkpoint                        *)
(* ------------------------------------------------------------------ *)

let test_replication_gating_blocks_checkpoint () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st = match sr with
       | Ok s -> s | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     Store.set_wal_autocheckpoint st 3;
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "k1") (Bytes.of_string "v1") in
     let* () = Store.put rw 16 (Bytes.of_string "k2") (Bytes.of_string "v2") in
     let* () = Store.commit rw in
     let epoch_before, frames_before = match Store.replication_state st with
       | Some s -> s | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool) "WAL has frames" true (frames_before > 0);
     (* Register replication consumer at position 0 -> blocks checkpoint *)
     Store.update_replication_position st ~shipped:0;
     let* rw2 = Store.rw_begin st in
     let* () = Store.put rw2 16 (Bytes.of_string "k3") (Bytes.of_string "v3") in
     let* () = Store.put rw2 16 (Bytes.of_string "k4") (Bytes.of_string "v4") in
     let* () = Store.commit rw2 in
     let epoch_after, frames_after = match Store.replication_state st with
       | Some s -> s | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check int64) "epoch unchanged" epoch_before epoch_after;
     Alcotest.(check bool) "frames grew" true (frames_after > frames_before);
     (* Advance replication position -> checkpoint can proceed *)
     Store.update_replication_position st ~shipped:max_int;
     let* rw3 = Store.rw_begin st in
     let* () = Store.put rw3 16 (Bytes.of_string "k5") (Bytes.of_string "v5") in
     let* () = Store.commit rw3 in
     let* () = Store.checkpoint st in
     let _, frames_final = match Store.replication_state st with
       | Some s -> s | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool) "WAL reset after checkpoint" true (frames_final < 10);
     let* () = Store.close st in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Test: commit callback fired                                          *)
(* ------------------------------------------------------------------ *)

let test_commit_callback_fired () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st = match sr with
       | Ok s -> s | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     let cb_fired = ref false in
     let cb_count = ref 0 in
     let cb_promise, cb_resolver = Lwt.wait () in
     Store.set_commit_callback st
       (Some (fun ~epoch:_ ~base_idx:_ ~count ->
          cb_fired := true;
          cb_count := count;
          Lwt.wakeup cb_resolver ()));
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "hello") (Bytes.of_string "world") in
     let* () = Store.commit rw in
     let* () = cb_promise in
     Alcotest.(check bool) "callback fired" true !cb_fired;
     Alcotest.(check bool) "non-zero count" true (!cb_count > 0);
     Store.set_commit_callback st None;
     let* () = Store.close st in
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "replication-gating"
    [ ( "gating"
      , [ Alcotest.test_case "position blocks checkpoint" `Quick
            test_replication_gating_blocks_checkpoint
        ; Alcotest.test_case "commit callback fires" `Quick
            test_commit_callback_fired
        ] )
    ]
;;
