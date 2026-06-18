(** Store-level tests for whole-DB as-of time travel (#266): the retention
    floor API ([history_pin]/[history_floor]/[history_release]), the
    [History_unavailable] guard, and the {!Store.ro_begin_as_of} time-travel
    read path (a historical snapshot sees an older commit; a live snapshot sees
    the latest). *)

open Lwt.Syntax
module S = Sqlocaml_store.Store
module H = Sqlocaml_store.History
module MB = Sqlocaml_mirage_block.Mirage_backend.Make (Block)

let bs s = Bytes.of_string s
let run = Lwt_main.run

(* An in-memory [History.sink] backed by a ref list: append conses, load
   returns the records in ascending (insertion) order. *)
let mem_sink () : H.sink =
  let buf = ref [] in
  { H.append =
      (fun r ->
        buf := r :: !buf;
        Lwt.return_unit)
  ; load = (fun () -> Lwt.return (List.rev !buf))
  }
;;

(* Monotonic wall-clock stub (ms): each call returns a strictly increasing
   value so distinct commits carry distinct timestamps. *)
let monotonic () =
  let n = ref 0L in
  fun () ->
    n := Int64.add !n 1L;
    !n
;;

let tmp_block_file () =
  let path = Filename.temp_file "sqlocaml_as_of_test" ".raw" in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.ftruncate fd (4 * 1024 * 1024);
  Unix.close fd;
  path
;;

(* Open a Btree store over a fresh temp block device.  [as_of] toggles the
   feature; when on, an in-memory sink and a monotonic clock are wired in. *)
let with_store ~as_of f =
  let path = tmp_block_file () in
  run
    (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
     let* adapter = MB.connect dev in
     let* result =
       if as_of
       then
         S.open_block
           ~as_of_history:true
           ~history:(mem_sink ())
           ~now:(monotonic ())
           ~init_if_corrupt:true
           ~read_page:(MB.read_page adapter)
           ~write_page:(MB.write_page adapter)
           ~sync:(MB.sync adapter)
           ~resize:(MB.resize adapter)
           ~n_pages:(MB.n_pages adapter)
           ~close:(fun () -> MB.close adapter)
           ()
       else
         S.open_block
           ~init_if_corrupt:true
           ~read_page:(MB.read_page adapter)
           ~write_page:(MB.write_page adapter)
           ~sync:(MB.sync adapter)
           ~resize:(MB.resize adapter)
           ~n_pages:(MB.n_pages adapter)
           ~close:(fun () -> MB.close adapter)
           ()
     in
     let store =
       match result with
       | Ok s -> s
       | Error e -> failwith (Format.asprintf "open_block failed: %a" S.pp_error e)
     in
     Lwt.finalize
       (fun () -> f store)
       (fun () ->
          let* () = S.close store in
          (try Unix.unlink path with
           | _ -> ());
          Lwt.return_unit))
;;

let test_pin_floor () =
  with_store ~as_of:true (fun store ->
    assert (S.history_floor store = None);
    S.history_pin store ~txn_id:5L;
    assert (S.history_floor store = Some 5L);
    S.history_release store;
    assert (S.history_floor store = None);
    Lwt.return_unit)
;;

let test_unavailable_without_flag () =
  with_store ~as_of:false (fun store ->
    Lwt.catch
      (fun () ->
         let* _ = S.ro_begin_as_of store (`Txn 1L) in
         failwith "expected History_unavailable")
      (function
        | S.History_error S.History_unavailable -> Lwt.return_unit
        | exn -> Lwt.fail exn))
;;

let test_time_travel () =
  with_store ~as_of:true (fun store ->
    (* commit row A *)
    let* tx = S.rw_begin store in
    let* () = S.put tx 0 (bs "A") (bs "rowA") in
    let* () = S.commit tx in
    (* read T1 (the txn id of the A commit) from the history log *)
    let* log = S.history_log store in
    let t1 =
      match log with
      | r :: _ -> r.H.txn_id
      | [] -> failwith "history log empty after first commit"
    in
    (* commit row B *)
    let* tx = S.rw_begin store in
    let* () = S.put tx 0 (bs "B") (bs "rowB") in
    let* () = S.commit tx in
    (* pin the floor at T1 so the A snapshot's pages stay retained *)
    S.history_pin store ~txn_id:t1;
    (* historical snapshot at T1: sees A, not B *)
    let* ro = S.ro_begin_as_of store (`Txn t1) in
    let* a = S.get ro 0 (bs "A") in
    let* b = S.get ro 0 (bs "B") in
    let* () = S.ro_end ro in
    assert (a = Some (bs "rowA"));
    assert (b = None);
    (* live snapshot: sees BOTH A and B *)
    let* live = S.ro_begin store in
    let* a2 = S.get live 0 (bs "A") in
    let* b2 = S.get live 0 (bs "B") in
    let* () = S.ro_end live in
    assert (a2 = Some (bs "rowA"));
    assert (b2 = Some (bs "rowB"));
    Lwt.return_unit)
;;

(* Mem backend: history_pin/floor/release are all no-ops; they must not raise. *)
let test_mem_retention_api_noop () =
  let store = S.create () in
  S.history_pin store ~txn_id:42L;
  assert (S.history_floor store = None);
  S.history_release store;
  assert (S.history_floor store = None);
  Lwt.return_unit
;;

(* Mem backend: ro_begin_as_of raises History_unavailable. *)
let test_mem_as_of_unavailable () =
  let store = S.create () in
  run
    (Lwt.catch
       (fun () ->
          let* _ = S.ro_begin_as_of store (`Txn 1L) in
          failwith "expected History_unavailable")
       (function
         | S.History_error S.History_unavailable -> Lwt.return_unit
         | exn -> Lwt.fail exn))
;;

(* Mem backend: history_log returns []. *)
let test_mem_history_log_empty () =
  let store = S.create () in
  run
    (let* log = S.history_log store in
     assert (log = []);
     Lwt.return_unit)
;;

(* Btree backend opened WITHOUT as_of: history_log returns []. *)
let test_btree_history_log_empty_when_disabled () =
  with_store ~as_of:false (fun store ->
    let* log = S.history_log store in
    assert (log = []);
    Lwt.return_unit)
;;

(* open_block with as_of_history=true but no history sink →
   History_misconfigured. *)
let test_misconfigured () =
  let path = tmp_block_file () in
  run
    (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
     let* adapter = MB.connect dev in
     let* result =
       S.open_block
         ~as_of_history:true (* no ~history *)
         ~init_if_corrupt:true
         ~read_page:(MB.read_page adapter)
         ~write_page:(MB.write_page adapter)
         ~sync:(MB.sync adapter)
         ~resize:(MB.resize adapter)
         ~n_pages:(MB.n_pages adapter)
         ~close:(fun () -> MB.close adapter)
         ()
     in
     (try Unix.unlink path with
      | _ -> ());
     match result with
     | Error S.History_misconfigured -> Lwt.return_unit
     | Ok _ -> failwith "expected History_misconfigured, got Ok"
     | Error e ->
       failwith (Format.asprintf "expected History_misconfigured, got %a" S.pp_error e))
;;

(* ro_begin_as_of: target exists in log but is below the floor →
   History_pruned.  Commit two txns (T1 < T2), set floor to T2, then
   try to read at T1. *)
let test_floor_pruned () =
  with_store ~as_of:true (fun store ->
    let* tx = S.rw_begin store in
    let* () = S.put tx 0 (bs "k1") (bs "v1") in
    let* () = S.commit tx in
    let* log1 = S.history_log store in
    let t1 =
      match log1 with
      | r :: _ -> r.H.txn_id
      | [] -> failwith "log empty after first commit"
    in
    let* tx = S.rw_begin store in
    let* () = S.put tx 0 (bs "k2") (bs "v2") in
    let* () = S.commit tx in
    let* log2 = S.history_log store in
    let t2 =
      match List.rev log2 with
      | r :: _ -> r.H.txn_id
      | [] -> failwith "log empty after second commit"
    in
    (* Pin floor AT T2; T1 < T2, so a read at T1 should raise History_pruned. *)
    S.history_pin store ~txn_id:t2;
    Lwt.catch
      (fun () ->
         let* _ = S.ro_begin_as_of store (`Txn t1) in
         failwith "expected History_pruned")
      (function
        | S.History_error S.History_pruned -> Lwt.return_unit
        | exn -> Lwt.fail exn))
;;

(* #266 (review): if the history sink's [load] REJECTS (reachable for the real
   Unix file sink: openfile/fstat/read can fail with EIO/EMFILE/…), the read
   lock that [ro_begin_as_of] used to acquire up front would leak.  This test
   wires a sink whose [load] always fails, asserts [ro_begin_as_of] raises, and
   then proves the read lock was NOT leaked by showing a subsequent live
   [ro_begin]/[ro_end] AND a [rw_begin]/[commit] both still complete (a leaked
   read lock would stall the writer/checkpoint/close coordination). *)
let test_load_error_no_lock_leak () =
  let failing_sink : H.sink =
    { H.append = (fun _ -> Lwt.return_unit); load = (fun () -> Lwt.fail (Failure "io")) }
  in
  let path = tmp_block_file () in
  run
    (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
     let* adapter = MB.connect dev in
     let* result =
       S.open_block
         ~as_of_history:true
         ~history:failing_sink
         ~now:(monotonic ())
         ~init_if_corrupt:true
         ~read_page:(MB.read_page adapter)
         ~write_page:(MB.write_page adapter)
         ~sync:(MB.sync adapter)
         ~resize:(MB.resize adapter)
         ~n_pages:(MB.n_pages adapter)
         ~close:(fun () -> MB.close adapter)
         ()
     in
     let store =
       match result with
       | Ok s -> s
       | Error e -> failwith (Format.asprintf "open_block failed: %a" S.pp_error e)
     in
     Lwt.finalize
       (fun () ->
          (* ro_begin_as_of must raise because [load] rejects *)
          let* () =
            Lwt.catch
              (fun () ->
                 let* _ = S.ro_begin_as_of store (`Txn 1L) in
                 failwith "expected load to reject")
              (function
                | Failure msg when msg = "io" -> Lwt.return_unit
                | exn -> Lwt.fail exn)
          in
          (* liveness proof: a normal RO snapshot opens and ends immediately *)
          let* ro = S.ro_begin store in
          let* () = S.ro_end ro in
          (* liveness proof: a write txn begins (would block if a read lock had
             leaked under a shared/exclusive coordinator) and commits *)
          let* tx = S.rw_begin store in
          let* () = S.put tx 0 (bs "k") (bs "v") in
          let* () = S.commit tx in
          Lwt.return_unit)
       (fun () ->
          let* () = S.close store in
          (try Unix.unlink path with
           | _ -> ());
          Lwt.return_unit))
;;

let () =
  test_pin_floor ();
  test_unavailable_without_flag ();
  test_time_travel ();
  run (test_mem_retention_api_noop ());
  test_mem_as_of_unavailable ();
  test_mem_history_log_empty ();
  test_btree_history_log_empty_when_disabled ();
  test_misconfigured ();
  test_floor_pruned ();
  test_load_error_no_lock_leak ();
  print_endline "test_store_as_of: OK"
;;
