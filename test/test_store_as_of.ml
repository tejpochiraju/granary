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

let () =
  test_pin_floor ();
  test_unavailable_without_flag ();
  test_time_travel ();
  print_endline "test_store_as_of: OK"
;;
