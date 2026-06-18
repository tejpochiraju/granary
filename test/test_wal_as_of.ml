(** WAL-mode tests for whole-DB as-of time travel (#266).

    The feature's primary deployment is WAL mode, so this exercises the as-of
    read path against a {!Store.open_block_wal} store backed by in-memory WAL
    and main devices (the same device pattern as {!test_shared_wal}).  It
    asserts that a historical snapshot pinned at an older txn sees only the rows
    committed up to that txn, including ACROSS an explicit {!Store.checkpoint}
    (the key WAL+checkpoint correctness guarantee: checkpointing newer frames to
    the main file must not disturb a pinned historical read). *)

open Lwt.Syntax
module S = Sqlocaml_store.Store
module H = Sqlocaml_store.History

let bs s = Bytes.of_string s
let run = Lwt_main.run

(* ------------------------------------------------------------------ *)
(* In-memory device helpers (same pattern as test_shared_wal.ml)       *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev size = { buf = Bytes.make size '\x00' }
let dev_size d = Int64.of_int (Bytes.length d.buf)

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (
    let new_size = max need (cur * 2) in
    let nb = Bytes.make new_size '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb)
;;

let read_page d ~page_id out =
  let page_size = 4096 in
  let off = Int64.to_int (Int64.mul page_id (Int64.of_int page_size)) in
  let len = Cstruct.length out in
  let cur = Bytes.length d.buf in
  if off + len > cur
  then Lwt.return (Error "read past EOF")
  else (
    Cstruct.blit_from_bytes d.buf off out 0 len;
    Lwt.return (Ok ()))
;;

let write_page d ~page_id src =
  let page_size = 4096 in
  let off = Int64.to_int (Int64.mul page_id (Int64.of_int page_size)) in
  let len = Cstruct.length src in
  dev_grow d (off + len);
  Cstruct.blit_to_bytes src 0 d.buf off len;
  Lwt.return (Ok ())
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  let cur = Bytes.length d.buf in
  if off + len > cur
  then Lwt.return (Error "read past EOF")
  else (
    Cstruct.blit_from_bytes d.buf off out 0 len;
    Lwt.return (Ok ()))
;;

let write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  dev_grow d (off + len);
  Cstruct.blit_to_bytes src 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())

(* In-memory [History.sink]: append conses, load returns ascending order. *)
let mem_sink () : H.sink =
  let buf = ref [] in
  { H.append =
      (fun r ->
        buf := r :: !buf;
        Lwt.return_unit)
  ; load = (fun () -> Lwt.return (List.rev !buf))
  }
;;

(* Strictly-increasing monotonic wall clock (ms). *)
let monotonic () =
  let n = ref 0L in
  fun () ->
    n := Int64.add !n 1L;
    !n
;;

let open_wal_store ~history () =
  let wal_dev = mk_dev 262144 in
  let main_dev = mk_dev (1024 * 4096) in
  let main_n_pages = Int64.of_int (Bytes.length main_dev.buf / 4096) in
  let* sr =
    S.open_block_wal
      ~as_of_history:true
      ~history
      ~now:(monotonic ())
      ~read_page:(read_page main_dev)
      ~write_page:(write_page main_dev)
      ~sync:sync_ok
      ~resize:(fun ~n_pages:_ -> Lwt.return (Ok ()))
      ~n_pages:main_n_pages
      ~wal_read_at:(read_at wal_dev)
      ~wal_write_at:(write_at wal_dev)
      ~wal_sync:sync_ok
      ~wal_size_bytes:(dev_size wal_dev)
      ~close:(fun () -> Lwt.return_unit)
      ~wal_close:(fun () -> Lwt.return_unit)
      ()
  in
  match sr with
  | Ok s -> Lwt.return s
  | Error e -> failwith (Format.asprintf "open_block_wal: %a" S.pp_error e)
;;

let commit_put store k v =
  let* tx = S.rw_begin store in
  let* () = S.put tx 0 (bs k) (bs v) in
  S.commit tx
;;

(* The whole-DB WAL as-of guarantee: pin an older txn, read at it, then commit
   newer rows and CHECKPOINT — the pinned historical snapshot must still see
   only the rows committed up to that txn. *)
let test_wal_as_of_across_checkpoint () =
  run
    (let* store = open_wal_store ~history:(mem_sink ()) () in
     Lwt.finalize
       (fun () ->
          (* commit row A; capture its txn id (t1) from the history log *)
          let* () = commit_put store "A" "rowA" in
          let* log = S.history_log store in
          let t1 =
            match log with
            | r :: _ -> r.H.txn_id
            | [] -> failwith "history log empty after first commit"
          in
          (* commit row B *)
          let* () = commit_put store "B" "rowB" in
          (* pin the floor at t1 so A's pages stay retained *)
          S.history_pin store ~txn_id:t1;
          (* historical snapshot at t1: sees A, not B *)
          let* ro = S.ro_begin_as_of store (`Txn t1) in
          let* a = S.get ro 0 (bs "A") in
          let* b = S.get ro 0 (bs "B") in
          let* () = S.ro_end ro in
          assert (a = Some (bs "rowA"));
          assert (b = None);
          (* commit row C, then explicitly checkpoint newer frames into the
             main file *)
          let* () = commit_put store "C" "rowC" in
          let* () = S.checkpoint store in
          (* re-assert the as-of t1 read AFTER checkpoint: still A only *)
          let* ro2 = S.ro_begin_as_of store (`Txn t1) in
          let* a2 = S.get ro2 0 (bs "A") in
          let* b2 = S.get ro2 0 (bs "B") in
          let* c2 = S.get ro2 0 (bs "C") in
          let* () = S.ro_end ro2 in
          assert (a2 = Some (bs "rowA"));
          assert (b2 = None);
          assert (c2 = None);
          (* a live snapshot sees all three *)
          let* live = S.ro_begin store in
          let* la = S.get live 0 (bs "A") in
          let* lb = S.get live 0 (bs "B") in
          let* lc = S.get live 0 (bs "C") in
          let* () = S.ro_end live in
          assert (la = Some (bs "rowA"));
          assert (lb = Some (bs "rowB"));
          assert (lc = Some (bs "rowC"));
          Lwt.return_unit)
       (fun () -> S.close store))
;;

let () =
  test_wal_as_of_across_checkpoint ();
  print_endline "test_wal_as_of: OK"
;;
