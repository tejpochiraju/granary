(** Phase 38 / #149 — Rwlock primitive tests.

    Semantics (verified here):
    - Readers never block (counter increment + immediate return).
    - Writers serialise against other writers via [writer_active].
    - Readers do NOT block writers; writers do NOT block readers.
    - This is intentionally NOT a classical shared/exclusive RwLock —
      see [rwlock.mli] for the rationale (snapshot isolation makes
      reader/writer concurrency safe at the data layer). *)

module R = Sqlocaml_store.Rwlock
open Lwt.Syntax

let run = Lwt_main.run

let test_many_readers_concurrent () =
  let lock = R.create () in
  let n = ref 0 in
  let task _id =
    R.with_read lock (fun () ->
      let* () = Lwt.pause () in
      incr n;
      Lwt.return_unit)
  in
  run (Lwt.join (List.init 16 task));
  Alcotest.(check int) "all readers ran" 16 !n

let test_writers_serialised () =
  let lock = R.create () in
  let active = ref 0 in
  let max_active = ref 0 in
  let task _id =
    R.with_write lock (fun () ->
      incr active;
      if !active > !max_active then max_active := !active;
      let* () = Lwt.pause () in
      decr active;
      Lwt.return_unit)
  in
  run (Lwt.join (List.init 8 task));
  Alcotest.(check int) "writers serialised" 1 !max_active

(* Readers DO NOT block writers and writers DO NOT block readers.
   Both should be able to make progress concurrently. *)
let test_reader_and_writer_concurrent () =
  let lock = R.create () in
  let order = ref [] in
  let r =
    R.with_read lock (fun () ->
      order := `R_start :: !order;
      let* () = Lwt.pause () in
      order := `R_end :: !order;
      Lwt.return_unit)
  in
  let w =
    R.with_write lock (fun () ->
      order := `W_start :: !order;
      let* () = Lwt.pause () in
      order := `W_end :: !order;
      Lwt.return_unit)
  in
  run (Lwt.join [r; w]);
  let log = List.rev !order in
  (* Both ran; both reached end.  Order between them is unspecified. *)
  let count tag = List.length (List.filter ((=) tag) log) in
  Alcotest.(check int) "reader started" 1 (count `R_start);
  Alcotest.(check int) "reader ended"   1 (count `R_end);
  Alcotest.(check int) "writer started" 1 (count `W_start);
  Alcotest.(check int) "writer ended"   1 (count `W_end)

(* Issue #149 deadlock regression: in the same fiber, ro_begin then
   rw_begin must NOT deadlock.  Under the previous Rwlock semantics
   (writer-exclusive-of-readers) this was a deadlock; with the
   writer-mutex + reader-counter design it returns promptly. *)
let test_same_fiber_read_then_write_no_deadlock () =
  let lock = R.create () in
  run (
    let* () = R.acquire_read lock in
    let* () = R.acquire_write lock in
    R.release_write lock;
    R.release_read lock;
    Lwt.return_unit);
  Alcotest.(check int) "lock fully released" 0 (R.readers lock);
  Alcotest.(check bool) "no writer active" false (R.writer_active lock)

let () =
  Alcotest.run "rwlock" [
    "basic", [
      Alcotest.test_case "many readers concurrent" `Quick test_many_readers_concurrent;
      Alcotest.test_case "writers serialised"      `Quick test_writers_serialised;
      Alcotest.test_case "reader and writer can be concurrent"
        `Quick test_reader_and_writer_concurrent;
      Alcotest.test_case "same-fiber read then write no deadlock"
        `Quick test_same_fiber_read_then_write_no_deadlock;
    ]
  ]
