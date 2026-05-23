(** Phase 38 / #149 — Rwlock primitive tests. *)

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

let test_writer_excludes_readers () =
  let lock = R.create () in
  let log = ref [] in
  let writer =
    R.with_write lock (fun () ->
      log := `Wstart :: !log;
      let* () = Lwt.pause () in
      log := `Wend :: !log;
      Lwt.return_unit)
  in
  let reader =
    R.with_read lock (fun () ->
      log := `R :: !log;
      Lwt.return_unit)
  in
  run (Lwt.join [writer; reader]);
  let log = List.rev !log in
  let rec ok inside = function
    | [] -> true
    | `Wstart :: rest -> ok true rest
    | `Wend :: rest -> ok false rest
    | `R :: rest -> if inside then false else ok inside rest
  in
  Alcotest.(check bool) "no reader inside writer" true (ok false log)

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

let test_writer_pending_blocks_new_readers () =
  let lock = R.create () in
  let order = ref [] in
  let r1_done, set_r1_done = Lwt.wait () in
  let r1 =
    R.with_read lock (fun () ->
      order := `R1 :: !order;
      r1_done)
  in
  Lwt_main.run (
    let* () = Lwt.pause () in
    let w =
      R.with_write lock (fun () ->
        order := `W :: !order; Lwt.return_unit)
    in
    let* () = Lwt.pause () in
    let r2 =
      R.with_read lock (fun () ->
        order := `R2 :: !order; Lwt.return_unit)
    in
    Lwt.wakeup_later set_r1_done ();
    let* () = Lwt.join [r1; w; r2] in
    let seen = List.rev !order in
    Alcotest.(check bool) "writer before new reader" true
      (match seen with [`R1; `W; `R2] -> true | _ -> false);
    Lwt.return_unit)

let () =
  Alcotest.run "rwlock" [
    "basic", [
      Alcotest.test_case "many readers concurrent" `Quick test_many_readers_concurrent;
      Alcotest.test_case "writer excludes readers"  `Quick test_writer_excludes_readers;
      Alcotest.test_case "writers serialised"       `Quick test_writers_serialised;
      Alcotest.test_case "writer pending blocks new readers"
        `Quick test_writer_pending_blocks_new_readers;
    ]
  ]
