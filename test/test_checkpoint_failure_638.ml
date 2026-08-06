(** #638 — a failing autocheckpoint must not vanish.

    Before this, both auto paths ran the checkpoint under
    [Lwt.catch … (fun _ -> Lwt.return_unit)]: every failure — I/O error, header
    write failure, anything — was discarded whole.  The background path
    ([maybe_autockpt_after_commit]) was the worst of the two, because nobody
    awaits that fiber: a checkpoint that failed on every attempt left the WAL
    growing without bound with no counter, no event and no log, and the first
    symptom was a full disk or a very slow recovery.

    These tests inject a main-file write failure — in WAL mode the main file is
    written ONLY by a checkpoint, so commits keep succeeding while every
    checkpoint fails, which is exactly the shape of the reported condition — and
    assert that the failure is observable three ways: a
    [Store_event.Checkpoint_failed] event, the sticky {!Store.checkpoint_health}
    counters, and [PRAGMA checkpoint_status] at the SQL layer.

    They also pin the deliberate NON-behaviour: the commit that triggered the
    autocheckpoint still succeeds and its data is still readable.  #638 asks for
    visibility, not for a transient checkpoint failure to abort a user
    transaction. *)

open Lwt.Syntax
module S = Granary_store.Store
module Ev = Granary_store.Store_event
module Db = Granary.Db
module Row = Granary_encoding.Row

(* Bounded yield helper: poll [f], yielding between attempts, up to
   [max_pauses] times.  The autocheckpoint runs in an [Lwt.async] fiber, so
   every assertion about it has to wait for that fiber rather than assume a
   fixed number of ticks. *)
let rec wait_for f max_pauses =
  if max_pauses <= 0
  then Lwt.return_unit
  else if f ()
  then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    wait_for f (max_pauses - 1)
;;

(* The background checkpoint fiber emits its [Checkpoint_failed] event before
   its [Lwt.finalize] clears [autockpt_in_flight], so a test that reacts to the
   event and immediately commits again could be coalesced away.  Yield a few
   more times so the fiber has finished unwinding. *)
let rec yield_n n =
  if n <= 0
  then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    yield_n (n - 1)
;;

(* ------------------------------------------------------------------ *)
(* In-memory devices, with an arm-able failure on the MAIN file only.   *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev size = { buf = Bytes.make size '\x00' }

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (
    let nb = Bytes.make (max need (cur * 2)) '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb)
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  if off + len > Bytes.length d.buf
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

(* Open a WAL-mode store over in-memory devices.  [fail] arms the MAIN-file
   write path: while it is [true] every [write_page] fails, which fails a
   checkpoint at [Pager.flush_one_to_main] while leaving commits (which touch
   only the WAL device) completely unaffected. *)
let open_test_store ~(fail : bool ref) () =
  let main_dev = mk_dev (1024 * 4096) in
  let wal_dev = mk_dev 65536 in
  let main_n_pages = Int64.of_int (Bytes.length main_dev.buf / 4096) in
  let read_page ~page_id buf =
    let off = Int64.to_int (Int64.mul page_id 4096L) in
    let len = Cstruct.length buf in
    if off + len > Bytes.length main_dev.buf
    then Lwt.return (Error "read past EOF")
    else (
      Cstruct.blit_from_bytes main_dev.buf off buf 0 len;
      Lwt.return (Ok ()))
  in
  let write_page ~page_id buf =
    if !fail
    then Lwt.return (Error "injected main-write failure")
    else (
      let off = Int64.to_int (Int64.mul page_id 4096L) in
      let len = Cstruct.length buf in
      dev_grow main_dev (off + len);
      Cstruct.blit_to_bytes buf 0 main_dev.buf off len;
      Lwt.return (Ok ()))
  in
  let resize ~n_pages =
    dev_grow main_dev (Int64.to_int n_pages * 4096);
    Lwt.return (Ok ())
  in
  S.open_block_wal
    ~read_page
    ~write_page
    ~sync:sync_ok
    ~resize
    ~n_pages:main_n_pages
    ~wal_read_at:(read_at wal_dev)
    ~wal_write_at:(write_at wal_dev)
    ~wal_sync:sync_ok
    ~wal_size_bytes:(Int64.of_int (Bytes.length wal_dev.buf))
    ~close:(fun () -> Lwt.return_unit)
    ~wal_close:(fun () -> Lwt.return_unit)
    ()
;;

let open_or_fail ~fail () =
  let* sr = open_test_store ~fail () in
  match sr with
  | Ok s -> Lwt.return s
  | Error e -> Alcotest.failf "open_block_wal: %a" S.pp_error e
;;

let bs = Bytes.of_string

(* Record every [Checkpoint_failed] the store emits. *)
let recorder st =
  let seen = ref [] in
  S.set_event_callback
    st
    (Some
       (fun ev ->
         match ev with
         | Ev.Checkpoint_failed { target_frames; consecutive; message } ->
           seen := (target_frames, consecutive, message) :: !seen
         | _ -> ()));
  seen
;;

(* One committed write, then wait for the background autocheckpoint the commit
   dispatched to have run (successfully or not). *)
let commit_one st ~k ~v =
  let* txn = S.rw_begin st in
  let* () = S.put txn 16 (bs k) (bs v) in
  S.commit txn
;;

(* ------------------------------------------------------------------ *)
(* 1. The background autocheckpoint failure becomes observable.         *)
(* ------------------------------------------------------------------ *)

let test_autocheckpoint_failure_is_observable () =
  Lwt_main.run
    (let fail = ref false in
     let* st = open_or_fail ~fail () in
     let seen = recorder st in
     (* Threshold 1: every commit crosses it, so every commit dispatches a
        checkpoint. *)
     S.set_wal_autocheckpoint st 1;
     fail := true;
     let* () = commit_one st ~k:"k1" ~v:"v1" in
     let* () = wait_for (fun () -> !seen <> []) 500 in
     let* () = yield_n 5 in
     (* The event fired at all — this is the whole point of the issue. *)
     Alcotest.(check bool) "a Checkpoint_failed event was emitted" true (!seen <> []);
     (match !seen with
      | (target, consecutive, message) :: _ ->
        Alcotest.(check bool) "target frames > 0" true (target > 0);
        Alcotest.(check int) "first failure is consecutive=1" 1 consecutive;
        Alcotest.(check bool) "message is non-empty" true (String.length message > 0)
      | [] -> ());
     (* And it is sticky on the store, readable long after the commit. *)
     let h = S.checkpoint_health st in
     Alcotest.(check int) "total failures" 1 h.S.total_failures;
     Alcotest.(check int) "consecutive failures" 1 h.S.consecutive_failures;
     Alcotest.(check bool) "last_error recorded" true (h.S.last_error <> None);
     (* The commit itself was NOT failed by the checkpoint failure, and its
        data is still there: the frames are valid WAL frames, they just were
        not migrated. *)
     let* got = S.with_ro st (fun tx -> S.get tx 16 (bs "k1")) in
     Alcotest.(check (option string))
       "commit survived"
       (Some "v1")
       (Option.map Bytes.to_string got);
     fail := false;
     S.close st)
;;

(* ------------------------------------------------------------------ *)
(* 2. Repeated failures accumulate instead of each one vanishing.       *)
(* ------------------------------------------------------------------ *)

let test_repeated_failures_accumulate () =
  Lwt_main.run
    (let fail = ref false in
     let* st = open_or_fail ~fail () in
     let seen = recorder st in
     S.set_wal_autocheckpoint st 1;
     fail := true;
     let rec drive i =
       if i > 3
       then Lwt.return_unit
       else (
         let before = List.length !seen in
         let* () = commit_one st ~k:(Printf.sprintf "k%d" i) ~v:"v" in
         let* () = wait_for (fun () -> List.length !seen > before) 500 in
         let* () = yield_n 5 in
         drive (i + 1))
     in
     let* () = drive 1 in
     let h = S.checkpoint_health st in
     let n = List.length !seen in
     Alcotest.(check bool) "every attempt failed and was reported" true (n >= 3);
     Alcotest.(check int) "one count per event" n h.S.total_failures;
     Alcotest.(check int) "none of them was cleared" n h.S.consecutive_failures;
     (* The event carries the running consecutive count, so a consumer that
        keeps only the latest event still sees the escalation. *)
     (match !seen with
      | (_, consecutive, _) :: _ ->
        Alcotest.(check int) "latest event reports the running count" n consecutive
      | [] -> Alcotest.fail "expected events");
     fail := false;
     S.close st)
;;

(* ------------------------------------------------------------------ *)
(* 3. A checkpoint that completes clears the "WAL is growing" signal —  *)
(*    but not the since-open total.                                     *)
(* ------------------------------------------------------------------ *)

let test_success_clears_consecutive_not_total () =
  Lwt_main.run
    (let fail = ref false in
     let* st = open_or_fail ~fail () in
     let seen = recorder st in
     S.set_wal_autocheckpoint st 1;
     fail := true;
     let* () = commit_one st ~k:"k1" ~v:"v1" in
     let* () = wait_for (fun () -> !seen <> []) 500 in
     let* () = yield_n 5 in
     Alcotest.(check int) "failed once" 1 (S.checkpoint_health st).S.consecutive_failures;
     (* Heal the device and checkpoint explicitly. *)
     fail := false;
     let* () = S.checkpoint st in
     let h = S.checkpoint_health st in
     Alcotest.(check int) "consecutive cleared" 0 h.S.consecutive_failures;
     Alcotest.(check bool) "last_error cleared" true (h.S.last_error = None);
     Alcotest.(check int) "total is NOT cleared" 1 h.S.total_failures;
     S.close st)
;;

(* ------------------------------------------------------------------ *)
(* 4. The manual path still raises — and now also records.              *)
(* ------------------------------------------------------------------ *)

let test_manual_checkpoint_raises_and_records () =
  Lwt_main.run
    (let fail = ref false in
     let* st = open_or_fail ~fail () in
     (* Disable the auto path so the only checkpoint is the explicit one. *)
     S.set_wal_autocheckpoint st 0;
     let* () = commit_one st ~k:"k1" ~v:"v1" in
     fail := true;
     let* raised =
       Lwt.catch
         (fun () ->
            let* () = S.checkpoint st in
            Lwt.return false)
         (fun _ -> Lwt.return true)
     in
     Alcotest.(check bool) "explicit checkpoint still raises" true raised;
     let h = S.checkpoint_health st in
     Alcotest.(check int) "and is counted" 1 h.S.total_failures;
     Alcotest.(check bool) "and recorded" true (h.S.last_error <> None);
     fail := false;
     S.close st)
;;

(* ------------------------------------------------------------------ *)
(* 5. An operator can acknowledge the condition.                        *)
(* ------------------------------------------------------------------ *)

let test_clear_checkpoint_error () =
  Lwt_main.run
    (let fail = ref false in
     let* st = open_or_fail ~fail () in
     let seen = recorder st in
     S.set_wal_autocheckpoint st 1;
     fail := true;
     let* () = commit_one st ~k:"k1" ~v:"v1" in
     let* () = wait_for (fun () -> !seen <> []) 500 in
     let* () = yield_n 5 in
     S.clear_checkpoint_error st;
     let h = S.checkpoint_health st in
     Alcotest.(check int) "consecutive cleared" 0 h.S.consecutive_failures;
     Alcotest.(check bool) "last_error cleared" true (h.S.last_error = None);
     Alcotest.(check int) "total survives the acknowledgement" 1 h.S.total_failures;
     fail := false;
     S.close st)
;;

(* ------------------------------------------------------------------ *)
(* 6. PRAGMA checkpoint_status surfaces the same state to SQL.          *)
(* ------------------------------------------------------------------ *)

let read_status db =
  let* r = Db.query db "PRAGMA checkpoint_status" in
  match r with
  | Error e -> Alcotest.failf "PRAGMA checkpoint_status: %a" Db.pp_error e
  | Ok stream ->
    let* rows = Lwt_stream.to_list stream in
    (match rows with
     | [ row ] when Array.length row = 3 ->
       let int_at i =
         match row.(i) with
         | Row.V_int n -> Int64.to_int n
         | _ -> Alcotest.failf "column %d: expected an integer" i
       in
       let last =
         match row.(2) with
         | Row.V_null -> None
         | Row.V_text s -> Some s
         | _ -> Alcotest.fail "column 2: expected text or NULL"
       in
       Lwt.return (int_at 0, int_at 1, last)
     | _ -> Alcotest.failf "expected one 3-column row, got %d rows" (List.length rows))
;;

let test_pragma_checkpoint_status () =
  Lwt_main.run
    (let fail = ref false in
     let* st = open_or_fail ~fail () in
     let* db = Db.of_store st in
     let exec sql =
       let* r = Db.execute db sql in
       match r with
       | Ok () -> Lwt.return_unit
       | Error e -> Alcotest.failf "execute(%s): %a" sql Db.pp_error e
     in
     let* () = exec "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
     (* Nothing has failed yet: the all-clear reads as (0, 0, NULL). *)
     let* total0, consecutive0, last0 = read_status db in
     Alcotest.(check int) "no failures yet" 0 total0;
     Alcotest.(check int) "no consecutive failures yet" 0 consecutive0;
     Alcotest.(check bool) "no last error yet" true (last0 = None);
     let seen = recorder st in
     let* () = exec "PRAGMA wal_autocheckpoint = 1" in
     fail := true;
     let* () = exec "INSERT INTO t (id, v) VALUES (1, 'a')" in
     let* () = wait_for (fun () -> !seen <> []) 500 in
     let* () = yield_n 5 in
     let* total, consecutive, last = read_status db in
     Alcotest.(check bool) "total advanced" true (total > 0);
     Alcotest.(check bool) "consecutive > 0" true (consecutive > 0);
     Alcotest.(check bool) "last error text present" true (last <> None);
     (* The INSERT was not failed by the checkpoint failure. *)
     let* r = Db.query db "SELECT v FROM t WHERE id = 1" in
     let* rows =
       match r with
       | Ok s -> Lwt_stream.to_list s
       | Error e -> Alcotest.failf "select: %a" Db.pp_error e
     in
     Alcotest.(check int) "row survived" 1 (List.length rows);
     fail := false;
     Db.close db)
;;

let () =
  Alcotest.run
    "checkpoint_failure_638"
    [ ( "surfacing"
      , [ Alcotest.test_case
            "#638 autocheckpoint failure emits an event and sticks"
            `Quick
            test_autocheckpoint_failure_is_observable
        ; Alcotest.test_case
            "#638 repeated failures accumulate"
            `Quick
            test_repeated_failures_accumulate
        ; Alcotest.test_case
            "#638 success clears consecutive, not total"
            `Quick
            test_success_clears_consecutive_not_total
        ; Alcotest.test_case
            "#638 manual checkpoint raises and records"
            `Quick
            test_manual_checkpoint_raises_and_records
        ; Alcotest.test_case
            "#638 clear_checkpoint_error acknowledges"
            `Quick
            test_clear_checkpoint_error
        ; Alcotest.test_case
            "#638 PRAGMA checkpoint_status"
            `Quick
            test_pragma_checkpoint_status
        ] )
    ]
;;
