(** #718: the writer lock's wait/hold accounting.

    #716 measured TPC-C {e service time} and concluded that transaction control
    is 75.3% of a NewOrder.  That much is arithmetic over what the profiler
    measures and stands.  What did not follow — and what #718 exists to make
    measurable — is any statement about the {e critical section}: [Store]
    releases the writer lock partway through [COMMIT], before the fsync, and a
    [BEGIN] that finds the lock held is waiting rather than working.  Service
    time cannot tell those apart from work.

    Two halves, and they fail differently:

    - The {b accumulator} ([Granary_store.Lock_stats]) is pure and is tested
      with a hand-stepped clock, so its arithmetic is exact rather than
      approximately right.
    - The {b funnel} — every [Rwlock.acquire_write]/[release_write] in [Store]
      going through [acquire_writer]/[release_writer] — is what makes the
      accumulator describe the real lock.  It is tested through a real WAL store
      on disk, and the assertion that matters is
      [unattributed_waits = 0 && unbalanced_releases = 0]: those two counters
      exist precisely to catch a future acquisition that bypasses the funnel,
      and a test that only checked the totals would not notice one.

    The site attribution cases must run on a {b file-backed WAL} store.  The Mem
    backend has no WAL, so [Store.checkpoint] returns before it acquires
    anything and no autocheckpoint is ever dispatched — an in-memory version of
    those two would pass while measuring nothing. *)

open Lwt.Syntax
module LS = Granary_store.Lock_stats
module Store = Granary_store.Store
module Ustore = Granary_unix.Store

let () = Granary_unix.install ()
let run = Lwt_main.run

(* Sibling worktrees run suites concurrently, so every path carries the pid —
   the convention in test_rowid_counter_ownership_632.ml and test_attach.ml. *)
let tmp_path name =
  Printf.sprintf "/tmp/granary_lock_stats_718_%s_%d.db" name (Unix.getpid ())
;;

let with_tmp_path name f =
  let path = tmp_path name in
  let cleanup () =
    List.iter
      (fun p ->
         try Unix.unlink p with
         | _ -> ())
      [ path; path ^ "-wal"; path ^ ".aslog" ]
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () -> f path)
;;

let stat_of (r : LS.report) site =
  match List.assoc_opt site r.LS.sites with
  | Some s -> s
  | None -> Alcotest.failf "report has no row for site %s" (LS.site_name site)
;;

let cell_of (r : LS.report) ~waiter ~holder =
  List.find_opt
    (fun (b : LS.blocked_by) -> b.LS.waiter = waiter && b.LS.holder = holder)
    r.LS.blocked_by
;;

(* ── the accumulator, with a clock we step by hand ───────────────────────── *)

(* A settable clock, so a hold of "exactly 5 seconds" is exactly that and the
   assertions below are equalities rather than inequalities. *)
let stepped () =
  let t = ref 0.0 in
  t, fun () -> !t
;;

let f = Alcotest.(check (float 1e-9))
let i = Alcotest.(check int)

(* Two one-argument checks rather than one [check bool msg expected got]: the
   latter takes two booleans in a row, which is exactly the shape that reads
   correctly and asserts the opposite when the arguments are transposed. *)
let is_true msg got = Alcotest.(check bool) msg true got
let is_false msg got = Alcotest.(check bool) msg false got

let fresh_report_is_empty_and_says_it_has_no_clock () =
  let r = LS.report (LS.create ()) in
  is_false "no clock installed" r.LS.clock_installed;
  i "one row per site" (List.length LS.all_sites) (List.length r.LS.sites);
  List.iter
    (fun (site, (s : LS.site_stat)) ->
       i (LS.site_name site ^ ": no acquisitions") 0 s.LS.acquisitions;
       f (LS.site_name site ^ ": no hold") 0.0 s.LS.hold_s)
    r.LS.sites;
  is_true "nothing holds it" (r.LS.held = None);
  i "no contention" 0 (List.length r.LS.blocked_by);
  i "no bypassed acquisition" 0 r.LS.unattributed_waits;
  i "no unbalanced release" 0 r.LS.unbalanced_releases
;;

(* Without a clock the DURATIONS are 0 but the COUNTS are still exact.  That
   asymmetry is the whole reason [clock_installed] is in the report: a
   pure-Mirage build measures contention truthfully and durations not at all,
   and a reader must be able to tell that from "nothing ever waited". *)
let counts_are_exact_without_a_clock () =
  let t = LS.create () in
  LS.note_acquired t LS.Txn ~waited:0.0 ~contended:false ~blocked_by:None;
  LS.note_released t ~at:0.0;
  LS.note_acquired
    t
    LS.Txn
    ~waited:0.0
    ~contended:true
    ~blocked_by:(Some LS.Autocheckpoint);
  LS.note_released t ~at:0.0;
  let r = LS.report t in
  let s = stat_of r LS.Txn in
  is_false "still reports no clock" r.LS.clock_installed;
  i "both acquisitions counted" 2 s.LS.acquisitions;
  i "the contended one counted" 1 s.LS.contended;
  f "no duration to report" 0.0 s.LS.hold_s;
  match cell_of r ~waiter:LS.Txn ~holder:LS.Autocheckpoint with
  | None -> Alcotest.fail "the contention cell is missing"
  | Some c -> i "attributed to the autocheckpoint" 1 c.LS.count
;;

let durations_come_from_the_installed_clock () =
  let now, clock = stepped () in
  let t = LS.create () in
  LS.set_clock t clock;
  now := 10.0;
  LS.note_acquired t LS.Txn ~waited:2.0 ~contended:false ~blocked_by:None;
  now := 17.0;
  LS.note_released t ~at:(LS.now t);
  let r = LS.report t in
  let s = stat_of r LS.Txn in
  is_true "clock installed" r.LS.clock_installed;
  f "held for 7" 7.0 s.LS.hold_s;
  f "and that is the longest" 7.0 s.LS.hold_max_s;
  f "waited 2" 2.0 s.LS.wait_s;
  f "and that is the longest" 2.0 s.LS.wait_max_s
;;

(* The sequence a real contended acquisition produces: A holds; B samples
   [contended]/[blocked_by] and then blocks; A releases; B's [note_acquired]
   runs with the values it sampled, at a point where nothing holds the lock any
   more.  The attribution must survive that gap — it is the ordinary case, not
   an edge case. *)
let a_wait_is_attributed_to_the_holder_at_its_start () =
  let now, clock = stepped () in
  let t = LS.create () in
  LS.set_clock t clock;
  LS.note_acquired t LS.Autocheckpoint ~waited:0.0 ~contended:false ~blocked_by:None;
  (* B arrives while the autocheckpoint holds it, and samples that fact. *)
  let b_contended = LS.held t <> None in
  let b_blocked_by = LS.held t in
  now := 4.0;
  LS.note_released t ~at:(LS.now t);
  now := 4.5;
  LS.note_acquired t LS.Txn ~waited:4.5 ~contended:b_contended ~blocked_by:b_blocked_by;
  let r = LS.report t in
  i "the wait was counted as contended" 1 (stat_of r LS.Txn).LS.contended;
  match cell_of r ~waiter:LS.Txn ~holder:LS.Autocheckpoint with
  | None -> Alcotest.fail "the txn-behind-autocheckpoint cell is missing"
  | Some c ->
    i "once" 1 c.LS.count;
    f "for the whole wait" 4.5 c.LS.wait_s
;;

(* [site_index] is a hand-written dense index into two arrays sized by
   [n_sites].  A constructor added to [site] without widening them would raise
   Index_out_of_bounds on the first acquisition from the new site — at run time,
   in whichever workload reached it first.  Exercising every site here turns
   that into a compile-and-test failure instead. *)
let every_site_is_indexable () =
  let t = LS.create () in
  List.iter
    (fun site ->
       LS.note_acquired t site ~waited:0.0 ~contended:false ~blocked_by:None;
       LS.note_released t ~at:0.0)
    LS.all_sites;
  let r = LS.report t in
  List.iter
    (fun site -> i (LS.site_name site) 1 (stat_of r site).LS.acquisitions)
    LS.all_sites;
  i "every site has a distinct name" (List.length LS.all_sites)
  @@ List.length (List.sort_uniq String.compare (List.map LS.site_name LS.all_sites))
;;

let reset_keeps_an_outstanding_hold_and_restamps_it () =
  let now, clock = stepped () in
  let t = LS.create () in
  LS.set_clock t clock;
  LS.note_acquired t LS.Txn ~waited:1.0 ~contended:false ~blocked_by:None;
  now := 10.0;
  LS.reset t;
  now := 15.0;
  LS.note_released t ~at:(LS.now t);
  let r = LS.report t in
  let s = stat_of r LS.Txn in
  f "only the post-reset 5 s is charged" 5.0 s.LS.hold_s;
  f "the pre-reset wait is gone" 0.0 s.LS.wait_s;
  i "the release still balanced" 0 r.LS.unbalanced_releases;
  i "the acquisition itself was discarded" 0 s.LS.acquisitions
;;

let an_unbalanced_release_is_counted_not_charged () =
  let t = LS.create () in
  LS.set_clock t (fun () -> 99.0);
  LS.note_released t ~at:99.0;
  let r = LS.report t in
  i "counted" 1 r.LS.unbalanced_releases;
  List.iter
    (fun (site, (s : LS.site_stat)) ->
       f (LS.site_name site ^ ": charged nothing") 0.0 s.LS.hold_s)
    r.LS.sites
;;

let a_contended_wait_with_no_recorded_holder_is_counted_not_guessed () =
  let t = LS.create () in
  LS.note_acquired t LS.Txn ~waited:3.0 ~contended:true ~blocked_by:None;
  let r = LS.report t in
  i "the contention is still counted" 1 (stat_of r LS.Txn).LS.contended;
  i "and flagged as unattributable" 1 r.LS.unattributed_waits;
  i "no holder was invented" 0 (List.length r.LS.blocked_by)
;;

(* A clock stepped backwards — NTP, or a caller passing a clock that is not
   monotonic — must not subtract from the total.  A negative hold reads as a
   measurement bug in whatever consumes the report, which is the wrong place to
   discover a clock problem. *)
let a_backwards_clock_cannot_produce_a_negative_hold () =
  let t = LS.create () in
  LS.set_clock t (fun () -> 100.0);
  LS.note_acquired t LS.Txn ~waited:0.0 ~contended:false ~blocked_by:None;
  LS.note_released t ~at:40.0;
  f "clamped at zero" 0.0 (stat_of (LS.report t) LS.Txn).LS.hold_s
;;

let pp_report_discloses_a_missing_clock () =
  let rendered r = Format.asprintf "%a" LS.pp_report r in
  let without = rendered (LS.report (LS.create ())) in
  let t = LS.create () in
  LS.set_clock t (fun () -> 0.0);
  let with_ = rendered (LS.report t) in
  let contains hay needle =
    let n = String.length needle in
    let rec go i =
      i + n <= String.length hay && (String.sub hay i n = needle || go (i + 1))
    in
    go 0
  in
  is_true "says so when there is no clock" (contains without "NO CLOCK INSTALLED");
  is_false "and does not when there is" (contains with_ "NO CLOCK INSTALLED")
;;

let pp_names_the_accumulator () =
  let t = LS.create () in
  LS.note_acquired t LS.Checkpoint ~waited:0.0 ~contended:false ~blocked_by:None;
  let s = Format.asprintf "%a" LS.pp t in
  is_true "mentions the live holder" (String.length s > 0 && s <> "");
  Alcotest.(check string)
    "one-line summary"
    "Lock_stats.t { clock = none; held = checkpoint; acquisitions = 1 }"
    s
;;

(* ── the funnel, through a real WAL store on disk ────────────────────────── *)

let open_wal path =
  match run (Ustore.open_file_wal ~path ()) with
  | Ok st ->
    Store.set_clock st Unix.gettimeofday;
    st
  | Error _ -> Alcotest.failf "open_file_wal %S failed" path
;;

let txn st f_ =
  run
    (let* tx = Store.rw_begin st in
     let* () = f_ tx in
     Store.commit tx)
;;

(* The invariant the whole design rests on: every acquisition and release of the
   writer lock inside [Store] goes through the accounting.  Both counters are
   bug signals, not measurements — a future [Rwlock.acquire_write t.lock] added
   directly would show up here and nowhere else, because the totals would still
   look plausible. *)
let every_acquisition_and_release_is_accounted () =
  with_tmp_path "funnel" (fun path ->
    let st = open_wal path in
    let tid = 1 in
    for n = 1 to 20 do
      txn st (fun tx ->
        Store.put tx tid (Bytes.of_string (string_of_int n)) (Bytes.of_string "v"))
    done;
    (* A rollback releases through a different path than a commit does; an
       explicit checkpoint acquires through a third. *)
    run
      (let* tx = Store.rw_begin st in
       let* () = Store.put tx tid (Bytes.of_string "doomed") (Bytes.of_string "v") in
       Store.rollback tx);
    run (Store.checkpoint st);
    let r = Store.lock_stats st in
    i "no acquisition bypassed the accounting" 0 r.LS.unattributed_waits;
    i "no release bypassed it either" 0 r.LS.unbalanced_releases;
    is_true "and nothing is left holding the lock" (r.LS.held = None);
    i
      "every commit and the rollback are one Txn acquisition each"
      21
      (stat_of r LS.Txn).LS.acquisitions;
    i
      "the explicit checkpoint is its own site"
      1
      (stat_of r LS.Checkpoint).LS.acquisitions;
    is_true "durations are real, not the no-clock zeroes" r.LS.clock_installed;
    is_true
      "the transactions held the lock for a measurable time"
      ((stat_of r LS.Txn).LS.hold_s > 0.0);
    run (Store.close st))
;;

(* The measurement #716 could not make.  Two fibers, one store: the second
   fiber's [rw_begin] finds the lock held and waits, and that wait is charged to
   the first fiber's transaction rather than disappearing into the second's
   service time.

   The interleaving is FORCED, and it has to be.  An earlier revision of this
   test relied on [Lwt.join [ holder (); waiter () ]] starting [holder] first,
   on the reasoning that a list literal is evaluated left to right.  That is
   false: OCaml does not specify the order, and both compilers evaluate the
   arguments of [::] right to left — [[a; b]] is [a :: (b :: [])], so [b] is
   invoked first.  (Checked: [let f n = print_string n; n] with
   [ignore [ f "A"; f "B" ]] prints [BA].)

   So the old spelling invoked [waiter] first, and the "yield repeatedly while
   holding" pauses below were in the fiber that actually started SECOND.  The
   assertions still passed, but only because [waiter]'s [Store.commit] happens
   to yield inside [commit_prepare_btree] before it reaches [unlock_once],
   letting [holder] be invoked while the lock was still held — incidental, and
   if that path ever stops yielding the test reads [contended = 0] and fails
   with a message pointing at the accounting rather than at the schedule.

   Binding the two promises in separate [let]s in the order we want removes the
   dependency entirely. *)
let a_second_fibers_begin_waits_behind_the_first () =
  with_tmp_path "contend" (fun path ->
    let st = open_wal path in
    let tid = 1 in
    Store.reset_lock_stats st;
    let holder () =
      let* tx = Store.rw_begin st in
      let* () = Store.put tx tid (Bytes.of_string "a") (Bytes.of_string "1") in
      (* Yield repeatedly while holding, so the waiter is certainly parked. *)
      let* () = Lwt_list.iter_s (fun _ -> Lwt.pause ()) [ 1; 2; 3; 4; 5 ] in
      Store.commit tx
    in
    let waiter () =
      let* tx = Store.rw_begin st in
      let* () = Store.put tx tid (Bytes.of_string "b") (Bytes.of_string "2") in
      Store.commit tx
    in
    (* [holder] first, explicitly: see the header.  Do not collapse these back
       into the [Lwt.join] literal — that reverses them. *)
    let h = holder () in
    let w = waiter () in
    run (Lwt.join [ h; w ]);
    let r = Store.lock_stats st in
    let s = stat_of r LS.Txn in
    i "two transactions" 2 s.LS.acquisitions;
    i "exactly one of them queued" 1 s.LS.contended;
    is_true "the wait is not zero" (s.LS.wait_s > 0.0);
    i "and it is attributed, not dropped" 0 r.LS.unattributed_waits;
    (match cell_of r ~waiter:LS.Txn ~holder:LS.Txn with
     | None -> Alcotest.fail "the txn-behind-txn cell is missing"
     | Some c -> i "once" 1 c.LS.count);
    run (Store.close st))
;;

(* #716 named the background autocheckpoint as the candidate for [BEGIN]'s
   3.228 ms and could not confirm it, because nothing could see the lock.  This
   pins the mechanism the confirmation needs: the fiber [Lwt.async]-dispatched
   after a commit takes the writer lock, and it is charged to its own site
   rather than to whichever transaction happens to be running.

   The threshold is 1 frame, so the very next commit dispatches one; the pauses
   afterwards are what let that unawaited fiber run to completion. *)
let the_background_autocheckpoint_is_charged_to_its_own_site () =
  with_tmp_path "autockpt" (fun path ->
    let st = open_wal path in
    let tid = 1 in
    Store.set_wal_autocheckpoint st 1;
    for n = 1 to 5 do
      txn st (fun tx ->
        Store.put tx tid (Bytes.of_string (string_of_int n)) (Bytes.of_string "v"));
      run (Lwt_list.iter_s (fun _ -> Lwt.pause ()) [ 1; 2; 3; 4; 5 ])
    done;
    (* The last commit dispatches one more autocheckpoint, and that fiber is
       unawaited — so the lock can genuinely still be held when the commit's
       caller has returned.  That is not a defect, it is the mechanism #716
       named as its candidate, observed directly for the first time; the report
       is only stable once the fiber drains.  Bounded, so a checkpoint that
       never releases fails here rather than hanging the suite.

       #719: "nobody holds the lock" is no longer sufficient to conclude the
       fiber has run.  Since the migration moved OUT of the critical section, a
       dispatched checkpoint spends most of its life holding nothing at all, so
       the very first sample would see [held = None] and prove nothing.  Drain
       until an [Autocheckpoint] acquisition has actually been recorded AND the
       lock is free again. *)
    let rec drain n =
      let r = Store.lock_stats st in
      if n = 0 || ((stat_of r LS.Autocheckpoint).LS.acquisitions > 0 && r.LS.held = None)
      then n
      else (
        run (Lwt.pause ());
        drain (n - 1))
    in
    let left = drain 500 in
    is_true "the background checkpoint eventually released the lock" (left > 0);
    let r = Store.lock_stats st in
    is_true
      "the background checkpoint took the lock"
      ((stat_of r LS.Autocheckpoint).LS.acquisitions > 0);
    is_true
      "and held it for a measurable time"
      ((stat_of r LS.Autocheckpoint).LS.hold_s > 0.0);
    i "no acquisition bypassed the accounting" 0 r.LS.unattributed_waits;
    i "no release bypassed it either" 0 r.LS.unbalanced_releases;
    is_true "nothing is left holding the lock" (r.LS.held = None);
    run (Store.close st))
;;

(* A store opened with no clock still counts.  This is the pure-Mirage shape,
   and the point is that the report says which kind of zero it is showing. *)
let a_store_without_a_clock_still_counts_acquisitions () =
  with_tmp_path "noclock" (fun path ->
    let st =
      match run (Ustore.open_file_wal ~path ()) with
      | Ok st -> st
      | Error _ -> Alcotest.failf "open_file_wal %S failed" path
    in
    txn st (fun tx -> Store.put tx 1 (Bytes.of_string "k") (Bytes.of_string "v"));
    let r = Store.lock_stats st in
    is_false "discloses the missing clock" r.LS.clock_installed;
    i "the acquisition is counted anyway" 1 (stat_of r LS.Txn).LS.acquisitions;
    f "with no duration to report" 0.0 (stat_of r LS.Txn).LS.hold_s;
    run (Store.close st))
;;

let reset_clears_a_stores_accounting () =
  with_tmp_path "reset" (fun path ->
    let st = open_wal path in
    txn st (fun tx -> Store.put tx 1 (Bytes.of_string "k") (Bytes.of_string "v"));
    is_true
      "something was recorded"
      ((stat_of (Store.lock_stats st) LS.Txn).LS.acquisitions > 0);
    Store.reset_lock_stats st;
    let r = Store.lock_stats st in
    i "cleared" 0 (stat_of r LS.Txn).LS.acquisitions;
    is_true "the clock survives the reset" r.LS.clock_installed;
    run (Store.close st))
;;

let () =
  Alcotest.run
    "test_lock_stats_718"
    [ ( "accumulator"
      , [ Alcotest.test_case
            "fresh_report_is_empty_and_says_it_has_no_clock"
            `Quick
            fresh_report_is_empty_and_says_it_has_no_clock
        ; Alcotest.test_case
            "counts_are_exact_without_a_clock"
            `Quick
            counts_are_exact_without_a_clock
        ; Alcotest.test_case
            "durations_come_from_the_installed_clock"
            `Quick
            durations_come_from_the_installed_clock
        ; Alcotest.test_case
            "a_wait_is_attributed_to_the_holder_at_its_start"
            `Quick
            a_wait_is_attributed_to_the_holder_at_its_start
        ; Alcotest.test_case "every_site_is_indexable" `Quick every_site_is_indexable
        ; Alcotest.test_case
            "reset_keeps_an_outstanding_hold_and_restamps_it"
            `Quick
            reset_keeps_an_outstanding_hold_and_restamps_it
        ; Alcotest.test_case
            "an_unbalanced_release_is_counted_not_charged"
            `Quick
            an_unbalanced_release_is_counted_not_charged
        ; Alcotest.test_case
            "a_contended_wait_with_no_recorded_holder_is_counted_not_guessed"
            `Quick
            a_contended_wait_with_no_recorded_holder_is_counted_not_guessed
        ; Alcotest.test_case
            "a_backwards_clock_cannot_produce_a_negative_hold"
            `Quick
            a_backwards_clock_cannot_produce_a_negative_hold
        ; Alcotest.test_case
            "pp_report_discloses_a_missing_clock"
            `Quick
            pp_report_discloses_a_missing_clock
        ; Alcotest.test_case "pp_names_the_accumulator" `Quick pp_names_the_accumulator
        ] )
    ; ( "funnel"
      , [ Alcotest.test_case
            "every_acquisition_and_release_is_accounted"
            `Quick
            every_acquisition_and_release_is_accounted
        ; Alcotest.test_case
            "a_second_fibers_begin_waits_behind_the_first"
            `Quick
            a_second_fibers_begin_waits_behind_the_first
        ; Alcotest.test_case
            "the_background_autocheckpoint_is_charged_to_its_own_site"
            `Quick
            the_background_autocheckpoint_is_charged_to_its_own_site
        ; Alcotest.test_case
            "a_store_without_a_clock_still_counts_acquisitions"
            `Quick
            a_store_without_a_clock_still_counts_acquisitions
        ; Alcotest.test_case
            "reset_clears_a_stores_accounting"
            `Quick
            reset_clears_a_stores_accounting
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
