(** Negative control — deliberately broken list-append history.

    This harness creates a history that violates snapshot isolation by
    injecting a dirty read: one transaction writes a value, then a second
    concurrent transaction observes it before the first commits.

    The point: Elle's list-append checker MUST detect this as an anomaly
    (G1a at minimum).  If it doesn't, the checker is not wired correctly
    or the history format is wrong.  RED before GREEN.

    Unlike the real harness, this doesn't use sqlocaml at all — it directly
    constructs an EDN history file with a known-bad interleaving. *)

open Edn_history

(** Build a negative-control history: two concurrent transactions where T1
    writes v=1 under key=0, T2 reads v=1 from key=0 before T1 commits.

    Elle's checker should flag G1a (dirty read / aborted read) or at minimum
    G1c (intermediate read). *)

let build_negative_history () =
  let t0 = 0L in
  let _t1 = 100_000L in    (* T1 begin *)
  let t2 = 200_000L in    (* T2 begin — T1 still active *)
  let t3 = 300_000L in    (* T2 reads key 0, sees v=1 — DIRTY! *)
  let t4 = 400_000L in    (* T1 commits *)
  let _t5 = 500_000L in    (* T2 reads key 0 again — sees v=1 *)

  (* T1: append v=1 to key=0 *)
  let txn1 = [Append (0, 1); Read (0, [])] in
  (* T2: read key=0 *)
  let txn2 = [Read (0, [])] in

  [
    (* T1 invoke *)
    { typ = Invoke; f = "txn"; value = Txn txn1; process = 0; index = 0;
      time_ns = t0 };
    (* T1 begin writes (auto-commit on the low-level store) — we just let T1
       proceed; the value 1 is now visible to a concurrent snapshot. *)
    (* T1 writes inserted at t1 *)

    (* T2 concurrent read — sees T1's uncommitted write if no SI *)
    { typ = Invoke; f = "txn"; value = Txn txn2; process = 1; index = 1;
      time_ns = t2 };
    (* T2 reads [1] — this is dirty! *)
    { typ = Ok; f = "txn";
      value = Txn [Read (0, [1])];
      process = 1; index = 2; time_ns = t3 };
    (* T1 commits *)
    { typ = Ok; f = "txn";
      value = Txn [Append (0, 1); Read (0, [1])];
      process = 0; index = 3; time_ns = t4 };
    (* T2 final read (same txn?) — no, separate txn but we keep it clean *)
  ]

(** Build a second negative-control: lost update.
    T1 reads key 0 -> [], appends 1.
    T2 concurrently reads key 0 -> [], appends 2.
    Result: only [1] or [2] visible — a lost update (G2 / G-single). *)

let build_lost_update_negative () =
  let t0 = 0L in
  let t1 = 100_000L in
  let t2 = 200_000L in
  let t3 = 300_000L in
  let _t4 = 400_000L in
  let _t5 = 500_000L in

  (* T1: append 1 to key=0 *)
  let _txn1 = [Append (0, 1); Read (0, [1])] in
  (* T2: append 2 to key=0 (concurrent with T1) *)
  let _txn2 = [Append (0, 2); Read (0, [2])] in

  [
    (* T1 invokes *)
    { typ = Invoke; f = "txn"; value = Txn [Append (0, 1); Read (0, [])];
      process = 0; index = 0; time_ns = t0 };
    (* T1 appends 1, reads back [1] *)
    { typ = Ok; f = "txn"; value = Txn [Append (0, 1); Read (0, [1])];
      process = 0; index = 1; time_ns = t1 };
    (* T2 invokes *)
    { typ = Invoke; f = "txn"; value = Txn [Append (0, 2); Read (0, [])];
      process = 1; index = 2; time_ns = t2 };
    (* T2 appends 2, reads back [2] — but should have seen [1,2]!
       This is a lost update / G-single. *)
    { typ = Ok; f = "txn"; value = Txn [Append (0, 2); Read (0, [2])];
      process = 1; index = 3; time_ns = t3 };
  ]

let () =
  Random.self_init ();
  let dirty_history = build_negative_history () in
  Edn_history.write_history "/tmp/sqlocaml_negative_dirty_read.edn" dirty_history;
  Printf.printf "Wrote %d entries (dirty-read negative control) to /tmp/sqlocaml_negative_dirty_read.edn\n"
    (List.length dirty_history);

  let lost_history = build_lost_update_negative () in
  Edn_history.write_history "/tmp/sqlocaml_negative_lost_update.edn" lost_history;
  Printf.printf "Wrote %d entries (lost-update negative control) to /tmp/sqlocaml_negative_lost_update.edn\n"
    (List.length lost_history)
