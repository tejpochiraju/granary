(** Negative controls — deliberately broken histories for each workload.

    Each negative control constructs a history that a correct checker MUST
    detect as invalid. RED before GREEN. *)

open Edn_history

(* ================================================================= *)
(* List-append negative controls                                     *)
(* ================================================================= *)

(** Dirty read: T2 observes T1's uncommitted write. *)
let build_dirty_read () =
  let t0 = 0L in
  let t2 = 200_000L in
  let t3 = 300_000L in
  let t4 = 400_000L in
  let txn1 = [Append (0, 1); Read (0, [])] in
  let txn2 = [Read (0, [])] in
  [ { typ = Invoke; f = "txn"; value = Txn txn1; process = 0; index = 0;
      time_ns = t0 };
    { typ = Invoke; f = "txn"; value = Txn txn2; process = 1; index = 1;
      time_ns = t2 };
    { typ = Ok; f = "txn"; value = Txn [Read (0, [1])];
      process = 1; index = 2; time_ns = t3 };
    { typ = Ok; f = "txn"; value = Txn [Append (0, 1); Read (0, [1])];
      process = 0; index = 3; time_ns = t4 };
  ]

(** Lost update: T1 and T2 both see [] then each append, losing one. *)
let build_lost_update () =
  let t0 = 0L in
  let t1 = 100_000L in
  let t2 = 200_000L in
  let t3 = 300_000L in
  [ { typ = Invoke; f = "txn"; value = Txn [Append (0, 1); Read (0, [])];
      process = 0; index = 0; time_ns = t0 };
    { typ = Ok; f = "txn"; value = Txn [Append (0, 1); Read (0, [1])];
      process = 0; index = 1; time_ns = t1 };
    { typ = Invoke; f = "txn"; value = Txn [Append (0, 2); Read (0, [])];
      process = 1; index = 2; time_ns = t2 };
    { typ = Ok; f = "txn"; value = Txn [Append (0, 2); Read (0, [2])];
      process = 1; index = 3; time_ns = t3 };
  ]

(* ================================================================= *)
(* Bank negative control: total not conserved                        *)
(* ================================================================= *)

let build_bank_lost_transfer () =
  let t0 = 0L in
  let t1 = 100_000L in
  let t2 = 200_000L in
  let t3 = 300_000L in
  [ (* Transfer 10 from account 0 to 1 *)
    { typ = Invoke; f = "transfer"; value = Transfer (0, 1, 10);
      process = 0; index = 0; time_ns = t0 };
    (* But the balance read shows total went from 200 to 210 — fabricated! *)
    { typ = Ok; f = "transfer";
      value = BankRead [(0, 90L); (1, 120L)];  (* 90+120=210, should be 200 *)
      process = 0; index = 1; time_ns = t1 };
    (* Final read: different total again *)
    { typ = Invoke; f = "read"; value = BankRead [];
      process = (-2); index = 0; time_ns = t2 };
    { typ = Ok; f = "read";
      value = BankRead [(0, 90L); (1, 110L)];  (* 90+110=200 *)
      process = (-2); index = 1; time_ns = t3 };
  ]

(* ================================================================= *)
(* Set negative control: lost element                                *)
(* ================================================================= *)

let build_set_lost_element () =
  let t0 = 0L in
  let t1 = 100_000L in
  let t2 = 200_000L in
  let t3 = 300_000L in
  let t4 = 400_000L in
  let t5 = 500_000L in
  [ (* Add element 1 *)
    { typ = Invoke; f = "add"; value = SetAdd 1;
      process = 0; index = 0; time_ns = t0 };
    { typ = Ok; f = "add"; value = SetAdd 1;
      process = 0; index = 1; time_ns = t1 };
    (* Add element 2 *)
    { typ = Invoke; f = "add"; value = SetAdd 2;
      process = 0; index = 2; time_ns = t2 };
    { typ = Ok; f = "add"; value = SetAdd 2;
      process = 0; index = 3; time_ns = t3 };
    (* Final read — element 1 is missing! *)
    { typ = Invoke; f = "read"; value = SetRead [];
      process = (-2); index = 0; time_ns = t4 };
    { typ = Ok; f = "read"; value = SetRead [2];  (* element 1 lost *)
      process = (-2); index = 1; time_ns = t5 };
  ]

(* ================================================================= *)
(* Counter negative control: non-monotonic read                      *)
(* ================================================================= *)

let build_counter_non_monotonic () =
  let t0 = 0L in
  let t1 = 100_000L in
  let t2 = 200_000L in
  let t3 = 300_000L in
  let t4 = 400_000L in
  let t5 = 500_000L in
  [ (* Increment key 0 twice *)
    { typ = Invoke; f = "add"; value = Add (0, 1);
      process = 0; index = 0; time_ns = t0 };
    { typ = Ok; f = "add"; value = Read (0, Some 1);
      process = 0; index = 1; time_ns = t1 };
    { typ = Invoke; f = "add"; value = Add (0, 1);
      process = 0; index = 2; time_ns = t2 };
    { typ = Ok; f = "add"; value = Read (0, Some 2);
      process = 0; index = 3; time_ns = t3 };
    (* Final reads — value goes down! (non-monotonic) *)
    { typ = Invoke; f = "read"; value = Read (0, None);
      process = (-2); index = 0; time_ns = t4 };
    { typ = Ok; f = "read"; value = Read (0, Some 1);  (* went from 2 to 1! *)
      process = (-2); index = 1; time_ns = t5 };
  ]

(* ================================================================= *)
(* Main                                                              *)
(* ================================================================= *)

let () =
  Random.self_init ();
  let write name entries =
    let path = Printf.sprintf "/tmp/sqlocaml_negative_%s.edn" name in
    Edn_history.write_history path entries;
    Printf.printf "Wrote %d entries (%s negative control) to %s\n"
      (List.length entries) name path
  in
  write "dirty_read" (build_dirty_read ());
  write "lost_update" (build_lost_update ());
  write "bank_lost_transfer" (build_bank_lost_transfer ());
  write "set_lost_element" (build_set_lost_element ());
  write "counter_non_monotonic" (build_counter_non_monotonic ())
