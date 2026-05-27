(** Bank workload: transfers between accounts with a total-conservation invariant.

    Schema:
      CREATE TABLE accounts (id INTEGER PRIMARY KEY, balance INTEGER)

    Each transaction:
      BEGIN;
      UPDATE accounts SET balance = balance - ?amount WHERE id = ?from;
      UPDATE accounts SET balance = balance + ?amount WHERE id = ?to;
      SELECT id, balance FROM accounts ORDER BY id;
      COMMIT;

    The full account snapshot read-back lets Jepsen's bank checker verify:
      - Total sum is conserved across transactions.
      - No account goes negative (if constrained).
      - Reads observe consistent snapshots. *)

open Edn_history

module Db = Sqlocaml.Db

(** Generate a bank transfer transaction.
    Picks two distinct random accounts from [0, n_accounts) and an amount
    between 1 and max_amount. *)
let gen_transfer n_accounts max_amount =
  let from = Random.int n_accounts in
  let to_acc = ref from in
  while !to_acc = from do
    to_acc := Random.int n_accounts
  done;
  let amount = 1 + Random.int max_amount in
  (from, !to_acc, amount)

(** Run a bank transfer transaction and return the observed account balances. *)
let run_transfer db from_acc to_acc amount =
  let open Lwt.Syntax in
  let pp_error e = Format.asprintf "%a" Db.pp_error e in
  let* r_begin = Db.execute db "BEGIN" in
  (match r_begin with
   | Ok () -> ()
   | Error e -> failwith (Printf.sprintf "BEGIN failed: %s" (pp_error e)));
  (* Debit from *)
  let debit_sql =
    Printf.sprintf "UPDATE accounts SET balance = balance - %d WHERE id = %d"
      amount from_acc
  in
  let* r_debit = Db.execute db debit_sql in
  (match r_debit with
   | Ok () -> ()
   | Error e -> failwith (Printf.sprintf "DEBIT failed: %s" (pp_error e)));
  (* Credit to *)
  let credit_sql =
    Printf.sprintf "UPDATE accounts SET balance = balance + %d WHERE id = %d"
      amount to_acc
  in
  let* r_credit = Db.execute db credit_sql in
  (match r_credit with
   | Ok () -> ()
   | Error e -> failwith (Printf.sprintf "CREDIT failed: %s" (pp_error e)));
  (* Read back full snapshot *)
  let* r_query = Db.query db "SELECT id, balance FROM accounts ORDER BY id" in
  let balances =
    match r_query with
    | Error e -> failwith (Printf.sprintf "SELECT failed: %s" (pp_error e))
    | Ok stream ->
      let rows = Lwt_main.run (Lwt_stream.to_list stream) in
      List.map (fun row ->
        let id = match row.(0) with Db.V_int n -> Int64.to_int n | _ -> 0 in
        let bal = match row.(1) with Db.V_int n -> n | _ -> 0L in
        (id, bal)
      ) rows
  in
  let* r_commit = Db.execute db "COMMIT" in
  (match r_commit with
   | Ok () -> Lwt.return balances
   | Error e ->
     (* Try rollback *)
     let* _ = Db.execute db "ROLLBACK" in
     failwith (Printf.sprintf "COMMIT failed: %s" (pp_error e)))

(** Run a transfer and produce invoke/ok|fail entries. *)
let run_and_record db from_acc to_acc amount process_id index =
  let open Lwt.Syntax in
  let invoke_entry =
    make_invoke ~f:"transfer"
      ~value:(Transfer (from_acc, to_acc, amount))
      ~process:process_id ~index
  in
  let* result =
    Lwt.catch
      (fun () ->
         let* balances = run_transfer db from_acc to_acc amount in
         Lwt.return (Stdlib.Ok balances))
      (fun exn -> Lwt.return (Stdlib.Error (Printexc.to_string exn)))
  in
  match result with
  | Stdlib.Ok balances ->
    let ok_entry =
      make_result ~typ:Edn_history.Ok ~f:"transfer"
        ~value:(BankRead balances)
        ~process:process_id ~index
    in
    Lwt.return (invoke_entry, ok_entry)
  | Stdlib.Error _msg ->
    let fail_entry =
      make_result ~typ:Fail ~f:"transfer"
        ~value:(Transfer (from_acc, to_acc, amount))
        ~process:process_id ~index
    in
    Lwt.return (invoke_entry, fail_entry)

(** Create the accounts table and populate with initial balances. *)
let create_schema db n_accounts =
  let open Lwt.Syntax in
  let pp_error e = Format.asprintf "%a" Db.pp_error e in
  let* r = Db.execute db
    "CREATE TABLE accounts (id INTEGER PRIMARY KEY, balance INTEGER)" in
  (match r with
   | Ok () -> ()
   | Error e -> failwith (Printf.sprintf "CREATE TABLE failed: %s" (pp_error e)));
  (* Populate with initial balance = 100 each *)
  let rec insert_all i =
    if i >= n_accounts then Lwt.return_unit
    else
      let sql = Printf.sprintf
        "INSERT INTO accounts (id, balance) VALUES (%d, 100)" i in
      let* r = Db.execute db sql in
      (match r with
       | Ok () -> insert_all (i + 1)
       | Error e ->
         failwith (Printf.sprintf "INSERT account %d failed: %s" i (pp_error e)))
  in
  insert_all 0
