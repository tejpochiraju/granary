(** #598: under ATTACH one [Db.t] holds one explicit-transaction slot PER
    schema, while [active_schema] is shared mutable routing state on the
    top-level handle. A [PRAGMA active_database = …] issued while a transaction
    is open therefore used to move the routing out from under that transaction:
    the caller's next write landed in a DIFFERENT database and was silently
    AUTOCOMMITTED, and the caller only found out at COMMIT — after the write was
    durable. No slot was ever double-booked, so #555's poison never fired and
    [transaction_poisoned] read [false] for the whole window.

    The containment pinned here is the interim one from the issue: refuse the
    schema switch while ANY schema on the connection has an open explicit
    transaction. Loud instead of silent. It costs the ability to switch schemas
    mid-transaction, which no correct program could rely on anyway.

    #555's own ATTACH behaviour must survive intact — see the poison tests in
    test_txn.ml — so the refusals it wired in (a poisoned schema may not be left
    via [PRAGMA active_database] or [DETACH]) are re-checked here against the
    new, stricter gate. *)

module Db = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
end

let () = Granary_unix.install ()
let run = Lwt_main.run
let db_counter = ref 0

let fresh_path () =
  let n = !db_counter in
  incr db_counter;
  let path = Printf.sprintf "/tmp/granary_attach598_test_%04d.db" n in
  (try Unix.unlink path with
   | _ -> ());
  path
;;

let fresh_file_db () =
  let path = fresh_path () in
  match run (Db.open_file ~path ()) with
  | Ok db -> db, path
  | Error _ -> Alcotest.fail "open_file failed"
;;

let rows_of stream = run (Lwt_stream.to_list stream)

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let err_msg db sql =
  match run (Db.execute db sql) with
  | Ok () -> None
  | Error (Db.Runtime m) -> Some m
  | Error e -> Some (Format.asprintf "%a" Db.pp_error e)
;;

(* Substring test, so the file needs no [str] dependency. *)
let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let query_ints db sql =
  match run (Db.query db sql) with
  | Error _ -> []
  | Ok stream ->
    List.map
      (fun row ->
         match row.(0) with
         | Db.V_int n -> Int64.to_int n
         | _ -> -1)
      (rows_of stream)
;;

let active_db db =
  match run (Db.query db "PRAGMA active_database") with
  | Error _ -> "?"
  | Ok stream ->
    (match rows_of stream with
     | [ [| Db.V_text s |] ] -> s
     | _ -> "?")
;;

(* Two-database fixture: main with table [m], aux with table [a]. *)
let fixture () =
  let db, path = fresh_file_db () in
  let aux_path = fresh_path () in
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path);
  exec db "PRAGMA active_database = 'aux'";
  exec db "CREATE TABLE a (n INTEGER)";
  exec db "PRAGMA active_database = 'main'";
  exec db "CREATE TABLE m (n INTEGER)";
  db, path, aux_path
;;

let cleanup db paths =
  run (Db.close db);
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    paths
;;

(* The issue's exact interleaving. The switch is now refused, so the second
   INSERT stays inside the transaction it was written for and COMMIT works. *)
let test_switch_refused_mid_transaction () =
  let db, path, aux_path = fixture () in
  exec db "BEGIN";
  exec db "INSERT INTO m VALUES (1)";
  let msg = err_msg db "PRAGMA active_database = 'aux'" in
  Alcotest.(check bool)
    "#598: the schema switch is refused, not silently honoured"
    true
    (msg <> None);
  (match msg with
   | Some m ->
     Alcotest.(check bool)
       "the refusal names the open transaction"
       true
       (contains ~needle:"explicit transaction" m)
   | None -> ());
  Alcotest.(check string) "routing did not move" "main" (active_db db);
  (* The write that used to be autocommitted into aux stays in main's txn. *)
  exec db "INSERT INTO m VALUES (2)";
  exec db "COMMIT";
  Alcotest.(check (list int))
    "both rows in main"
    [ 1; 2 ]
    (query_ints db "SELECT n FROM m");
  Alcotest.(check bool) "never poisoned" false (Db.transaction_poisoned db);
  exec db "PRAGMA active_database = 'aux'";
  Alcotest.(check (list int)) "aux untouched" [] (query_ints db "SELECT n FROM a");
  cleanup db [ path; aux_path ]
;;

(* Symmetric: a transaction open on AUX blocks a switch back to main. The gate
   is over every schema on the connection, not just the active one. *)
let test_switch_refused_when_other_schema_has_txn () =
  let db, path, aux_path = fixture () in
  exec db "PRAGMA active_database = 'aux'";
  exec db "BEGIN";
  exec db "INSERT INTO a VALUES (7)";
  Alcotest.(check bool)
    "#598: cannot switch away from aux's open transaction"
    true
    (err_msg db "PRAGMA active_database = 'main'" <> None);
  exec db "COMMIT";
  (* Once committed, switching is fine again. *)
  exec db "PRAGMA active_database = 'main'";
  Alcotest.(check string) "switch allowed after COMMIT" "main" (active_db db);
  cleanup db [ path; aux_path ]
;;

(* A switch to the schema already active is a no-op and stays legal, so a
   caller that re-asserts its own routing inside a transaction is not broken. *)
let test_self_switch_allowed_in_transaction () =
  let db, path, aux_path = fixture () in
  exec db "BEGIN";
  exec db "INSERT INTO m VALUES (1)";
  exec db "PRAGMA active_database = 'main'";
  exec db "INSERT INTO m VALUES (2)";
  exec db "COMMIT";
  Alcotest.(check (list int))
    "transaction intact"
    [ 1; 2 ]
    (query_ints db "SELECT n FROM m");
  cleanup db [ path; aux_path ]
;;

(* SAVEPOINT opens a transaction too (auto-BEGIN), so the gate must see it. *)
let test_switch_refused_under_savepoint () =
  let db, path, aux_path = fixture () in
  exec db "BEGIN";
  exec db "SAVEPOINT sp";
  exec db "INSERT INTO m VALUES (1)";
  Alcotest.(check bool)
    "#598: refused inside a savepoint too"
    true
    (err_msg db "PRAGMA active_database = 'aux'" <> None);
  exec db "ROLLBACK";
  cleanup db [ path; aux_path ]
;;

(* With no transaction open, switching is unaffected. *)
let test_switch_allowed_without_transaction () =
  let db, path, aux_path = fixture () in
  exec db "PRAGMA active_database = 'aux'";
  Alcotest.(check string) "switched" "aux" (active_db db);
  exec db "INSERT INTO a VALUES (3)";
  exec db "PRAGMA active_database = 'main'";
  Alcotest.(check string) "switched back" "main" (active_db db);
  Alcotest.(check (list int))
    "autocommit write landed in aux"
    []
    (query_ints db "SELECT n FROM m");
  cleanup db [ path; aux_path ]
;;

(* #555 must be undisturbed: a POISONED schema still cannot be left, and the
   message must still be the poison message rather than the new one — the
   recovering caller has to be told to ROLLBACK. *)
let test_poisoned_schema_still_refuses_switch () =
  let db, path, aux_path = fixture () in
  exec db "BEGIN";
  Alcotest.(check bool) "second BEGIN fails" true (err_msg db "BEGIN" <> None);
  Alcotest.(check bool) "poisoned" true (Db.transaction_poisoned db);
  (match err_msg db "PRAGMA active_database = 'aux'" with
   | None -> Alcotest.fail "switch away from a poisoned schema must be refused"
   | Some m ->
     Alcotest.(check bool)
       "#555's poison message wins over #598's gate"
       true
       (contains ~needle:"poisoned" m));
  (* DETACH stays refused too, so ROLLBACK remains the sole exit. *)
  Alcotest.(check bool) "DETACH refused" true (err_msg db "DETACH DATABASE aux" <> None);
  exec db "ROLLBACK";
  Alcotest.(check bool) "poison cleared" false (Db.transaction_poisoned db);
  exec db "PRAGMA active_database = 'aux'";
  Alcotest.(check string) "usable again" "aux" (active_db db);
  cleanup db [ path; aux_path ]
;;

(* DETACH has the same shape as the switch: dropping a schema whose transaction
   is open would discard it silently. Refuse it while any transaction is open. *)
let test_detach_refused_mid_transaction () =
  let db, path, aux_path = fixture () in
  exec db "BEGIN";
  exec db "INSERT INTO m VALUES (1)";
  Alcotest.(check bool)
    "#598: DETACH refused while a transaction is open"
    true
    (err_msg db "DETACH DATABASE aux" <> None);
  exec db "COMMIT";
  exec db "DETACH DATABASE aux";
  cleanup db [ path; aux_path ]
;;

let () =
  Alcotest.run
    "test_attach_active_txn_598"
    [ ( "active_database_gate"
      , [ Alcotest.test_case
            "switch_refused_mid_transaction"
            `Quick
            test_switch_refused_mid_transaction
        ; Alcotest.test_case
            "switch_refused_when_other_schema_has_txn"
            `Quick
            test_switch_refused_when_other_schema_has_txn
        ; Alcotest.test_case
            "self_switch_allowed_in_transaction"
            `Quick
            test_self_switch_allowed_in_transaction
        ; Alcotest.test_case
            "switch_refused_under_savepoint"
            `Quick
            test_switch_refused_under_savepoint
        ; Alcotest.test_case
            "switch_allowed_without_transaction"
            `Quick
            test_switch_allowed_without_transaction
        ; Alcotest.test_case
            "detach_refused_mid_transaction"
            `Quick
            test_detach_refused_mid_transaction
        ] )
    ; ( "poison_555_unchanged"
      , [ Alcotest.test_case
            "poisoned_schema_still_refuses_switch"
            `Quick
            test_poisoned_schema_still_refuses_switch
        ] )
    ]
;;
