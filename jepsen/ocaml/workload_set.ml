(** Set workload: insert unique elements and verify durability.

    Schema:
      CREATE TABLE s (e INTEGER PRIMARY KEY)

    Each operation:
      INSERT INTO s (e) VALUES (?element);

    Auto-commit mode (no explicit BEGIN/COMMIT) so multiple workers
    sharing a single [Db.t] handle don't collide on [explicit_txn].

    After all transactions, a final read verifies which elements are present.
    Jepsen's set checker then verifies:
      - Every acked element is present (no lost writes).
      - No fabricated elements (elements never acked are not present).
      - Recovery durability: elements survive crash/restart. *)

open Edn_history
module Db = Sqlocaml.Db

(** Generate a set-add operation with a unique element value. *)
let gen_add element = element

(** Run a set-add operation (auto-commit). *)
let run_add db element =
  let open Lwt.Syntax in
  let pp_error e = Format.asprintf "%a" Db.pp_error e in
  let sql = Printf.sprintf "INSERT INTO s (e) VALUES (%d)" element in
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> failwith (Printf.sprintf "INSERT %d failed: %s" element (pp_error e))
;;

(** Run a set-add and produce invoke/ok|fail entries. *)
let run_and_record db element process_id index =
  let open Lwt.Syntax in
  let invoke_entry =
    make_invoke ~f:"add" ~value:(SetAdd element) ~process:process_id ~index
  in
  let* result =
    Lwt.catch
      (fun () ->
         let* () = run_add db element in
         Lwt.return (Stdlib.Ok ()))
      (fun exn -> Lwt.return (Stdlib.Error (Printexc.to_string exn)))
  in
  match result with
  | Ok () ->
    let ok_entry =
      make_result ~typ:Ok ~f:"add" ~value:(SetAdd element) ~process:process_id ~index
    in
    Lwt.return (invoke_entry, ok_entry)
  | Stdlib.Error _msg ->
    let fail_entry =
      make_result ~typ:Fail ~f:"add" ~value:(SetAdd element) ~process:process_id ~index
    in
    Lwt.return (invoke_entry, fail_entry)
;;

(** Read back all elements currently in the set. *)
let read_all db =
  let open Lwt.Syntax in
  let pp_error e = Format.asprintf "%a" Db.pp_error e in
  let* r = Db.query db "SELECT e FROM s ORDER BY e" in
  match r with
  | Error e -> failwith (Printf.sprintf "SELECT failed: %s" (pp_error e))
  | Ok stream ->
    let* rows = Lwt_stream.to_list stream in
    let elements =
      List.map
        (fun row ->
           match row.(0) with
           | Db.V_int n -> Int64.to_int n
           | _ -> 0)
        rows
    in
    Lwt.return elements
;;

(** Create the set table. *)
let create_schema db =
  let open Lwt.Syntax in
  let* r = Db.execute db "CREATE TABLE s (e INTEGER PRIMARY KEY)" in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e ->
    failwith
      (Printf.sprintf "CREATE TABLE failed: %s" (Format.asprintf "%a" Db.pp_error e))
;;
