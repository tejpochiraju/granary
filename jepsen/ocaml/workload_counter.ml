(** Counter workload: atomic read-modify-write increment on a single row.

    Schema:
      CREATE TABLE c (k INTEGER PRIMARY KEY, n INTEGER)

    Each transaction:
      BEGIN;
      UPDATE c SET n = n + 1 WHERE k = ?key;
      SELECT n FROM c WHERE k = ?key;
      COMMIT;

    Jepsen's counter checker verifies:
      - Reads are monotonic non-decreasing.
      - Final value is within [acked, attempted].
      - No lost increments under concurrency. *)

open Edn_history

module Db = Sqlocaml.Db

(** Generate a counter increment for a random key in [0, key_range). *)
let gen_incr key_range =
  Random.int key_range

(** Run a counter increment transaction. Returns (key, new_value). *)
let run_incr db key =
  let open Lwt.Syntax in
  let pp_error e = Format.asprintf "%a" Db.pp_error e in
  let* r_begin = Db.execute db "BEGIN" in
  (match r_begin with
   | Ok () -> ()
   | Error e -> failwith (Printf.sprintf "BEGIN failed: %s" (pp_error e)));
  let update_sql = Printf.sprintf
    "UPDATE c SET n = n + 1 WHERE k = %d" key in
  let* r_update = Db.execute db update_sql in
  (match r_update with
   | Ok () -> ()
   | Error e ->
     failwith (Printf.sprintf "UPDATE failed: %s" (pp_error e)));
  let* r_read = Db.query db
    (Printf.sprintf "SELECT n FROM c WHERE k = %d" key) in
  let new_val =
    match r_read with
    | Error e -> failwith (Printf.sprintf "SELECT failed: %s" (pp_error e))
    | Ok stream ->
      let rows = Lwt_main.run (Lwt_stream.to_list stream) in
      match rows with
      | [| Db.V_int n |] :: _ -> Int64.to_int n
      | _ -> failwith "unexpected counter read result"
  in
  let* r_commit = Db.execute db "COMMIT" in
  (match r_commit with
   | Ok () -> Lwt.return (key, new_val)
   | Error e ->
     let* _ = Db.execute db "ROLLBACK" in
     failwith (Printf.sprintf "COMMIT failed: %s" (pp_error e)))

(** Run an increment and produce invoke/ok|fail entries. *)
let run_and_record db key process_id index =
  let open Lwt.Syntax in
  let invoke_entry =
    make_invoke ~f:"add"
      ~value:(Add (key, 1))
      ~process:process_id ~index
  in
  let* result =
    Lwt.catch
      (fun () ->
         let* (k, n) = run_incr db key in
         Lwt.return (Stdlib.Ok (k, n)))
      (fun exn -> Lwt.return (Stdlib.Error (Printexc.to_string exn)))
  in
  match result with
  | Ok (k, n) ->
    let ok_entry =
      make_result ~typ:Ok ~f:"add"
        ~value:(Read (k, Some n))
        ~process:process_id ~index
    in
    Lwt.return (invoke_entry, ok_entry)
  | Error _msg ->
    let fail_entry =
      make_result ~typ:Fail ~f:"add"
        ~value:(Add (key, 1))
        ~process:process_id ~index
    in
    Lwt.return (invoke_entry, fail_entry)

(** Read the final counter value for a key. *)
let read_key db key =
  let open Lwt.Syntax in
  let pp_error e = Format.asprintf "%a" Db.pp_error e in
  let* r = Db.query db
    (Printf.sprintf "SELECT n FROM c WHERE k = %d" key) in
  match r with
  | Error e -> failwith (Printf.sprintf "SELECT failed: %s" (pp_error e))
  | Ok stream ->
    let* rows = Lwt_stream.to_list stream in
    match rows with
    | [| Db.V_int n |] :: _ -> Lwt.return (Some (Int64.to_int n))
    | _ -> Lwt.return None

(** Create the counter table and seed keys. *)
let create_schema db key_range =
  let open Lwt.Syntax in
  let* r = Db.execute db "CREATE TABLE c (k INTEGER PRIMARY KEY, n INTEGER)" in
  (match r with
   | Ok () -> ()
   | Error e -> failwith (Printf.sprintf "CREATE TABLE failed: %s"
                  (Format.asprintf "%a" Db.pp_error e)));
  let rec seed i =
    if i >= key_range then Lwt.return_unit
    else
      let sql = Printf.sprintf "INSERT INTO c (k, n) VALUES (%d, 0)" i in
      let* r = Db.execute db sql in
      (match r with
       | Ok () -> seed (i + 1)
       | Error e -> failwith (Printf.sprintf "INSERT counter seed failed: %s"
                      (Format.asprintf "%a" Db.pp_error e)))
  in
  seed 0
