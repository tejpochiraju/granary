(** Workload driver that performs list-append transactions à la Elle.

    Each transaction appends a fresh integer to a key's value list and reads
    back the concatenated list.  Elle's list-append checker then detects any
    isolation anomaly (G0, G1a/b/c, G-single, G2, lost update) by analysing
    the dependency graph.

    Schema (created once):
      CREATE TABLE elle (k INTEGER, v INTEGER, rowid INTEGER PRIMARY KEY AUTOINCREMENT)

    Each txn:
      BEGIN;
      INSERT INTO elle (k, v) VALUES (?key, ?val);
      SELECT v FROM elle WHERE k = ?key ORDER BY rowid;
      COMMIT;

    The read-back list is recorded in the EDN history so Elle can reconstruct
    version order. *)

open Edn_history

(** Module alias for the DB — all references are fully qualified. *)
module Db = Sqlocaml.Db

type outcome =
  | Completed of txn_op list  (* observed Read ops with results filled in *)
  | Failed of string

(** Generate a list-append transaction.
    [key_range] is the exclusive upper bound for keys (0..key_range-1).
    [value] is the integer value to append. *)
let gen_txn key_range value =
  let k = Random.int key_range in
  [ Append (k, value); Read (k, []) ]

(** Run a single list-append transaction on the database.
    Returns the observed read values. *)
let run_txn db txn =
  let open Lwt.Syntax in
  let result = ref [] in
  let pp_error e = Format.asprintf "%a" Db.pp_error e in
  let process_op : Edn_history.txn_op -> unit Lwt.t = function
    | Append (k, v) ->
      let sql = Printf.sprintf "INSERT INTO elle (k, v) VALUES (%d, %d)" k v in
      let* r = Db.execute db sql in
      (match r with
       | Ok () -> Lwt.return_unit
       | Error e ->
         failwith (Printf.sprintf "INSERT failed (k=%d, v=%d): %s" k v (pp_error e)))
    | Read (k, _) ->
      let sql = Printf.sprintf "SELECT v FROM elle WHERE k = %d" k in
      let* r = Db.query db sql in
      (match r with
       | Error e ->
         failwith (Printf.sprintf "SELECT failed (k=%d): %s" k (pp_error e))
       | Ok stream ->
         let* rows = Lwt_stream.to_list stream in
         let vs = List.map (fun row ->
           match row.(0) with
           | Db.V_int n -> Int64.to_int n
           | _ -> failwith "non-int in elle.v") rows
         in
         result := Read (k, vs) :: !result;
         Lwt.return_unit)
  in
  let* r_begin = Db.execute db "BEGIN" in
  (match r_begin with
   | Ok () -> ()
   | Error e -> failwith (Printf.sprintf "BEGIN failed: %s" (pp_error e)));
  let* () = Lwt_list.iter_s process_op txn in
  let* r_commit = Db.execute db "COMMIT" in
  (match r_commit with
   | Ok () -> ()
   | Error e -> failwith (Printf.sprintf "COMMIT failed: %s" (pp_error e)));
  Lwt.return (Completed (List.rev !result))

(** Run a single list-append transaction and produce invoke/ok|fail entries. *)
let run_and_record db txn process_id index =
  let open Lwt.Syntax in
  let start_ns = Int64.of_float (Unix.gettimeofday () *. 1e9) in
  let invoke_entry = {
    typ = Invoke; f = "txn";
    value = Txn txn;
    process = process_id; index; time_ns = start_ns;
  } in
  let* outcome = run_txn db txn in
  let end_ns = Int64.of_float (Unix.gettimeofday () *. 1e9) in
  match outcome with
  | Completed observed ->
    let completed_txn = List.map (fun op ->
      match op with
      | Append _ as a -> a
      | Read (k, _) ->
        match List.find_opt
                (function Read (k', _) -> k' = k | _ -> false) observed with
        | Some (Read (_, vs)) -> Read (k, vs)
        | _ -> Read (k, [])
    ) txn in
    let ok_entry = {
      typ = Ok; f = "txn";
      value = Txn completed_txn;
      process = process_id; index; time_ns = end_ns;
    } in
    Lwt.return (invoke_entry, ok_entry)
  | Failed _err ->
    let fail_entry = {
      typ = Fail; f = "txn";
      value = Txn txn;
      process = process_id; index; time_ns = end_ns;
    } in
    Lwt.return (invoke_entry, fail_entry)

type config = {
  key_range : int;
}

let create_schema db =
  let open Lwt.Syntax in
  let* r = Db.execute db "CREATE TABLE elle (k INTEGER, v INTEGER)" in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> failwith (Printf.sprintf "CREATE TABLE failed: %s" (Format.asprintf "%a" Db.pp_error e))

