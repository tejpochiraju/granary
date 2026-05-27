(** Jepsen-style concurrent workload harness for sqlocaml.

    Usage: harness.exe [options]

    Runs N concurrent Lwt worker fibers against a single in-memory or
    file-backed database, each executing list-append (or other workload)
    transactions.  Every operation's {:invoke/:ok/:fail} is recorded with a
    nanosecond timestamp into a Jepsen-format EDN history file.

    Phase 1 (core, no faults): list-append only, no nemeses. *)

open Lwt.Syntax

module Db = struct
  include Sqlocaml.Db
  let open_file = Sqlocaml_unix.open_file
  let open_file_wal = Sqlocaml_unix.open_file_wal
end

(* ----------------------------------------------------------------- *)
(* State shared across workers — the history accumulator              *)
(* ----------------------------------------------------------------- *)

type worker_state = {
  mutable entries : Edn_history.entry list;
  mutable count : int;
}

let make_worker_state () = { entries = []; count = 0 }

(* ----------------------------------------------------------------- *)
(* Main harness driver                                                *)
(* ----------------------------------------------------------------- *)

type backend = Mem | File of string | WAL of string

let run_harness
    ~backend
    ~n_workers
    ~ops_per_worker
    ~key_range
    ~history_path
  =
  (* Open database *)
  let* db =
    match backend with
    | Mem ->
      Db.open_in_memory ()
    | File path ->
      let* r = Db.open_file ~path () in
      (match r with
       | Ok db -> Lwt.return db
       | Error e -> failwith (Printf.sprintf "open_file(%s) failed: %s" path (Format.asprintf "%a" Db.pp_error e)))
    | WAL path ->
      let* r = Db.open_file_wal ~path () in
      (match r with
       | Ok db -> Lwt.return db
       | Error e -> failwith (Printf.sprintf "open_file_wal(%s) failed: %s" path (Format.asprintf "%a" Db.pp_error e)))
  in
  (* Create schema *)
  let* () = Workload_list_append.create_schema db in
  (* Launch workers — each runs exactly ops_per_worker txns and stops *)
  let states = Array.init n_workers (fun _ -> make_worker_state ()) in
  let workers = Array.to_list (Array.mapi (fun _ st ->
    let rec work () =
      if st.count >= ops_per_worker
      then Lwt.return_unit
      else
        let txn = Workload_list_append.gen_txn key_range st.count in
        let idx = st.count in
        let* invoke_e, outcome_e =
          Workload_list_append.run_and_record db txn 0 idx
        in
        st.entries <- outcome_e :: invoke_e :: st.entries;
        st.count <- idx + 1;
        let* () = Lwt.pause () in
        work ()
    in
    work ()
  ) states) in
  (* Wait for all workers to complete *)
  let* () = Lwt.join workers in
  (* Collect history *)
  let all_entries =
    Array.fold_left (fun acc st ->
      List.rev_append (List.rev st.entries) acc
    ) [] states
  in
  let history = List.rev all_entries in
  Printf.printf "Completed %d txns across %d workers\n" (List.length history / 2) n_workers;
  (* Write history *)
  Edn_history.write_history history_path history;
  Printf.printf "Wrote %d history entries to %s\n" (List.length history) history_path;
  (* Close *)
  let* () = Db.close db in
  (* Clean up file if needed *)
  (match backend with
   | File path | WAL path ->
     (try Unix.unlink path with _ -> ());
     (try Unix.unlink (path ^ "-wal") with _ -> ())
   | Mem -> ());
  Lwt.return_unit

(* ----------------------------------------------------------------- *)
(* CLI entry point *)
(* ----------------------------------------------------------------- *)

let () =
  Random.self_init ();
  let backend = ref "mem" in
  let path = ref "/tmp/sqlocaml_jepsen.db" in
  let n_workers = ref 4 in
  let ops_per_worker = ref 100 in
  let key_range = ref 10 in
  let history_path = ref "/tmp/sqlocaml_jepsen_history.edn" in
  let args = [
    ("--backend", Arg.Set_string backend, " Backend (mem|file|wal)");
    ("--path", Arg.Set_string path, " Database file path (for file/wal)");
    ("--workers", Arg.Set_int n_workers, " Number of concurrent worker fibers");
    ("--ops", Arg.Set_int ops_per_worker, " Ops per worker (total ops = workers * ops)");
    ("--keys", Arg.Set_int key_range, " Number of distinct keys");
    ("--history", Arg.Set_string history_path, " Output EDN history path");
  ] in
  Arg.parse (Arg.align args)
    (fun _ -> ())
    "sqlocaml Jepsen harness — concurrent list-append workload driver";
  Lwt_main.run (run_harness
    ~backend:(match !backend with
      | "mem" -> Mem
      | "file" -> File !path
      | "wal" -> WAL !path
      | _ -> failwith (Printf.sprintf "unknown backend: %s" !backend))
    ~n_workers:!n_workers
    ~ops_per_worker:!ops_per_worker
    ~key_range:!key_range
    ~history_path:!history_path)
