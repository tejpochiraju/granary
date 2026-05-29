(** Jepsen-style concurrent workload harness for sqlocaml.

    Usage: harness.exe [options]

    Runs N concurrent Lwt worker fibers against a single in-memory or
    file-backed database, each executing one of the supported workloads
    (list-append, bank, set, counter).  Every operation's
    {:invoke/:ok/:fail} is recorded with a nanosecond timestamp into a
    Jepsen-format EDN history file.

    Supports nemeses: crash-restart, process pause, lazyfs, clock-skew. *)

open Lwt.Syntax

module Db = struct
  include Sqlocaml.Db

  let open_file = Sqlocaml_unix.open_file
  let open_file_wal = Sqlocaml_unix.open_file_wal
end

(* ----------------------------------------------------------------- *)
(* Types                                                              *)
(* ----------------------------------------------------------------- *)

type backend =
  | Mem
  | File of string
  | WAL of string

type workload =
  | ListAppend
  | Bank of
      { n_accounts : int
      ; max_amount : int
      }
  | Set
  | Counter of { key_range : int }

type nemesis =
  | NoNemesis
  | CrashRestart of { crash_after_ops : int }
  | ProcessPause of
      { pause_after_ops : int
      ; pause_duration_s : float
      }
  | LazyFS of { lose_after_ops : int }
  | ClockSkew

(* ----------------------------------------------------------------- *)
(* State shared across workers                                       *)
(* ----------------------------------------------------------------- *)

type worker_state =
  { mutable entries : Edn_history.entry list
  ; mutable count : int
  }

let make_worker_state () = { entries = []; count = 0 }

(* ----------------------------------------------------------------- *)
(* Database open helpers                                              *)
(* ----------------------------------------------------------------- *)

let open_db backend =
  match backend with
  | Mem -> Db.open_in_memory ()
  | File path ->
    let* r = Db.open_file ~path () in
    (match r with
     | Ok db -> Lwt.return db
     | Error e ->
       failwith
         (Printf.sprintf
            "open_file(%s) failed: %s"
            path
            (Format.asprintf "%a" Db.pp_error e)))
  | WAL path ->
    let* r = Db.open_file_wal ~path () in
    (match r with
     | Ok db -> Lwt.return db
     | Error e ->
       failwith
         (Printf.sprintf
            "open_file_wal(%s) failed: %s"
            path
            (Format.asprintf "%a" Db.pp_error e)))
;;

(* ----------------------------------------------------------------- *)
(* Create schema based on workload                                    *)
(* ----------------------------------------------------------------- *)

let create_schema db = function
  | ListAppend -> Workload_list_append.create_schema db
  | Bank { n_accounts; _ } -> Workload_bank.create_schema db n_accounts
  | Set -> Workload_set.create_schema db
  | Counter { key_range } -> Workload_counter.create_schema db key_range
;;

(* ----------------------------------------------------------------- *)
(* Single-op executor per workload (returns invoke+ok/fail entries)   *)
(* ----------------------------------------------------------------- *)

let run_one_op db workload worker_id idx _nem_state =
  match workload with
  | ListAppend ->
    let txn = Workload_list_append.gen_txn 10 idx in
    Workload_list_append.run_and_record db txn worker_id idx
  | Bank { n_accounts; max_amount } ->
    let f, t, a = Workload_bank.gen_transfer n_accounts max_amount in
    Workload_bank.run_and_record db f t a worker_id idx
  | Set -> Workload_set.run_and_record db idx worker_id idx
  | Counter { key_range } ->
    let k = Workload_counter.gen_incr key_range in
    Workload_counter.run_and_record db k worker_id idx
;;

(* ----------------------------------------------------------------- *)
(* Worker loop                                                        *)
(* ----------------------------------------------------------------- *)

let worker_loop db workload st ops_per_worker nem_state pause_config =
  let rec loop () =
    if st.count >= ops_per_worker
    then Lwt.return_unit
    else (
      let idx = st.count in
      let* invoke_e, outcome_e = run_one_op db workload idx idx nem_state in
      st.entries <- outcome_e :: invoke_e :: st.entries;
      st.count <- idx + 1;
      (* Handle nemesis pause *)
      (match Nemesis.check_pause nem_state with
       | Some ev -> st.entries <- ev :: st.entries
       | None -> ());
      (* Trigger process-pause nemesis if configured *)
      (match pause_config with
       | Some (after, dur) when st.count = after ->
         let ev = Nemesis.start_pause nem_state dur in
         st.entries <- ev :: st.entries
       | _ -> ());
      let* () = Lwt.pause () in
      loop ())
  in
  loop ()
;;

(* ----------------------------------------------------------------- *)
(* Final read phases                                                  *)
(* ----------------------------------------------------------------- *)

let final_read db workload =
  let open Lwt.Syntax in
  match workload with
  | Set ->
    let* elements = Workload_set.read_all db in
    Lwt.return
      [ Edn_history.make_invoke ~f:"read" ~value:(SetRead []) ~process:(-2) ~index:0
      ; Edn_history.make_result
          ~typ:Ok
          ~f:"read"
          ~value:(SetRead elements)
          ~process:(-2)
          ~index:0
      ]
  | Counter { key_range } ->
    let rec read_keys i acc =
      if i >= key_range
      then Lwt.return (List.rev acc)
      else
        let* val_opt = Workload_counter.read_key db i in
        let inv =
          Edn_history.make_invoke ~f:"read" ~value:(Read (i, None)) ~process:(-2) ~index:i
        in
        let ok =
          Edn_history.make_result
            ~typ:Ok
            ~f:"read"
            ~value:(Read (i, val_opt))
            ~process:(-2)
            ~index:i
        in
        read_keys (i + 1) (ok :: inv :: acc)
    in
    read_keys 0 []
  | Bank { n_accounts = _; _ } ->
    let* r = Db.query db "SELECT id, balance FROM accounts ORDER BY id" in
    (match r with
     | Error _ ->
       Lwt.return
         [ Edn_history.make_invoke ~f:"read" ~value:(BankRead []) ~process:(-2) ~index:0
         ; Edn_history.make_result
             ~typ:Fail
             ~f:"read"
             ~value:(BankRead [])
             ~process:(-2)
             ~index:0
         ]
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       let balances =
         List.map
           (fun row ->
              let id =
                match row.(0) with
                | Db.V_int n -> Int64.to_int n
                | _ -> 0
              in
              let bal =
                match row.(1) with
                | Db.V_int n -> n
                | _ -> 0L
              in
              id, bal)
           rows
       in
       Lwt.return
         [ Edn_history.make_invoke ~f:"read" ~value:(BankRead []) ~process:(-2) ~index:0
         ; Edn_history.make_result
             ~typ:Ok
             ~f:"read"
             ~value:(BankRead balances)
             ~process:(-2)
             ~index:0
         ])
  | ListAppend -> Lwt.return []
;;

(* ----------------------------------------------------------------- *)
(* Collect history from all workers + final reads                     *)
(* ----------------------------------------------------------------- *)

let collect_history states final_entries =
  Array.fold_left (fun acc st -> List.rev_append st.entries acc) final_entries states
  |> List.rev
;;

(* ----------------------------------------------------------------- *)
(* Clean up database files                                            *)
(* ----------------------------------------------------------------- *)

let cleanup_files = function
  | File path | WAL path ->
    (try Unix.unlink path with
     | _ -> ());
    (try Unix.unlink (path ^ "-wal") with
     | _ -> ())
  | Mem -> ()
;;

(* ----------------------------------------------------------------- *)
(* Main harness: no-crash path                                        *)
(* ----------------------------------------------------------------- *)

let run_harness ~backend ~workload ~nemesis ~n_workers ~ops_per_worker ~history_path =
  let nem_state = Nemesis.create () in
  let pause_config =
    match nemesis with
    | ProcessPause { pause_after_ops; pause_duration_s } ->
      Some (pause_after_ops, pause_duration_s)
    | _ -> None
  in
  (* Record clock-skew if FAKETIME is set *)
  (match nemesis with
   | ClockSkew ->
     let _ = Nemesis.record_clock_skew nem_state in
     ()
   | _ -> ());
  (* Derive lazyfs mount dir from the backend path *)
  let lazyfs_mount =
    match nemesis with
    | LazyFS _ ->
      let db_path =
        match backend with
        | WAL path | File path -> path
        | Mem -> failwith "lazyfs nemesis requires file or WAL backend"
      in
      let mount_dir = Filename.dirname db_path in
      (try Unix.mkdir mount_dir 0o755 with
       | Unix.Unix_error (EEXIST, _, _) -> ());
      Some mount_dir
    | _ -> None
  in
  (* Start lazyfs before opening the database *)
  let lazyfs_st =
    match lazyfs_mount with
    | Some mount_dir -> Some (Nemesis.start_lazyfs mount_dir)
    | None -> None
  in
  let* db = open_db backend in
  let* () = create_schema db workload in
  (* For workloads that need multi-statement atomicity (e.g. bank's
     debit+credit), create per-worker handles sharing the same store
     so each worker has its own [explicit_txn] field.  Workers still
     serialise on the store's write lock but never get "transaction
     already active". *)
  let* worker_dbs =
    match workload with
    | Bank _ ->
      let rec make n acc =
        if n < 0
        then Lwt.return (Array.of_list acc)
        else
          let* h = Db.create_worker_handle db in
          make (n - 1) (h :: acc)
      in
      make (n_workers - 1) []
    | _ -> Lwt.return (Array.make n_workers db)
  in
  let states = Array.init n_workers (fun _ -> make_worker_state ()) in
  let workers =
    Array.to_list
      (Array.mapi
         (fun worker_id st ->
            let wdb = worker_dbs.(worker_id) in
            worker_loop wdb workload st ops_per_worker nem_state pause_config)
         states)
  in
  let* () = Lwt.join workers in
  (* If lazyfs: close db, trigger lose-unsynced, reopen *)
  let* n_entries, final_db =
    match nemesis with
    | LazyFS _ ->
      let ls = Option.get lazyfs_st in
      let* () = Db.close db in
      let entry = Nemesis.trigger_lose_unsynced nem_state ls in
      let* reopened = open_db backend in
      Lwt.return ([ entry ], reopened)
    | _ -> Lwt.return ([], db)
  in
  let* final_entries = final_read final_db workload in
  let history = collect_history states (n_entries @ final_entries) in
  Printf.printf
    "Completed %d operations across %d workers\n"
    (List.length history)
    n_workers;
  Edn_history.write_history history_path history;
  Printf.printf "Wrote %d history entries to %s\n" (List.length history) history_path;
  (* Close the (possibly reopened) db handle *)
  let* () = Db.close final_db in
  (* Stop lazyfs if it was started *)
  (match lazyfs_st with
   | Some ls -> Nemesis.stop_lazyfs ls
   | None -> ());
  cleanup_files backend;
  Lwt.return_unit
;;

(* ----------------------------------------------------------------- *)
(* Crash-restart harness                                              *)
(* ----------------------------------------------------------------- *)

let run_crash_restart ~backend ~workload ~n_workers ~ops_before ~ops_after ~history_path =
  let nem_state = Nemesis.create () in
  let* db = open_db backend in
  let* () = create_schema db workload in
  let states = Array.init n_workers (fun _ -> make_worker_state ()) in
  let workers_before =
    Array.to_list
      (Array.mapi
         (fun _worker_id st -> worker_loop db workload st ops_before nem_state None)
         states)
  in
  let* () = Lwt.join workers_before in
  (* Crash *)
  let crash_entry = Nemesis.record_crash nem_state in
  let* () = Db.close db in
  (* Restart *)
  let restart_entry = Nemesis.record_restart nem_state in
  let* db2 = open_db backend in
  let states2 = Array.init n_workers (fun _ -> make_worker_state ()) in
  let workers_after =
    Array.to_list
      (Array.mapi
         (fun _worker_id st -> worker_loop db2 workload st ops_after nem_state None)
         states2)
  in
  let* () = Lwt.join workers_after in
  let* final_entries = final_read db2 workload in
  let history =
    crash_entry
    :: restart_entry
    :: collect_history states (collect_history states2 final_entries)
  in
  Printf.printf "Crash-restart: %d entries total\n" (List.length history);
  Edn_history.write_history history_path history;
  Printf.printf "Wrote %d history entries to %s\n" (List.length history) history_path;
  let* () = Db.close db2 in
  cleanup_files backend;
  Lwt.return_unit
;;

(* ----------------------------------------------------------------- *)
(* CLI entry point                                                    *)
(* ----------------------------------------------------------------- *)

let () =
  Random.self_init ();
  let backend = ref "mem" in
  let path = ref "/tmp/sqlocaml_jepsen.db" in
  let n_workers = ref 4 in
  let ops_per_worker = ref 100 in
  let key_range = ref 10 in
  let history_path = ref "/tmp/sqlocaml_jepsen_history.edn" in
  let workload_name = ref "list-append" in
  let nemesis_name = ref "none" in
  let crash_after = ref 50 in
  let pause_after = ref 30 in
  let pause_dur = ref 2.0 in
  let args =
    [ "--backend", Arg.Set_string backend, " Backend (mem|file|wal)"
    ; "--path", Arg.Set_string path, " Database file path"
    ; ( "--workload"
      , Arg.Set_string workload_name
      , " Workload: list-append|bank|set|counter" )
    ; ( "--nemesis"
      , Arg.Set_string nemesis_name
      , " Nemesis: none|crash-restart|pause|lazyfs|clock-skew" )
    ; "--workers", Arg.Set_int n_workers, " Concurrent workers"
    ; "--ops", Arg.Set_int ops_per_worker, " Ops per worker"
    ; "--keys", Arg.Set_int key_range, " Distinct keys/accounts"
    ; "--history", Arg.Set_string history_path, " Output EDN path"
    ; "--crash-after", Arg.Set_int crash_after, " Ops per worker before crash"
    ; "--pause-after", Arg.Set_int pause_after, " Ops before pause"
    ; "--pause-dur", Arg.Set_float pause_dur, " Pause duration (seconds)"
    ]
  in
  Arg.parse (Arg.align args) (fun _ -> ()) "sqlocaml Jepsen harness";
  let backend_val =
    match !backend with
    | "mem" -> Mem
    | "file" -> File !path
    | "wal" -> WAL !path
    | s -> failwith (Printf.sprintf "unknown backend: %s" s)
  in
  let workload_val =
    match !workload_name with
    | "list-append" -> ListAppend
    | "bank" -> Bank { n_accounts = !key_range; max_amount = 10 }
    | "set" -> Set
    | "counter" -> Counter { key_range = !key_range }
    | s -> failwith (Printf.sprintf "unknown workload: %s" s)
  in
  let nemesis_val =
    match !nemesis_name with
    | "none" -> NoNemesis
    | "crash-restart" -> CrashRestart { crash_after_ops = !crash_after }
    | "pause" ->
      ProcessPause { pause_after_ops = !pause_after; pause_duration_s = !pause_dur }
    | "lazyfs" -> LazyFS { lose_after_ops = !crash_after }
    | "clock-skew" -> ClockSkew
    | s -> failwith (Printf.sprintf "unknown nemesis: %s" s)
  in
  match nemesis_val with
  | CrashRestart { crash_after_ops } ->
    Lwt_main.run
      (run_crash_restart
         ~backend:backend_val
         ~workload:workload_val
         ~n_workers:!n_workers
         ~ops_before:crash_after_ops
         ~ops_after:(!ops_per_worker - crash_after_ops)
         ~history_path:!history_path)
  | _ ->
    Lwt_main.run
      (run_harness
         ~backend:backend_val
         ~workload:workload_val
         ~nemesis:nemesis_val
         ~n_workers:!n_workers
         ~ops_per_worker:!ops_per_worker
         ~history_path:!history_path)
;;
