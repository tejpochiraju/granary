(** Nemesis (fault injection) module for Jepsen-style testing.

    Implements single-node faults:
    - Process pause (cooperative sleep)
    - Crash + restart (close/reopen the database, record events)
    - Clock skew (detect faketime via env var, record events)
    - LazyFS (un-fsynced write loss via FUSE; record events)

    Each nemesis phase emits :info entries into the EDN history so
    checkers can correlate anomalies with faults. *)

open Edn_history

type running =
  | NoNemesis
  | Paused of { mutable resume_after : float }

type state = {
  mutable running : running;
  mutable entry_index : int;
}

let create () = { running = NoNemesis; entry_index = 0 }

(* ---- process pause ---- *)

let start_pause st duration_s =
  st.running <- Paused { resume_after = Unix.gettimeofday () +. duration_s };
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"start-pause" ~process:(-1) ~index:idx

let check_pause st =
  match st.running with
  | Paused { resume_after } when Unix.gettimeofday () >= resume_after ->
    st.running <- NoNemesis;
    let idx = st.entry_index in
    st.entry_index <- idx + 1;
    Some (make_nemesis ~nemesis_name:"stop-pause" ~process:(-1) ~index:idx)
  | _ -> None

(* ---- crash + restart ---- *)

let record_crash st =
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"crash" ~process:(-1) ~index:idx

let record_restart st =
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"restart" ~process:(-1) ~index:idx

(* ---- clock skew ---- *)

let clock_skew_active =
  try ignore (Sys.getenv "FAKETIME"); true
  with Not_found -> false

let record_clock_skew st =
  if clock_skew_active then
    let idx = st.entry_index in
    st.entry_index <- idx + 1;
    make_nemesis ~nemesis_name:"clock-skew" ~process:(-1) ~index:idx
  else
    make_nemesis ~nemesis_name:"no-clock-skew" ~process:(-1) ~index:0

(* ---- lazyfs (un-fsynced write loss) ---- *)

(** Path to the lazyfs binary. *)
let lazyfs_binary = ref "/usr/local/bin/lazyfs"

(** Type holding lazyfs runtime state needed across start/trigger/stop. *)
type lazyfs_state = {
  pid : int;
  mount_dir : string;
  fifo_path : string;
  config_path : string;
}

(** Generate a minimal TOML config for lazyfs with the given fifo path. *)
let write_lazyfs_config config_path fifo_path =
  let oc = open_out config_path in
  Printf.fprintf oc {|[faults]
fifo_path="%s"
[cache]
apply_eviction=false
[cache.simple]
custom_size="256mb"
blocks_per_page=1
[filesystem]
log_all_operations=false
logfile=""
|}
    fifo_path;
  close_out oc

(** Start a lazyfs FUSE mount.
    [mount_dir] is where the fused DB lives.  LazyFS 0.3.1+ uses an
    in-memory page cache — there is no separate backing directory.
    Returns a [lazyfs_state] record on success, or raises [Failure]
    on error. *)
let start_lazyfs mount_dir =
  let prog = !lazyfs_binary in
  let pid_str = string_of_int (Unix.getpid ()) in
  let fifo_path = Printf.sprintf "/tmp/lazyfs_fifo_%s" pid_str in
  let config_path = Printf.sprintf "/tmp/lazyfs_config_%s.toml" pid_str in
  (* Create the FIFO before starting lazyfs *)
  (try Unix.mkfifo fifo_path 0o666
   with Unix.Unix_error (EEXIST, _, _) -> ());
  write_lazyfs_config config_path fifo_path;
  let argv =
    [| prog; "-f"; mount_dir;
       "-o"; "allow_other";
       "-o"; "default_permissions";
       "--config-path"; config_path |]
  in
  let pid =
    Unix.create_process prog argv Unix.stdin Unix.stdout Unix.stderr
  in
  (* Give FUSE a moment to mount before handing control back *)
  Unix.sleep 2;
  if pid = 0 then
    failwith "lazyfs failed to start"
  else
    { pid; mount_dir; fifo_path; config_path }

(** Tell lazyfs to lose all writes that were not fsynced.
    Writes the 'clear-cache' command to the lazyfs FIFO. *)
let trigger_lose_unsynced st ls =
  (try
     let oc = open_out ls.fifo_path in
     output_string oc "lazyfs::clear-cache\n";
     close_out oc
   with _ -> ());
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"lose-unsynced" ~process:(-1) ~index:idx

(** Stop lazyfs by killing its process, cleaning up FIFO and config,
    and unmounting. *)
let stop_lazyfs ls =
  if ls.pid > 0 then begin
    (try Unix.kill ls.pid Sys.sigterm with _ -> ());
    Unix.sleep 1;
    (try Sys.remove ls.fifo_path with _ -> ());
    (try Sys.remove ls.config_path with _ -> ());
    (try ignore (Sys.command
                   (Printf.sprintf "fusermount -u %s 2>/dev/null" ls.mount_dir))
     with _ -> ())
  end
