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

(** HTTP port lazyfs listens on for lose-unsynced commands. *)
let lazyfs_port = ref 5555

(** Start a lazyfs FUSE mount.  [mount_dir] is where the fused DB lives;
    [backing_dir] stores the actual written data.  Returns the lazyfs PID. *)
let start_lazyfs mount_dir backing_dir =
  let cmd =
    Printf.sprintf "%s --port %d %s %s &"
      !lazyfs_binary !lazyfs_port mount_dir backing_dir
  in
  let _ = Sys.command cmd in
  (* Give lazyfs a moment to mount *)
  Unix.sleep 1;
  (* Return the PID by reading pgrep *)
  let pid =
    try
      let ic = Unix.open_process_in "pgrep -f lazyfs" in
      let line = input_line ic in
      let _ = close_in ic in
      int_of_string line
    with _ -> 0
  in
  pid

(** Tell lazyfs to lose all writes that were not fsynced.
    Sends HTTP GET to lazyfs control port. *)
let trigger_lose_unsynced st =
  let cmd =
    Printf.sprintf "curl -s http://localhost:%d/lose-unsynced 2>/dev/null"
      !lazyfs_port
  in
  let _ = Sys.command cmd in
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"lose-unsynced" ~process:(-1) ~index:idx

(** Stop lazyfs by killing its process. *)
let stop_lazyfs pid =
  if pid > 0 then
    let _ = Sys.command (Printf.sprintf "kill %d 2>/dev/null" pid) in
    Unix.sleep 1;
    let _ = Sys.command (Printf.sprintf "fusermount -u /tmp/lazyfs_mount 2>/dev/null") in
    ()

(* ---- subprocess helper ---- *)

let run_in_child (fn : unit -> unit) : int * (unit -> Unix.process_status) =
  match Unix.fork () with
  | 0 ->
    (try fn () with _ -> ());
    exit 0
  | child_pid ->
    let wait_fn () =
      match Unix.waitpid [] child_pid with
      | _, status -> status
    in
    (child_pid, wait_fn)
