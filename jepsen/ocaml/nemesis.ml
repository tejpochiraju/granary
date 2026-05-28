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
    [backing_dir] stores the actual written data.  Returns the lazyfs PID
    (0 on failure).  Uses Unix.create_process so we get the real PID
    without racing on pgrep. *)
let start_lazyfs mount_dir backing_dir =
  let prog = !lazyfs_binary in
  let argv =
    [| prog; "--port"; string_of_int !lazyfs_port; mount_dir; backing_dir |]
  in
  try
    let pid = Unix.create_process prog argv Unix.stdin Unix.stdout Unix.stderr in
    (* Give FUSE a moment to mount before handing control back *)
    Unix.sleep 1;
    pid
  with _ -> 0

(** Tell lazyfs to lose all writes that were not fsynced.
    Sends HTTP GET to lazyfs control port. *)
let trigger_lose_unsynced st =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, !lazyfs_port) in
  let buf = Bytes.create 4096 in
  (try
     let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
     Unix.connect sock addr;
     let _ = Unix.send sock (Bytes.of_string "GET /lose-unsynced HTTP/1.0\r\nHost: localhost\r\n\r\n") 0
               (String.length "GET /lose-unsynced HTTP/1.0\r\nHost: localhost\r\n\r\n") [] in
     ignore (Unix.recv sock buf 0 4096 []);
     Unix.close sock
   with _ -> ());
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"lose-unsynced" ~process:(-1) ~index:idx

(** Stop lazyfs by killing its process and unmounting. *)
let stop_lazyfs ?mount_dir pid =
  if pid > 0 then begin
    (try Unix.kill pid Sys.sigterm with _ -> ());
    Unix.sleep 1;
    let mount =
      match mount_dir with Some d -> d | None -> "/tmp/lazyfs_mount"
    in
    (* force unmount if it didn't clean up *)
    (try ignore (Sys.command (Printf.sprintf "fusermount -u %s 2>/dev/null" mount))
     with _ -> ())
  end
