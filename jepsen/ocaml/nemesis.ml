(** Nemesis (fault injection) module for Jepsen-style testing.

    Implements single-node faults:
    - Process pause (SIGSTOP/SIGCONT the harness itself)
    - Crash + restart (kill the worker subprocess, reopen the database)
    - Clock skew (via faketime environment)

    Each nemesis phase emits :info entries into the EDN history so
    checkers can correlate anomalies with faults. *)

open Edn_history

(** Representation of a running nemesis sleep timer. *)
type running =
  | NoNemesis
  | Paused of { mutable resume_after : float }

(** Current nemesis state. *)
type state = {
  mutable running : running;
  mutable entry_index : int;
}

let create () = { running = NoNemesis; entry_index = 0 }

(** Start a process-pause nemesis.  The harness should periodically
    check whether the pause has elapsed and record resume events.
    [duration_s] is how long to pause for. *)
let start_pause st duration_s =
  st.running <- Paused { resume_after = Unix.gettimeofday () +. duration_s };
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"start-pause" ~process:(-1) ~index:idx

(** Check whether a pause nemesis has finished, and if so,
    return the stop-pause event. *)
let check_pause st =
  match st.running with
  | Paused { resume_after } when Unix.gettimeofday () >= resume_after ->
    st.running <- NoNemesis;
    let idx = st.entry_index in
    st.entry_index <- idx + 1;
    Some (make_nemesis ~nemesis_name:"stop-pause" ~process:(-1) ~index:idx)
  | _ -> None

(** Record a crash event.  The harness should kill itself and restart. *)
let record_crash st =
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"crash" ~process:(-1) ~index:idx

(** Record a recovery/restart event. *)
let record_restart st =
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"restart" ~process:(-1) ~index:idx

(** Record a start of clock-skew phase. *)
let start_clock_skew st =
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"start-clock-skew" ~process:(-1) ~index:idx

(** Record end of clock-skew phase. *)
let stop_clock_skew st =
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"stop-clock-skew" ~process:(-1) ~index:idx

(** Record a lazyfs lose-unsynced event (un-fsynced write loss). *)
let record_lose_unsynced st =
  let idx = st.entry_index in
  st.entry_index <- idx + 1;
  make_nemesis ~nemesis_name:"lose-unsynced" ~process:(-1) ~index:idx

(** Fork a child process, run [fn] in the child, and return the child PID
    so the parent can kill it.  The parent waits for the child to finish
    and collects its exit status. *)
let run_in_child (fn : unit -> unit) : int * (unit -> Unix.process_status) =
  match Unix.fork () with
  | 0 ->
    (* Child: run the function, exit on exception *)
    (try fn () with _ -> ());
    exit 0
  | child_pid ->
    (* Parent: return PID and a wait function *)
    let wait_fn () =
      match Unix.waitpid [] child_pid with
      | _, status -> status
    in
    (child_pid, wait_fn)
