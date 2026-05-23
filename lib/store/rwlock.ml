(* Writer-mutex + reader-counter built on Lwt_condition.

   Semantics (intentionally NOT a classic shared/exclusive lock):
   - Many readers run concurrently and never block.
   - Writers serialise against other writers via [writer_active].
   - Readers do NOT block writers.  Writers do NOT block readers.

   Rationale for #149: snapshot isolation makes reader/writer
   concurrency safe at the pager layer — the writer's dirty bytes and
   uncommitted WAL frames are invisible to a snapshot bounded by
   [committed_frames].  So the lock only needs to:
     (a) serialise concurrent writers, and
     (b) track active readers for the checkpoint coordinator (which
         lives in [Store] — see [active_reader_frames]). *)

open Lwt.Syntax

type t = {
  mutable readers         : int;
  mutable writer_active   : bool;
  mutable writers_waiting : int;
  cond                    : unit Lwt_condition.t;
}

let create () =
  { readers = 0;
    writer_active = false;
    writers_waiting = 0;
    cond = Lwt_condition.create () }

let readers t = t.readers
let writer_pending t = t.writer_active || t.writers_waiting > 0
let writer_active t = t.writer_active

(* Readers never wait — the snapshot-isolation work upstream makes
   concurrent reader/writer access safe at the data layer. *)
let acquire_read t =
  t.readers <- t.readers + 1;
  Lwt.return_unit

let release_read t =
  t.readers <- t.readers - 1;
  if t.readers = 0 then Lwt_condition.broadcast t.cond ()

(* Writers wait only on other writers, not on readers.  The decrement
   of [writers_waiting] and the recursive re-check are atomic w.r.t.
   the cooperative Lwt scheduler. *)
let rec acquire_write t =
  if t.writer_active then begin
    t.writers_waiting <- t.writers_waiting + 1;
    let* () = Lwt_condition.wait t.cond in
    t.writers_waiting <- t.writers_waiting - 1;
    acquire_write t
  end else begin
    t.writer_active <- true;
    Lwt.return_unit
  end

let release_write t =
  t.writer_active <- false;
  Lwt_condition.broadcast t.cond ()

let with_read t f =
  let* () = acquire_read t in
  Lwt.finalize f (fun () -> release_read t; Lwt.return_unit)

let with_write t f =
  let* () = acquire_write t in
  Lwt.finalize f (fun () -> release_write t; Lwt.return_unit)
