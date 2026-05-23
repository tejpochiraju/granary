(* Shared/exclusive lock built on Lwt_condition.  Writer-priority. *)

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

let rec acquire_read t =
  if t.writer_active || t.writers_waiting > 0 then
    let* () = Lwt_condition.wait t.cond in
    acquire_read t
  else begin
    t.readers <- t.readers + 1;
    Lwt.return_unit
  end

let release_read t =
  t.readers <- t.readers - 1;
  if t.readers = 0 then Lwt_condition.broadcast t.cond ()

(* The decrement of [writers_waiting] and the recursive re-check are
   atomic w.r.t. the cooperative Lwt scheduler: no other fiber can run
   between them because nothing yields. So while [writer_pending] may
   briefly return [false] between wakeup and re-entry on a non-cooperative
   scheduler, under Lwt no reader can slip in during that window. *)
let rec acquire_write t =
  if t.writer_active || t.readers > 0 then begin
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
