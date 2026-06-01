(** Fault-injecting wrapper around [Unix_file] — see .mli for details. *)

open Lwt.Syntax

type config =
  { fail_after_writes : int option
  ; fail_on_sync : bool
  }

let default_config = { fail_after_writes = None; fail_on_sync = false }

type t =
  { config : config
  ; mutable writes : int
  ; mutable faulted : bool
  }

let writes_completed t = t.writes
let faulted t = t.faulted

let pp fmt t =
  Format.fprintf fmt "@[<hv>{ writes = %d;@ faulted = %b }@]" t.writes t.faulted
;;

(* Convert a Unix_file.error into the (unit, string) shape that
   Db.open_block's callbacks expect. *)
let convert_unit_result = function
  | Ok () -> Ok ()
  | Error e -> Error (Format.asprintf "%a" Unix_file.pp_error e)
;;

(* The fault-injecting write callback: refuses once faulted, trips the
   sticky fault after [fail_after_writes] writes, else writes through. *)
let make_faulted_write t uf ~page_id buf =
  if t.faulted
  then Lwt.return (Error "injected crash (sticky)")
  else (
    match t.config.fail_after_writes with
    | Some n when t.writes >= n ->
      t.faulted <- true;
      Lwt.return (Error "injected crash")
    | _ ->
      let* r = Unix_file.write_page uf ~page_id buf in
      (match r with
       | Ok () ->
         t.writes <- t.writes + 1;
         Lwt.return (Ok ())
       | Error e -> Lwt.return (Error (Format.asprintf "%a" Unix_file.pp_error e))))
;;

(* The fault-injecting sync callback: refuses once faulted, optionally trips
   the sticky fault on sync, else syncs through. *)
let make_faulted_sync t uf () =
  if t.faulted
  then Lwt.return (Error "injected crash (sticky)")
  else if t.config.fail_on_sync
  then (
    t.faulted <- true;
    Lwt.return (Error "injected sync failure"))
  else
    let* r = Unix_file.sync uf in
    Lwt.return (convert_unit_result r)
;;

let open_with_faults ~path ~size_bytes ~config =
  (* Ensure the file exists and is sized to [size_bytes] bytes
     before opening it with Unix_file (which expects an already-sized
     file: it derives n_pages from the file length). *)
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.ftruncate fd size_bytes;
  Unix.close fd;
  let t = { config; writes = 0; faulted = false } in
  let* uf_result = Unix_file.open_ ~path () in
  match uf_result with
  | Error e ->
    Lwt.fail_with
      (Format.asprintf "Fault_inject: Unix_file.open_ failed: %a" Unix_file.pp_error e)
  | Ok uf ->
    let n_pages_init = Unix_file.n_pages uf in
    let read ~page_id buf =
      let* r = Unix_file.read_page uf ~page_id buf in
      Lwt.return (convert_unit_result r)
    in
    let write = make_faulted_write t uf in
    let sync = make_faulted_sync t uf in
    let resize ~n_pages =
      let* r = Unix_file.resize uf ~n_pages in
      Lwt.return (convert_unit_result r)
    in
    let close () =
      (* Swallow any error from the underlying close — the wrapper's
         [close] returns [unit Lwt.t] to match Db.open_block. *)
      let* r = Unix_file.close uf in
      match r with
      | Ok () -> Lwt.return_unit
      | Error _ -> Lwt.return_unit
    in
    Lwt.return (t, read, write, sync, resize, n_pages_init, close)
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
