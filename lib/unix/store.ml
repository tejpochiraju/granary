(* Unix-file convenience constructors for {!Sqlocaml_store.Store}.  These build
   the pager block-IO closures over a [Unix_file] (and a WAL sidecar fd) and
   hand them to the platform-agnostic [Store.open_block]/[open_block_wal], so
   the store core itself carries no [unix] dependency (#170). *)

open Lwt.Syntax
module Core = Sqlocaml_store.Store

let err_of_unix (e : Unix_file.error) : Core.error =
  Core.Block_error (Format.asprintf "%a" Unix_file.pp_error e)
;;

let wrap_unit = function
  | Ok () -> Lwt.return_ok ()
  | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
;;

(* The four pager block-IO callbacks backed by [file], each mapping the file's
   typed error to the [string] error the pager expects. *)
let pager_ops file =
  let read_page ~page_id buf =
    let* r = Unix_file.read_page file ~page_id buf in
    wrap_unit r
  in
  let write_page ~page_id buf =
    let* r = Unix_file.write_page file ~page_id buf in
    wrap_unit r
  in
  let sync () =
    let* r = Unix_file.sync file in
    wrap_unit r
  in
  let resize ~n_pages =
    let* r = Unix_file.resize file ~n_pages in
    wrap_unit r
  in
  read_page, write_page, sync, resize
;;

(* A fresh (zero-length) file must be pre-sized to 2 pages so [Header.init] can
   write the two alternating header pages; [Unix_file] bound-checks writes
   against the current page count. *)
let ensure_sized file =
  if Int64.equal (Unix_file.n_pages file) 0L
  then
    let* _ = Unix_file.resize file ~n_pages:2L in
    Lwt.return_unit
  else Lwt.return_unit
;;

let open_file ~path : (Core.t, Core.error) result Lwt.t =
  let* fr = Unix_file.open_ ~path in
  match fr with
  | Error e -> Lwt.return_error (err_of_unix e)
  | Ok file ->
    (* A zero-length file is genuinely new and gets initialised; an existing
       file with corrupt headers must surface [Header_error] rather than be
       silently re-initialised, so only fresh files allow header init. *)
    let was_fresh = Int64.equal (Unix_file.n_pages file) 0L in
    let* () = ensure_sized file in
    let read_page, write_page, sync, resize = pager_ops file in
    let n_pages = Unix_file.n_pages file in
    let close () =
      let* _ = Unix_file.close file in
      Lwt.return_unit
    in
    Core.open_block
      ~init_if_corrupt:was_fresh
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages
      ~close
;;

(* [pread]/[pwrite]-style positioned I/O over the raw WAL sidecar fd.  Reading
   past EOF yields zeros for the remainder (the WAL grows lazily). *)
let wal_read_at fd ~offset (out : Cstruct.t) =
  let len = Cstruct.length out in
  try
    let _ = Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET in
    let tmp = Bytes.create len in
    let rec loop o r =
      if r = 0
      then ()
      else (
        let n = Unix.read fd tmp o r in
        if n = 0 then Bytes.fill tmp o r '\x00' else loop (o + n) (r - n))
    in
    loop 0 len;
    Cstruct.blit_from_bytes tmp 0 out 0 len;
    Lwt.return (Ok ())
  with
  | Unix.Unix_error (e, _, _) -> Lwt.return (Error (Unix.error_message e))
;;

let wal_write_at fd ~offset (src : Cstruct.t) =
  let len = Cstruct.length src in
  try
    let _ = Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET in
    let tmp = Bytes.create len in
    Cstruct.blit_to_bytes src 0 tmp 0 len;
    let rec loop o r =
      if r = 0
      then ()
      else (
        let n = Unix.write fd tmp o r in
        if n = 0 then failwith "short write" else loop (o + n) (r - n))
    in
    loop 0 len;
    Lwt.return (Ok ())
  with
  | Unix.Unix_error (e, _, _) -> Lwt.return (Error (Unix.error_message e))
;;

let open_file_wal ~path : (Core.t, Core.error) result Lwt.t =
  let* fr = Unix_file.open_ ~path in
  match fr with
  | Error e -> Lwt.return_error (err_of_unix e)
  | Ok file ->
    let wal_path = path ^ "-wal" in
    let wal_fd =
      try Unix.openfile wal_path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 with
      | Unix.Unix_error _ -> Unix.openfile wal_path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644
    in
    let wal_size_bytes = Int64.of_int (Unix.lseek wal_fd 0 Unix.SEEK_END) in
    let* () = ensure_sized file in
    let read_page, write_page, sync, resize = pager_ops file in
    let n_pages = Unix_file.n_pages file in
    let wal_sync () =
      try
        Unix.fsync wal_fd;
        Lwt.return_ok ()
      with
      | Unix.Unix_error (e, _, _) -> Lwt.return_error (Unix.error_message e)
    in
    let close () =
      let* _ = Unix_file.close file in
      Lwt.return_unit
    in
    let wal_close () =
      (try Unix.close wal_fd with
       | Unix.Unix_error _ -> ());
      Lwt.return_unit
    in
    Core.open_block_wal
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages
      ~wal_read_at:(wal_read_at wal_fd)
      ~wal_write_at:(wal_write_at wal_fd)
      ~wal_sync
      ~wal_size_bytes
      ~close
      ~wal_close
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
