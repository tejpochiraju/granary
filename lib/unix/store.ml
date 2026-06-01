(* Unix-file convenience constructors for {!Sqlocaml_store.Store}.  These build
   the pager block-IO closures over a [Unix_file] (and a WAL sidecar fd) and
   hand them to the platform-agnostic [Store.open_block]/[open_block_wal], so
   the store core itself carries no [unix] dependency (#170). *)

open Lwt.Syntax
module Core = Sqlocaml_store.Store
module Geometry = Sqlocaml_storage.Geometry
module Header = Sqlocaml_storage.Header

(* #95: resolve the geometry to open [file] with.  A fresh (zero-page) file uses
   the caller's [requested] geometry; an existing file's geometry is peeked from
   page 0 (page_size at byte 56, reserved at byte 64).  Reconfigures the file's
   addressing page size to match, then returns the geometry to hand to
   [Core.open_block].  Errors if the caller explicitly requested a geometry that
   disagrees with an existing file's. *)
let resolve_geometry file ~requested ~explicit =
  if Int64.equal (Unix_file.n_pages file) 0L
  then Lwt.return_ok requested
  else (
    let buf = Cstruct.create 4096 in
    let* r = Unix_file.read_page file ~page_id:0L buf in
    match r with
    | Error _ -> Lwt.return_ok requested (* let open_block surface header errors *)
    | Ok () ->
      (match Header.peek_geometry buf with
       | None -> Lwt.return_ok requested
       | Some existing ->
         if
           explicit
           && (existing.Geometry.page_size <> requested.Geometry.page_size
               || existing.reserved_bytes_per_page <> requested.reserved_bytes_per_page)
         then
           Lwt.return_error
             (Core.Block_error
                (Printf.sprintf
                   "page geometry mismatch: file is page_size=%d reserved=%d but \
                    page_size=%d reserved=%d was requested"
                   existing.page_size
                   existing.reserved_bytes_per_page
                   requested.Geometry.page_size
                   requested.reserved_bytes_per_page))
         else Lwt.return_ok existing))
;;

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

let open_file
      ?(page_size = 4096)
      ?(reserved_bytes_per_page = 0)
      ?(explicit_geometry = false)
      ?key
      ~path
      ()
  : (Core.t, Core.error) result Lwt.t
  =
  match Geometry.create ~page_size ~reserved_bytes_per_page with
  | Error e ->
    Lwt.return_error (Core.Block_error (Format.asprintf "%a" Geometry.pp_error e))
  | Ok requested ->
    let* fr = Unix_file.open_ ~path () in
    (match fr with
     | Error e -> Lwt.return_error (err_of_unix e)
     | Ok file ->
       (* A zero-length file is genuinely new and gets initialised; an existing
          file with corrupt headers must surface [Header_error] rather than be
          silently re-initialised, so only fresh files allow header init. *)
       let was_fresh = Int64.equal (Unix_file.n_pages file) 0L in
       let* gr = resolve_geometry file ~requested ~explicit:explicit_geometry in
       (match gr with
        | Error e -> Lwt.return_error e
        | Ok geom ->
          (* Configure addressing for this geometry before any page write. *)
          Unix_file.set_page_size file geom.Geometry.page_size;
          let* () = ensure_sized file in
          let read_page, write_page, sync, resize = pager_ops file in
          let n_pages = Unix_file.n_pages file in
          let close () =
            let* _ = Unix_file.close file in
            Lwt.return_unit
          in
          let* r =
            Core.open_block
              ?key
              ~geom
              ~init_if_corrupt:was_fresh
              ~read_page
              ~write_page
              ~sync
              ~resize
              ~n_pages
              ~close
              ()
          in
          (* On an open error (e.g. wrong/missing key) [Core.open_block] never
             wires [close] into a store, so close the fd here rather than leak it
             (otherwise a retry hits "Io already open in this process"). *)
          (match r with
           | Ok _ -> Lwt.return r
           | Error _ ->
             let* _ = Unix_file.close file in
             Lwt.return r)))
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

let open_file_wal
      ?(page_size = 4096)
      ?(reserved_bytes_per_page = 0)
      ?(explicit_geometry = false)
      ?key
      ~path
      ()
  : (Core.t, Core.error) result Lwt.t
  =
  match Geometry.create ~page_size ~reserved_bytes_per_page with
  | Error e ->
    Lwt.return_error (Core.Block_error (Format.asprintf "%a" Geometry.pp_error e))
  | Ok requested ->
    let* fr = Unix_file.open_ ~path () in
    (match fr with
     | Error e -> Lwt.return_error (err_of_unix e)
     | Ok file ->
       let* gr = resolve_geometry file ~requested ~explicit:explicit_geometry in
       (match gr with
        | Error e -> Lwt.return_error e
        | Ok geom ->
          Unix_file.set_page_size file geom.Geometry.page_size;
          let wal_path = path ^ "-wal" in
          let wal_fd =
            try Unix.openfile wal_path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 with
            | Unix.Unix_error _ ->
              Unix.openfile wal_path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644
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
          let* r =
            Core.open_block_wal
              ?key
              ~geom
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
              ()
          in
          (* On an open error close both fds rather than leak them (see
             [open_file]); [Core.open_block_wal] does not wire close on error. *)
          (match r with
           | Ok _ -> Lwt.return r
           | Error _ ->
             let* () = wal_close () in
             let* _ = Unix_file.close file in
             Lwt.return r)))
;;

(** Convenience: copy the entire store to a new file at [dest].
    Writes to a temporary file first, then atomically renames to [dest]
    (with a directory fsync), so [dest] is never left in a partial state.
    The destination is a self-contained DB with no WAL sidecar — it opens
    standalone via {!open_file}. *)
let copy_to_file (src : Core.t) ~dest : (unit, Core.error) result Lwt.t =
  let open Lwt.Syntax in
  let tmp = dest ^ ".tmp" in
  let n = Core.n_pages src in
  let page_size =
    match Core.geometry src with
    | g -> g.Geometry.page_size
  in
  let* fr = Unix_file.open_ ~path:tmp () in
  match fr with
  | Error e ->
    Lwt.return_error (Core.Block_error (Format.asprintf "%a" Unix_file.pp_error e))
  | Ok file ->
    (* A leftover .tmp from a previous crash may be larger than n pages,
       but resize(n) calls ftruncate so it cuts back to exactly n. *)
    Unix_file.set_page_size file page_size;
    let* rr =
      if Int64.compare n 0L > 0
      then Unix_file.resize file ~n_pages:n
      else Lwt.return (Ok ())
    in
    (match rr with
     | Error e ->
       let* _ = Unix_file.close file in
       let (_ : unit Lwt.t) = Lwt_unix.unlink tmp in
       Lwt.return_error (Core.Block_error (Format.asprintf "%a" Unix_file.pp_error e))
     | Ok () ->
       let write_page ~page_id buf =
         let* r = Unix_file.write_page file ~page_id buf in
         match r with
         | Ok () -> Lwt.return_ok ()
         | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
       in
       let sink : Core.page_sink =
         fun ~page_id ~page ->
         let open Lwt.Syntax in
         let* r = write_page ~page_id page in
         match r with
         | Ok () -> Lwt.return_unit
         | Error msg ->
           Lwt.fail_with (Printf.sprintf "copy_to_file write pg=%Ld: %s" page_id msg)
       in
       Lwt.catch
         (fun () ->
            let* () = Core.copy_to src sink in
            let* sr = Unix_file.sync file in
            match sr with
            | Ok () ->
              let* _ = Unix_file.close file in
              let* () = Lwt_unix.rename tmp dest in
              let dir_path = Filename.dirname dest in
              Lwt.catch
                (fun () ->
                   let* dir_fd = Lwt_unix.openfile dir_path [ Unix.O_RDONLY ] 0 in
                   let* () = Lwt_unix.fsync dir_fd in
                   let* () = Lwt_unix.close dir_fd in
                   Lwt.return_ok ())
                (fun exn ->
                   (* rename succeeded — dest is the live file, but the
                     directory entry may not be durable.  Return an error
                     rather than silently swallowing the fsync failure. *)
                   Lwt.return_error
                     (Core.Block_error
                        (Printf.sprintf
                           "dir fsync after rename: %s"
                           (Printexc.to_string exn))))
            | Error e ->
              let* _ = Unix_file.close file in
              let (_ : unit Lwt.t) = Lwt_unix.unlink tmp in
              Lwt.return_error
                (Core.Block_error (Format.asprintf "%a" Unix_file.pp_error e)))
         (fun exn ->
            let* _ = Unix_file.close file in
            let (_ : unit Lwt.t) = Lwt_unix.unlink tmp in
            let msg = Printexc.to_string exn in
            Lwt.return_error (Core.Block_error msg)))
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
