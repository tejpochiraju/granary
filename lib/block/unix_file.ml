open Lwt.Syntax

let page_size = 4096

type error =
  | Io of string
  | Out_of_bounds of { page_id : int64; n_pages : int64 }

let pp_error fmt = function
  | Io msg ->
    Format.fprintf fmt "Io %s" msg
  | Out_of_bounds { page_id; n_pages } ->
    Format.fprintf fmt
      "Out_of_bounds page_id=%Ld n_pages=%Ld" page_id n_pages

(* In-process lock table: tracks open files by (st_dev, st_ino) inode key.
   POSIX lockf is per-process, so two opens from the same process won't
   conflict via lockf alone.  We guard against that here.
   Using the inode key is portable and correct for hardlinks/symlinks. *)
let locked_inodes : (int * int, unit) Hashtbl.t = Hashtbl.create 16

type t = {
  fd : Lwt_unix.file_descr;
  inode_key : int * int;
  mutable n_pages : int64;
}

let n_pages t = t.n_pages

let pp fmt t = Format.fprintf fmt "Unix_file.t { n_pages = %Ld }" t.n_pages

let in_bounds t page_id =
  Int64.compare page_id 0L >= 0
  && Int64.compare page_id t.n_pages < 0

(* [pread]/[pwrite] avoid the lseek+read race that the previous Unix-based
   implementation papered over with Lwt's cooperative-only assumption.
   With concurrent fibers each driving their own offsets, the kernel
   syscall now carries the offset so two reads never trample each other. *)
let pread_exactly fd ~file_offset buf off len =
  let rec loop o r abs_off =
    if r = 0 then Lwt.return_unit
    else
      let* n = Lwt_unix.pread fd buf ~file_offset:abs_off o r in
      if n = 0 then Lwt.fail (Failure "unexpected EOF")
      else loop (o + n) (r - n) (abs_off + n)
  in
  loop off len file_offset

let pwrite_exactly fd ~file_offset buf off len =
  let rec loop o r abs_off =
    if r = 0 then Lwt.return_unit
    else
      let* n = Lwt_unix.pwrite fd buf ~file_offset:abs_off o r in
      if n = 0 then Lwt.fail (Failure "unexpected write returned 0")
      else loop (o + n) (r - n) (abs_off + n)
  in
  loop off len file_offset

(* Close [fd], swallowing any error — callers use this only on the
   cleanup-and-error path, where the original error is what we want to
   report. *)
let close_silently fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd)
    (fun _ -> Lwt.return_unit)

let open_ ~path =
  Lwt.catch
    (fun () ->
      let* fd =
        Lwt_unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o644
      in
      Lwt.catch
        (fun () ->
          let* st = Lwt_unix.fstat fd in
          let key = (st.Unix.st_dev, st.Unix.st_ino) in
          if Hashtbl.mem locked_inodes key then
            let* () = close_silently fd in
            Lwt.return_error (Io "already open in this process")
          else
            Lwt.catch
              (fun () ->
                let* () = Lwt_unix.lockf fd Unix.F_TLOCK 0 in
                let* size = Lwt_unix.lseek fd 0 Unix.SEEK_END in
                let n_pages = Int64.of_int (size / page_size) in
                Hashtbl.add locked_inodes key ();
                Lwt.return_ok { fd; inode_key = key; n_pages })
              (function
                | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
                  let* () = close_silently fd in
                  Lwt.return_error (Io "already locked")
                | Unix.Unix_error (e, _, _) ->
                  let* () = close_silently fd in
                  Lwt.return_error (Io (Unix.error_message e))
                | exn -> Lwt.fail exn))
        (function
          | Unix.Unix_error (e, _, _) ->
            let* () = close_silently fd in
            Lwt.return_error (Io (Unix.error_message e))
          | exn -> Lwt.fail exn))
    (function
      | Unix.Unix_error (e, _, _) ->
        Lwt.return_error (Io (Unix.error_message e))
      | exn -> Lwt.fail exn)

let close t =
  Lwt.catch
    (fun () ->
      let* () = Lwt_unix.lockf t.fd Unix.F_ULOCK 0 in
      let* () = Lwt_unix.close t.fd in
      Hashtbl.remove locked_inodes t.inode_key;
      Lwt.return_ok ())
    (function
      | Unix.Unix_error (e, _, _) ->
        Lwt.return_error (Io (Unix.error_message e))
      | exn -> Lwt.fail exn)

let read_page t ~page_id buf =
  if not (in_bounds t page_id) then
    Lwt.return_error (Out_of_bounds { page_id; n_pages = t.n_pages })
  else
    Lwt.catch
      (fun () ->
        let offset =
          Int64.to_int (Int64.mul page_id (Int64.of_int page_size))
        in
        let tmp = Bytes.create page_size in
        let* () = pread_exactly t.fd ~file_offset:offset tmp 0 page_size in
        Cstruct.blit_from_bytes tmp 0 buf 0 page_size;
        Lwt.return_ok ())
      (function
        | Unix.Unix_error (e, _, _) ->
          Lwt.return_error (Io (Unix.error_message e))
        | Failure msg ->
          Lwt.return_error (Io msg)
        | exn -> Lwt.fail exn)

let write_page t ~page_id buf =
  if not (in_bounds t page_id) then
    Lwt.return_error (Out_of_bounds { page_id; n_pages = t.n_pages })
  else
    Lwt.catch
      (fun () ->
        let offset =
          Int64.to_int (Int64.mul page_id (Int64.of_int page_size))
        in
        let tmp = Bytes.create page_size in
        Cstruct.blit_to_bytes buf 0 tmp 0 page_size;
        let* () = pwrite_exactly t.fd ~file_offset:offset tmp 0 page_size in
        Lwt.return_ok ())
      (function
        | Unix.Unix_error (e, _, _) ->
          Lwt.return_error (Io (Unix.error_message e))
        | Failure msg ->
          Lwt.return_error (Io msg)
        | exn -> Lwt.fail exn)

let sync t =
  Lwt.catch
    (fun () ->
      let* () = Lwt_unix.fsync t.fd in
      Lwt.return_ok ())
    (function
      | Unix.Unix_error (e, _, _) ->
        Lwt.return_error (Io (Unix.error_message e))
      | exn -> Lwt.fail exn)

let resize t ~n_pages =
  Lwt.catch
    (fun () ->
      let new_size =
        Int64.to_int (Int64.mul n_pages (Int64.of_int page_size))
      in
      let* () = Lwt_unix.ftruncate t.fd new_size in
      t.n_pages <- n_pages;
      Lwt.return_ok ())
    (function
      | Unix.Unix_error (e, _, _) ->
        Lwt.return_error (Io (Unix.error_message e))
      | exn -> Lwt.fail exn)

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
