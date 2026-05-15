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
  fd : Unix.file_descr;
  inode_key : int * int;
  mutable n_pages : int64;
}

let n_pages t = t.n_pages

let in_bounds t page_id =
  Int64.compare page_id 0L >= 0
  && Int64.compare page_id t.n_pages < 0

(* Read exactly [len] bytes into [buf] starting at [off]; loops on short reads. *)
let read_exactly fd buf off len =
  let rec loop off remaining =
    if remaining = 0 then ()
    else begin
      let n = Unix.read fd buf off remaining in
      if n = 0 then
        failwith "unexpected EOF";
      loop (off + n) (remaining - n)
    end
  in
  loop off len

(* Write exactly [len] bytes from [buf] starting at [off]; loops on short writes. *)
let write_exactly fd buf off len =
  let rec loop off remaining =
    if remaining = 0 then ()
    else begin
      let n = Unix.write fd buf off remaining in
      if n = 0 then
        failwith "unexpected write returned 0";
      loop (off + n) (remaining - n)
    end
  in
  loop off len

let open_ ~path =
  Lwt.return (
    try
      let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
      (try
         (* Build inode key from fstat — correct for hardlinks/symlinks. *)
         let st = Unix.fstat fd in
         let key = (st.Unix.st_dev, st.Unix.st_ino) in
         (* Check in-process lock table first. *)
         if Hashtbl.mem locked_inodes key then begin
           Unix.close fd;
           Error (Io "already open in this process")
         end else begin
           (* Try OS-level non-blocking exclusive lock (cross-process). *)
           (try Unix.lockf fd Unix.F_TLOCK 0
            with Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
              Unix.close fd;
              raise (Unix.Unix_error (Unix.EWOULDBLOCK, "lockf", path)));
           let size = Unix.lseek fd 0 Unix.SEEK_END in
           let n_pages = Int64.of_int (size / page_size) in
           Hashtbl.add locked_inodes key ();
           Ok { fd; inode_key = key; n_pages }
         end
       with
       | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
         (* fd already closed in the handler above; return error *)
         Error (Io "already locked")
       | Unix.Unix_error (e, _, _) ->
         Unix.close fd;
         Error (Io (Unix.error_message e)))
    with
    | Unix.Unix_error (e, _, _) ->
      Error (Io (Unix.error_message e))
  )

let close t =
  Lwt.return (
    try
      Unix.lockf t.fd Unix.F_ULOCK 0;
      Unix.close t.fd;
      Hashtbl.remove locked_inodes t.inode_key;
      Ok ()
    with
    | Unix.Unix_error (e, _, _) ->
      Error (Io (Unix.error_message e))
  )

let read_page t ~page_id buf =
  Lwt.return (
    if not (in_bounds t page_id) then
      Error (Out_of_bounds { page_id; n_pages = t.n_pages })
    else
      try
        let offset = Int64.mul page_id (Int64.of_int page_size) in
        let _ = Unix.lseek t.fd (Int64.to_int offset) Unix.SEEK_SET in
        let tmp = Bytes.create page_size in
        read_exactly t.fd tmp 0 page_size;
        Cstruct.blit_from_bytes tmp 0 buf 0 page_size;
        Ok ()
      with
      | Unix.Unix_error (e, _, _) ->
        Error (Io (Unix.error_message e))
      | Failure msg ->
        Error (Io msg)
  )

let write_page t ~page_id buf =
  Lwt.return (
    if not (in_bounds t page_id) then
      Error (Out_of_bounds { page_id; n_pages = t.n_pages })
    else
      try
        let offset = Int64.mul page_id (Int64.of_int page_size) in
        let _ = Unix.lseek t.fd (Int64.to_int offset) Unix.SEEK_SET in
        let tmp = Bytes.create page_size in
        Cstruct.blit_to_bytes buf 0 tmp 0 page_size;
        write_exactly t.fd tmp 0 page_size;
        Ok ()
      with
      | Unix.Unix_error (e, _, _) ->
        Error (Io (Unix.error_message e))
      | Failure msg ->
        Error (Io msg)
  )

let sync t =
  Lwt.return (
    try
      Unix.fsync t.fd;
      Ok ()
    with
    | Unix.Unix_error (e, _, _) ->
      Error (Io (Unix.error_message e))
  )

let resize t ~n_pages =
  Lwt.return (
    try
      let new_size = Int64.to_int (Int64.mul n_pages (Int64.of_int page_size)) in
      Unix.ftruncate t.fd new_size;
      t.n_pages <- n_pages;
      Ok ()
    with
    | Unix.Unix_error (e, _, _) ->
      Error (Io (Unix.error_message e))
  )
