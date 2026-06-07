(** Unix platform driver for sqlocaml (#170): file-backed database
    constructors plus the file provider that powers ATTACH and VACUUM on the
    otherwise platform-agnostic core. *)

(** Unix-file BLOCK backend (pread/pwrite over a Unix fd). *)
module Unix_file = Unix_file

(** Fault-injecting wrapper around {!Unix_file} for crash-recovery tests. *)
module Fault_inject = Fault_inject

(** Unix-file convenience constructors for {!Sqlocaml_store.Store}. *)
module Store = Store

(** The Unix file provider, exposed for explicit installation. *)
val provider : Sqlocaml.Db.file_provider

(** Install the process-wide file provider (see
    {!Sqlocaml.Db.set_file_provider}) so ATTACH and VACUUM can touch the local
    filesystem.  Idempotent; also called automatically by {!open_file} /
    {!open_file_wal}. *)
val install : unit -> unit

(** Open a persistent B+-tree database at file [path], registering the file
    provider for this process.  Creates the file if absent.

    [page_size] (default 4096, a multiple of 4096 up to 65536) and
    [reserved_bytes_per_page] (default 0) set the page geometry when CREATING a
    new file (#95); they are immutable thereafter and ignored when reopening an
    existing file (whose stored geometry is used).  Reopening with an explicit
    geometry that disagrees with the file is rejected.

    [clock] is forwarded to {!Sqlocaml.Db.of_store} (used by datetime).  Note:
    durability applies to WAL mode only; the non-WAL commit path syncs inline
    and ignores the sync mode, so {!open_file} takes no [durability] (#298). *)
val open_file
  :  ?page_size:int
  -> ?reserved_bytes_per_page:int
  -> ?clock:(unit -> float)
  -> path:string
  -> unit
  -> (Sqlocaml.Db.t, Sqlocaml.Db.error) result Lwt.t

(** Open a persistent WAL-mode database ([path] for the main DB, [path ^ "-wal"]
    for the WAL), registering the file provider.  Crash recovery runs
    automatically at open.  Geometry arguments behave as in {!open_file} (#95).
    [clock] and [durability] are forwarded to {!Sqlocaml.Db.of_store} (#298). *)
val open_file_wal
  :  ?page_size:int
  -> ?reserved_bytes_per_page:int
  -> ?clock:(unit -> float)
  -> ?durability:Sqlocaml_store.Store.durability
  -> path:string
  -> unit
  -> (Sqlocaml.Db.t, Sqlocaml.Db.error) result Lwt.t
