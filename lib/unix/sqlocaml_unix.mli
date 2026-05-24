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
    provider for this process.  Creates the file if absent. *)
val open_file : path:string -> (Sqlocaml.Db.t, Sqlocaml.Db.error) result Lwt.t

(** Open a persistent WAL-mode database ([path] for the main DB, [path ^ "-wal"]
    for the WAL), registering the file provider.  Crash recovery runs
    automatically at open. *)
val open_file_wal : path:string -> (Sqlocaml.Db.t, Sqlocaml.Db.error) result Lwt.t
