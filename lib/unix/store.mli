(** Unix-file convenience constructors for {!Sqlocaml_store.Store}.
    They build pager block-IO closures over a [Unix_file] (and a WAL sidecar)
    and hand them to the platform-agnostic [Store.open_block]/[open_block_wal],
    so the store core stays free of any [unix] dependency (#170). *)

(** Open a B+-tree store over a Unix file at [path].  Creates and pre-sizes the
    file if absent; otherwise reopens an existing sqlocaml database. *)
val open_file
  :  path:string
  -> (Sqlocaml_store.Store.t, Sqlocaml_store.Store.error) result Lwt.t

(** Open a WAL-mode store using [path] for the main DB and [path ^ "-wal"] for
    the WAL.  Crash recovery on the WAL runs automatically at open. *)
val open_file_wal
  :  path:string
  -> (Sqlocaml_store.Store.t, Sqlocaml_store.Store.error) result Lwt.t
