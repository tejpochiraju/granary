(** Public API for sqlocaml — pure-OCaml in-memory SQL engine (Phase 0). *)

type t

(** Re-export value type for convenience. *)
type value = Sqlocaml_encoding.Row.value =
  | V_int  of int64
  | V_text of string
  | V_null
  | V_real of float
  | V_blob of bytes

type row = Sqlocaml_encoding.Row.t   (* value array *)

type error =
  | Parse   of string                (** SQL syntax error *)
  | Sema    of Sqlocaml_sql.Sema.error  (** name/type error *)
  | Runtime of string                (** unexpected internal error *)

val open_in_memory : unit -> t Lwt.t

(** Open a persistent B+-tree-backed database at the given file path.
    Creates the file if absent; reopens an existing database otherwise. *)
val open_file : path:string -> (t, error) result Lwt.t

(** Open a SQL engine on any block device given as I/O callbacks.
    Use with [Sqlocaml_mirage_block.Mirage_backend.Make(B)] to build
    the callbacks from a [Mirage_block.S] device.  Pass [~n_pages:0L]
    for Mirage adapters; the adapter handles device-capacity bounds
    internally.  [~close] is called by [Db.close]. *)
val open_block :
  read_page  : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  write_page : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  sync       : (unit -> (unit, string) result Lwt.t) ->
  resize     : (n_pages:int64 -> (unit, string) result Lwt.t) ->
  n_pages    : int64 ->
  close      : (unit -> unit Lwt.t) ->
  (t, error) result Lwt.t

val close : t -> unit Lwt.t

(** Execute a DDL or DML statement (CREATE TABLE, INSERT, UPDATE, ...).
    Returns [Ok ()] on success, [Error e] on failure. *)
val execute : t -> string -> (unit, error) result Lwt.t

(** Like [execute], but returns the rows-affected count.  For DDL the
    count is [0]; for INSERT it is [1]; for UPDATE it is the number of
    rows whose contents were modified. *)
val execute_change_count : t -> string -> (int, error) result Lwt.t

(** Execute a query (SELECT).
    Returns [Ok stream] on success, [Error e] on failure.
    The stream is lazy — rows are produced on demand. *)
val query : t -> string -> (row Lwt_stream.t, error) result Lwt.t
