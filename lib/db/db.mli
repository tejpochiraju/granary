(** Public API for sqlocaml — pure-OCaml in-memory SQL engine (Phase 0). *)

type t

(** Re-export value type for convenience. *)
type value = Sqlocaml_encoding.Row.value =
  | V_int  of int64
  | V_text of string
  | V_null

type row = Sqlocaml_encoding.Row.t   (* value array *)

type error =
  | Parse   of string                (** SQL syntax error *)
  | Sema    of Sqlocaml_sql.Sema.error  (** name/type error *)
  | Runtime of string                (** unexpected internal error *)

val open_in_memory : unit -> t Lwt.t

val close : t -> unit Lwt.t

(** Execute a DDL or DML statement (CREATE TABLE, INSERT).
    Returns [Ok ()] on success, [Error e] on failure. *)
val execute : t -> string -> (unit, error) result Lwt.t

(** Execute a query (SELECT).
    Returns [Ok stream] on success, [Error e] on failure.
    The stream is lazy — rows are produced on demand. *)
val query : t -> string -> (row Lwt_stream.t, error) result Lwt.t
