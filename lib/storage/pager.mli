(** Page cache + allocator over a BLOCK backend.
    Holds dirty pages in memory until [flush] is called. *)

type t
type error = Block_error of string | Corruption of string

val pp_error : Format.formatter -> error -> unit

(** Create a pager over an open Unix_file or Mem block device.
    [n_pages]: current total pages in the file (from header, or 0 for empty file).
    [freelist]: the current freelist state (deserialized from the file, or [Freelist.empty]). *)
val create :
  read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  sync:(unit -> (unit, string) result Lwt.t) ->
  resize:(n_pages:int64 -> (unit, string) result Lwt.t) ->
  n_pages:int64 ->
  freelist:Freelist.t ->
  t

(** Read a page. Returns cached copy if present; reads from BLOCK otherwise.
    The returned Cstruct.t is a fresh copy — caller may modify it freely. *)
val read : t -> int64 -> (Cstruct.t, error) result Lwt.t

(** Mark a page as dirty with new contents. Buffered until [flush].
    Does not write to BLOCK immediately. The Cstruct.t is copied internally. *)
val write : t -> int64 -> Cstruct.t -> unit

(** Allocate a new page ID. Tries freelist first; extends file if empty.
    Does not write to BLOCK — caller must [write] the allocated page contents. *)
val alloc : t -> current_txn_id:int64 -> (int64, error) result Lwt.t

(** Free a page (add to in-memory freelist with [freed_at_txn_id]). *)
val free : t -> page_id:int64 -> freed_at_txn_id:int64 -> unit

(** Flush all dirty pages to BLOCK: write each dirty page, then sync.
    Clears the dirty set after success. *)
val flush : t -> (unit, error) result Lwt.t

(** Current total page count (updated by [alloc]). *)
val n_pages : t -> int64

(** Current freelist state (for serialisation into header). *)
val freelist : t -> Freelist.t
