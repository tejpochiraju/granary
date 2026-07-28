(** Page-level signal emitted by {!Granary_storage.Pager} for the internals
    monitor (#384).  Storage-local on purpose: [Store_event] lives a layer up in
    [granary.store], which depends on this library, so the pager cannot
    reference it without creating a dependency cycle.  [Store.set_event_callback]
    translates these into [Store_event.t] variants.

    Emitted on a pager-level backend resolution (the pager's own page cache
    missed): [Page_read] fires when the page is served from the MAIN FILE,
    [Wal_read] (#392) when it is served from the WAL overlay.  Neither fires on a
    pager cache hit.  Like [Page_read], [Wal_read] fires per pager-level
    resolution regardless of any cache internal to the WAL layer (its frame
    cache), mirroring how [Page_read] fires regardless of the OS/block-device
    cache under [read_page].  [Page_write] fires once per dirty page handed to
    the WAL/main on flush. *)
type t =
  | Page_read of { page_id : int64 }
  (** main-file backend read — pager cache MISS only; WAL-served reads emit
      [Wal_read] instead *)
  | Wal_read of { page_id : int64 }
  (** WAL-overlay backend read (#392) — pager cache MISS resolved from a WAL
      frame rather than the main file *)
  | Page_write of { page_id : int64 } (** page written to WAL/main on flush *)
  | Page_alloc of
      { page_id : int64
      ; reused : bool (** [true] = freelist/txn-pool reuse; [false] = file extend *)
      }
  | Page_free of { page_id : int64 } (** page pushed to the freelist / txn pool *)

let pp fmt = function
  | Page_read { page_id } -> Format.fprintf fmt "PAGE_READ page=%Ld" page_id
  | Wal_read { page_id } -> Format.fprintf fmt "WAL_READ page=%Ld" page_id
  | Page_write { page_id } -> Format.fprintf fmt "PAGE_WRITE page=%Ld" page_id
  | Page_alloc { page_id; reused } ->
    Format.fprintf fmt "PAGE_ALLOC page=%Ld reused=%b" page_id reused
  | Page_free { page_id } -> Format.fprintf fmt "PAGE_FREE page=%Ld" page_id
;;
