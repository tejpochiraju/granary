(** Page-level signal emitted by {!Sqlocaml_storage.Pager} for the internals
    monitor (#384).  Storage-local on purpose: [Store_event] lives a layer up in
    [sqlocaml.store], which depends on this library, so the pager cannot
    reference it without creating a dependency cycle.  [Store.set_event_callback]
    translates these into [Store_event.t] variants.

    Emitted only on PHYSICAL I/O: [Page_read] fires on a backend read (cache
    miss), never on a cache hit; [Page_write] fires once per dirty page handed to
    the WAL/main on flush. *)
type t =
  | Page_read of { page_id : int64 } (** backend read — cache MISS only *)
  | Page_write of { page_id : int64 } (** page written to WAL/main on flush *)
  | Page_alloc of
      { page_id : int64
      ; reused : bool (** [true] = freelist/txn-pool reuse; [false] = file extend *)
      }
  | Page_free of { page_id : int64 } (** page pushed to the freelist / txn pool *)
