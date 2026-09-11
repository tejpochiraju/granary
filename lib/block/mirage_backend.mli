(** Mirage_block.S adapter.  Wraps any [Mirage_block.S] implementation
    so it can be used as a granary block backend via [Store.open_block].

    {b Durability (#772).}  [Mirage_block.S] has exactly four operations —
    [get_info], [read], [write], [disconnect] — and no flush or barrier of any
    kind, so by itself this adapter {e cannot} make a write durable.  It does
    not pretend otherwise: with [~barrier:None], {!Make.sync} returns
    [Error] and {!Make.durability_barrier} reports [`Unavailable], which
    [Store.open_block]/[Store.open_block_wal] turn into a refusal of any
    durability level above [Off].  Pass the adapter's
    {!Make.durability_barrier} straight to their [~barrier] argument.

    Usage:
      module MB = Mirage_backend.Make(Block)   (* Block = mirage-block-unix *)
      let* dev = Block.connect path in
      let* adapter = MB.connect ~barrier:None dev in
      let* store = Store.open_block
        ~barrier:(MB.durability_barrier adapter)
        ~durability:Store.Off      (* required while the barrier is missing *)
        ~read_page:(MB.read_page adapter)
        ~write_page:(MB.write_page adapter)
        ~sync:(MB.sync adapter)
        ~resize:(MB.resize adapter)
        ~n_pages:(MB.n_pages adapter)
        ~close:(fun () -> MB.close adapter) in
      ...
*)

(** The reason text reported by a barrier-less adapter, from both
    {!Make.sync}'s [Error] and {!Make.durability_barrier}'s [`Unavailable].
    Exposed so the refusal a caller sees at open time and the message a stray
    [sync] returns can be pinned to one string (#772). *)
val no_barrier_reason : string

module Make (B : Mirage_block.S) : sig
  (** Adapter state: wraps [B.t] with page-granularity access. *)
  type t

  (** [connect ?page_size ~barrier dev] reads [get_info] from [dev] to
      determine sector size and capacity, then creates an adapter with logical
      [n_pages = 0].  [page_size] defaults to 4096 (#95).  Raises [Failure] if
      [page_size] is not a multiple of the device's [sector_size].

      [barrier] (#772) is the platform-supplied flush this adapter has no way
      to obtain from [B] itself: a [mirage-block-unix] caller can pass an
      [fsync], and a Solo5 build can pass a stub for a block-flush hypercall,
      without [Mirage_block.S] changing.  [Some f] makes [f] the adapter's
      {!sync} and reports [`Available]; [None] declares that this device cannot
      flush, so {!sync} returns [Error] and the adapter reports [`Unavailable].

      (#785) It is deliberately {e mandatory} rather than defaulted.  A default
      lets a caller written before #772 keep compiling while the meaning of its
      wiring has changed underneath it; being forced to write [~barrier:None]
      puts the decision — and this doc comment — in front of the one caller who
      knows the answer.  Pass {!durability_barrier} to
      [Store.open_block]/[Store.open_block_wal]'s [~barrier] immediately after.

      What that closes is the {e representable} half, and only at this
      boundary: an adapter can no longer exist without having said what it can
      do.  The other half is a documented residual, not an unreachable state.
      [~barrier:None] here, followed by a [Store.open_block] that says nothing
      about [~barrier], still compiles — that argument defaults to
      [`Available], which is right for [Unix_file] and wrong for this
      adapter — so the store believes in a barrier every {!sync} then refuses.
      Under the default [Full] durability the store opens and the first commit
      fails, carrying {!no_barrier_reason}.  Under [~durability:Off] the store
      substitutes a no-op for [sync] only when it has been TOLD the barrier is
      missing, so nothing is substituted here either: a non-WAL store still
      fails at its first commit (that path syncs inline at every commit
      whatever the level), and a [Store.open_block_wal] store opens, commits,
      and then fails at its first checkpoint or at [Store.close]'s final
      flush — the "opens fine, cannot close" shape [Store.resolve_barrier_wal]
      exists to prevent, arrived at from the other side.  Both shapes are loud
      failures rather than a silent durability lie, which is why the residual
      is accepted rather than closed by making the store-side argument
      mandatory too.  The case
      ["Store.open_block's ?barrier default still believes the adapter"] in
      [test/test_mirage_sync_durability_772.ml] pins it, so changing that
      default is a deliberate act rather than an accident. *)
  val connect
    :  ?page_size:int
    -> barrier:(unit -> (unit, string) result Lwt.t) option
    -> B.t
    -> t Lwt.t

  val n_pages : t -> int64

  (** This adapter's page size in bytes (#95). *)
  val page_size : t -> int

  val read_page : t -> page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t

  (** Issue a durability barrier.  With a [~barrier] from {!connect} this is
      that function; without one it returns [Error {!no_barrier_reason}] rather
      than the [Ok ()] it returned before #772 — an adapter that cannot flush
      must not report success. *)
  val sync : t -> unit -> (unit, string) result Lwt.t

  (** Whether this adapter can make a write durable (#772).  Pass straight to
      [Store.open_block]/[Store.open_block_wal]'s [~barrier]. *)
  val durability_barrier : t -> [ `Available | `Unavailable of string ]

  (** [resize t ~n_pages] validates [n_pages <= capacity] and updates the
      logical count. The physical device is not modified. *)
  val resize : t -> n_pages:int64 -> (unit, string) result Lwt.t

  val close : t -> unit Lwt.t
end
