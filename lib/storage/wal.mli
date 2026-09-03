(** Write-Ahead Log over byte-addressable storage.

    Frames are 4096-byte pages tagged with metadata; the WAL is a sequence
    of frames followed by an optional uncommitted tail. Each batch ends
    with a frame that has the [commit] bit set; recovery on open scans
    forward and accepts only frames whose commit batch is complete.

    Layout
    ------
    The WAL begins with a 24-byte header containing a magic identifier,
    a 64-bit salt assigned at open time when the WAL is empty, and a
    64-bit seed used by the per-frame checksum. Frame [i] occupies
    [header_size + i * frame_size_bytes] bytes.

    Per-frame layout:
        offset  0   page_id : uint64 big-endian
        offset  8   flags   : uint64 big-endian  (bit 0 = commit marker)
        offset 16   checksum: uint64 big-endian  (FNV-1a-64 of preceding
                                                  fields || salt || seed
                                                  || page bytes)
        offset 24   page    : 4096 bytes

    Crash-safety guarantee
    ----------------------
    A frame is considered durable only if (a) its checksum verifies and
    (b) some later frame in the same forward scan has the commit bit set
    without an intervening checksum failure. Partial trailing batches are
    silently discarded on recovery and overwritten by the next append.

    This module is purely an append-only frame store; integration with the
    pager (read-from-WAL routing, checkpointing back to the main DB) lives
    in the [Granary_store.Store] module. *)

type t

(** Pretty-print committed-frame count and current WAL size in bytes. *)
val pp : Format.formatter -> t -> unit

type frame =
  { frame_idx : int (** 0-based index in WAL *)
  ; page_id : int64
  ; is_commit : bool
  ; page : Cstruct.t (** 4096 bytes *)
  }

(** Size of one WAL frame for the DEFAULT 4096-byte geometry (24-byte frame
    header + 4096-byte page = 4120).  A WAL opened with a non-default
    [page_size] uses a proportionally larger frame internally (#95). *)
val frame_size_bytes : int

(** Size of a WAL frame header in bytes (24). *)
val header_size_bytes : int

type error =
  | Block_error of string
  | Corrupt_frame of int
  (** frame_idx of a structurally-present frame that failed integrity: a bad
      checksum on a directly-addressed read, or (on an encrypted WAL) a frame
      whose checksum passed but whose GCM tag failed — i.e. tampering (#219) *)

val pp_error : Format.formatter -> error -> unit

(** Open a WAL over the given byte-addressable callbacks. If the device is
    empty (or smaller than [header_size_bytes]) the WAL is initialised
    with a fresh salt and seed. Otherwise the header is read, and a
    forward scan recovers the index of every page in the last contiguous
    committed batch. A trailing partial batch is silently discarded.

    [page_size] (default {!Geometry.default}'s 4096) sets the frame's page
    payload size; it must match the main DB geometry (#95).

    [cipher] (default [None]): when supplied, the page payload of each frame
    is AES-256-GCM encrypted; the 24-byte meta header stays plaintext so
    keyless recovery forward-scans still work. The FNV checksum covers the
    whole payload region (ciphertext + nonce + tag). Encrypted frames are
    [Crypto.overhead] (32) bytes larger than plaintext frames. *)
val open_
  :  ?cipher:Crypto.t option
  -> ?page_size:int
  -> ?frame_cache_capacity:int
       (** Max decrypted frames cached for re-read (#246); 0 disables.
           Defaults from [GRANARY_WAL_FRAME_CACHE] (else 1024). *)
  -> ?resize:(int64 -> (unit, string) result Lwt.t)
       (** #612: shrink the WAL device to the given byte length.  Supplied, a
           checkpoint reclaims the file instead of leaving its size as a
           permanent high-water mark; omitted, the pre-#612 behaviour stands and
           the space is reused rather than returned.  Optional so the in-memory
           and fixed-extent device stubs need not implement it.  Called only
           from {!reset}, only after the generation-marker rotation is durable,
           and never with a target below [header_size_bytes]. *)
  -> read_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> size_bytes:int64
  -> unit
  -> (t, error) result Lwt.t

(** Total committed frames currently in the WAL. *)
val committed_frames : t -> int

(** Byte length of the WAL device as this handle understands it: reads past it
    are refused.  Grows as frames are appended and — since #612, and only when
    {!open_} was given a [resize] callback — drops back to [header_size_bytes]
    at {!reset}.

    It may lag the device's real length (a WAL that was never truncated is
    longer than this says, and harmlessly so) but must never {e lead} it within
    a generation: frame reads are bounds-checked against it, so understating it
    while the current generation still owns frames above the bound loses them.
    That is why {!reset} lowers it only when the truncation it pairs with
    actually succeeded. *)
val size_bytes : t -> int64

(** Number of successful device syncs since this WAL was opened.
    Exposed for #77 group-commit testing: tracks how many fsyncs the
    coordinator has issued. *)
val sync_count : t -> int

(** Most recent committed frame index for [page_id], or [None] if absent. *)
val find_page : t -> int64 -> int option

(** Snapshot-aware lookup: most recent frame index for [page_id] strictly
    less than [max_frame]. Used by RO snapshots so a reader captured at
    [committed_frames = max_frame] does not see frames written after.
    Returns [None] if no frame for [page_id] satisfies the bound. *)
val find_page_at : t -> int64 -> max_frame:int -> int option

(** Read the decrypted page bytes at a given frame index.

    {b Ownership (#246):} the returned Cstruct {b may be a cache-shared buffer}
    that the WAL retains in its decrypted-frame cache and hands to other readers.
    The caller MUST treat it as read-only — [cstruct_dup] it before any in-place
    mutation, or only borrow it read-only and drop it before the next
    {!reset}/checkpoint.  Mutating it in place would corrupt the cache and every
    concurrent borrower.  (The buffer is immutable for the life of the WAL
    generation; the cache is dropped wholesale on {!reset}.) *)
val read_frame : t -> int -> (Cstruct.t, error) result Lwt.t

(** Read the full frame metadata AND decrypted page bytes at a given
    committed frame index.  Like {!read_frame} but also returns the
    [page_id] and [is_commit] flag so the caller can reconstruct the
    full frame record (e.g. for incremental backup / frame capture).

    Raises [Corrupt_frame] for indices outside [0, committed_frames). *)
val read_committed_frame : t -> int -> (frame, error) result Lwt.t

(** Append a batch of pages; the LAST entry in the list is automatically
    marked as the commit frame. Performs a single [sync] at the end and
    only then updates the in-memory index. If [sync] fails the index is
    left unchanged so the partial batch is invisible to readers. *)
val append_commit : t -> (int64 * Cstruct.t) list -> (unit, error) result Lwt.t

(** Append a batch of pages but skip the trailing [sync].  The in-memory
    index and [committed_frames] are bumped immediately so concurrent
    writers (still under the store's [rw_mutex]) can locate the newly
    written frames and subsequent appends place their frames at the
    correct base.  Durability is deferred to a separate {!flush_sync}
    call by the group-commit coordinator.  Sync failure is treated as
    fatal by upstream callers — see [Granary_store.Store.commit]. *)
val append_commit_no_sync : t -> (int64 * Cstruct.t) list -> (unit, error) result Lwt.t

(** Invoke the underlying device sync.  Used by the group-commit
    coordinator after one or more {!append_commit_no_sync} calls. *)
val flush_sync : t -> (unit, error) result Lwt.t

(** Reset the WAL: discards all committed frames and the in-memory index.
    Used by checkpointing to truncate the log after migrating its contents to
    the main DB.  Bumps {!epoch} so that replication consumers can detect that
    their snapshot has been invalidated.

    {b #562: this also rotates the header's generation marker on disk and
    fsyncs it, which is why it is now an Lwt operation that can fail.}  The
    previous generation's frames no longer verify, so recovery stops at the new
    generation's tail instead of replaying them.  Before #562 they did replay,
    which (a) made a checkpoint invisible across a reopen, so every page kept
    resolving through the WAL overlay forever, and (b) silently resurrected
    pre-checkpoint page contents whenever the new generation was shorter than
    the old one.

    {b #612: when {!open_} was given a [resize] callback, the file is then
    physically truncated back to [header_size_bytes] and {!size_bytes} drops to
    match} — the two move together or not at all, because {!size_bytes} is what
    bounds-checks a frame read.  Without the callback the pre-#612 behaviour
    stands: the bytes stay and the next generation overwrites them, so the file
    size is a permanent high-water mark.

    The truncation runs {b after} the marker rotation is durable, and that order
    is the whole crash-safety argument: whatever fraction of it reaches the
    device, the surviving trailing bytes are old-generation frames that fail the
    new marker, so recovery reads the WAL as empty either way.  The reverse
    order is unsafe — a crash between a truncation and its rotation leaves a
    short file under the OLD marker, where the next generation's frames are
    indistinguishable from the old one's survivors.  The floor is
    [header_size_bytes], never 0, so the header — and with it the generation
    chain — always survives.

    A truncation failure is {b not} reported: it costs disk space, not
    correctness, and a failed [reset] fails the whole checkpoint.  {!size_bytes}
    is lowered only on success.

    The caller MUST hold the writer lock across this call: it yields on the
    header fsync, and an append landing in that window would be written under
    the old marker.

    {b On failure the WAL is POISONED (#636).}  If the header write or its
    fsync fails, which marker the device holds is unknown, and neither answer
    is safe to append under: had the new one landed, every later commit would
    be written and fsynced under a marker recovery rejects, and the application
    would be told those commits are durable.  So every subsequent append is
    refused with an error until the database is reopened.  Reads are
    unaffected.  Callers must therefore SURFACE this error rather than swallow
    it — see {!is_poisoned}.

    A [reset] over a WAL that has had nothing written to it since the last
    rotation is a no-op that touches no device: there is nothing to
    invalidate. *)
val reset : t -> (unit, error) result Lwt.t

(** [true] once a {!reset} has failed with its generation marker's durability
    unknown.  Every append is refused from that point until the database is
    reopened; reads are unaffected.  See {!reset}. *)
val is_poisoned : t -> bool

(** WAL generation counter — bumped every time [reset] is called (i.e. after
    each checkpoint).  Starts at 0 on open.  Replication can use this to
    detect whether a checkpoint expired its snapshot. *)
val epoch : t -> int64

(** Iterate over every (page_id, frame_idx) currently in the WAL index.
    Order is unspecified. *)
val iter_index : t -> (int64 -> int -> unit) -> unit

(** Compute the FNV-1a-64 checksum for a single WAL frame, given the source
    salt and seed.  Used by replication to verify transport integrity. *)
val frame_checksum
  :  salt:int64
  -> seed:int64
  -> page_id:int64
  -> flags:int64
  -> page:Cstruct.t
  -> int64

(** Return the WAL's salt (assigned at open). *)
val salt : t -> int64

(** Return the WAL's seed (assigned at open). *)
val seed : t -> int64

(** Override where a freshly created WAL's [(salt, seed)] generation marker is
    drawn from (#613).

    The default draws 16 bytes from {!Mirage_crypto_rng} and returns [None]
    when that generator is absent or unseeded, in which case the WAL falls back
    to OCaml's default [Random] state — which is IDENTICAL in every freshly
    started process, so every WAL created by such a process shares one marker.
    A Unix application gets a properly seeded generator from
    [Granary_unix.install]; a unikernel gets one from the Mirage runtime.

    Set this to supply your own entropy, or to make the marker reproducible for
    a fault-injection test. Returning [None] selects the degraded [Random]
    fallback described above. Process-global; call
    {!reset_initial_marker_source} to restore the default. *)
val set_initial_marker_source : (unit -> (int64 * int64) option) -> unit

(** Restore the default generation-marker source installed by this module
    (#613). *)
val reset_initial_marker_source : unit -> unit
