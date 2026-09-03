(** Writer-lock wait/hold accounting, attributed by acquisition site (#718).

    {!Rwlock} serialises writers against each other and never blocks readers,
    so the writer lock is the engine's only global critical section — and
    nothing in the tree could see inside it.  {!Granary_tpc.Tpcc_stmt_profile}
    measures {e service time}, elapsed from call to promise resolution, which
    cuts across the lock in both directions: [Store.commit_wal] releases the
    lock {e before} it fsyncs, so a large share of a [COMMIT]'s service time is
    held outside the critical section, and a [BEGIN] that finds the lock held is
    waiting rather than working.  #716 read "75.3% of NewOrder service time is
    transaction control" off that measurement; "75.3% of the {e critical
    section}" does not follow from it, and this module is what tells the two
    apart.

    {1 What a clock buys, and what it does not}

    Durations come from a clock the embedder installs through
    {!Granary_store.Store.set_clock}, defaulting to one that returns [0.] — the
    same contract the batched-durability time trigger already runs under, since
    pure-Mirage has no ambient clock.  So on a build with no clock every
    duration in a {!report} reads [0.], and {!report}'s [clock_installed] field
    exists so a reader cannot mistake that for "no contention".

    {b The contention counts need no clock and are always valid.}
    {!note_acquired}'s [contended] argument comes from
    {!Granary_store.Rwlock.writer_active}, which is exact: [acquire_write]
    blocks if and only if a writer holds the lock when it is called.  So
    [contended] and the [blocked_by] matrix are trustworthy on a pure-Mirage
    build, where the durations are not.

    {1 Attribution}

    A wait is attributed to whoever held the lock at the moment the wait
    {e began}.  A wait that spans several successive holders is therefore
    attributed entirely to the first of them.  That is an approximation and it
    is the one this module makes deliberately: {!Rwlock.acquire_write} wakes
    every waiter on release and lets whichever fiber the scheduler runs first
    win, so there is no queue position to read and no cheap way to apportion one
    wait across the holders that elapsed during it.  For the question this was
    built to answer — is a [BEGIN]'s wait the background autocheckpoint or the
    previous terminal's transaction — the first holder is the answer.

    A {e hold} is attributed to the site that acquired the lock, which is not
    always the code that releases it: {!Txn} is acquired by [Store.rw_begin] and
    released by [Store.commit] or [Store.rollback], so one {!Txn} hold spans a
    whole transaction — [BEGIN] through the mid-[COMMIT] unlock.  That span is
    exactly the critical section a sibling terminal queues behind, which is why
    it is the unit measured rather than per-statement holds. *)

(** Where an acquisition of the writer lock came from.  One constructor per
    [Rwlock.acquire_write] call site in {!Granary_store.Store}; a new call site
    owes itself a constructor here rather than borrowing one, since an
    acquisition that misreports its site corrupts the [blocked_by] matrix in
    both directions at once. *)
type site =
  | Txn
  (** [Store.rw_begin], released by [Store.commit]/[Store.rollback] — the
          whole transaction's critical section.

          {b One bucket for read-only and read-write transactions alike, and
          that is the subdivision an occupancy conclusion most wants.}
          [Db.begin_txn] calls [Store.rw_begin] for every explicit [BEGIN],
          read-only included, so a transaction that never writes still takes
          the exclusive lock and its hold lands here.  In the #718 artifact
          that folds TPC-C's [order_status] and [stock_level] — 768.989 ms of
          10 034.406 ms of attributed service time, up to ~7.7 of the 74.7
          points of [Txn] hold — into a figure that reads as workload.  So
          this report can say how much of the interval the critical section
          occupies, but NOT how much of it is needlessly exclusive.

          Splitting it is not a matter of adding a constructor: read-only-ness
          is not known at acquire time (SQL [BEGIN] does not declare it) and is
          only decidable at release, whereas the site is what a WAITER reads to
          fill [blocked_by] while the hold is still in flight.  A split
          therefore has to reclassify a hold retroactively, which is a design
          change rather than a label change.

          {b This is NOT what #719 fixed and is still open.}  #719 took the
          checkpoint's migration out of the critical section; it left [Txn]
          exactly as it is, so a read-only [BEGIN] still takes the exclusive
          lock and still lands in this bucket. *)
  | Checkpoint
  (** an explicit [Store.checkpoint].

          {b #719: this hold is the checkpoint's INSTALL phase, not the whole
          checkpoint.}  The page migration and its main-file fsync now run with
          the lock free; what is held here is the catch-up on frames committed
          during that migration, the reader/replication gate, and
          [Wal.reset].  A checkpoint's total duration is therefore no longer
          readable off this row — only the part of it that excludes writers,
          which is the part this module exists to measure. *)
  | Autocheckpoint
  (** the post-commit background fiber [maybe_autockpt_after_commit]
          dispatches through [Lwt.async].  Nobody awaits it, so it is a genuine
          second writer even at one terminal — the candidate #716 named for
          [BEGIN]'s 3.228 ms and #719 confirmed: in the #718 artifact it held
          the lock for 25.1% of the interval and was the blocker for 99.99% of
          all writer-lock wait in the run.

          {b Same #719 caveat as {!Checkpoint}}: since that fix the hold covers
          the install phase only.  It is still one acquisition per checkpoint —
          the split moved work OUT of the critical section rather than dividing
          the critical section in two — so nothing here had to grow a
          constructor. *)
  | Commit_sink (** [Store.set_commit_sink]'s registration barrier. *)

(** Every {!site}, in report order.  Fixed rather than derived so a CSV written
    by two different builds has the same rows in the same order. *)
val all_sites : site list

(** [site_name s] is the lower-case identifier used in reports and CSV output
    ([txn], [autocheckpoint], …). *)
val site_name : site -> string

(** What one {!site}'s acquisitions cost.  Durations are seconds and are [0.]
    when no clock is installed (see the module header). *)
type site_stat =
  { acquisitions : int (** total [acquire_write] calls made from this site. *)
  ; contended : int
    (** of those, how many found the lock already held — measured without a
          clock, and therefore valid even when every duration below is [0.]. *)
  ; wait_s : float (** total time spent waiting to acquire. *)
  ; wait_max_s : float (** the longest single wait. *)
  ; hold_s : float (** total time the lock was held by this site. *)
  ; hold_max_s : float (** the longest single hold. *)
  }

(** One cell of the contention matrix: [waiter] was blocked [count] times, for
    [wait_s] in total, by an acquisition from [holder].  Only non-empty cells
    are reported. *)
type blocked_by =
  { waiter : site
  ; holder : site
  ; count : int
  ; wait_s : float
  }

(** A snapshot of the accumulator.  Immutable — taking a report and then
    continuing to run does not change what the report says. *)
type report =
  { clock_installed : bool
    (** [false] when durations are all [0.] because no clock was installed,
          rather than because nothing waited. *)
  ; sites : (site * site_stat) list (** every site, in {!all_sites} order. *)
  ; blocked_by : blocked_by list (** non-empty cells only, costliest [wait_s] first. *)
  ; held : site option (** who held the lock when the snapshot was taken. *)
  ; unattributed_waits : int
    (** waits that began while the accumulator believed nobody held the
          lock.  A non-zero value means some [Rwlock.acquire_write] bypassed
          this accumulator, so the matrix is incomplete — a bug signal, not a
          measurement. *)
  ; unbalanced_releases : int
    (** releases recorded with no matching acquisition.  Same status as
          {!unattributed_waits}: a bug signal. *)
  }

(** The mutable accumulator.  One per {!Granary_store.Store.t}, because one
    store has one writer lock. *)
type t

(** [pp fmt t] renders a one-line summary of the accumulator's current state. *)
val pp : Format.formatter -> t -> unit

(** A fresh accumulator with no observations and no clock. *)
val create : unit -> t

(** [set_clock t c] installs the wall-clock source ([unit -> float], seconds).
    Called from {!Granary_store.Store.set_clock} so one installation covers both
    the durability time trigger and this. *)
val set_clock : t -> (unit -> float) -> unit

(** [now t] reads the installed clock, or [0.] when there is none.  Exposed so
    the caller can bracket its own [acquire_write] with the {e same} clock the
    accumulator uses — a wait measured against a different clock is not
    comparable with the hold it is subtracted from. *)
val now : t -> float

(** [note_acquired t site ~waited ~contended ~blocked_by] records one completed
    acquisition.

    - [waited] is the elapsed time from the [acquire_write] call to its
      resolution, read from {!now} on both sides.
    - [contended] is whether the lock was already held when [acquire_write] was
      called — {!Granary_store.Rwlock.writer_active} sampled immediately before
      it, which is exact and needs no clock.
    - [blocked_by] is {!held} sampled at the same instant: who that holder was.
      A [contended] acquisition with [blocked_by = None] means an acquisition
      elsewhere bypassed this accumulator, and is counted in the report's
      [unattributed_waits] rather than guessed at.

    The accumulator then treats [site] as the holder until {!note_released}. *)
val note_acquired
  :  t
  -> site
  -> waited:float
  -> contended:bool
  -> blocked_by:site option
  -> unit

(** [note_released t ~at] records that the current holder released the lock at
    clock reading [at].  Ignored (and counted in [unbalanced_releases]) when no
    acquisition is outstanding. *)
val note_released : t -> at:float -> unit

(** [held t] is the site currently holding the lock, if any.  Used to fill
    {!note_acquired}'s [blocked_by]. *)
val held : t -> site option

(** [reset t] discards every observation.  The clock survives, and so does a
    {e currently outstanding} hold — but its start is re-stamped to now, so the
    release that follows contributes only the time since the reset.

    Both halves matter.  Dropping the outstanding hold would make the release
    that follows unbalanced, silently corrupting the very counter that detects
    bypassed acquisitions; keeping its original start would charge the measured
    window for time spent before it began.  A reset taken where the benchmark
    harness takes it — between the warm-up and measured windows, with no
    transaction in flight — is unaffected either way.

    {b Consequence for a consumer: after a reset with a hold outstanding, that
    site can report [acquisitions = 0] with [hold_s > 0.].}  The acquisition
    was counted before the reset and the hold is charged after it, so a mean
    hold computed as [hold_s /. float acquisitions] divides by zero.  This is
    reachable in [bench_tpcc]: the reset fires right after the warm-up join,
    and a warm-up [Lwt.async] autocheckpoint can still be holding at that
    instant.  Guard the division. *)
val reset : t -> unit

(** [report t] snapshots the accumulator. *)
val report : t -> report

(** [pp_report fmt r] renders a human-readable block: one line per site, then
    the contention matrix, then any bug-signal counters.  Ends with a newline. *)
val pp_report : Format.formatter -> report -> unit
