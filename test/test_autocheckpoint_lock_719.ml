(** #719: the background autocheckpoint held the writer lock for the whole
    checkpoint.

    Under #718's accounting a one-terminal TPC-C run spent 25.1% of the interval
    with the autocheckpoint holding the writer lock, and that hold was the
    blocker for 99.99% of all writer-lock wait in the run: 60 of 670
    transactions waited, a mean of 41.9 ms each. The lock was 99.8% occupied at
    ONE terminal, so no part of #716's headline could convert to throughput
    until something left the critical section.

    What left it is the {b migration} — reading every frame out of the WAL,
    writing each page into the main file, and fsyncing it. That is essentially
    the whole cost of a checkpoint, and it needs no exclusion from writers:

    - a committed WAL frame's bytes are immutable for the life of the
      generation, and appends only ever land above [committed_frames];
    - in WAL mode nothing but a checkpoint writes the main file;
    - a half-migrated main file is invisible, because every read resolves
      through the WAL overlay first and a migrated page still has its frame.

    Only [Wal.reset] — which retires that overlay — needs the lock, together
    with a final catch-up pass over whatever was committed while the migration
    ran. So a checkpoint still takes the writer lock exactly once, and #718's
    [Lock_stats.site] constructors did not have to grow; what changed is that
    the hold now covers the install rather than the migration.

    {1 Why these tests are shaped this way}

    They are {b file-backed and WAL-mode}, because the Mem backend has no WAL at
    all: [Store.checkpoint] returns before it acquires anything and no
    autocheckpoint is ever dispatched, so an in-memory version would pass while
    measuring nothing.

    The first two use a store whose main-file [write_page] is {b hooked}, so the
    migration can be parked at a chosen page and the interleaving is forced
    rather than raced. That matters more than it looks: the property under test
    is "the lock is free {i during} the migration", and a test that merely
    sampled the accounting at some convenient moment would pass on the old code
    whenever it sampled outside a checkpoint. Parking makes the sample land
    inside one by construction.

    Nothing here asserts on wall-clock time. The assertions are
    [Lock_stats.report]'s counts and its [held] field, which are exact and need
    no clock, plus the data itself. *)

open Lwt.Syntax
module LS = Granary_store.Lock_stats
module Store = Granary_store.Store
module Wal = Granary_storage.Wal
module Unix_file = Granary_unix.Unix_file
module Ustore = Granary_unix.Store

let () = Granary_unix.install ()
let run = Lwt_main.run
let tid = 1
let bs = Bytes.of_string
let i = Alcotest.(check int)
let is_true msg got = Alcotest.(check bool) msg true got

let stat_of (r : LS.report) site =
  match List.assoc_opt site r.LS.sites with
  | Some s -> s
  | None -> Alcotest.failf "report has no row for site %s" (LS.site_name site)
;;

let cell_of (r : LS.report) ~waiter ~holder =
  List.find_opt
    (fun (b : LS.blocked_by) -> b.LS.waiter = waiter && b.LS.holder = holder)
    r.LS.blocked_by
;;

(* Sibling worktrees run suites concurrently, so every path carries the pid —
   the convention in test_lock_stats_718.ml and test_rowid_counter_ownership_632.ml. *)
let tmp_path name =
  Printf.sprintf "/tmp/granary_autockpt_lock_719_%s_%d.db" name (Unix.getpid ())
;;

let with_tmp_path name f =
  let path = tmp_path name in
  let cleanup () =
    List.iter
      (fun p ->
         try Unix.unlink p with
         | _ -> ())
      [ path; path ^ "-wal" ]
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () -> f path)
;;

(* ------------------------------------------------------------------ *)
(* A file-backed WAL store whose MAIN-file page writes call a hook.     *)
(* ------------------------------------------------------------------ *)

(* Same wiring as [Granary_unix.Store.open_file_wal], with one difference: every
   main-file [write_page] first awaits [on_main_write]. The main file in WAL
   mode is written by the checkpoint and by nothing else, so that hook is a
   precise handle on the migration — and it is the only way to hold the
   migration still long enough to look at the lock while it runs. *)
let wal_read_at fd ~offset (out : Cstruct.t) =
  let len = Cstruct.length out in
  let _ = Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET in
  let tmp = Bytes.create len in
  let rec loop o r =
    if r = 0
    then ()
    else (
      let n = Unix.read fd tmp o r in
      if n = 0 then Bytes.fill tmp o r '\x00' else loop (o + n) (r - n))
  in
  loop 0 len;
  Cstruct.blit_from_bytes tmp 0 out 0 len;
  Lwt.return (Ok ())
;;

let wal_write_at fd ~offset (src : Cstruct.t) =
  let len = Cstruct.length src in
  let _ = Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET in
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  let rec loop o r =
    if r = 0
    then ()
    else (
      let n = Unix.write fd tmp o r in
      if n = 0 then failwith "short write" else loop (o + n) (r - n))
  in
  loop 0 len;
  Lwt.return (Ok ())
;;

let wrap_file_err = function
  | Ok () -> Lwt.return_ok ()
  | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
;;

let open_hooked_wal ~path ~(on_main_write : unit -> unit Lwt.t) =
  let* fr = Unix_file.open_ ~path () in
  let file =
    match fr with
    | Ok f -> f
    | Error e -> Alcotest.failf "Unix_file.open_ %S: %a" path Unix_file.pp_error e
  in
  let* () =
    if Int64.equal (Unix_file.n_pages file) 0L
    then
      let* _ = Unix_file.resize file ~n_pages:2L in
      Lwt.return_unit
    else Lwt.return_unit
  in
  let wal_path = path ^ "-wal" in
  let wal_fd = Unix.openfile wal_path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  let wal_size_bytes = Int64.of_int (Unix.lseek wal_fd 0 Unix.SEEK_END) in
  let read_page ~page_id buf =
    let* r = Unix_file.read_page file ~page_id buf in
    wrap_file_err r
  in
  let write_page ~page_id buf =
    let* () = on_main_write () in
    let* r = Unix_file.write_page file ~page_id buf in
    wrap_file_err r
  in
  let sync () =
    let* r = Unix_file.sync file in
    wrap_file_err r
  in
  let resize ~n_pages =
    let* r = Unix_file.resize file ~n_pages in
    wrap_file_err r
  in
  let wal_sync () =
    Unix.fsync wal_fd;
    Lwt.return_ok ()
  in
  let wal_resize n =
    Unix.ftruncate wal_fd (Int64.to_int n);
    Lwt.return_ok ()
  in
  let close () =
    let* _ = Unix_file.close file in
    Lwt.return_unit
  in
  let wal_close () =
    (try Unix.close wal_fd with
     | Unix.Unix_error _ -> ());
    Lwt.return_unit
  in
  let* r =
    Store.open_block_wal
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages:(Unix_file.n_pages file)
      ~wal_read_at:(wal_read_at wal_fd)
      ~wal_write_at:(wal_write_at wal_fd)
      ~wal_sync
      ~wal_size_bytes
      ~wal_resize
      ~close
      ~wal_close
      ()
  in
  match r with
  | Ok st ->
    Store.set_clock st Unix.gettimeofday;
    (* The explicit checkpoint is the one under test here; leave no background
       one running underneath it. *)
    Store.set_wal_autocheckpoint st 0;
    Lwt.return st
  | Error e -> Alcotest.failf "open_block_wal %S: %a" path Store.pp_error e
;;

let put_one st k v =
  let* tx = Store.rw_begin st in
  let* () = Store.put tx tid (bs k) (bs v) in
  Store.commit tx
;;

let read_one st k =
  Store.with_ro st (fun tx ->
    let* v = Store.get tx tid (bs k) in
    Lwt.return (Option.map Bytes.to_string v))
;;

let check_key st ~what k expected =
  let* got = read_one st k in
  Alcotest.(check (option string)) what (Some expected) got;
  Lwt.return_unit
;;

(* A parked migration: [reached] resolves at the checkpoint's first main-file
   page write and the write does not proceed until [release] is resolved. *)
type park =
  { armed : bool ref
  ; writes : int ref (** main-file page writes seen, armed or not. *)
  ; reached : unit Lwt.t
  ; release_u : unit Lwt.u
  ; hook : unit -> unit Lwt.t
  }

let make_park () =
  let armed = ref false in
  let writes = ref 0 in
  let reached, reached_u = Lwt.wait () in
  let release, release_u = Lwt.wait () in
  let hook () =
    incr writes;
    if not !armed
    then Lwt.return_unit
    else (
      armed := false;
      Lwt.wakeup_later reached_u ();
      release)
  in
  { armed; writes; reached; release_u; hook }
;;

let rec settle n =
  if n = 0
  then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    settle (n - 1)
;;

let check_all st ~what n =
  let rec go k =
    if k > n
    then Lwt.return_unit
    else
      let* () =
        check_key
          st
          ~what:(Printf.sprintf "%s: k%04d" what k)
          (Printf.sprintf "k%04d" k)
          (Printf.sprintf "v%04d" k)
      in
      go (k + 1)
  in
  go 1
;;

(* Seed enough committed pages that the migration has real work to park in. *)
let seed_from st ~first ~last =
  let rec go k =
    if k > last
    then Lwt.return_unit
    else
      let* () = put_one st (Printf.sprintf "k%04d" k) (Printf.sprintf "v%04d" k) in
      go (k + 1)
  in
  go first
;;

let seed st n = seed_from st ~first:1 ~last:n

(* ------------------------------------------------------------------ *)
(* 1. The lock is free while the checkpoint migrates.                   *)
(* ------------------------------------------------------------------ *)

(* The headline. On [main] this fails at the very first assertion: the
   checkpoint acquires the writer lock before it reads a single frame, so
   [held] reads [Some Checkpoint] and [acquisitions] is already 1 by the time
   the first page reaches the main file. *)
let the_migration_runs_with_the_writer_lock_free () =
  with_tmp_path "free" (fun path ->
    run
      (let park = make_park () in
       let* st = open_hooked_wal ~path ~on_main_write:park.hook in
       let* () = seed st 60 in
       Store.reset_lock_stats st;
       park.armed := true;
       let ckpt = Store.checkpoint st in
       let* () = park.reached in
       let r = Store.lock_stats st in
       is_true "nobody holds the writer lock while the migration runs" (r.LS.held = None);
       i
         "and the checkpoint has not acquired it yet"
         0
         (stat_of r LS.Checkpoint).LS.acquisitions;
       (* A whole write transaction, begin to commit, inside the migration. *)
       let* () = put_one st "during" "migration" in
       let r = Store.lock_stats st in
       i "the transaction ran" 1 (stat_of r LS.Txn).LS.acquisitions;
       i "and was never contended" 0 (stat_of r LS.Txn).LS.contended;
       is_true
         "so no wait was charged to the checkpoint"
         (cell_of r ~waiter:LS.Txn ~holder:LS.Checkpoint = None);
       Lwt.wakeup_later park.release_u ();
       let* () = ckpt in
       let r = Store.lock_stats st in
       i
         "the checkpoint took the lock exactly once, at the end"
         1
         (stat_of r LS.Checkpoint).LS.acquisitions;
       i "no acquisition bypassed the accounting" 0 r.LS.unattributed_waits;
       i "no release bypassed it either" 0 r.LS.unbalanced_releases;
       is_true "and nothing is left holding the lock" (r.LS.held = None);
       Store.close st))
;;

(* ------------------------------------------------------------------ *)
(* 2. A commit that lands mid-migration is caught up, not lost.         *)
(* ------------------------------------------------------------------ *)

(* The correctness half, and the one a wrong split would fail silently. The
   migration snapshots the WAL index; a commit that lands afterwards is invisible
   to that snapshot, so if [Wal.reset] then truncated the WAL the row would be
   gone from a file that reports success. The catch-up pass under the writer
   lock is what makes it not so, and the reopen is what proves the row reached
   the MAIN file rather than merely surviving in an un-truncated WAL. *)
let a_commit_during_the_migration_is_migrated_not_lost () =
  with_tmp_path "catchup" (fun path ->
    run
      (let park = make_park () in
       let* st = open_hooked_wal ~path ~on_main_write:park.hook in
       let* () = seed st 60 in
       park.armed := true;
       let ckpt = Store.checkpoint st in
       let* () = park.reached in
       let* () = put_one st "during" "migration" in
       Lwt.wakeup_later park.release_u ();
       let* () = ckpt in
       let* () = check_key st ~what:"the seeded row is readable" "k0001" "v0001" in
       let* () = check_key st ~what:"so is the mid-migration row" "during" "migration" in
       let* () = Store.close st in
       (* The WAL is back to its bare header, so a reopen can only be reading
          the main file — which is the point: the mid-migration commit's frames
          were migrated before they were recycled. *)
       let wal_size = (Unix.stat (path ^ "-wal")).Unix.st_size in
       i "the WAL was truncated to its header" Wal.header_size_bytes wal_size;
       let* r = Ustore.open_file_wal ~path () in
       let st2 =
         match r with
         | Ok s -> s
         | Error e -> Alcotest.failf "reopen %S: %a" path Store.pp_error e
       in
       let* () = check_key st2 ~what:"the seeded row survived" "k0001" "v0001" in
       let* () =
         check_key st2 ~what:"and so did the mid-migration row" "during" "migration"
       in
       Store.close st2))
;;

(* ------------------------------------------------------------------ *)
(* 3. The background path, end to end, on a plain file-backed store.    *)
(* ------------------------------------------------------------------ *)

(* No hook: the real [maybe_autockpt_after_commit] fiber, dispatched by a real
   commit, with a threshold low enough that every commit crosses it. The
   accounting assertions are the #718 bug signals — a split that acquired or
   released the writer lock outside [acquire_writer]/[release_writer] would show
   up here and nowhere else — and the data assertions are what a lost catch-up
   pass would break. *)
let the_background_autocheckpoint_is_accounted_and_loses_nothing () =
  with_tmp_path "background" (fun path ->
    run
      (let* r = Ustore.open_file_wal ~path () in
       let st =
         match r with
         | Ok s -> s
         | Error e -> Alcotest.failf "open_file_wal %S: %a" path Store.pp_error e
       in
       Store.set_clock st Unix.gettimeofday;
       Store.set_wal_autocheckpoint st 1;
       let* () = seed st 40 in
       (* The last commit dispatches an unawaited fiber; drain until it has both
          taken the lock and let go of it. Bounded, so a checkpoint that never
          releases fails here rather than hanging the suite. *)
       let rec drain n =
         let r = Store.lock_stats st in
         if
           n = 0 || ((stat_of r LS.Autocheckpoint).LS.acquisitions > 0 && r.LS.held = None)
         then Lwt.return n
         else
           let* () = Lwt.pause () in
           drain (n - 1)
       in
       let* left = drain 2000 in
       is_true "the background checkpoint ran and released the lock" (left > 0);
       let r = Store.lock_stats st in
       is_true
         "it is charged to its own site"
         ((stat_of r LS.Autocheckpoint).LS.acquisitions > 0);
       i "no acquisition bypassed the accounting" 0 r.LS.unattributed_waits;
       i "no release bypassed it either" 0 r.LS.unbalanced_releases;
       is_true "nothing is left holding the lock" (r.LS.held = None);
       let* () = check_all st ~what:"live" 40 in
       let* () = Store.close st in
       let* r2 = Ustore.open_file_wal ~path () in
       let st2 =
         match r2 with
         | Ok s -> s
         | Error e -> Alcotest.failf "reopen %S: %a" path Store.pp_error e
       in
       let* () = check_all st2 ~what:"after reopen" 40 in
       Store.close st2))
;;

(* ------------------------------------------------------------------ *)
(* 4. The RO gate runs before the MIGRATION, not only before the reset. *)
(* ------------------------------------------------------------------ *)

(* The subtlest half of the split, and the one the obvious implementation gets
   wrong. Moving the whole gate to just before [Wal.reset] looks safe — the WAL
   index stays intact during the migration, so a snapshot resolves its pages
   from the WAL — but that is only true of pages the snapshot can SEE a frame
   for. A snapshot at frame [m] falls through to the MAIN FILE for any page
   whose every frame is at or above [m], i.e. any page written for the first
   time since the reader began. Copying that page's newer content into the main
   file is immediately visible to it.

   So the assertion is not about [Wal.reset] at all: with a snapshot pinned
   below the checkpoint's target, the checkpoint must not write a single page to
   the main file. [test_wal_truncate_612.ml]'s
   [snapshot_reader_is_not_broken_by_a_checkpoint] observes the consequence
   through a reader; this observes the cause directly, and fails on a
   gate-before-reset-only split even when the reader happens not to touch an
   affected page. *)
let the_reader_gate_blocks_the_migration_not_just_the_reset () =
  with_tmp_path "gate" (fun path ->
    run
      (let park = make_park () in
       let* st = open_hooked_wal ~path ~on_main_write:park.hook in
       let* () = seed st 30 in
       (* The snapshot that must not be migrated out from under. *)
       let* ro = Store.ro_begin st in
       (* Frames it cannot see, so the checkpoint's target is strictly above the
          snapshot's floor and the gate actually engages. *)
       let* () = seed_from st ~first:31 ~last:42 in
       let writes_before = !(park.writes) in
       let ckpt = Store.checkpoint st in
       let* () = settle 30 in
       i
         "the gated checkpoint has not written a single page to the main file"
         writes_before
         !(park.writes);
       (* Release the reader; the migration and the truncation both proceed. *)
       let* () = Store.ro_end ro in
       let* () = ckpt in
       is_true "and then it migrated" (!(park.writes) > writes_before);
       let* () = check_all st ~what:"live" 42 in
       let* () = Store.close st in
       let wal_size = (Unix.stat (path ^ "-wal")).Unix.st_size in
       i "the WAL was truncated once the reader left" Wal.header_size_bytes wal_size;
       Lwt.return_unit))
;;

let () =
  Alcotest.run
    "autocheckpoint_lock_719"
    [ ( "#719"
      , [ Alcotest.test_case
            "the_migration_runs_with_the_writer_lock_free"
            `Quick
            the_migration_runs_with_the_writer_lock_free
        ; Alcotest.test_case
            "a_commit_during_the_migration_is_migrated_not_lost"
            `Quick
            a_commit_during_the_migration_is_migrated_not_lost
        ; Alcotest.test_case
            "the_background_autocheckpoint_is_accounted_and_loses_nothing"
            `Quick
            the_background_autocheckpoint_is_accounted_and_loses_nothing
        ; Alcotest.test_case
            "the_reader_gate_blocks_the_migration_not_just_the_reset"
            `Quick
            the_reader_gate_blocks_the_migration_not_just_the_reset
        ] )
    ]
;;
