(** #739: a follower's RO snapshot could register BELOW a checkpoint's target
    and observe a migrated page.

    {1 The hole}

    [Store.wait_for_ro_readers_past] gates a checkpoint behind live RO snapshots
    so the migration cannot write, into the main file, a page some older
    snapshot still resolves from the WAL. The invariant it establishes is "every
    live snapshot sits at or above the boundary this pass may migrate", and for
    an ordinary store it is maintainable: [Store.ro_begin_at] registers at
    [Wal.committed_frames], which only grows, so a snapshot that registers after
    the gate has cleared is still at or above it.

    On a store in FOLLOWER mode it registers at
    [min committed_frames follower_ack_position] instead — deliberately, so a
    reader never observes a frame past the last commit the replication apply
    loop has applied (#263). With the ack position below the checkpoint's
    target, a snapshot opened at any yield inside the migration registers
    {b below} the gate that just cleared. It then resolves any page whose every
    WAL frame is at or above its horizon from the {b main file}, which is where
    the migration has just written newer content. Snapshot violation, with no
    [Wal.reset] involved.

    {1 Why a bigger gate is not the fix}

    [Rwlock.acquire_read] never blocks, so the registration happens whenever the
    application likes. A gate can only exclude snapshots that already exist; one
    that registers below the target afterwards defeats a bigger gate, an earlier
    gate and a repeated gate equally.

    Clamping the checkpoint's target to the follower's ack floor does not work
    either, and the reason is worth stating because it is the obvious second
    idea: this engine's checkpoint truncates the whole WAL ([Wal.reset]), so a
    migration that stopped at the floor would have to leave the WAL
    un-truncated — it would not be a checkpoint. And the damage outlives the
    checkpoint in any case: once the overlay is retired the migrated content is
    in the main file permanently, where the ack cap cannot exclude it.

    {1 What is implemented instead}

    Two things, and the second exists only because the first leaves one way in:

    - {b [Store.checkpoint] is refused in follower mode.} A follower's WAL
      belongs to the apply loop; [Wal.reset] would also bump the local epoch out
      from under [Standby]'s [last_epoch]. It is the same shape as [rw_begin]'s
      refusal of writes there, and it makes the auto path moot: no commit is
      possible, so no autocheckpoint is ever dispatched — a manual
      [Store.checkpoint] was already the only way in.
    - {b a snapshot below [ckpt_migrated_through] is refused.} Each migration
      pass publishes its coverage boundary before writing a page, and
      [ro_begin]/[ro_begin_as_of] refuse a snapshot whose horizon sits below it.
      For a non-follower that can never fire (its horizon is
      [Wal.committed_frames], at or above every published boundary by
      construction), which the last test below asserts directly. It covers
      follower mode being switched ON while a checkpoint that started on a
      non-follower is already migrating — the one path the refusal above cannot
      reach, and a real one, since a store handed to [Standby.follow] may have
      an autocheckpoint in flight from its last commit.

    {1 Reachability, precisely}

    - Pre-existing, not a #719 regression: the pre-#719 migration yielded too,
      and [ro_begin] was equally lock-free. #719 widens the window (the whole
      migration now runs outside the writer lock) rather than opening it.
    - Unreachable from the autocheckpoint path in both codebases, because
      [rw_begin] refuses writes in follower mode, so no commit dispatches one.
    - [Standby] itself never took this path: it migrates through
      [Replication.checkpoint_wal_to_main].
    - So the reachable caller is an application that puts a store into follower
      mode and calls [Store.checkpoint] (or [PRAGMA wal_checkpoint]) on it, or
      one that enters follower mode with a checkpoint already in flight.

    {1 Why these tests are file-backed and WAL-mode}

    The Mem backend has no WAL: there are no frames, no snapshot horizon and no
    migration, so an in-memory version of every case below passes while
    measuring nothing. The migration is parked through a hooked main-file
    [write_page] — in WAL mode the main file is written by a checkpoint and by
    nothing else — so the interleaving is forced rather than raced. *)

open Lwt.Syntax
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

(* Substring test, so the file needs no [str] dependency. *)
let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* Sibling worktrees run suites concurrently, so every path carries the pid —
   the convention in test_lock_stats_718.ml and test_autocheckpoint_lock_719.ml. *)
let tmp_path name =
  Printf.sprintf "/tmp/granary_follower_snapshot_739_%s_%d.db" name (Unix.getpid ())
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
(* Same wiring as [Granary_unix.Store.open_file_wal] plus the hook —    *)
(* lifted from test_autocheckpoint_lock_719.ml, which needs it for the  *)
(* same reason: the migration has to be held still to look at it.       *)
(* ------------------------------------------------------------------ *)

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
    (* Every case here drives the checkpoint explicitly; leave no background one
       running underneath it. *)
    Store.set_wal_autocheckpoint st 0;
    Lwt.return st
  | Error e -> Alcotest.failf "open_block_wal %S: %a" path Store.pp_error e
;;

let put_one st k v =
  let* tx = Store.rw_begin st in
  let* () = Store.put tx tid (bs k) (bs v) in
  Store.commit tx
;;

let seed st n =
  let rec go k =
    if k > n
    then Lwt.return_unit
    else
      let* () = put_one st (Printf.sprintf "k%04d" k) (Printf.sprintf "v%04d" k) in
      go (k + 1)
  in
  go 1
;;

let read_one st k =
  Store.with_ro st (fun tx ->
    let* v = Store.get tx tid (bs k) in
    Lwt.return (Option.map Bytes.to_string v))
;;

(* A parked migration: [reached] resolves at the checkpoint's first main-file
   page write and the write does not proceed until [release] is resolved. *)
type park =
  { armed : bool ref
  ; reached : unit Lwt.t
  ; release_u : unit Lwt.u
  ; hook : unit -> unit Lwt.t
  }

let make_park () =
  let armed = ref false in
  let reached, reached_u = Lwt.wait () in
  let release, release_u = Lwt.wait () in
  let hook () =
    if not !armed
    then Lwt.return_unit
    else (
      armed := false;
      Lwt.wakeup_later reached_u ();
      release)
  in
  { armed; reached; release_u; hook }
;;

(* Run [f], returning [Some msg] when it fails with a [Failure]. *)
let failure_of f =
  Lwt.catch
    (fun () ->
       let* () = f () in
       Lwt.return None)
    (function
      | Failure m -> Lwt.return (Some m)
      | exn -> Lwt.fail exn)
;;

let wal_frames st =
  match Store.replication_state st with
  | Some (_epoch, frames) -> frames
  | None -> Alcotest.fail "no WAL on this store"
;;

(* ------------------------------------------------------------------ *)
(* 1. A follower does not checkpoint its own WAL.                       *)
(* ------------------------------------------------------------------ *)

(* The primary fix. On [main] this checkpoint SUCCEEDS, migrating frames past
   the follower's ack position into the main file — after which no gate and no
   snapshot cap can hide them again, because the overlay that was excluding them
   has been retired. *)
let a_follower_refuses_a_checkpoint () =
  with_tmp_path "refuse" (fun path ->
    run
      (let* st = open_hooked_wal ~path ~on_main_write:(fun () -> Lwt.return_unit) in
       let* () = seed st 30 in
       let frames_before = wal_frames st in
       is_true "the seeded commits are in the WAL" (frames_before > 0);
       Store.set_follower st true;
       Store.set_follower_ack_position st ~frames:1;
       let* r = failure_of (fun () -> Store.checkpoint st) in
       (match r with
        | None -> Alcotest.fail "a follower checkpointed its own WAL"
        | Some m ->
          is_true "the refusal names follower mode" (contains ~needle:"follower mode" m);
          is_true "and names the issue" (contains ~needle:"#739" m));
       i "and nothing was migrated" frames_before (wal_frames st);
       (* The refusal is about follower mode, not about the store: clearing it
          restores the statement. *)
       Store.set_follower st false;
       let* () = Store.checkpoint st in
       i "a non-follower checkpoint truncates the WAL" 0 (wal_frames st);
       let* got = read_one st "k0001" in
       Alcotest.(check (option string)) "the data survived" (Some "v0001") got;
       Store.close st))
;;

(* ------------------------------------------------------------------ *)
(* 2. The autocheckpoint path really is unreachable there.              *)
(* ------------------------------------------------------------------ *)

(* The reachability claim above, asserted rather than argued: an autocheckpoint
   is dispatched only by a commit, and a follower cannot commit. So refusing the
   manual entry point refuses every entry point. *)
let a_follower_cannot_dispatch_an_autocheckpoint () =
  with_tmp_path "autopath" (fun path ->
    run
      (let* st = open_hooked_wal ~path ~on_main_write:(fun () -> Lwt.return_unit) in
       let* () = seed st 10 in
       (* A threshold every commit would cross, if a commit were possible. *)
       Store.set_wal_autocheckpoint st 1;
       Store.set_follower st true;
       let frames_before = wal_frames st in
       let* r = failure_of (fun () -> put_one st "nope" "nope") in
       (match r with
        | None -> Alcotest.fail "a follower committed a write transaction"
        | Some m ->
          is_true "rw_begin refuses in follower mode" (contains ~needle:"follower" m));
       (* Drain generously: an autocheckpoint fiber, had one been dispatched,
          would have run and truncated the WAL by now. *)
       let rec pause n =
         if n = 0
         then Lwt.return_unit
         else Lwt.bind (Lwt.pause ()) (fun () -> pause (n - 1))
       in
       let* () = pause 200 in
       i "no autocheckpoint ran" frames_before (wal_frames st);
       Store.set_follower st false;
       Store.close st))
;;

(* ------------------------------------------------------------------ *)
(* 3. Follower mode switched on UNDER an in-flight migration.           *)
(* ------------------------------------------------------------------ *)

(* The one path case 1 cannot cover: the checkpoint is already past the refusal.
   It is reachable — a store handed to [Standby.follow] may carry an
   autocheckpoint in flight from its last commit — and it is exactly the
   interleaving the issue describes, forced rather than raced.

   On [main] the [ro_begin] below SUCCEEDS, at frame 1, while the migration is
   writing pages whose newest frame is far above it. Here it is refused, and the
   refusal clears by itself once the checkpoint completes and [Wal.reset] starts
   a generation the old boundary no longer describes. *)
let a_snapshot_below_the_migrated_boundary_is_refused () =
  with_tmp_path "floor" (fun path ->
    run
      (let park = make_park () in
       let* st = open_hooked_wal ~path ~on_main_write:park.hook in
       let* () = seed st 60 in
       park.armed := true;
       let ckpt = Store.checkpoint st in
       let* () = park.reached in
       (* The application turns this store into a follower mid-migration and
          records an ack position well below what the pass is migrating. *)
       Store.set_follower st true;
       Store.set_follower_ack_position st ~frames:1;
       let* r = failure_of (fun () -> Lwt.map (fun _ -> ()) (Store.ro_begin st)) in
       (match r with
        | None -> Alcotest.fail "a snapshot registered below the migrated boundary"
        | Some m ->
          is_true "the refusal names the issue" (contains ~needle:"#739" m);
          is_true
            "and says what it is refusing"
            (contains ~needle:"migrated into the main file" m));
       i "the refused snapshot registered nothing" 0 (Store.active_reader_count st);
       (* [ro_begin_as_of] carries the same refusal (a retained historical root is
          still resolved through the snapshot's frame horizon), but it is not
          asserted here: as-of history is off on this store, so it would fail
          with [History_unavailable] first and the assertion would be vacuous.
          Exercising it needs a retention sink and a pin, which is a different
          fixture; the shared predicate is [snapshot_below_ckpt_floor]. *)
       Lwt.wakeup_later park.release_u ();
       let* () = ckpt in
       (* [Wal.reset] started a new generation, so the boundary is cleared and
          the follower can read again — from a main file that is now the truth
          for everything the checkpoint migrated. *)
       i "the WAL was truncated" 0 (wal_frames st);
       let* tx = Store.ro_begin st in
       let* v = Store.get tx tid (bs "k0001") in
       Alcotest.(check (option string))
         "and the snapshot reads"
         (Some "v0001")
         (Option.map Bytes.to_string v);
       let* () = Store.ro_end tx in
       Store.set_follower st false;
       Store.close st))
;;

(* ------------------------------------------------------------------ *)
(* 4. The control: an ordinary store is never refused.                  *)
(* ------------------------------------------------------------------ *)

(* The new refusal must be inert outside follower mode, or #719's whole point —
   commits and reads proceeding while the migration runs — is undone. A
   non-follower's horizon is [Wal.committed_frames], which is at or above every
   boundary a pass publishes by construction, so this holds for a structural
   reason rather than by luck; the test is here because "by construction" is
   what regressions are made of. *)
let a_non_follower_snapshot_during_a_migration_is_served () =
  with_tmp_path "control" (fun path ->
    run
      (let park = make_park () in
       let* st = open_hooked_wal ~path ~on_main_write:park.hook in
       let* () = seed st 60 in
       park.armed := true;
       let ckpt = Store.checkpoint st in
       let* () = park.reached in
       let* tx = Store.ro_begin st in
       let* v = Store.get tx tid (bs "k0001") in
       Alcotest.(check (option string))
         "a snapshot opened mid-migration reads"
         (Some "v0001")
         (Option.map Bytes.to_string v);
       i "and it registered" 1 (Store.active_reader_count st);
       let* () = Store.ro_end tx in
       (* And a commit still lands mid-migration, as #719 requires. *)
       let* () = put_one st "during" "migration" in
       Lwt.wakeup_later park.release_u ();
       let* () = ckpt in
       let* got = read_one st "during" in
       Alcotest.(check (option string))
         "the mid-migration commit was caught up"
         (Some "migration")
         got;
       let r = Store.lock_stats st in
       i
         "no acquisition bypassed the #718 accounting"
         0
         r.Granary_store.Lock_stats.unattributed_waits;
       i "no release bypassed it either" 0 r.Granary_store.Lock_stats.unbalanced_releases;
       Store.close st))
;;

let () =
  Alcotest.run
    "follower_snapshot_739"
    [ ( "checkpoint refusal"
      , [ Alcotest.test_case
            "a follower refuses a checkpoint"
            `Quick
            a_follower_refuses_a_checkpoint
        ; Alcotest.test_case
            "a follower cannot dispatch an autocheckpoint"
            `Quick
            a_follower_cannot_dispatch_an_autocheckpoint
        ] )
    ; ( "snapshot floor"
      , [ Alcotest.test_case
            "a snapshot below the migrated boundary is refused"
            `Quick
            a_snapshot_below_the_migrated_boundary_is_refused
        ; Alcotest.test_case
            "a non-follower snapshot during a migration is served"
            `Quick
            a_non_follower_snapshot_during_a_migration_is_served
        ] )
    ]
;;
