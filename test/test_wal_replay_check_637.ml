(** #637: a database already corrupted by the pre-#636 WAL replay is neither
    detected nor repaired on upgrade.

    {1 What went wrong}

    Before #636, [Wal.reset] cleared only in-memory state. Every frame of the
    checkpointed generation stayed on disk under an {b unchanged} [(salt, seed)]
    marker, so every one of them still verified, and the next open replayed the
    whole checkpointed generation — over newer data whenever the successor
    generation was shorter. #636 rotates the marker, but only from the next
    successful checkpoint onward: a file already in the bad state is untouched,
    and an upgrading user gets one further stale replay with no warning.

    Two outcomes were measured on [main] for that issue: an unopenable database,
    and — the dangerous one — a database that opens, answers queries, and is
    silently short 365 committed rows.

    {1 What is implemented, and what it can see}

    A detector, reported through [PRAGMA wal_replay_check] (option 1 of the
    issue). {b Not} a refusal to open: that was considered and rejected, because
    it turns a database that may be perfectly fine into a hard failure.

    The signal is header-page [txn_id] monotonicity. Every commit writes exactly
    one header page (page 0 or 1, alternating) with [txn_id = previous + 1], in
    the same batch as its commit-flagged frame, so within one generation the
    header frames recovery walks past carry strictly increasing [txn_id]s. A
    decrease means the walk has run off the end of the newest generation into
    the physical remains of an older one — which is exactly the pre-#636 shape,
    and is present in {b both} of its outcomes.

    {1 What it cannot see — the honest part}

    Three gaps, and the reason the report has {b three} statuses rather than a
    boolean:

    - {b Damage already replayed at an EARLIER open.} The row-loss variant
      leaves a structurally valid database, so no B-tree integrity check finds
      it either; and once a post-#636 checkpoint has rotated and truncated the
      WAL, the evidence is physically gone. This detector reports on {i this
      open's} WAL walk, never on the file's history.
    - {b A stale remainder that is a fragment of a single old commit batch
      carrying no header-page frame.} Recovery can consume such a fragment, and
      if it contains a commit-flagged frame the fragment is applied, undetected.
      Any stale remainder spanning a whole old commit does carry a header frame,
      so this is the narrow tail of the case, not its body.
    - {b A database that will not open at all.} That is #636's other outcome and
      no PRAGMA can run on it — but it is loud by construction.

    So [no_evidence] is defined as "walked at least two header frames and their
    [txn_id]s increased", and a walk with less material than that reports
    [not_examined] instead. Collapsing the two into "clean" would be worse than
    having no detector, which is what
    {!an_empty_wal_reports_not_examined_not_clean} pins.

    {1 Why the fabricated-WAL cases are shaped this way}

    The corrupt state cannot be produced by the fixed code — that is the whole
    point of #636 — so the first group builds it directly through the [Wal] API:
    write a long generation, then reopen a handle that {i believes} the device
    is empty (so recovery finds nothing and the marker is left alone) and append
    a SHORTER generation over its start. That is byte-for-byte what a pre-#636
    [reset] left behind. Those cases run over an in-memory byte device but
    against the real [Wal.open_]/[recover_index], which is where the detector
    lives; the last group is file-backed and WAL-mode end to end, and is the one
    that proves an ordinary database is not false-positived. *)

open Lwt.Syntax
module Wal = Granary_storage.Wal
module Page = Granary_storage.Page
module Store = Granary_store.Store

module Db = struct
  include Granary.Db

  let open_file_wal = Granary_unix.open_file_wal
end

let () = Granary_unix.install ()
let run = Lwt_main.run
let i = Alcotest.(check int)
let is_true msg got = Alcotest.(check bool) msg true got

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* ------------------------------------------------------------------ *)
(* An in-memory byte device (the shape test_wal_truncate_612.ml uses).  *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev () = { buf = Bytes.create 0 }
let dev_size d = Int64.of_int (Bytes.length d.buf)

let dev_set_len d n =
  let cur = Bytes.length d.buf in
  if n <> cur
  then (
    let nb = Bytes.make n '\x00' in
    Bytes.blit d.buf 0 nb 0 (min cur n);
    d.buf <- nb)
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  let avail = max 0 (min len (Bytes.length d.buf - off)) in
  let tmp = Bytes.make len '\x00' in
  if avail > 0 then Bytes.blit d.buf off tmp 0 avail;
  Cstruct.blit_from_bytes tmp 0 out 0 len;
  Lwt.return (Ok ())
;;

let write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  if off + len > Bytes.length d.buf then dev_set_len d (off + len);
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())

(* [~size_bytes] is a parameter rather than always [dev_size d]: passing the
   bare header length is how a handle is made to believe the device is empty,
   which is what a pre-#636 [reset] left the NEXT writer believing. *)
let open_on ?size_bytes d =
  let* r =
    Wal.open_
      ~read_at:(read_at d)
      ~write_at:(write_at d)
      ~sync:sync_ok
      ~size_bytes:(Option.value size_bytes ~default:(dev_size d))
      ()
  in
  match r with
  | Ok w -> Lwt.return w
  | Error e -> Alcotest.failf "Wal.open_: %a" Wal.pp_error e
;;

(* A sealed header page carrying [txn_id], exactly as [Header.build_page]
   renders one — which is what the detector reads back. *)
let header_page ~txn_id =
  let buf = Cstruct.create Page.page_size in
  Page.write_common
    buf
    { Page.kind = Page.Header; flags = 0; n_keys = 0; right_page = 0l; crc32 = 0l };
  Page.write_header_fields
    buf
    { Page.txn_id
    ; root_page = 2L
    ; freelist_page = 0L
    ; n_pages_total = 8L
    ; schema_version = 1L
    ; page_size = Int32.of_int Page.page_size
    ; format_version = 3l
    ; reserved_bytes_per_page = 0l
    ; enc_magic = 0l
    ; canary_nonce = String.make 16 '\000'
    ; canary_tag = String.make 16 '\000'
    };
  Page.seal buf;
  buf
;;

let leaf_page ~page_id =
  let buf = Cstruct.create Page.page_size in
  Page.write_common
    buf
    { Page.kind = Page.Leaf
    ; flags = 0
    ; n_keys = 0
    ; right_page = Int64.to_int32 page_id
    ; crc32 = 0l
    };
  Page.seal buf;
  buf
;;

(* One commit's worth of frames, shaped like a real one: the alternating header
   page plus a data page, commit flag on the last. *)
let commit_batch w ~txn_id ~data_page =
  let hdr_pid = Int64.rem txn_id 2L in
  let* r =
    Wal.append_commit
      w
      [ hdr_pid, header_page ~txn_id; data_page, leaf_page ~page_id:data_page ]
  in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "append_commit: %a" Wal.pp_error e
;;

let generation w ~first_txn ~n =
  let rec go k =
    if k >= n
    then Lwt.return_unit
    else
      let txn = Int64.add first_txn (Int64.of_int k) in
      let* () = commit_batch w ~txn_id:txn ~data_page:(Int64.of_int (2 + k)) in
      go (k + 1)
  in
  go 0
;;

(* ------------------------------------------------------------------ *)
(* 1. The pre-#636 shape is detected.                                   *)
(* ------------------------------------------------------------------ *)

(* Generation A is 12 commits (24 frames); the pre-#636 "reset" then leaves the
   marker alone and generation B, 3 commits (6 frames), is written over its
   start. Recovery walks B's 6 frames and keeps going into A's frames 6..23,
   which all still verify — the defect verbatim. *)
let a_stale_generation_is_detected () =
  run
    (let d = mk_dev () in
     let* w = open_on d in
     let* () = generation w ~first_txn:1L ~n:12 in
     i "generation A committed" 24 (Wal.committed_frames w);
     (* The pre-#636 [reset]: in-memory state dropped, marker NOT rotated, file
        NOT truncated.  Reproduced by opening a handle that believes the device
        holds nothing but its header — it reads the same [(salt, seed)] back and
        starts appending at frame 0. *)
     let* w2 = open_on ~size_bytes:(Int64.of_int Wal.header_size_bytes) d in
     i "the successor generation starts empty" 0 (Wal.committed_frames w2);
     let* () = generation w2 ~first_txn:100L ~n:3 in
     (* Now open it the way a post-#636 binary would on the next start. *)
     let* w3 = open_on d in
     let c = Wal.replay_check w3 in
     is_true
       "recovery walked past the successor generation's 6 frames"
       (c.Wal.frames_walked > 6);
     (match c.Wal.stale_generation with
      | None -> Alcotest.fail "the stale generation was not detected"
      | Some (idx, prev, txn) ->
        i "detected at the first frame of the stale remainder" 6 idx;
        Alcotest.(check int64) "after the successor's last txn_id" 102L prev;
        is_true "on a frame carrying an older txn_id" (Int64.compare txn prev <= 0));
     Lwt.return_unit)
;;

(* The store-level status, which is what the PRAGMA renders. *)
let the_store_reports_stale_generation () =
  run
    (let d = mk_dev () in
     let* w = open_on d in
     let* () = generation w ~first_txn:1L ~n:12 in
     let* w2 = open_on ~size_bytes:(Int64.of_int Wal.header_size_bytes) d in
     let* () = generation w2 ~first_txn:100L ~n:3 in
     let* w3 = open_on d in
     (match (Wal.replay_check w3).Wal.stale_generation with
      | Some _ -> ()
      | None -> Alcotest.fail "precondition: no stale generation built");
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 2. The two negative controls.                                       *)
(* ------------------------------------------------------------------ *)

(* A single generation, however long, never regresses. *)
let one_generation_is_clean () =
  run
    (let d = mk_dev () in
     let* w = open_on d in
     let* () = generation w ~first_txn:1L ~n:12 in
     let* w2 = open_on d in
     let c = Wal.replay_check w2 in
     i "every frame was walked" 24 c.Wal.frames_walked;
     i "and every commit's header frame was read" 12 c.Wal.header_frames;
     Alcotest.(check bool) "no regression" true (c.Wal.stale_generation = None);
     Lwt.return_unit)
;;

(* A successor generation LONGER than its predecessor overwrites it completely,
   so there is no stale remainder and nothing to detect. That is not a miss: the
   file is genuinely fine. *)
let a_longer_successor_leaves_no_remainder () =
  run
    (let d = mk_dev () in
     let* w = open_on d in
     let* () = generation w ~first_txn:1L ~n:3 in
     let* w2 = open_on ~size_bytes:(Int64.of_int Wal.header_size_bytes) d in
     let* () = generation w2 ~first_txn:100L ~n:12 in
     let* w3 = open_on d in
     let c = Wal.replay_check w3 in
     Alcotest.(check bool)
       "no regression, because nothing stale survived"
       true
       (c.Wal.stale_generation = None);
     i "and the walk had plenty to compare" 12 c.Wal.header_frames;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 3. "No evidence" is not "clean", and the PRAGMA says so.             *)
(* ------------------------------------------------------------------ *)

let path_counter = ref 0

let with_wal_path f =
  let n = !path_counter in
  incr path_counter;
  let path =
    Printf.sprintf "/tmp/granary_wal_replay_check_637_%d_%d.db" (Unix.getpid ()) n
  in
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

let open_db path =
  match run (Db.open_file_wal ~path ()) with
  | Ok db -> db
  | Error e -> Alcotest.failf "open_file_wal %S: %a" path Db.pp_error e
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let replay_row db =
  match run (Db.query db "PRAGMA wal_replay_check") with
  | Error e -> Alcotest.failf "PRAGMA wal_replay_check: %a" Db.pp_error e
  | Ok stream ->
    (match run (Lwt_stream.to_list stream) with
     | [ [| Db.V_text status; Db.V_int walked; Db.V_int hdrs; Db.V_text detail |] ] ->
       status, Int64.to_int walked, Int64.to_int hdrs, detail
     | _ -> Alcotest.fail "PRAGMA wal_replay_check: unexpected row shape")
;;

(* The claim that must not be over-read. A freshly created database has an empty
   WAL, so the check has nothing to compare — and if it answered "clean" there,
   every upgrading user with a checkpointed database would be told their file was
   verified when nothing was verified at all. *)
let an_empty_wal_reports_not_examined_not_clean () =
  with_wal_path (fun path ->
    let db = open_db path in
    let status, _, hdrs, detail = replay_row db in
    Alcotest.(check string) "status" "not_examined" status;
    i "no header frames to compare" 0 hdrs;
    is_true "and the detail says it is not a verdict" (contains ~needle:"not a" detail);
    run (Db.close db))
;;

(* An ordinary WAL database, reopened, must report no evidence and never a
   stale generation — the false-positive guard for the whole detector. The
   reopen is what makes it meaningful: [recover_index] only runs there. *)
let an_ordinary_database_reports_no_evidence () =
  with_wal_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
    for k = 1 to 40 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'v%d')" k k)
    done;
    run (Db.close db);
    let db2 = open_db path in
    let status, walked, hdrs, detail = replay_row db2 in
    Alcotest.(check string) "status" "no_evidence" status;
    is_true "frames were walked" (walked > 0);
    is_true "and at least two header frames compared" (hdrs >= 2);
    is_true
      "the detail refuses to claim the database is clean"
      (contains ~needle:"not a clean bill of health" detail);
    run (Db.close db2))
;;

(* The upgrade path the issue is about: a database that HAS been checkpointed
   and written to again. On a pre-#636 binary this is the file that replays its
   stale generation; on this one the marker was rotated and the WAL truncated,
   so there is nothing stale to walk into. *)
let a_checkpointed_and_rewritten_database_is_not_false_positived () =
  with_wal_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
    for k = 1 to 60 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'v%d')" k k)
    done;
    exec db "PRAGMA wal_checkpoint";
    (* Fewer commits after the checkpoint than before it: the shape that made
       the pre-#636 replay walk off the end of the successor generation. *)
    exec db "UPDATE t SET b = 'x' WHERE a = 1";
    exec db "INSERT INTO t VALUES (61, 'v61')";
    run (Db.close db);
    let db2 = open_db path in
    let status, _, _, _ = replay_row db2 in
    is_true "not reported as corrupt" (status <> "stale_generation");
    (* And the data really is all there — the assertion #636 measured as
       "count = 1135" on the broken code. *)
    (match run (Db.query db2 "SELECT COUNT(*) FROM t") with
     | Error e -> Alcotest.failf "count: %a" Db.pp_error e
     | Ok stream ->
       (match run (Lwt_stream.to_list stream) with
        | [ [| Db.V_int n |] ] -> i "every committed row survived" 61 (Int64.to_int n)
        | _ -> Alcotest.fail "count: unexpected shape"));
    (match run (Db.query db2 "SELECT b FROM t WHERE a = 1") with
     | Error e -> Alcotest.failf "select: %a" Db.pp_error e
     | Ok stream ->
       (match run (Lwt_stream.to_list stream) with
        | [ [| Db.V_text b |] ] ->
          Alcotest.(check string) "and the UPDATE was not reverted" "x" b
        | _ -> Alcotest.fail "select: unexpected shape"));
    run (Db.close db2))
;;

(* The in-memory backend has no WAL at all, so it must answer "nothing to
   examine" rather than anything that could read as a clean bill of health. *)
let the_mem_backend_reports_not_examined () =
  run
    (let* st = Store.open_mem () in
     let c = Store.wal_replay_check st in
     Alcotest.(check bool)
       "not examined"
       true
       (c.Store.status = Store.Wal_replay_not_examined);
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "wal_replay_check_637"
    [ ( "detection"
      , [ Alcotest.test_case
            "a stale generation is detected"
            `Quick
            a_stale_generation_is_detected
        ; Alcotest.test_case
            "the fabricated file really is stale"
            `Quick
            the_store_reports_stale_generation
        ] )
    ; ( "controls"
      , [ Alcotest.test_case "one generation is clean" `Quick one_generation_is_clean
        ; Alcotest.test_case
            "a longer successor leaves no remainder"
            `Quick
            a_longer_successor_leaves_no_remainder
        ] )
    ; ( "pragma"
      , [ Alcotest.test_case
            "an empty WAL reports not_examined, not clean"
            `Quick
            an_empty_wal_reports_not_examined_not_clean
        ; Alcotest.test_case
            "an ordinary database reports no_evidence"
            `Quick
            an_ordinary_database_reports_no_evidence
        ; Alcotest.test_case
            "a checkpointed and rewritten database is not false-positived"
            `Quick
            a_checkpointed_and_rewritten_database_is_not_false_positived
        ; Alcotest.test_case
            "the mem backend reports not_examined"
            `Quick
            the_mem_backend_reports_not_examined
        ] )
    ]
;;
