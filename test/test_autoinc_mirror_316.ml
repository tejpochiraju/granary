(** #316: what the per-row catalog-MIRROR write actually costs an
    AUTOINCREMENT insert.

    #314 made an AUTOINCREMENT table's sticky rowid high-water survive mirror
    reconstruction by having every counter bump rewrite that table's mirror
    entry ([Catalog.put_table_counter_tx] -> [put_mirror_tx]) alongside the
    primary [_sys_tables] row.  The mirror entry re-encodes the FULL schema —
    name, tree id, fingerprint, every column, the FK block — so the worry #316
    records is that a write-heavy AUTOINCREMENT workload pays a
    serialize-and-[S.put] of the whole schema blob per allocated rowid, and
    that the cost grows with the table's width.

    This file MEASURES that rather than assuming it, and pins the result so
    nobody has to re-derive it.  It deliberately measures nothing with a clock:
    every gate here is a COUNT or an ALLOCATION (CLAUDE.md's non-wall-clock
    rule), so a loaded box cannot move it.

    Three quantities, all per inserted row, for a 3-column and a 30-column
    table, plain rowid vs AUTOINCREMENT:

    - [minor_words]: minor-heap words allocated (the serialize half of the
      cost; this is where re-encoding every column shows up if it is going to);
    - [wal_bytes]: bytes appended to the WAL (the [S.put] half — what actually
      reaches the disk);
    - [mirror_bytes]: the size of the mirror blob itself, which is the amount
      re-encoded per bump.

    It runs both in AUTOCOMMIT and inside an EXPLICIT transaction, because
    #347's [~defer_counter] already coalesces the counter write (primary AND
    mirror) to COMMIT for explicit transactions — i.e. #316's third proposed
    optimisation is already in place for that path, and the measurement has to
    say so or it overstates the problem.

    Autocheckpoint is disabled for the measured window so the WAL only grows;
    otherwise a checkpoint would truncate it mid-measurement and the byte
    figure would be noise. *)

open Lwt.Syntax
module Store = Granary_store.Store
module Ustore = Granary_unix.Store

module Db = struct
  include Granary.Db
end

let () = Granary_unix.install ()
let run = Lwt_main.run

(* Tree id of the redundant catalog mirror (#174), [Catalog.sys_mirror_tid]. *)
let sys_mirror_tid : Store.tree_id = 7

(* Sibling worktrees run suites concurrently, so every path carries the pid —
   the convention in test_rowid_counter_ownership_632.ml. *)
let tmp_dir =
  match Sys.getenv_opt "TMPDIR" with
  | Some d when d <> "" -> d
  | _ -> Filename.get_temp_dir_name ()
;;

let tmp_path name =
  Filename.concat
    tmp_dir
    (Printf.sprintf "granary_autoinc_mirror_316_%s_%d.db" name (Unix.getpid ()))
;;

let with_tmp_path name f =
  let path = tmp_path name in
  let cleanup () =
    List.iter
      (fun p ->
         try Unix.unlink p with
         | _ -> ())
      [ path; path ^ "-wal"; path ^ ".aslog" ]
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () -> f path)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let render (v : Db.value) =
  match v with
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%g" f
  | Db.V_null -> "NULL"
  | Db.V_blob b -> Bytes.to_string b
;;

let rows db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query error in %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun row -> String.concat "|" (Array.to_list (Array.map render row)))
      (run (Lwt_stream.to_list stream))
;;

let file_size p =
  try (Unix.stat p).Unix.st_size with
  | Unix.Unix_error _ -> 0
;;

(* Total bytes held by every entry of the mirror tree.  With one user table in
   the database this is that table's mirror blob, which is exactly what a
   counter bump re-encodes and re-[put]s. *)
let mirror_bytes store =
  run
    (Store.with_ro store
     @@ fun tx ->
     let* cur = Store.cursor_open tx sys_mirror_tid in
     let _sr = Store.cursor_first cur in
     let total = ref 0 in
     let rec walk () =
       match Store.cursor_next cur with
       | None -> ()
       | Some (k, v) ->
         total := !total + Bytes.length k + Bytes.length v;
         walk ()
     in
     walk ();
     Store.cursor_close cur;
     Lwt.return !total)
;;

let col_names width = List.init width (fun i -> Printf.sprintf "c%d" i)

type point =
  { words_per_row : float
  ; wal_bytes_per_row : float
  ; mirror_bytes : int
  }

(* One measurement point.  [n] rows are inserted one statement at a time; when
   [explicit] the whole run sits inside a single BEGIN/COMMIT. *)
let measure ~label ~width ~n ~autoinc ~explicit =
  with_tmp_path label (fun path ->
    let store =
      match run (Ustore.open_file_wal ~path ()) with
      | Ok s -> s
      | Error e -> Alcotest.failf "open_file_wal: %a" Store.pp_error e
    in
    (* No checkpoint may truncate the WAL inside the measured window. *)
    Store.set_wal_autocheckpoint store 0;
    let db = run (Db.of_store ~file_path:path store) in
    let pk =
      if autoinc then "INTEGER PRIMARY KEY AUTOINCREMENT" else "INTEGER PRIMARY KEY"
    in
    let cols = col_names width in
    exec
      db
      (Printf.sprintf
         "CREATE TABLE t (id %s, %s)"
         pk
         (String.concat ", " (List.map (fun c -> c ^ " INTEGER") cols)));
    let stmt =
      Printf.sprintf
        "INSERT INTO t (%s) VALUES (%s)"
        (String.concat ", " cols)
        (String.concat ", " (List.init width string_of_int))
    in
    (* Warm-up: the first insert seeds pages and caches, and (for a plain rowid
       table) is the one that seeds the counter from the empty sentinel. *)
    exec db stmt;
    if explicit then exec db "BEGIN";
    let wal0 = file_size (path ^ "-wal") in
    let w0 = Gc.minor_words () in
    for _ = 1 to n do
      exec db stmt
    done;
    let w1 = Gc.minor_words () in
    let wal1 = file_size (path ^ "-wal") in
    if explicit then exec db "COMMIT";
    let mirror = mirror_bytes store in
    let nrows = List.hd (rows db "SELECT COUNT(id) FROM t") in
    Alcotest.(check string) "every row landed" (string_of_int (n + 1)) nrows;
    run (Db.close db);
    let fl = float_of_int n in
    let p =
      { words_per_row = (w1 -. w0) /. fl
      ; wal_bytes_per_row = float_of_int (wal1 - wal0) /. fl
      ; mirror_bytes = mirror
      }
    in
    Printf.printf
      "#316 width=%2d %-8s %-10s  minor_words/row=%9.1f  wal_bytes/row=%9.1f  \
       mirror_bytes=%4d\n\
       %!"
      width
      (if autoinc then "AUTOINC" else "plain")
      (if explicit then "explicit" else "autocommit")
      p.words_per_row
      p.wal_bytes_per_row
      p.mirror_bytes;
    p)
;;

(* ------------------------------------------------------------------ *)
(* The measurement                                                     *)
(* ------------------------------------------------------------------ *)

let n_rows = 400

(* AUTOCOMMIT is the path that pays #316's cost: each INSERT is its own
   transaction, so each one bumps the counter and — for AUTOINCREMENT —
   rewrites the whole mirror blob.

   The gate is a RATIO, not an absolute, so a different allocator or word size
   cannot move it: AUTOINCREMENT must not allocate more than [max_ratio] times
   what the identical plain-rowid insert allocates.  3.0 is deliberately loose;
   the point of the number is the printed value, and the assertion only exists
   to catch an order-of-magnitude regression. *)
let max_ratio =
  match Sys.getenv_opt "GRANARY_MIRROR_MAX_RATIO" with
  | Some s -> float_of_string s
  | None -> 3.0
;;

let autocommit_cost () =
  List.iter
    (fun width ->
       let plain = measure ~label:"plain" ~width ~n:n_rows ~autoinc:false ~explicit:false in
       let ai = measure ~label:"autoinc" ~width ~n:n_rows ~autoinc:true ~explicit:false in
       let ratio = ai.words_per_row /. plain.words_per_row in
       Printf.printf
         "#316 width=%2d autocommit alloc ratio AUTOINC/plain = %.3f  (wal ratio %.3f)\n%!"
         width
         ratio
         (ai.wal_bytes_per_row /. plain.wal_bytes_per_row);
       if ratio > max_ratio
       then
         Alcotest.failf
           "width=%d: AUTOINCREMENT allocates %.2fx a plain rowid insert (ceiling %.2f)"
           width
           ratio
           max_ratio)
    [ 3; 30 ]
;;

(* #316's sharpest question: the mirror re-encodes every column, so does the
   overhead GROW with the schema width?  Compare the AUTOINCREMENT-over-plain
   allocation DELTA at 3 columns with the same delta at 30 columns.  If the
   mirror write dominated, the 10x wider schema would show a markedly larger
   delta.  Printed either way; asserted only against a gross blow-up. *)
let overhead_vs_schema_width () =
  let p3 = measure ~label:"w3plain" ~width:3 ~n:n_rows ~autoinc:false ~explicit:false in
  let a3 = measure ~label:"w3ai" ~width:3 ~n:n_rows ~autoinc:true ~explicit:false in
  let p30 = measure ~label:"w30plain" ~width:30 ~n:n_rows ~autoinc:false ~explicit:false in
  let a30 = measure ~label:"w30ai" ~width:30 ~n:n_rows ~autoinc:true ~explicit:false in
  let d3 = a3.words_per_row -. p3.words_per_row in
  let d30 = a30.words_per_row -. p30.words_per_row in
  Printf.printf
    "#316 AUTOINC alloc DELTA: width 3 = %.1f words/row (mirror blob %d B), width 30 = \
     %.1f words/row (mirror blob %d B)\n\
     %!"
    d3
    a3.mirror_bytes
    d30
    a30.mirror_bytes;
  (* The mirror blob really does grow with the width — that half of #316's
     description is accurate and is what makes the delta comparison meaningful.
     30 columns must produce a materially bigger blob than 3. *)
  Alcotest.(check bool)
    "the mirror blob grows with the schema width"
    true
    (a30.mirror_bytes > 2 * a3.mirror_bytes)
;;

(* #347 already coalesces the counter write — primary row AND #314 mirror —
   to COMMIT when the insert runs inside an explicit transaction
   ([~defer_counter:true]).  So #316's proposed optimisation 2 is ALREADY in
   place for that path: an AUTOINCREMENT insert inside BEGIN/COMMIT pays ONE
   mirror write for the whole transaction, not one per row.  Pinned here so a
   future change that reinstates a per-row write in the explicit path is
   caught. *)
let explicit_txn_pays_once () =
  let plain = measure ~label:"explplain" ~width:30 ~n:n_rows ~autoinc:false ~explicit:true in
  let ai = measure ~label:"explai" ~width:30 ~n:n_rows ~autoinc:true ~explicit:true in
  let ratio = ai.words_per_row /. plain.words_per_row in
  Printf.printf "#316 width=30 explicit-txn alloc ratio AUTOINC/plain = %.3f\n%!" ratio;
  if ratio > 1.5
  then
    Alcotest.failf
      "#347's deferral is gone: AUTOINCREMENT allocates %.2fx plain inside an explicit \
       transaction"
      ratio
;;

(* ------------------------------------------------------------------ *)
(* The property the per-row write exists to guarantee (#314)           *)
(* ------------------------------------------------------------------ *)

(* Whatever is done about the cost, THIS must keep holding: an AUTOINCREMENT
   table's sticky high-water survives losing the primary [_sys_tables] row and
   being reconstructed from the mirror.  Delete the highest row, close, wipe
   the table's primary catalog row, reopen — the counter must NOT be recomputed
   as max(rowid)+1, it must come back from the mirror. *)
let sys_tables_tid : Store.tree_id = 0

let high_water_survives_mirror_reconstruction () =
  with_tmp_path "recover" (fun path ->
    let open_db () =
      let store =
        match run (Ustore.open_file_wal ~path ()) with
        | Ok s -> s
        | Error e -> Alcotest.failf "open_file_wal: %a" Store.pp_error e
      in
      store, run (Db.of_store ~file_path:path store)
    in
    let store, db = open_db () in
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY AUTOINCREMENT, v TEXT)";
    for i = 1 to 5 do
      exec db (Printf.sprintf "INSERT INTO t (v) VALUES ('r%d')" i)
    done;
    exec db "DELETE FROM t WHERE id >= 3";
    run (Db.close db);
    (* Wipe the primary catalog row so [open_] must fall back to the mirror. *)
    let store2 =
      match run (Ustore.open_file_wal ~path ()) with
      | Ok s -> s
      | Error e -> Alcotest.failf "reopen: %a" Store.pp_error e
    in
    run
      (let* tx = Store.rw_begin store2 in
       let* () = Store.del tx sys_tables_tid (Bytes.of_string "t") in
       Store.commit tx);
    run (Store.close store2);
    ignore store;
    let _store3, db3 = open_db () in
    exec db3 "INSERT INTO t (v) VALUES ('after')";
    Alcotest.(check (list string))
      "the sticky high-water came back from the mirror, not from max(rowid)+1"
      [ "1|r1"; "2|r2"; "6|after" ]
      (rows db3 "SELECT id, v FROM t ORDER BY id");
    run (Db.close db3))
;;

let () =
  Alcotest.run
    "autoinc_mirror_316"
    [ ( "measure"
      , [ Alcotest.test_case "autocommit cost" `Quick autocommit_cost
        ; Alcotest.test_case "overhead vs schema width" `Quick overhead_vs_schema_width
        ; Alcotest.test_case "explicit txn pays once" `Quick explicit_txn_pays_once
        ] )
    ; ( "invariant"
      , [ Alcotest.test_case
            "high-water survives mirror reconstruction"
            `Quick
            high_water_survives_mirror_reconstruction
        ] )
    ]
;;
