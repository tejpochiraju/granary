(** #408 — Stateful (STM-style) QCheck model test for the transaction /
    SAVEPOINT contract.

    The suite has strong example-based and differential (vs real SQLite)
    coverage of transactions, but no generated command-sequence testing. The
    transaction/SAVEPOINT bugs we shipped (#293, #303) were all
    transition-sequence bugs caught reactively by hand-written cases. This
    test generates valid sequences of BEGIN / COMMIT / ROLLBACK / SAVEPOINT /
    RELEASE / ROLLBACK TO interleaved with INSERT / UPDATE / DELETE, runs them
    against the engine, and after every command asserts the visible rows equal
    an OCaml reference model.

    Oracle insight: for a plain rowid table ([INTEGER PRIMARY KEY], no
    AUTOINCREMENT) the correct next rowid is [max(id)+1] over the current
    working rows. A model that tracks only row content therefore computes the
    right next-rowid for free, while the engine caches the counter separately
    — so this content model is automatically the right oracle for the
    #293/#303 counter-revert bug class without modelling the cache at all. *)

module Db = Granary.Db

let run = Lwt_main.run

(* ------------------------------------------------------------------ *)
(* Commands                                                            *)
(* ------------------------------------------------------------------ *)

type cmd =
  | Insert of int (* value; rowid auto-assigned *)
  | Update of int * int (* nth existing row (mod count), new value *)
  | Delete of int (* nth existing row (mod count) *)
  | Begin
  | Commit
  | Rollback
  | Savepoint of string
  | Release of string
  | Rollback_to of string

let name_pool = [| "s0"; "s1"; "s2"; "s3" |]

let show_cmd = function
  | Insert v -> Printf.sprintf "INSERT %d" v
  | Update (k, v) -> Printf.sprintf "UPDATE[#%d]:=%d" k v
  | Delete k -> Printf.sprintf "DELETE[#%d]" k
  | Begin -> "BEGIN"
  | Commit -> "COMMIT"
  | Rollback -> "ROLLBACK"
  | Savepoint n -> "SAVEPOINT " ^ n
  | Release n -> "RELEASE " ^ n
  | Rollback_to n -> "ROLLBACK TO " ^ n
;;

let print_cmds cs = "[ " ^ String.concat " ; " (List.map show_cmd cs) ^ " ]"

(* ------------------------------------------------------------------ *)
(* Reference model                                                     *)
(* ------------------------------------------------------------------ *)

(* [saves] is innermost-first: each frame snapshots the working rows as of
   the SAVEPOINT that created it. *)
type model =
  { committed : (int * int) list
  ; current : (int * int) list
  ; in_txn : bool
  ; saves : (string * (int * int) list) list
  }

let empty = { committed = []; current = []; in_txn = false; saves = [] }
let sort_rows = List.sort (fun (a, _) (b, _) -> compare a b)
let next_id rows = 1 + List.fold_left (fun m (i, _) -> max m i) 0 rows

(* DML outside an explicit txn auto-commits. *)
let autocommit m = if m.in_txn then m else { m with committed = m.current }

(* Interpret one command against the model. Returns the SQL to run against the
   engine ([None] = precondition unmet, skip both model and engine) and the
   post-state. *)
let interpret m cmd : string option * model =
  match cmd with
  | Insert v ->
    let id = next_id m.current in
    let current = sort_rows ((id, v) :: m.current) in
    Some (Printf.sprintf "INSERT INTO t (b) VALUES (%d)" v), autocommit { m with current }
  | Update (k, v) ->
    (match m.current with
     | [] -> None, m
     | _ ->
       let ids = List.map fst m.current in
       let id = List.nth ids (k mod List.length ids) in
       let current =
         sort_rows (List.map (fun (i, x) -> if i = id then i, v else i, x) m.current)
       in
       ( Some (Printf.sprintf "UPDATE t SET b=%d WHERE a=%d" v id)
       , autocommit { m with current } ))
  | Delete k ->
    (match m.current with
     | [] -> None, m
     | _ ->
       let ids = List.map fst m.current in
       let id = List.nth ids (k mod List.length ids) in
       let current = List.filter (fun (i, _) -> i <> id) m.current in
       Some (Printf.sprintf "DELETE FROM t WHERE a=%d" id), autocommit { m with current })
  | Begin ->
    if m.in_txn then None, m else Some "BEGIN", { m with in_txn = true; saves = [] }
  | Commit ->
    if not m.in_txn
    then None, m
    else Some "COMMIT", { m with in_txn = false; saves = []; committed = m.current }
  | Rollback ->
    if not m.in_txn
    then None, m
    else Some "ROLLBACK", { m with in_txn = false; saves = []; current = m.committed }
  | Savepoint n ->
    if not m.in_txn
    then None, m
    else Some ("SAVEPOINT " ^ n), { m with saves = (n, m.current) :: m.saves }
  | Release n ->
    if not (List.mem_assoc n m.saves)
    then None, m
    else (
      (* RELEASE drops the innermost frame named [n] and all inner frames;
         rows are kept (folded into the enclosing scope). *)
      let rec drop = function
        | (nm, _) :: rest -> if nm = n then rest else drop rest
        | [] -> []
      in
      Some ("RELEASE " ^ n), { m with saves = drop m.saves })
  | Rollback_to n ->
    if not (List.mem_assoc n m.saves)
    then None, m
    else (
      (* ROLLBACK TO restores the snapshot of the nearest frame named [n],
         discards inner frames, but KEEPS [n] on the stack. *)
      let rec find = function
        | (nm, snap) :: rest -> if nm = n then (nm, snap) :: rest, snap else find rest
        | [] -> m.saves, m.current
      in
      let saves, snap = find m.saves in
      Some ("ROLLBACK TO " ^ n), { m with saves; current = snap })
;;

let run_model cmds = List.fold_left (fun m c -> snd (interpret m c)) empty cmds

(* ------------------------------------------------------------------ *)
(* Generator: biased toward precondition-valid sequences. The runtime model
   is the source of truth and no-ops any residual-invalid command, which keeps
   model and engine in sync under shrinking. *)
(* ------------------------------------------------------------------ *)

let gen_cmds : cmd list QCheck.Gen.t =
  fun rng ->
  let ri lo hi = QCheck.Gen.int_range lo hi rng in
  let len = ri 1 40 in
  let in_txn = ref false in
  let names = ref [] in
  (* innermost-first, mirrors model.saves *)
  let nrows = ref 0 in
  (* approx live row count (only biases generation) *)
  let crows = ref 0 in
  (* approx committed row count *)
  let acc = ref [] in
  for _ = 1 to len do
    let opts = ref [ `Ins ] in
    if !nrows > 0 then opts := `Upd :: `Del :: !opts;
    if !in_txn
    then (
      opts := `Commit :: `Rollback :: `Sp :: !opts;
      if !names <> [] then opts := `Rel :: `Rbto :: !opts)
    else opts := `Begin :: !opts;
    let o = List.nth !opts (ri 0 (List.length !opts - 1)) in
    let cmd =
      match o with
      | `Ins ->
        incr nrows;
        if not !in_txn then crows := !nrows;
        Insert (ri 0 999)
      | `Upd -> Update (ri 0 1_000_000, ri 0 999)
      | `Del ->
        if !nrows > 0 then decr nrows;
        if not !in_txn then crows := !nrows;
        Delete (ri 0 1_000_000)
      | `Begin ->
        in_txn := true;
        names := [];
        Begin
      | `Commit ->
        in_txn := false;
        names := [];
        crows := !nrows;
        Commit
      | `Rollback ->
        in_txn := false;
        names := [];
        nrows := !crows;
        Rollback
      | `Sp ->
        let n = name_pool.(ri 0 (Array.length name_pool - 1)) in
        names := n :: !names;
        Savepoint n
      | `Rel ->
        let n = List.nth !names (ri 0 (List.length !names - 1)) in
        let rec drop = function
          | x :: r -> if x = n then r else drop r
          | [] -> []
        in
        names := drop !names;
        Release n
      | `Rbto ->
        let n = List.nth !names (ri 0 (List.length !names - 1)) in
        let rec keep = function
          | x :: r -> if x = n then x :: r else keep r
          | [] -> []
        in
        names := keep !names;
        Rollback_to n
    in
    acc := cmd :: !acc
  done;
  List.rev !acc
;;

let arb_cmds = QCheck.make ~print:print_cmds ~shrink:QCheck.Shrink.list gen_cmds

(* ------------------------------------------------------------------ *)
(* Engine driver                                                       *)
(* ------------------------------------------------------------------ *)

let err_str = function
  | Db.Parse e -> "parse: " ^ e
  | Db.Sema _ -> "sema"
  | Db.Runtime e -> "runtime: " ^ e
  | Db.History_unavailable -> "history_unavailable"
  | Db.History_pruned -> "history_pruned"
;;

let query_pairs db =
  match run (Db.query db "SELECT a, b FROM t ORDER BY a ASC") with
  | Error _ -> [ -1, -1 ]
  | Ok stream ->
    List.map
      (fun row ->
         match row.(0), row.(1) with
         | Db.V_int a, Db.V_int b -> Int64.to_int a, Int64.to_int b
         | _ -> -2, -2)
      (run (Lwt_stream.to_list stream))
;;

let show_rows rs =
  "[" ^ String.concat ";" (List.map (fun (a, b) -> Printf.sprintf "(%d,%d)" a b) rs) ^ "]"
;;

exception Diverged of string

let prop_model_matches_engine =
  QCheck.Test.make
    ~count:400
    ~name:"savepoint/txn: engine matches reference model"
    arb_cmds
    (fun cmds ->
       let db = run (Db.open_in_memory ()) in
       Fun.protect
         ~finally:(fun () -> run (Db.close db))
         (fun () ->
            (match
               run (Db.execute db "CREATE TABLE t (a INTEGER PRIMARY KEY, b INTEGER)")
             with
             | Ok () -> ()
             | Error e -> failwith ("create: " ^ err_str e));
            let m = ref empty in
            try
              List.iter
                (fun cmd ->
                   let sql_opt, m' = interpret !m cmd in
                   m := m';
                   (match sql_opt with
                    | None -> ()
                    | Some sql ->
                      (match run (Db.execute db sql) with
                       | Ok () -> ()
                       | Error e ->
                         raise
                           (Diverged (Printf.sprintf "exec %S failed: %s" sql (err_str e)))));
                   let actual = query_pairs db in
                   if actual <> m'.current
                   then
                     raise
                       (Diverged
                          (Printf.sprintf
                             "after %s: engine=%s model=%s"
                             (show_cmd cmd)
                             (show_rows actual)
                             (show_rows m'.current))))
                cmds;
              true
            with
            | Diverged msg ->
              Printf.eprintf "DIVERGENCE: %s\n%!" msg;
              false))
;;

(* ------------------------------------------------------------------ *)
(* Deterministic model self-checks: guard against a WRONG model passing
   silently. These mirror the oracle-verified cases in test_txn.ml. *)
(* ------------------------------------------------------------------ *)

let check_model name cmds expected =
  let m = run_model cmds in
  Alcotest.(check (list (pair int int))) name expected m.current
;;

(* #303 nested: a bump under the INNER savepoint is reverted by ROLLBACK TO the
   OUTER one; the rolled-back rowid 2 is then reused. *)
let test_model_rollback_to_outer_reuses_rowid () =
  check_model
    "ROLLBACK TO outer reuses rowid bumped under inner savepoint"
    [ Begin
    ; Insert 10
    ; Savepoint "s0"
    ; Savepoint "s1"
    ; Insert 20
    ; Rollback_to "s0"
    ; Insert 30
    ; Commit
    ]
    [ 1, 10; 2, 30 ]
;;

(* #303: ROLLBACK TO reuses the savepoint-reverted rowid. *)
let test_model_rollback_to_reuses_rowid () =
  check_model
    "ROLLBACK TO reuses rolled-back rowid 2"
    [ Begin; Insert 1; Savepoint "s0"; Insert 2; Rollback_to "s0"; Insert 3; Commit ]
    [ 1, 1; 2, 3 ]
;;

(* RELEASE keeps the changes made since the savepoint. *)
let test_model_release_keeps_changes () =
  check_model
    "RELEASE folds changes into the enclosing scope"
    [ Begin; Insert 1; Savepoint "s0"; Insert 2; Release "s0"; Commit ]
    [ 1, 1; 2, 2 ]
;;

(* Full ROLLBACK reverts to the last committed state and reuses the rowid. *)
let test_model_rollback_reverts_and_reuses () =
  check_model
    "full ROLLBACK reverts to committed state"
    [ Insert 10; Begin; Insert 20; Rollback; Insert 30 ]
    [ 1, 10; 2, 30 ]
;;

(* ------------------------------------------------------------------ *)
(* Engine regression tests for #409 — rowid reuse contract for a PLAIN
   (non-AUTOINCREMENT) rowid table, with the real-SQLite oracle baked into the
   expected values. These define the fix target (TDD) and also probe whether
   the divergence is autocommit-only or also occurs inside an explicit txn. *)
(* ------------------------------------------------------------------ *)

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect ~finally:(fun () -> run (Db.close db)) (fun () -> f db)
;;

let exec_ok db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S failed: %s" sql (err_str e)
;;

let check_rows db expected msg =
  Alcotest.(check (list (pair int int))) msg expected (query_pairs db)
;;

(* Real SQLite: empty plain rowid table reuses rowid 1 after a committed
   delete-of-max. *)
let test_engine_autocommit_delete_only_reuses () =
  with_db (fun db ->
    exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY, b INTEGER)";
    exec_ok db "INSERT INTO t (b) VALUES (990)";
    exec_ok db "DELETE FROM t WHERE a = 1";
    exec_ok db "INSERT INTO t (b) VALUES (514)";
    check_rows db [ 1, 514 ] "reuses rowid 1 after deleting the only row")
;;

(* Real SQLite: deleting the max of several rows lets the next insert reuse it. *)
let test_engine_autocommit_delete_max_reuses () =
  with_db (fun db ->
    exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY, b INTEGER)";
    exec_ok db "INSERT INTO t (b) VALUES (1)";
    exec_ok db "INSERT INTO t (b) VALUES (2)";
    exec_ok db "INSERT INTO t (b) VALUES (3)";
    exec_ok db "DELETE FROM t WHERE a = 3";
    exec_ok db "INSERT INTO t (b) VALUES (9)";
    check_rows db [ 1, 1; 2, 2; 3, 9 ] "reuses rowid 3 after deleting the max row")
;;

(* Real SQLite: deleting a NON-max row does not change the next allocation. *)
let test_engine_autocommit_delete_middle_no_reuse () =
  with_db (fun db ->
    exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY, b INTEGER)";
    exec_ok db "INSERT INTO t (b) VALUES (1)";
    exec_ok db "INSERT INTO t (b) VALUES (2)";
    exec_ok db "INSERT INTO t (b) VALUES (3)";
    exec_ok db "DELETE FROM t WHERE a = 2";
    exec_ok db "INSERT INTO t (b) VALUES (9)";
    check_rows
      db
      [ 1, 1; 3, 3; 4, 9 ]
      "next rowid is 4 (unchanged) after deleting a middle row")
;;

(* Probe: same as the first case but inside an explicit transaction. *)
let test_engine_intxn_delete_reuses () =
  with_db (fun db ->
    exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY, b INTEGER)";
    exec_ok db "BEGIN";
    exec_ok db "INSERT INTO t (b) VALUES (990)";
    exec_ok db "DELETE FROM t WHERE a = 1";
    exec_ok db "INSERT INTO t (b) VALUES (514)";
    exec_ok db "COMMIT";
    check_rows db [ 1, 514 ] "in-txn: reuses rowid 1 after deleting the only row")
;;

(* Regression guard: AUTOINCREMENT must NOT reuse (sticky high-water). *)
let test_engine_autoincrement_no_reuse () =
  with_db (fun db ->
    exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b INTEGER)";
    exec_ok db "INSERT INTO t (b) VALUES (1)";
    exec_ok db "DELETE FROM t WHERE a = 1";
    exec_ok db "INSERT INTO t (b) VALUES (9)";
    check_rows db [ 2, 9 ] "AUTOINCREMENT does not reuse rowid 1")
;;

let () =
  Alcotest.run
    "savepoint_stm"
    [ ( "engine_409"
      , [ Alcotest.test_case
            "autocommit_delete_only_reuses"
            `Quick
            test_engine_autocommit_delete_only_reuses
        ; Alcotest.test_case
            "autocommit_delete_max_reuses"
            `Quick
            test_engine_autocommit_delete_max_reuses
        ; Alcotest.test_case
            "autocommit_delete_middle_no_reuse"
            `Quick
            test_engine_autocommit_delete_middle_no_reuse
        ; Alcotest.test_case "intxn_delete_reuses" `Quick test_engine_intxn_delete_reuses
        ; Alcotest.test_case
            "autoincrement_no_reuse"
            `Quick
            test_engine_autoincrement_no_reuse
        ] )
    ; ( "model_self_check"
      , [ Alcotest.test_case
            "rollback_to_outer_reuses_rowid"
            `Quick
            test_model_rollback_to_outer_reuses_rowid
        ; Alcotest.test_case
            "rollback_to_reuses_rowid"
            `Quick
            test_model_rollback_to_reuses_rowid
        ; Alcotest.test_case
            "release_keeps_changes"
            `Quick
            test_model_release_keeps_changes
        ; Alcotest.test_case
            "rollback_reverts_and_reuses"
            `Quick
            test_model_rollback_reverts_and_reuses
        ] )
    ; "qcheck", [ QCheck_alcotest.to_alcotest prop_model_matches_engine ]
    ]
;;
