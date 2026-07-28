(** #417 Phase 3: closing the loop.  Lift the row-level delta feed (#419) into
    Z-set deltas ({!Granary_ivm.Delta}) and drive the incremental operators
    (#421) with them, so a materialized view is maintained from a transaction's
    changes instead of recomputed — and verify the maintained view tracks the
    authoritative database state across INSERT/UPDATE/DELETE. *)

module Zset = Granary_ivm.Zset
module Delta = Granary_ivm.Delta

(* ---- The lift: row mutation events -> Z-set deltas ---- *)

module ZI = Zset.Make (struct
    type t = int

    let compare = Int.compare
    let pp = Format.pp_print_int
  end)

module DI = Delta.Make (ZI)

let zlist z = List.sort compare (ZI.to_list z)

let test_lift_insert () =
  Alcotest.(check (list (pair int int)))
    "insert is +1"
    [ 7, 1 ]
    (zlist (DI.of_event (Delta.Insert 7)))
;;

let test_lift_delete () =
  Alcotest.(check (list (pair int int)))
    "delete is -1"
    [ 7, -1 ]
    (zlist (DI.of_event (Delta.Delete 7)))
;;

let test_lift_update () =
  Alcotest.(check (list (pair int int)))
    "update is -old +new"
    [ 1, -1; 2, 1 ]
    (zlist (DI.of_event (Delta.Update (1, 2))))
;;

let test_lift_update_same_cancels () =
  Alcotest.(check bool)
    "update to identical element is a no-op"
    true
    (ZI.is_zero (DI.of_event (Delta.Update (5, 5))))
;;

let test_lift_events_sum () =
  let z =
    DI.of_events [ Delta.Insert 1; Delta.Insert 1; Delta.Delete 2; Delta.Update (3, 1) ]
  in
  (* elt 1: +1,+1, and +new from Update(3,1) => 3; elt 2: -1; elt 3: -old => -1 *)
  Alcotest.(check (list (pair int int)))
    "events accumulate"
    [ 1, 3; 2, -1; 3, -1 ]
    (zlist z)
;;

(* ---- End-to-end: a maintained view tracks the authoritative DB state ---- *)

module Db = Granary.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* Result row of a (group, aggregate) relation. *)
module GAgg = struct
  type t = string * int

  let compare = compare
  let pp ppf (g, a) = Format.fprintf ppf "(%s,%d)" g a
end

(* COUNT grouped by grp: the input element is just the group label. *)
module ZGrp = Zset.Make (struct
    type t = string

    let compare = compare
    let pp = Format.pp_print_string
  end)

module DGrp = Delta.Make (ZGrp)
module ZOut = Zset.Make (GAgg)

module CountView = Granary_ivm.Aggregate.Make (struct
    module In = ZGrp
    module Out = ZOut

    type group = string

    let compare_group = compare
    let group_of g = g
    let measure _ = 1
    let result g c = g, c
  end)

(* SUM(amt) GROUP BY grp: the input element is (group, amt). *)
module ZGA = Zset.Make (GAgg)
module DGA = Delta.Make (ZGA)

module SumView = Granary_ivm.Aggregate.Make (struct
    module In = ZGA
    module Out = ZOut

    type group = string

    let compare_group = compare
    let group_of (g, _) = g
    let measure (_, amt) = amt
    let result g s = g, s
  end)

let grp_of (row : Db.row) =
  match row.(1) with
  | Db.V_text s -> s
  | _ -> Alcotest.fail "expected TEXT grp at column 1"
;;

let amt_of (row : Db.row) =
  match row.(2) with
  | Db.V_int i -> Int64.to_int i
  | _ -> Alcotest.fail "expected INTEGER amt at column 2"
;;

let changes_for db sql =
  match List.assoc_opt "t" (unwrap (run (Db.execute_with_changes db sql))) with
  | Some cs -> cs
  | None -> []
;;

(* The maintained-view sorted (group, value) rows. *)
let view_rows out = out |> ZOut.to_list |> List.map fst |> List.sort compare

(* The authoritative answer, read straight from the engine. *)
let db_rows db agg : (string * int) list =
  let sql = Printf.sprintf "SELECT grp, %s FROM t GROUP BY grp" agg in
  let stream = unwrap (run (Db.query db sql)) in
  run (Lwt_stream.to_list stream)
  |> List.map (fun r ->
    match r.(0), r.(1) with
    | Db.V_text g, Db.V_int v -> g, Int64.to_int v
    | _ -> Alcotest.fail "unexpected aggregate row shape")
  |> List.sort compare
;;

(* Run [sql], feed its row-changes into both maintained views, then assert each
   view equals the authoritative GROUP BY read back from the engine. *)
let test_e2e_views_track_db () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    let cv = CountView.create ()
    and sv = SumView.create () in
    let step sql =
      let cs = changes_for db sql in
      let _ =
        CountView.step
          cv
          (DGrp.of_events
             (List.map
                (function
                  | Db.Inserted { row; _ } -> Delta.Insert (grp_of row)
                  | Db.Deleted { row; _ } -> Delta.Delete (grp_of row)
                  | Db.Updated { old_row; new_row; _ } ->
                    Delta.Update (grp_of old_row, grp_of new_row))
                cs))
      in
      let _ =
        SumView.step
          sv
          (DGA.of_events
             (List.map
                (function
                  | Db.Inserted { row; _ } -> Delta.Insert (grp_of row, amt_of row)
                  | Db.Deleted { row; _ } -> Delta.Delete (grp_of row, amt_of row)
                  | Db.Updated { old_row; new_row; _ } ->
                    Delta.Update
                      ((grp_of old_row, amt_of old_row), (grp_of new_row, amt_of new_row)))
                cs))
      in
      Alcotest.(check (list (pair string int)))
        (Printf.sprintf "COUNT view tracks DB after %S" sql)
        (db_rows db "COUNT(*)")
        (view_rows (CountView.output cv));
      Alcotest.(check (list (pair string int)))
        (Printf.sprintf "SUM view tracks DB after %S" sql)
        (db_rows db "SUM(amt)")
        (view_rows (SumView.output sv))
    in
    step "INSERT INTO t VALUES (1, 'a', 10)";
    step "INSERT INTO t VALUES (2, 'a', 20)";
    step "INSERT INTO t VALUES (3, 'b', 5)";
    step "UPDATE t SET amt = 99 WHERE id = 1";
    (* 1 to b: a loses a row, b gains one *)
    step "UPDATE t SET grp = 'b' WHERE id = 2";
    (* delete the last 'a' row: group 'a' must vanish from both view and DB *)
    step "DELETE FROM t WHERE id = 1";
    (* 'a' reappears *)
    step "INSERT INTO t VALUES (4, 'a', 7)";
    (* multi-row delete *)
    step "DELETE FROM t WHERE grp = 'b'")
;;

let () =
  Alcotest.run
    "ivm_view_417"
    [ ( "lift"
      , [ Alcotest.test_case "insert" `Quick test_lift_insert
        ; Alcotest.test_case "delete" `Quick test_lift_delete
        ; Alcotest.test_case "update" `Quick test_lift_update
        ; Alcotest.test_case "update same cancels" `Quick test_lift_update_same_cancels
        ; Alcotest.test_case "events sum" `Quick test_lift_events_sum
        ] )
    ; ( "end-to-end"
      , [ Alcotest.test_case "count+sum views track db" `Quick test_e2e_views_track_db ] )
    ]
;;
