(** Contract tests for the B+-tree backed Store (via [Store.open_file]).

    These complement [test_store.ml] (which exercises the in-memory
    backend) by verifying the persistent backend's behaviour: durability
    across reopen, the same put/get/del/cursor semantics, and the
    additional B+-tree-specific size limits. *)

open Lwt.Syntax

module S = Sqlocaml_store.Store

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let bs s = Bytes.of_string s

let run = Lwt_main.run

let counter = ref 0
let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_store_btree_%04d.db" n

let cleanup path = (try Unix.unlink path with _ -> ())

let ok_store : (S.t, S.error) result -> S.t = function
  | Ok t -> t
  | Error e -> Alcotest.failf "open_file error: %a" S.pp_error e

let bytes_eq =
  Alcotest.testable
    (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b))
    Bytes.equal

let bytes_opt_eq =
  Alcotest.(option
    (testable (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b))
       Bytes.equal))

(* Run a test against a fresh DB file, cleaning up on the way out. *)
let with_fresh_db ~f =
  let path = fresh_path () in
  cleanup path;
  Lwt.finalize
    (fun () -> f path)
    (fun () -> cleanup path; Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* 1. open_file basics                                                  *)
(* ------------------------------------------------------------------ *)

let test_open_creates_file () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    Alcotest.(check bool) "file exists on disk" true (Sys.file_exists path);
    let* () = S.close s in
    Lwt.return_unit))

let test_basic_put_get () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "put then get" (Some (bs "v")) got;
    let* () = S.close s in
    Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* 2. Persistence across reopen                                         *)
(* ------------------------------------------------------------------ *)

let test_persistence_after_reopen () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "alpha") (bs "1") in
    let* () = S.put tx 0 (bs "beta")  (bs "2") in
    let* () = S.put tx 0 (bs "gamma") (bs "3") in
    let* () = S.commit tx in
    let* () = S.close s in
    let* r2 = S.open_file ~path in
    let s2 = ok_store r2 in
    let* tx = S.ro_begin s2 in
    let* a = S.get tx 0 (bs "alpha") in
    let* b = S.get tx 0 (bs "beta") in
    let* g = S.get tx 0 (bs "gamma") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "alpha" (Some (bs "1")) a;
    Alcotest.check bytes_opt_eq "beta"  (Some (bs "2")) b;
    Alcotest.check bytes_opt_eq "gamma" (Some (bs "3")) g;
    let* () = S.close s2 in
    Lwt.return_unit))

let test_delete_persists () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.rw_begin s in
    let* () = S.del tx 0 (bs "k") in
    let* () = S.commit tx in
    let* () = S.close s in
    let* r2 = S.open_file ~path in
    let s2 = ok_store r2 in
    let* tx = S.ro_begin s2 in
    let* got = S.get tx 0 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "deleted stays deleted" None got;
    let* () = S.close s2 in
    Lwt.return_unit))

let test_no_commit_no_persistence () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    (* Rollback instead of commit — data must NOT persist. *)
    let* () = S.rollback tx in
    let* () = S.close s in
    let* r2 = S.open_file ~path in
    let s2 = ok_store r2 in
    let* tx = S.ro_begin s2 in
    let* got = S.get tx 0 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "rolled-back put does not persist" None got;
    let* () = S.close s2 in
    Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* 3. Multiple trees                                                    *)
(* ------------------------------------------------------------------ *)

let test_multiple_trees_independent () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 5 (bs "k") (bs "five") in
    let* () = S.put tx 7 (bs "k") (bs "seven") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* g5 = S.get tx 5 (bs "k") in
    let* g7 = S.get tx 7 (bs "k") in
    let* g9 = S.get tx 9 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "tree 5"  (Some (bs "five"))  g5;
    Alcotest.check bytes_opt_eq "tree 7"  (Some (bs "seven")) g7;
    Alcotest.check bytes_opt_eq "tree 9 empty" None g9;
    let* () = S.close s in
    Lwt.return_unit))

let test_multiple_trees_persist () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 16 (bs "x") (bs "X") in
    let* () = S.put tx 17 (bs "y") (bs "Y") in
    let* () = S.put tx 18 (bs "z") (bs "Z") in
    let* () = S.commit tx in
    let* () = S.close s in
    let* r2 = S.open_file ~path in
    let s2 = ok_store r2 in
    let* tx = S.ro_begin s2 in
    let* a = S.get tx 16 (bs "x") in
    let* b = S.get tx 17 (bs "y") in
    let* c = S.get tx 18 (bs "z") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "tid 16 persists" (Some (bs "X")) a;
    Alcotest.check bytes_opt_eq "tid 17 persists" (Some (bs "Y")) b;
    Alcotest.check bytes_opt_eq "tid 18 persists" (Some (bs "Z")) c;
    let* () = S.close s2 in
    Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* 4. Cursor over B+-tree backend                                       *)
(* ------------------------------------------------------------------ *)

let collect_all c =
  let rec loop acc =
    match S.cursor_next c with
    | None -> List.rev acc
    | Some kv -> loop (kv :: acc)
  in
  loop []

let test_cursor_returns_sorted () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    (* Insert in arbitrary order *)
    let* () = S.put tx 0 (bs "c") (bs "3") in
    let* () = S.put tx 0 (bs "a") (bs "1") in
    let* () = S.put tx 0 (bs "b") (bs "2") in
    let* () = S.put tx 0 (bs "d") (bs "4") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let _ = S.cursor_first cur in
    let entries = collect_all cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    let keys = List.map (fun (k, _) -> Bytes.to_string k) entries in
    Alcotest.(check (list string)) "sorted" ["a"; "b"; "c"; "d"] keys;
    let* () = S.close s in
    Lwt.return_unit))

let test_cursor_seek_between () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "1") in
    let* () = S.put tx 0 (bs "c") (bs "3") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_seek cur (bs "b") in
    let nxt = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Not_found (`Greater k) ->
       Alcotest.check bytes_eq "greater" (bs "c") k
     | _ -> Alcotest.fail "expected Not_found Greater");
    (match nxt with
     | Some (k, _) -> Alcotest.check bytes_eq "next" (bs "c") k
     | None -> Alcotest.fail "expected Some");
    let* () = S.close s in
    Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* 5. Transaction serialisation                                         *)
(* ------------------------------------------------------------------ *)

(* Verify that a second rw_begin BLOCKS until the first commits.
   We do this by launching a second writer in the background after
   acquiring the first, observing it does not complete, then committing
   the first and confirming the second proceeds. *)
let test_rw_serialises () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx1 = S.rw_begin s in
    (* Start a second rw_begin in the background — it should block. *)
    let snd_started = ref false in
    let snd =
      let* tx2 = S.rw_begin s in
      snd_started := true;
      let* () = S.put tx2 0 (bs "from-2") (bs "v") in
      S.commit tx2
    in
    (* Give Lwt a chance to schedule.  Pause yields to the scheduler. *)
    let* () = Lwt.pause () in
    Alcotest.(check bool) "second rw blocks while first holds lock"
      false !snd_started;
    (* Commit first — second should now run. *)
    let* () = S.put tx1 0 (bs "from-1") (bs "v") in
    let* () = S.commit tx1 in
    let* () = snd in
    Alcotest.(check bool) "second rw eventually runs" true !snd_started;
    let* tx = S.ro_begin s in
    let* a = S.get tx 0 (bs "from-1") in
    let* b = S.get tx 0 (bs "from-2") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "first writer's write" (Some (bs "v")) a;
    Alcotest.check bytes_opt_eq "second writer's write" (Some (bs "v")) b;
    let* () = S.close s in
    Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* 6. Overwrite                                                          *)
(* ------------------------------------------------------------------ *)

let test_overwrite_persists () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "first") in
    let* () = S.commit tx in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "second") in
    let* () = S.commit tx in
    let* () = S.close s in
    let* r2 = S.open_file ~path in
    let s2 = ok_store r2 in
    let* tx = S.ro_begin s2 in
    let* got = S.get tx 0 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "overwrite persists"
      (Some (bs "second")) got;
    let* () = S.close s2 in
    Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* 7. Many puts (exercises B+-tree splits)                              *)
(* ------------------------------------------------------------------ *)

let test_many_puts () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let n = 200 in
    let* tx = S.rw_begin s in
    let* () =
      Lwt_list.iter_s (fun i ->
        let k = Bytes.of_string (Printf.sprintf "k%05d" i) in
        let v = Bytes.of_string (Printf.sprintf "v%05d" i) in
        S.put tx 0 k v
      ) (List.init n Fun.id)
    in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* results =
      Lwt_list.map_s (fun i ->
        let k = Bytes.of_string (Printf.sprintf "k%05d" i) in
        S.get tx 0 k
      ) (List.init n Fun.id)
    in
    let* () = S.ro_end tx in
    List.iteri (fun i got ->
      let expected = Some (Bytes.of_string (Printf.sprintf "v%05d" i)) in
      Alcotest.check bytes_opt_eq (Printf.sprintf "k%05d" i) expected got
    ) results;
    let* () = S.close s in
    Lwt.return_unit))

let test_many_puts_persist () =
  run (with_fresh_db ~f:(fun path ->
    let n = 100 in
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () =
      Lwt_list.iter_s (fun i ->
        let k = Bytes.of_string (Printf.sprintf "k%05d" i) in
        let v = Bytes.of_string (Printf.sprintf "v%05d" i) in
        S.put tx 0 k v
      ) (List.init n Fun.id)
    in
    let* () = S.commit tx in
    let* () = S.close s in
    let* r2 = S.open_file ~path in
    let s2 = ok_store r2 in
    let* tx = S.ro_begin s2 in
    let* results =
      Lwt_list.map_s (fun i ->
        let k = Bytes.of_string (Printf.sprintf "k%05d" i) in
        S.get tx 0 k
      ) (List.init n Fun.id)
    in
    let* () = S.ro_end tx in
    List.iteri (fun i got ->
      let expected = Some (Bytes.of_string (Printf.sprintf "v%05d" i)) in
      Alcotest.check bytes_opt_eq (Printf.sprintf "k%05d persists" i)
        expected got
    ) results;
    let* () = S.close s2 in
    Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* 8. Empty / missing                                                   *)
(* ------------------------------------------------------------------ *)

let test_missing_key () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 (bs "absent") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "missing key" None got;
    let* () = S.close s in
    Lwt.return_unit))

let test_cursor_empty () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let first = S.cursor_first cur in
    let nxt = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match first with
     | S.Not_found `End -> ()
     | _ -> Alcotest.fail "expected Not_found End");
    Alcotest.(check (option (pair bytes_eq bytes_eq)))
      "cursor_next on empty" None nxt;
    let* () = S.close s in
    Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* 9. QCheck: random put/del/get sequence matches in-memory backend     *)
(* ------------------------------------------------------------------ *)

(* The B+-tree limits keys to 512 bytes and values to 1024.  Restrict
   the generators below to stay well within those limits. *)

type op =
  | Put of bytes * bytes
  | Del of bytes

let op_gen =
  let open QCheck.Gen in
  let small_bytes =
    map Bytes.of_string (string_size ~gen:char (int_bound 32))
  in
  let small_val =
    map Bytes.of_string (string_size ~gen:char (int_bound 64))
  in
  oneof_weighted
    [ 3, map2 (fun k v -> Put (k, v)) small_bytes small_val
    ; 1, map (fun k -> Del k) small_bytes
    ]

let arb_ops =
  QCheck.make ~print:(fun ops ->
    let one = function
      | Put (k, v) ->
        Printf.sprintf "Put(%S,%S)" (Bytes.to_string k) (Bytes.to_string v)
      | Del k -> Printf.sprintf "Del(%S)" (Bytes.to_string k)
    in
    "[" ^ String.concat "; " (List.map one ops) ^ "]")
    QCheck.Gen.(list_size (int_bound 30) op_gen)

let apply_to_store s ops =
  let* tx = S.rw_begin s in
  let* () =
    Lwt_list.iter_s (fun op ->
      match op with
      | Put (k, v) -> S.put tx 0 k v
      | Del k      -> S.del tx 0 k
    ) ops
  in
  S.commit tx

let collect_via_cursor s =
  let* tx = S.ro_begin s in
  let* cur = S.cursor_open tx 0 in
  let _ = S.cursor_first cur in
  let acc = collect_all cur in
  S.cursor_close cur;
  let* () = S.ro_end tx in
  Lwt.return acc

let prop_btree_matches_mem =
  QCheck.Test.make ~count:200 ~name:"btree backend matches Mem backend"
    arb_ops
    (fun ops ->
      Lwt_main.run (
        let path = fresh_path () in
        cleanup path;
        Lwt.finalize (fun () ->
          let* r = S.open_file ~path in
          let bt = ok_store r in
          let mem = S.create () in
          let* () = apply_to_store bt ops in
          let* () = apply_to_store mem ops in
          let* bt_entries = collect_via_cursor bt in
          let* mem_entries = collect_via_cursor mem in
          let* () = S.close bt in
          let eq =
            List.length bt_entries = List.length mem_entries
            && List.for_all2 (fun (k1, v1) (k2, v2) ->
                 Bytes.equal k1 k2 && Bytes.equal v1 v2)
                 bt_entries mem_entries
          in
          Lwt.return eq
        ) (fun () -> cleanup path; Lwt.return_unit)
      ))

let prop_persist_roundtrip =
  QCheck.Test.make ~count:100
    ~name:"persistence: writes survive close/reopen"
    arb_ops
    (fun ops ->
      Lwt_main.run (
        let path = fresh_path () in
        cleanup path;
        Lwt.finalize (fun () ->
          let* r = S.open_file ~path in
          let s = ok_store r in
          let* () = apply_to_store s ops in
          let* before = collect_via_cursor s in
          let* () = S.close s in
          let* r2 = S.open_file ~path in
          let s2 = ok_store r2 in
          let* after = collect_via_cursor s2 in
          let* () = S.close s2 in
          let eq =
            List.length before = List.length after
            && List.for_all2 (fun (k1, v1) (k2, v2) ->
                 Bytes.equal k1 k2 && Bytes.equal v1 v2)
                 before after
          in
          Lwt.return eq
        ) (fun () -> cleanup path; Lwt.return_unit)
      ))

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_btree_matches_mem;
      prop_persist_roundtrip;
    ]
  in
  Alcotest.run "store_btree" [
    "open_file", [
      Alcotest.test_case "creates_file"   `Quick test_open_creates_file;
      Alcotest.test_case "basic_put_get"  `Quick test_basic_put_get;
    ];
    "persistence", [
      Alcotest.test_case "reopen"         `Quick test_persistence_after_reopen;
      Alcotest.test_case "delete_persists" `Quick test_delete_persists;
      Alcotest.test_case "no_commit"      `Quick test_no_commit_no_persistence;
      Alcotest.test_case "overwrite"      `Quick test_overwrite_persists;
      Alcotest.test_case "many_persist"   `Quick test_many_puts_persist;
    ];
    "multi_tree", [
      Alcotest.test_case "independent"    `Quick test_multiple_trees_independent;
      Alcotest.test_case "persist"        `Quick test_multiple_trees_persist;
    ];
    "cursor", [
      Alcotest.test_case "sorted"         `Quick test_cursor_returns_sorted;
      Alcotest.test_case "seek_between"   `Quick test_cursor_seek_between;
      Alcotest.test_case "empty"          `Quick test_cursor_empty;
    ];
    "txn", [
      Alcotest.test_case "rw_serialises"  `Quick test_rw_serialises;
    ];
    "scale", [
      Alcotest.test_case "many_puts"      `Quick test_many_puts;
    ];
    "missing", [
      Alcotest.test_case "missing_key"    `Quick test_missing_key;
    ];
    "qcheck", qcheck_tests;
  ]
