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
  QCheck.Test.make ~count:10_000 ~name:"btree backend matches Mem backend"
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
  QCheck.Test.make ~count:10_000
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
(* 10. Additional coverage: pp_error / rollback / cursor seek/value     *)
(* ------------------------------------------------------------------ *)

let test_pp_error_all_variants () =
  let s_block = Format.asprintf "%a" S.pp_error (S.Block_error "io")    in
  let s_corr  = Format.asprintf "%a" S.pp_error (S.Corruption  "bad")   in
  let s_klg   = Format.asprintf "%a" S.pp_error (S.Key_too_large 999)   in
  let s_vlg   = Format.asprintf "%a" S.pp_error (S.Value_too_large 12345) in
  let s_hdr   = Format.asprintf "%a" S.pp_error (S.Header_error "h")    in
  let contains hay needle =
    let hl = String.length hay and nl = String.length needle in
    let rec go i =
      if i > hl - nl then false
      else if String.sub hay i nl = needle then true
      else go (i + 1)
    in
    go 0
  in
  Alcotest.(check bool) "Block_error fmt"     true (contains s_block "Block_error");
  Alcotest.(check bool) "Corruption fmt"      true (contains s_corr  "Corruption");
  Alcotest.(check bool) "Key_too_large fmt"   true (contains s_klg   "999");
  Alcotest.(check bool) "Value_too_large fmt" true (contains s_vlg   "12345");
  Alcotest.(check bool) "Header_error fmt"    true (contains s_hdr   "Header_error")

(* Rollback on a Btree-backed store should restore the meta-tree from
   the committed header and drop the in-memory tree handles. *)
let test_rollback_btree_drops_uncommitted () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    (* First: commit one value *)
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "committed") (bs "1") in
    let* () = S.commit tx in
    (* Second: begin txn, put more, then rollback *)
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "rolled-back") (bs "2") in
    let* () = S.rollback tx in
    (* committed value still visible *)
    let* tx = S.ro_begin s in
    let* a = S.get tx 0 (bs "committed") in
    let* b = S.get tx 0 (bs "rolled-back") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "committed survives" (Some (bs "1")) a;
    Alcotest.check bytes_opt_eq "rolled-back gone"   None b;
    let* () = S.close s in
    Lwt.return_unit))

(* cursor_seek over Btree backend hitting `Found *)
let test_cursor_seek_found () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "alpha") (bs "1") in
    let* () = S.put tx 0 (bs "beta")  (bs "2") in
    let* () = S.put tx 0 (bs "gamma") (bs "3") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_seek cur (bs "beta") in
    let v_before_next = S.cursor_value cur in
    let nxt = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Found k -> Alcotest.check bytes_eq "Found beta" (bs "beta") k
     | _ -> Alcotest.fail "expected Found");
    Alcotest.check bytes_opt_eq "cursor_value at Found"
      (Some (bs "2")) v_before_next;
    (match nxt with
     | Some (k, v) ->
       Alcotest.check bytes_eq "next is beta (positioned)" (bs "beta") k;
       Alcotest.check bytes_eq "value is 2" (bs "2") v
     | None -> Alcotest.fail "expected Some");
    let* () = S.close s in
    Lwt.return_unit))

(* cursor_seek past end on Btree backend yields Not_found `End *)
let test_cursor_seek_past_end () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "1") in
    let* () = S.put tx 0 (bs "b") (bs "2") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_seek cur (bs "zzz") in
    let nxt = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Not_found `End -> ()
     | _ -> Alcotest.fail "expected Not_found End");
    Alcotest.(check (option (pair bytes_eq bytes_eq)))
      "next past end is None" None nxt;
    let* () = S.close s in
    Lwt.return_unit))

(* cursor_first on a non-empty Btree backend returns Found of first key.
   cursor_value before any next call should yield the positioned value. *)
let test_cursor_first_btree () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "x") (bs "1") in
    let* () = S.put tx 0 (bs "y") (bs "2") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let f = S.cursor_first cur in
    let v = S.cursor_value cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match f with
     | S.Found k -> Alcotest.check bytes_eq "first is x" (bs "x") k
     | _ -> Alcotest.fail "expected Found");
    Alcotest.check bytes_opt_eq "cursor_value at first"
      (Some (bs "1")) v;
    let* () = S.close s in
    Lwt.return_unit))

(* put with key > 512 bytes triggers Btree.Key_too_large which Store
   surfaces as a failed Lwt promise (via unwrap_error/fail_with).  *)
let test_put_key_too_large_btree () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let big_key = Bytes.make 600 'k' in
    let* exc =
      Lwt.catch
        (fun () -> let* () = S.put tx 0 big_key (bs "v") in Lwt.return_none)
        (fun e  -> Lwt.return_some (Printexc.to_string e))
    in
    Alcotest.(check bool) "put raised on oversized key" true (exc <> None);
    (* Try to recover so we can close cleanly *)
    let* () = Lwt.catch (fun () -> S.rollback tx) (fun _ -> Lwt.return_unit) in
    let* () = S.close s in
    Lwt.return_unit))

(* put with value > 1024 bytes triggers Btree.Value_too_large *)
let test_put_value_too_large_btree () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    let* tx = S.rw_begin s in
    let big_val = Bytes.make 2000 'v' in
    let* exc =
      Lwt.catch
        (fun () -> let* () = S.put tx 0 (bs "k") big_val in Lwt.return_none)
        (fun e  -> Lwt.return_some (Printexc.to_string e))
    in
    Alcotest.(check bool) "put raised on oversized value" true (exc <> None);
    let* () = Lwt.catch (fun () -> S.rollback tx) (fun _ -> Lwt.return_unit) in
    let* () = S.close s in
    Lwt.return_unit))

(* Overwrite a tree page on disk with a valid-CRC Header-kind page,
   so Btree descends into it and returns Tree_corrupt (caught by
   Store and surfaced as a failed Lwt promise via map_btree_err).
   This exercises map_btree_err's Tree_corrupt arm and the
   get/put/del/cursor_open Btree-error tails. *)
let test_btree_corrupt_propagation () =
  run (with_fresh_db ~f:(fun path ->
    let* r = S.open_file ~path in
    let s = ok_store r in
    (* Insert enough rows that the table tree has multiple pages so
       the meta-tree references a real page (>= page 2). *)
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "1") in
    let* () = S.put tx 0 (bs "b") (bs "2") in
    let* () = S.commit tx in
    let* () = S.close s in
    (* Overwrite pages 2 onwards with a fully-valid Header-kind page
       (CRC sealed) so read_common succeeds and Btree returns
       Tree_corrupt. *)
    let module Pg = Sqlocaml_storage.Page in
    let bogus = Cstruct.create Pg.page_size in
    Cstruct.memset bogus 0;
    Pg.write_common bogus
      { Pg.kind = Pg.Header; flags = 0; n_keys = 0;
        right_page = 0l; crc32 = 0l };
    Pg.seal bogus;
    let bogus_bytes = Bytes.create Pg.page_size in
    Cstruct.blit_to_bytes bogus 0 bogus_bytes 0 Pg.page_size;
    let fd = Unix.openfile path [Unix.O_RDWR] 0o644 in
    let st = Unix.fstat fd in
    let n_pages = st.Unix.st_size / Pg.page_size in
    for i = 2 to n_pages - 1 do
      let _ = Unix.lseek fd (i * Pg.page_size) Unix.SEEK_SET in
      let _ = Unix.write fd bogus_bytes 0 Pg.page_size in
      ()
    done;
    Unix.close fd;
    let* r2 = S.open_file ~path in
    let s2 = ok_store r2 in
    let* tx = S.ro_begin s2 in
    let* exc =
      Lwt.catch
        (fun () -> let* _ = S.get tx 0 (bs "a") in Lwt.return_none)
        (fun e  -> Lwt.return_some (Printexc.to_string e))
    in
    let* () = S.ro_end tx in
    Alcotest.(check bool) "get raised on corruption" true (exc <> None);
    let* tx = S.rw_begin s2 in
    let* exc2 =
      Lwt.catch
        (fun () -> let* _ = S.cursor_open tx 0 in Lwt.return_none)
        (fun e  -> Lwt.return_some (Printexc.to_string e))
    in
    Alcotest.(check bool) "cursor_open raised on corruption" true (exc2 <> None);
    let* exc3 =
      Lwt.catch
        (fun () -> let* () = S.put tx 0 (bs "c") (bs "3") in Lwt.return_none)
        (fun e  -> Lwt.return_some (Printexc.to_string e))
    in
    Alcotest.(check bool) "put raised on corruption" true (exc3 <> None);
    let* exc4 =
      Lwt.catch
        (fun () -> let* () = S.del tx 0 (bs "a") in Lwt.return_none)
        (fun e  -> Lwt.return_some (Printexc.to_string e))
    in
    Alcotest.(check bool) "del raised on corruption" true (exc4 <> None);
    let* () = Lwt.catch (fun () -> S.rollback tx) (fun _ -> Lwt.return_unit) in
    let* () = S.close s2 in
    Lwt.return_unit))

(* Open a file whose two header pages are both corrupt -- should yield
   Header_error in Store.open_file's Btree-init path. *)
let test_open_corrupt_headers_both () =
  let path = fresh_path () in
  cleanup path;
  (* Create a file with two pages of garbage *)
  let oc = open_out_bin path in
  let garbage = Bytes.make 4096 '\xFF' in
  output_bytes oc garbage;
  output_bytes oc garbage;
  close_out oc;
  let finished = ref false in
  Lwt_main.run (
    let* r = S.open_file ~path in
    (match r with
     | Ok s ->
       let* () = S.close s in
       Alcotest.fail "expected Header_error for corrupt headers"
     | Error (S.Header_error _) ->
       finished := true;
       Lwt.return_unit
     | Error e ->
       Alcotest.failf "expected Header_error, got: %a" S.pp_error e)
  );
  cleanup path;
  Alcotest.(check bool) "got expected error" true !finished

(* ------------------------------------------------------------------ *)
(* 11. Freelist persistence                                             *)
(* ------------------------------------------------------------------ *)

let test_freelist_survives_reopen () =
  let path = Filename.temp_file "sqlocaml_fl_" ".db" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Insert 50 rows to force some B+-tree page allocations *)
    for i = 1 to 50 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      let value = Bytes.of_string "value" in
      run (S.put tx 16 key value);
      run (S.commit tx)
    done;
    (* Delete half to free pages via CoW *)
    for i = 1 to 25 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.del tx 16 key);
      run (S.commit tx)
    done;
    let fl_before = S.freelist_size store in
    run (S.close store);
    (* Reopen and verify freelist recovered *)
    let store2 = Result.get_ok (run (S.open_file ~path)) in
    let fl_after = S.freelist_size store2 in
    Alcotest.(check bool) "freelist non-empty after reopen"
      true (fl_after > 0);
    Alcotest.(check int) "freelist size matches" fl_before fl_after;
    run (S.close store2))

let test_freed_pages_reused_after_reopen () =
  let path = Filename.temp_file "sqlocaml_reuse_" ".db" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Build initial state *)
    for i = 1 to 30 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    (* Delete all to free pages *)
    for i = 1 to 30 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.del tx 16 key);
      run (S.commit tx)
    done;
    let n_pages_after_delete = S.n_pages store in
    run (S.close store);
    (* Reopen: freelist should be loaded *)
    let store2 = Result.get_ok (run (S.open_file ~path)) in
    Alcotest.(check bool) "freelist loaded on reopen"
      true (S.freelist_size store2 > 0);
    (* Reinsert: should reuse freed pages, file should not grow significantly *)
    for i = 1 to 30 do
      let tx = run (S.rw_begin store2) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    let n_pages_after_reinsert = S.n_pages store2 in
    (* Allow some growth for freelist pages themselves, but it should be bounded *)
    Alcotest.(check bool) "pages reused — file doesn't grow much"
      true (n_pages_after_reinsert <= Int64.add n_pages_after_delete 10L);
    run (S.close store2))

(* ------------------------------------------------------------------ *)
(* 12. Rollback correctness                                             *)
(* ------------------------------------------------------------------ *)

let test_rollback_restores_data () =
  let path = Filename.temp_file "sqlocaml_rb_" ".db" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "key") (Bytes.of_string "original"));
    run (S.commit tx1);
    let tx2 = run (S.rw_begin store) in
    run (S.put tx2 16 (Bytes.of_string "key") (Bytes.of_string "changed"));
    run (S.rollback tx2);
    let tx3 = run (S.ro_begin store) in
    let v = run (S.get tx3 16 (Bytes.of_string "key")) in
    run (S.ro_end tx3);
    Alcotest.(check (option string)) "rolled back to original"
      (Some "original") (Option.map Bytes.to_string v);
    run (S.close store))

let test_rollback_freelist_not_corrupted () =
  let path = Filename.temp_file "sqlocaml_rb2_" ".db" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "k1") (Bytes.of_string "v1"));
    run (S.commit tx1);
    (* Capture exact freelist entries after commit *)
    let fl_entries_after_commit = S.freelist_entries store in
    (* Start a txn that modifies the tree (causes CoW frees) *)
    let tx2 = run (S.rw_begin store) in
    run (S.put tx2 16 (Bytes.of_string "k2") (Bytes.of_string "v2"));
    run (S.rollback tx2);
    (* Freelist must be identical (same entries, same order) after rollback *)
    let fl_entries_after_rollback = S.freelist_entries store in
    Alcotest.(check int) "freelist size unchanged after rollback"
      (List.length fl_entries_after_commit) (List.length fl_entries_after_rollback);
    Alcotest.(check bool) "freelist contents identical after rollback"
      true (fl_entries_after_commit = fl_entries_after_rollback);
    run (S.close store))

let test_rollback_then_commit_works () =
  let path = Filename.temp_file "sqlocaml_rb3_" ".db" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "k") (Bytes.of_string "first"));
    run (S.rollback tx1);
    let tx2 = run (S.rw_begin store) in
    run (S.put tx2 16 (Bytes.of_string "k") (Bytes.of_string "second"));
    run (S.commit tx2);
    let tx3 = run (S.ro_begin store) in
    let v = run (S.get tx3 16 (Bytes.of_string "k")) in
    run (S.ro_end tx3);
    Alcotest.(check (option string)) "second write committed"
      (Some "second") (Option.map Bytes.to_string v);
    run (S.close store))

let test_rollback_new_key_absent () =
  let path = Filename.temp_file "sqlocaml_rb4_" ".db" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Insert a brand-new key then rollback — key must not exist after *)
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "new_key") (Bytes.of_string "val"));
    run (S.rollback tx1);
    let tx2 = run (S.ro_begin store) in
    let v = run (S.get tx2 16 (Bytes.of_string "new_key")) in
    run (S.ro_end tx2);
    Alcotest.(check (option string)) "new key absent after rollback"
      None (Option.map Bytes.to_string v);
    run (S.close store))

(* ------------------------------------------------------------------ *)
(* 13. Snapshot isolation                                               *)
(* ------------------------------------------------------------------ *)

let test_ro_sees_committed_not_in_progress () =
  let path = Filename.temp_file "sqlocaml_snap_" ".db" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "k") (Bytes.of_string "committed"));
    run (S.commit tx1);
    (* Open RO snapshot at txn_id=1 *)
    let ro = run (S.ro_begin store) in
    (* Start a concurrent RW txn that modifies the same key *)
    let tx2 = run (S.rw_begin store) in
    run (S.put tx2 16 (Bytes.of_string "k") (Bytes.of_string "uncommitted"));
    (* RO snapshot should NOT see the uncommitted write *)
    let v = run (S.get ro 16 (Bytes.of_string "k")) in
    Alcotest.(check (option string)) "RO sees committed value"
      (Some "committed") (Option.map Bytes.to_string v);
    run (S.commit tx2);
    (* RO snapshot still sees OLD committed value even after commit *)
    let v2 = run (S.get ro 16 (Bytes.of_string "k")) in
    Alcotest.(check (option string)) "RO still sees snapshot value"
      (Some "committed") (Option.map Bytes.to_string v2);
    run (S.ro_end ro);
    (* New RO txn sees latest committed value *)
    let ro3 = run (S.ro_begin store) in
    let v3 = run (S.get ro3 16 (Bytes.of_string "k")) in
    run (S.ro_end ro3);
    Alcotest.(check (option string)) "new RO sees latest commit"
      (Some "uncommitted") (Option.map Bytes.to_string v3);
    run (S.close store))

let test_active_reader_gates_freelist () =
  let path = Filename.temp_file "sqlocaml_gate_" ".db" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Build some tree structure *)
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    (* Open RO snapshot — active reader pins current pages *)
    let ro = run (S.ro_begin store) in
    let n_pages_before_delete = S.n_pages store in
    (* Delete all rows — CoW frees pages, but reader is active so they can't be reused *)
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.del tx 16 key);
      run (S.commit tx)
    done;
    (* Reinsert — with active reader, freed pages are gated; file may grow *)
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    let n_with_reader = S.n_pages store in
    (* Close reader — freed pages now ungated *)
    run (S.ro_end ro);
    (* Verify reader saw consistent snapshot throughout *)
    Alcotest.(check bool) "file grew while reader was active"
      true (n_with_reader > n_pages_before_delete);
    (* Second delete+reinsert cycle without reader — pages reused, file doesn't grow *)
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.del tx 16 key);
      run (S.commit tx)
    done;
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Bytes.of_string (Printf.sprintf "%04d" i) in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    let n_no_reader = S.n_pages store in
    Alcotest.(check bool) "file doesn't grow after reader closes"
      true (n_no_reader <= n_with_reader);
    run (S.close store))

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
    "extra", [
      Alcotest.test_case "pp_error all variants"   `Quick test_pp_error_all_variants;
      Alcotest.test_case "rollback drops uncommitted" `Quick test_rollback_btree_drops_uncommitted;
      Alcotest.test_case "cursor_seek found (btree)"  `Quick test_cursor_seek_found;
      Alcotest.test_case "cursor_seek past end (btree)" `Quick test_cursor_seek_past_end;
      Alcotest.test_case "cursor_first (btree)"       `Quick test_cursor_first_btree;
      Alcotest.test_case "put key too large"         `Quick test_put_key_too_large_btree;
      Alcotest.test_case "put value too large"       `Quick test_put_value_too_large_btree;
      Alcotest.test_case "open corrupt headers"      `Quick test_open_corrupt_headers_both;
      Alcotest.test_case "btree corruption surfaces" `Quick test_btree_corrupt_propagation;
    ];
    "freelist", [
      Alcotest.test_case "survives reopen"          `Quick test_freelist_survives_reopen;
      Alcotest.test_case "freed pages reused"       `Quick test_freed_pages_reused_after_reopen;
    ];
    "rollback", [
      Alcotest.test_case "restores_data"            `Quick test_rollback_restores_data;
      Alcotest.test_case "freelist_not_corrupted"   `Quick test_rollback_freelist_not_corrupted;
      Alcotest.test_case "then_commit_works"        `Quick test_rollback_then_commit_works;
      Alcotest.test_case "new_key_absent"           `Quick test_rollback_new_key_absent;
    ];
    "snapshot", [
      Alcotest.test_case "ro_sees_committed_not_in_progress" `Quick test_ro_sees_committed_not_in_progress;
      Alcotest.test_case "active_reader_gates_freelist" `Quick test_active_reader_gates_freelist;
    ];
    "qcheck", qcheck_tests;
  ]
