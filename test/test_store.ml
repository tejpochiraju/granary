open Lwt.Syntax

module S = Sqlocaml_store.Store

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let bs s = Bytes.of_string s

(** Run an Lwt value synchronously. *)
let run = Lwt_main.run

(** bytes testable using structural equality *)
let bytes_eq =
  Alcotest.testable
    (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b))
    Bytes.equal

let bytes_opt_eq =
  Alcotest.(option (testable (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b)) Bytes.equal))

(* ------------------------------------------------------------------ *)
(* Group 1: Basic put/get                                               *)
(* ------------------------------------------------------------------ *)

let test_put_then_get () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "put then get" (Some (bs "v")) got;
    Lwt.return_unit
  )

let test_missing_key () =
  run (
    let s = S.create () in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 (bs "missing") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "missing key" None got;
    Lwt.return_unit
  )

let test_overwrite () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v1") in
    let* () = S.commit tx in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v2") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "overwrite" (Some (bs "v2")) got;
    Lwt.return_unit
  )

let test_delete_existing () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.rw_begin s in
    let* () = S.del tx 0 (bs "k") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "delete existing" None got;
    Lwt.return_unit
  )

let test_delete_missing () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    (* Should not raise *)
    let* () = S.del tx 0 (bs "nonexistent") in
    let* () = S.commit tx in
    Lwt.return_unit
  )

let test_multiple_trees () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v0") in
    let* () = S.put tx 1 (bs "other") (bs "v1") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got0 = S.get tx 0 (bs "k") in
    let* got1 = S.get tx 1 (bs "other") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "tree 0" (Some (bs "v0")) got0;
    Alcotest.check bytes_opt_eq "tree 1" (Some (bs "v1")) got1;
    Lwt.return_unit
  )

let test_tree_isolation () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 1 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "tree isolation" None got;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 2: Transaction semantics                                       *)
(* ------------------------------------------------------------------ *)

let test_rw_then_ro () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "x") (bs "y") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 (bs "x") in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "rw then ro" (Some (bs "y")) got;
    Lwt.return_unit
  )

let test_get_within_rw_txn () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* got = S.get tx 0 (bs "k") in
    let* () = S.commit tx in
    Alcotest.check bytes_opt_eq "get within rw txn" (Some (bs "v")) got;
    Lwt.return_unit
  )

let test_rollback_releases_lock () =
  run (
    let s = S.create () in
    let* tx1 = S.rw_begin s in
    let* () = S.rollback tx1 in
    (* If lock was released, this should not deadlock *)
    let* tx2 = S.rw_begin s in
    let* () = S.commit tx2 in
    Lwt.return_unit
  )

let test_commit_releases_lock () =
  run (
    let s = S.create () in
    let* tx1 = S.rw_begin s in
    let* () = S.commit tx1 in
    let* tx2 = S.rw_begin s in
    let* () = S.commit tx2 in
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 3: Cursor — full iteration                                     *)
(* ------------------------------------------------------------------ *)

let collect_all_via_next cur =
  let rec loop acc =
    match S.cursor_next cur with
    | None -> List.rev acc
    | Some entry -> loop (entry :: acc)
  in
  loop []

let test_cursor_iteration_ordered () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "1") in
    let* () = S.put tx 0 (bs "b") (bs "2") in
    let* () = S.put tx 0 (bs "c") (bs "3") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let _r = S.cursor_first cur in
    let entries = collect_all_via_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    let keys = List.map (fun (k, _) -> Bytes.to_string k) entries in
    let vals = List.map (fun (_, v) -> Bytes.to_string v) entries in
    Alcotest.(check (list string)) "keys ordered" ["a"; "b"; "c"] keys;
    Alcotest.(check (list string)) "vals ordered" ["1"; "2"; "3"] vals;
    Lwt.return_unit
  )

let test_cursor_empty_tree () =
  run (
    let s = S.create () in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_first cur in
    let after = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Not_found `End -> ()
     | _ -> Alcotest.fail "expected Not_found `End on empty tree");
    Alcotest.(check (option (pair bytes_eq bytes_eq))) "cursor_next on empty" None after;
    Lwt.return_unit
  )

let test_cursor_single_key () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_first cur in
    let first = S.cursor_next cur in
    let second = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Found k -> Alcotest.check bytes_eq "cursor_first key" (bs "k") k
     | _ -> Alcotest.fail "expected Found");
    (match first with
     | Some (k, v) ->
       Alcotest.check bytes_eq "first next key" (bs "k") k;
       Alcotest.check bytes_eq "first next val" (bs "v") v
     | None -> Alcotest.fail "expected Some on first next");
    Alcotest.(check (option (pair bytes_eq bytes_eq))) "second next" None second;
    Lwt.return_unit
  )

let test_cursor_exhaustion () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "1") in
    let* () = S.put tx 0 (bs "b") (bs "2") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let _ = S.cursor_first cur in
    let _ = S.cursor_next cur in (* "a" *)
    let _ = S.cursor_next cur in (* "b" *)
    let after = S.cursor_next cur in (* exhausted *)
    let again = S.cursor_next cur in (* still exhausted *)
    S.cursor_close cur;
    let* () = S.ro_end tx in
    Alcotest.(check (option (pair bytes_eq bytes_eq))) "after exhaustion" None after;
    Alcotest.(check (option (pair bytes_eq bytes_eq))) "again after exhaustion" None again;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 4: Cursor seek                                                 *)
(* ------------------------------------------------------------------ *)

let test_seek_exact_match () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "va") in
    let* () = S.put tx 0 (bs "c") (bs "vc") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_seek cur (bs "a") in
    let entry = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Found k -> Alcotest.check bytes_eq "seek exact" (bs "a") k
     | _ -> Alcotest.fail "expected Found");
    (match entry with
     | Some (k, v) ->
       Alcotest.check bytes_eq "entry key" (bs "a") k;
       Alcotest.check bytes_eq "entry val" (bs "va") v
     | None -> Alcotest.fail "expected Some");
    Lwt.return_unit
  )

let test_seek_between_keys () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "va") in
    let* () = S.put tx 0 (bs "c") (bs "vc") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_seek cur (bs "b") in
    let entry = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Not_found (`Greater k) -> Alcotest.check bytes_eq "greater key" (bs "c") k
     | _ -> Alcotest.fail "expected Not_found Greater");
    (match entry with
     | Some (k, v) ->
       Alcotest.check bytes_eq "entry key" (bs "c") k;
       Alcotest.check bytes_eq "entry val" (bs "vc") v
     | None -> Alcotest.fail "expected Some");
    Lwt.return_unit
  )

let test_seek_before_all () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "va") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    (* Empty bytes is before "a" *)
    let r = S.cursor_seek cur Bytes.empty in
    let entry = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (* Either Found "a" (if empty is prefix of "a" in comparison) or Not_found Greater "a" *)
    (match r with
     | S.Found k -> Alcotest.check bytes_eq "found key" (bs "a") k
     | S.Not_found (`Greater k) -> Alcotest.check bytes_eq "greater key" (bs "a") k
     | S.Not_found `End -> Alcotest.fail "unexpected End");
    (match entry with
     | Some (k, _) -> Alcotest.check bytes_eq "entry key" (bs "a") k
     | None -> Alcotest.fail "expected Some");
    Lwt.return_unit
  )

let test_seek_after_all () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "va") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_seek cur (bs "\xff\xff\xff") in
    let entry = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Not_found `End -> ()
     | _ -> Alcotest.fail "expected Not_found End");
    Alcotest.(check (option (pair bytes_eq bytes_eq))) "after all" None entry;
    Lwt.return_unit
  )

let test_seek_exact_then_next () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "va") in
    let* () = S.put tx 0 (bs "b") (bs "vb") in
    let* () = S.put tx 0 (bs "c") (bs "vc") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let r = S.cursor_seek cur (bs "b") in
    let e1 = S.cursor_next cur in
    let e2 = S.cursor_next cur in
    let e3 = S.cursor_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    (match r with
     | S.Found k -> Alcotest.check bytes_eq "seek found" (bs "b") k
     | _ -> Alcotest.fail "expected Found b");
    (match e1 with
     | Some (k, v) ->
       Alcotest.check bytes_eq "e1 key" (bs "b") k;
       Alcotest.check bytes_eq "e1 val" (bs "vb") v
     | None -> Alcotest.fail "expected e1");
    (match e2 with
     | Some (k, v) ->
       Alcotest.check bytes_eq "e2 key" (bs "c") k;
       Alcotest.check bytes_eq "e2 val" (bs "vc") v
     | None -> Alcotest.fail "expected e2");
    Alcotest.(check (option (pair bytes_eq bytes_eq))) "e3 none" None e3;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 5: cursor_value                                                *)
(* ------------------------------------------------------------------ *)

let test_cursor_value_after_first () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let _ = S.cursor_first cur in
    let v = S.cursor_value cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "cursor_value after first" (Some (bs "v")) v;
    Lwt.return_unit
  )

let test_cursor_value_exhausted () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let _ = S.cursor_first cur in
    let _ = S.cursor_next cur in (* consume "k" *)
    let _ = S.cursor_next cur in (* exhausted *)
    let v = S.cursor_value cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "cursor_value exhausted" None v;
    Lwt.return_unit
  )

let test_cursor_value_after_seek () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 (bs "a") (bs "va") in
    let* () = S.put tx 0 (bs "b") (bs "vb") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let _ = S.cursor_seek cur (bs "b") in
    let v = S.cursor_value cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "cursor_value after seek" (Some (bs "vb")) v;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 6: Multiple trees + large data                                 *)
(* ------------------------------------------------------------------ *)

let test_many_trees () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    let* () = Lwt_list.iter_s (fun i ->
      S.put tx i (bs "k") (Bytes.of_string (string_of_int i))
    ) (List.init 10 Fun.id) in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* results = Lwt_list.map_s (fun i ->
      S.get tx i (bs "k")
    ) (List.init 10 Fun.id) in
    let* () = S.ro_end tx in
    List.iteri (fun i got ->
      let expected = Some (Bytes.of_string (string_of_int i)) in
      Alcotest.check bytes_opt_eq (Printf.sprintf "tree %d" i) expected got
    ) results;
    Lwt.return_unit
  )

let test_large_key () =
  run (
    let s = S.create () in
    let key = Bytes.make 1000 'k' in
    let value = Bytes.make 5000 'v' in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 key value in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 key in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "large key" (Some value) got;
    Lwt.return_unit
  )

let test_large_value () =
  run (
    let s = S.create () in
    let key = bs "key" in
    let value = Bytes.make 100000 'x' in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 key value in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 key in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "large value" (Some value) got;
    Lwt.return_unit
  )

let test_binary_key () =
  run (
    let s = S.create () in
    let key = Bytes.of_string "\x00\x01\xff\xfe" in
    let value = bs "bval" in
    let* tx = S.rw_begin s in
    let* () = S.put tx 0 key value in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* got = S.get tx 0 key in
    let* () = S.ro_end tx in
    Alcotest.check bytes_opt_eq "binary key" (Some value) got;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 7: Ordering                                                    *)
(* ------------------------------------------------------------------ *)

let test_lexicographic_order () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    (* Insert in reverse order *)
    let* () = S.put tx 0 (bs "c") (bs "3") in
    let* () = S.put tx 0 (bs "b") (bs "2") in
    let* () = S.put tx 0 (bs "a") (bs "1") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let _ = S.cursor_first cur in
    let entries = collect_all_via_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    let keys = List.map (fun (k, _) -> Bytes.to_string k) entries in
    Alcotest.(check (list string)) "lexicographic order" ["a"; "b"; "c"] keys;
    Lwt.return_unit
  )

let test_binary_order () =
  run (
    let s = S.create () in
    let* tx = S.rw_begin s in
    (* Insert in shuffled order *)
    let* () = S.put tx 0 (Bytes.of_string "\xff") (bs "ff") in
    let* () = S.put tx 0 (Bytes.of_string "\x00") (bs "00") in
    let* () = S.put tx 0 (Bytes.of_string "\x80") (bs "80") in
    let* () = S.put tx 0 (Bytes.of_string "\x01") (bs "01") in
    let* () = S.commit tx in
    let* tx = S.ro_begin s in
    let* cur = S.cursor_open tx 0 in
    let _ = S.cursor_first cur in
    let entries = collect_all_via_next cur in
    S.cursor_close cur;
    let* () = S.ro_end tx in
    let keys = List.map (fun (k, _) -> Bytes.get k 0 |> Char.code) entries in
    Alcotest.(check (list int)) "binary order" [0x00; 0x01; 0x80; 0xff] keys;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* Group 8: QCheck property tests                                       *)
(* ------------------------------------------------------------------ *)

let prop_put_get_roundtrip =
  QCheck.Test.make ~count:500 ~name:"put/get roundtrip"
    (QCheck.list (QCheck.pair QCheck.bytes QCheck.bytes))
    (fun pairs ->
       let keys = List.map fst pairs in
       let unique = List.sort_uniq Bytes.compare keys in
       (* Skip (not fail) if there are duplicate keys — last-write-wins makes
          the expected value ambiguous for duplicates. *)
       QCheck.assume (List.length unique = List.length keys);
       Lwt_main.run (
         let s = S.create () in
         let* tx = S.rw_begin s in
         let* () = Lwt_list.iter_s (fun (k, v) -> S.put tx 0 k v) pairs in
         let* () = S.commit tx in
         let* tx = S.ro_begin s in
         let* results = Lwt_list.map_s (fun (k, v) ->
           let* got = S.get tx 0 k in
           Lwt.return (Option.map Bytes.to_string got = Some (Bytes.to_string v))
         ) pairs in
         let* () = S.ro_end tx in
         Lwt.return (List.for_all Fun.id results)
       ))

let prop_cursor_sorted =
  QCheck.Test.make ~count:500 ~name:"cursor always sorted"
    (QCheck.list QCheck.bytes)
    (fun keys ->
       let unique_keys = List.sort_uniq Bytes.compare keys in
       Lwt_main.run (
         let s = S.create () in
         let* tx = S.rw_begin s in
         let* () = Lwt_list.iter_s (fun k -> S.put tx 0 k (Bytes.of_string "v")) unique_keys in
         let* () = S.commit tx in
         let* tx = S.ro_begin s in
         let* cur = S.cursor_open tx 0 in
         let _ = S.cursor_first cur in
         let collected = ref [] in
         let rec loop () =
           match S.cursor_next cur with
           | None -> ()
           | Some (k, _) -> collected := k :: !collected; loop ()
         in
         loop ();
         S.cursor_close cur;
         let* () = S.ro_end tx in
         let got = List.rev !collected in
         let expected = List.sort Bytes.compare unique_keys in
         Lwt.return (got = expected)
       ))

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_put_get_roundtrip;
      prop_cursor_sorted;
    ]
  in
  Alcotest.run "store" [
    "put_get", [
      Alcotest.test_case "put_then_get"       `Quick test_put_then_get;
      Alcotest.test_case "missing_key"        `Quick test_missing_key;
      Alcotest.test_case "overwrite"          `Quick test_overwrite;
      Alcotest.test_case "delete_existing"    `Quick test_delete_existing;
      Alcotest.test_case "delete_missing"     `Quick test_delete_missing;
      Alcotest.test_case "multiple_trees"     `Quick test_multiple_trees;
      Alcotest.test_case "tree_isolation"     `Quick test_tree_isolation;
    ];
    "txn", [
      Alcotest.test_case "rw_then_ro"             `Quick test_rw_then_ro;
      Alcotest.test_case "get_within_rw_txn"      `Quick test_get_within_rw_txn;
      Alcotest.test_case "rollback_releases_lock"  `Quick test_rollback_releases_lock;
      Alcotest.test_case "commit_releases_lock"    `Quick test_commit_releases_lock;
    ];
    "cursor_iteration", [
      Alcotest.test_case "iteration_ordered"  `Quick test_cursor_iteration_ordered;
      Alcotest.test_case "empty_tree"         `Quick test_cursor_empty_tree;
      Alcotest.test_case "single_key"         `Quick test_cursor_single_key;
      Alcotest.test_case "exhaustion"         `Quick test_cursor_exhaustion;
    ];
    "cursor_seek", [
      Alcotest.test_case "exact_match"        `Quick test_seek_exact_match;
      Alcotest.test_case "between_keys"       `Quick test_seek_between_keys;
      Alcotest.test_case "before_all"         `Quick test_seek_before_all;
      Alcotest.test_case "after_all"          `Quick test_seek_after_all;
      Alcotest.test_case "exact_then_next"    `Quick test_seek_exact_then_next;
    ];
    "cursor_value", [
      Alcotest.test_case "after_first"        `Quick test_cursor_value_after_first;
      Alcotest.test_case "exhausted"          `Quick test_cursor_value_exhausted;
      Alcotest.test_case "after_seek"         `Quick test_cursor_value_after_seek;
    ];
    "large_data", [
      Alcotest.test_case "many_trees"         `Quick test_many_trees;
      Alcotest.test_case "large_key"          `Quick test_large_key;
      Alcotest.test_case "large_value"        `Quick test_large_value;
      Alcotest.test_case "binary_key"         `Quick test_binary_key;
    ];
    "ordering", [
      Alcotest.test_case "lexicographic_order" `Quick test_lexicographic_order;
      Alcotest.test_case "binary_order"        `Quick test_binary_order;
    ];
    "qcheck", qcheck_tests;
  ]
