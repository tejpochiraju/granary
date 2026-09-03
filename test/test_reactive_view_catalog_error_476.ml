(** #476 — the reactive-view registry's catalog writes report a store fault as
    an [Error], not a raised exception.

    [Catalog.persist_reactive_view], [remove_reactive_view] and
    [load_all_reactive_views] returned [_ Lwt.t] and let a store-level failure
    escape as an exception. Their callers — [Db.rv_create] and [Db.rv_drop] —
    sit directly under [Db.execute]'s [(unit, error) result] contract, so a
    caller matching [Ok _ | Error _] got an unhandled exception instead of the
    [Error] branch. Unlike the view/trigger siblings, these two are not on the
    staged-DDL path, so nothing above them converted it.

    [Db.rv_drop] used to compensate with an [Lwt.catch] of its own; #476
    converts at the source and that wrapper is gone.

    The fault is injected at the block device: a non-WAL store writes its dirty
    pages to the main file at COMMIT, so arming [write_page] fails the
    autocommit that [borrow_or_autocommit] opens for the catalog write. The
    non-WAL commit path releases the writer lock from an [Lwt.finalize] handler,
    so an armed failure leaves the store usable rather than deadlocked. *)

open Lwt.Syntax
module S = Granary_store.Store
module Cat = Granary_catalog.Catalog
module Db = Granary.Db

(* ------------------------------------------------------------------ *)
(* An in-memory main device with an arm-able write failure.            *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev size = { buf = Bytes.make size '\x00' }

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (
    let nb = Bytes.make (max need (cur * 2)) '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb)
;;

let sync_ok () = Lwt.return (Ok ())

let open_test_store ~(fail : bool ref) () =
  let dev = mk_dev (1024 * 4096) in
  let n_pages = Int64.of_int (Bytes.length dev.buf / 4096) in
  let read_page ~page_id buf =
    let off = Int64.to_int (Int64.mul page_id 4096L) in
    let len = Cstruct.length buf in
    if off + len > Bytes.length dev.buf
    then Lwt.return (Error "read past EOF")
    else (
      Cstruct.blit_from_bytes dev.buf off buf 0 len;
      Lwt.return (Ok ()))
  in
  let write_page ~page_id buf =
    if !fail
    then Lwt.return (Error "injected main-write failure")
    else (
      let off = Int64.to_int (Int64.mul page_id 4096L) in
      let len = Cstruct.length buf in
      dev_grow dev (off + len);
      Cstruct.blit_to_bytes buf 0 dev.buf off len;
      Lwt.return (Ok ()))
  in
  let resize ~n_pages =
    dev_grow dev (Int64.to_int n_pages * 4096);
    Lwt.return (Ok ())
  in
  S.open_block
    ~init_if_corrupt:true
    ~read_page
    ~write_page
    ~sync:sync_ok
    ~resize
    ~n_pages
    ~close:(fun () -> Lwt.return_unit)
    ()
;;

let open_or_fail ~fail () =
  let* sr = open_test_store ~fail () in
  match sr with
  | Ok s -> Lwt.return s
  | Error e -> Alcotest.failf "open_block: %a" S.pp_error e
;;

let exec db sql =
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* ------------------------------------------------------------------ *)
(* 1. The clean path: a [result], and it is [Ok].                       *)
(* ------------------------------------------------------------------ *)

let the_clean_path_returns_ok () =
  Lwt_main.run
    (let fail = ref false in
     let* store = open_or_fail ~fail () in
     let* pr =
       Cat.persist_reactive_view store ~name:"v" ~sql:"CREATE REACTIVE VIEW v AS SELECT 1"
     in
     (match pr with
      | Ok () -> ()
      | Error msg -> Alcotest.failf "persist_reactive_view: %s" msg);
     let* lr = Cat.load_all_reactive_views store in
     (match lr with
      | Ok pairs ->
        Alcotest.(check (list string)) "the row is there" [ "v" ] (List.map fst pairs)
      | Error msg -> Alcotest.failf "load_all_reactive_views: %s" msg);
     let* rr = Cat.remove_reactive_view store ~name:"v" in
     (match rr with
      | Ok () -> ()
      | Error msg -> Alcotest.failf "remove_reactive_view: %s" msg);
     let* lr2 = Cat.load_all_reactive_views store in
     (match lr2 with
      | Ok pairs ->
        Alcotest.(check (list string)) "and gone again" [] (List.map fst pairs)
      | Error msg -> Alcotest.failf "load_all_reactive_views: %s" msg);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 2. A store fault is an [Error], not an escaping exception.           *)
(* ------------------------------------------------------------------ *)

(* Before #476 each of these RAISED out through [Lwt_main.run]; the test would
   not have failed an assertion, it would have died with an unhandled
   exception. *)
let a_store_fault_becomes_an_error () =
  Lwt_main.run
    (let fail = ref false in
     let* store = open_or_fail ~fail () in
     fail := true;
     let* pr =
       Cat.persist_reactive_view store ~name:"v" ~sql:"CREATE REACTIVE VIEW v AS SELECT 1"
     in
     (match pr with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "persist_reactive_view reported success on a dead device");
     let* rr = Cat.remove_reactive_view store ~name:"v" in
     (match rr with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "remove_reactive_view reported success on a dead device");
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 3. …and it reaches the caller of [Db.execute] as [Error].            *)
(* ------------------------------------------------------------------ *)

(* This is the contract #476 is actually about: [DROP REACTIVE VIEW] is driven
   by [Db.rv_drop], which is not on the staged-DDL path, so the catalog write's
   exception used to escape [Db.execute] entirely. *)
let drop_reactive_view_reports_a_store_fault_as_error () =
  Lwt_main.run
    (let fail = ref false in
     let* store = open_or_fail ~fail () in
     let* db = Db.of_store store in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)" in
     let* () =
       exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp"
     in
     fail := true;
     let* r = Db.execute db "DROP REACTIVE VIEW cnt" in
     (match r with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "DROP REACTIVE VIEW reported success on a dead device");
     (* The registry entry survives an errored drop by design (#477): nothing in
        memory is mutated unless the persistent work succeeded, so the caller's
        error is the truth and a retry is meaningful. *)
     Alcotest.(check (list string))
       "the view is still live after the failed drop"
       [ "cnt" ]
       (Db.reactive_view_names db);
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "reactive view catalog error contract (#476)"
    [ ( "result, not raise"
      , [ Alcotest.test_case "the clean path returns Ok" `Quick the_clean_path_returns_ok
        ; Alcotest.test_case
            "a store fault becomes an Error"
            `Quick
            a_store_fault_becomes_an_error
        ; Alcotest.test_case
            "DROP REACTIVE VIEW reports a store fault as Error"
            `Quick
            drop_reactive_view_reports_a_store_fault_as_error
        ] )
    ]
;;
