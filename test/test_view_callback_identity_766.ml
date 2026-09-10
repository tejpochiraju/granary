(** #766: the reactive-view callback API's two identity gaps, both surfaced by
    the #757 review (PR #761, round 2, items 3 and 4) and deliberately left
    open there.

    {b Gap 1 — [unregister_view_callback] conflated four situations into one
    [false].} After [DROP REACTIVE VIEW v; CREATE REACTIVE VIEW v AS ...] a
    handle registered against the OLD incarnation looked the new entry up by
    name, failed to find its id in the new (empty) callback list, and answered
    [false] — indistinguishable from "I already unregistered this". It now
    answers a closed four-way variant, and this file hits every arm
    deliberately:

    - [`Removed] — still attached, now detached;
    - [`Not_registered] — the view is live at THIS handle's generation but the
      handle is not in its list (already unregistered, or minted on another
      [Db.t]);
    - [`Stale_generation g] — the name is live at a DIFFERENT incarnation, and
      [g] is the live one, so the caller's re-registration needs no second
      lookup;
    - [`Unknown_view v] — [v] is not a live reactive view here at all.

    {b Gap 2 — the documented re-registration pattern had a TOCTOU window.}
    #757 documented "compare [reactive_view_generation] against the one you
    recorded, and re-register if they differ". Nothing stopped a SECOND
    drop-and-recreate landing between the comparison and the re-registration,
    so the re-registration could itself attach to an already-stale
    incarnation with no way to notice. [register_view_callback] now takes
    [?expected_generation]: supplied and mismatched, it attaches NOTHING and
    reports [`Stale_generation live].

    {b The atomicity claim this file cannot test directly, and why it holds
    anyway.} [register_view_callback] is synchronous — no [Lwt] bind, no yield
    point, between reading the registry entry and prepending to its callback
    list — while [CREATE]/[DROP REACTIVE VIEW] both go through the [Lwt]-bound
    statement path. Under a cooperative scheduler nothing can interleave
    between the check and the attach, so there is no interleaving for a test to
    construct: the property is structural. What IS pinned here is the
    observable consequence — a mismatch attaches nothing at all, rather than
    attaching and reporting the mismatch afterwards.

    Also pinned:

    - the handle carries its own generation ([view_callback_generation]) and
      view name ([view_callback_view]), so a caller never needs a side table of
      (handle, generation) pairs — that side table is exactly where the races
      would have to be got right;
    - [pp_view_callback] renders the generation, so a log line says which
      incarnation a handle belonged to;
    - omitting [?expected_generation] behaves exactly as before (the
      backward-compatible path);
    - the #746 mid-flush snapshot rule is unchanged by either half: a
      registration made from inside a firing callback — with
      [~expected_generation] or without — starts firing from the NEXT batch,
      and an unregistration from inside one still reports [`Removed] and takes
      effect from the next batch;
    - the documented re-registration recipe, walked verbatim across a real
      drop-and-recreate, ends attached to the LIVE incarnation and fires;
    - {b the documented limit}: all of this is decided against THIS handle's
      registry, so a SIBLING handle's drop-and-recreate does not produce
      [`Stale_generation] — the per-handle DDL-visibility caveat
      (#589/#633/#634) applies here exactly as it does to tables, views and
      triggers. A camel-side author must not read the new API as a
      cross-handle recreate detector, and the last test in this file is what
      keeps that promise honest rather than aspirational. *)

module Db = Granary.Db

let run = Lwt_main.run

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

let setup db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
  exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp"
;;

let create_view db =
  exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp"
;;

let drop_view db = exec db "DROP REACTIVE VIEW cnt"

let counter () =
  let n = ref 0 in
  ( n
  , fun _ ->
      incr n;
      Lwt.return_unit )
;;

(* Rendering both result types keeps the Alcotest failure messages informative:
   the whole point of #766 is that these constructors are distinguishable, so a
   failure that says only "expected true, got false" would be a poor guard for
   it. *)
let unreg db h =
  match Db.unregister_view_callback db h with
  | `Removed -> "`Removed"
  | `Not_registered -> "`Not_registered"
  | `Stale_generation g -> Printf.sprintf "`Stale_generation %d" g
  | `Unknown_view v -> Printf.sprintf "`Unknown_view %S" v
;;

let reg db ?expected_generation ~view_name cb =
  match Db.register_view_callback db ?expected_generation ~view_name cb with
  | Ok ((_ : Db.view_callback), g) -> Printf.sprintf "Ok %d" g
  | Error (`Unknown_view v) -> Printf.sprintf "`Unknown_view %S" v
  | Error (`Stale_generation g) -> Printf.sprintf "`Stale_generation %d" g
;;

let attach db ?expected_generation ~view_name cb =
  match Db.register_view_callback db ?expected_generation ~view_name cb with
  | Ok (h, g) -> h, g
  | Error (`Unknown_view v) -> Alcotest.failf "expected %S to be a live view" v
  | Error (`Stale_generation g) -> Alcotest.failf "unexpected `Stale_generation %d" g
;;

let generation db name =
  match Db.reactive_view_generation db name with
  | Some g -> g
  | None -> Alcotest.failf "expected %S to be live" name
;;

(* ------------------------- the four unregister arms ------------------------ *)

(* [`Removed] and [`Not_registered] on the same handle, back to back: the
   idempotence #746 already promised, but now saying WHICH case it is. *)
let removed_then_not_registered () =
  with_db (fun db ->
    setup db;
    let n, cb = counter () in
    let h, _ = attach db ~view_name:"cnt" cb in
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "fires while attached" 1 !n;
    Alcotest.(check string) "a live handle detaches" "`Removed" (unreg db h);
    Alcotest.(check string)
      "and detaching it again names the case rather than answering a bare false"
      "`Not_registered"
      (unreg db h);
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "silent once detached" 1 !n)
;;

(* [`Not_registered], not [`Unknown_view]: [cnt] IS live on [db2] and — because
   each store's generation counter starts fresh, so both first views are
   generation 1 — [h1] is not stale either. It is simply not in this registry's
   list, ids being process-global. *)
let foreign_handle_is_not_registered () =
  with_db (fun db1 ->
    setup db1;
    let n1, cb1 = counter () in
    let h1, g1 = attach db1 ~view_name:"cnt" cb1 in
    with_db (fun db2 ->
      setup db2;
      let n2, cb2 = counter () in
      let _h2, g2 = attach db2 ~view_name:"cnt" cb2 in
      Alcotest.(check int)
        "each store mints generations from its own counter, so these coincide"
        g1
        g2;
      Alcotest.(check string)
        "a handle from another Db.t removes nothing, and says so precisely"
        "`Not_registered"
        (unreg db2 h1);
      exec db2 "INSERT INTO t VALUES (1, 'a', 10)";
      Alcotest.(check int) "db2's own callback is untouched" 1 !n2);
    exec db1 "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "and db1's is too" 1 !n1)
;;

(* The case #766 exists for: the name is still live, but not as the thing this
   handle attached to. The old [false] could not say that, and the reported
   generation is the LIVE one, so a caller's next step needs no second lookup. *)
let drop_and_recreate_is_stale_generation () =
  with_db (fun db ->
    setup db;
    let n, cb = counter () in
    let h, g_old = attach db ~view_name:"cnt" cb in
    drop_view db;
    create_view db;
    let g_new = generation db "cnt" in
    Alcotest.(check bool) "a recreate mints a fresh generation" true (g_new <> g_old);
    Alcotest.(check string)
      "the name is live, but at a different incarnation than the handle's"
      (Printf.sprintf "`Stale_generation %d" g_new)
      (unreg db h);
    Alcotest.(check int)
      "the handle's own generation is still the one it registered against"
      g_old
      (Db.view_callback_generation h);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "and the stale callback never fires for the new view" 0 !n)
;;

(* Dropped and NOT recreated: there is no incarnation to be stale relative to,
   so this is the fourth arm and it carries the name — spelled exactly as
   [register_view_callback]'s own error so a caller can share a match arm. *)
let dropped_view_is_unknown_view () =
  with_db (fun db ->
    setup db;
    let n, cb = counter () in
    let h, _ = attach db ~view_name:"cnt" cb in
    drop_view db;
    Alcotest.(check string)
      "a dropped, non-recreated view reports the name as unknown"
      "`Unknown_view \"cnt\""
      (unreg db h);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "and nothing fires" 0 !n)
;;

(* --------------------------- expected_generation --------------------------- *)

let expected_generation_matching_attaches () =
  with_db (fun db ->
    setup db;
    let g = generation db "cnt" in
    let n, cb = counter () in
    let h, g' = attach db ~expected_generation:g ~view_name:"cnt" cb in
    Alcotest.(check int) "the Ok case reports the generation it attached to" g g';
    Alcotest.(check int)
      "which is also what the handle carries"
      g
      (Db.view_callback_generation h);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "and it really is attached" 1 !n)
;;

(* The heart of item 4: a mismatch must attach NOTHING. A version that
   attached and then reported the mismatch would pass an error-code assertion
   while leaving the caller with a live callback on an incarnation it never
   agreed to, so the firing count below is the real assertion here. *)
let expected_generation_mismatch_attaches_nothing () =
  with_db (fun db ->
    setup db;
    let g_live = generation db "cnt" in
    let n, cb = counter () in
    Alcotest.(check string)
      "registering against a generation that is not the live one is refused, and the \
       error carries the live generation"
      (Printf.sprintf "`Stale_generation %d" g_live)
      (reg db ~expected_generation:(g_live + 1000) ~view_name:"cnt" cb);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "a refused registration attached nothing at all" 0 !n;
    (* And the refusal did not disturb the registry: the same callback attaches
       fine when it asks for the generation that is actually live. *)
    let _h, _g = attach db ~expected_generation:g_live ~view_name:"cnt" cb in
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "the retry against the live generation works" 1 !n)
;;

let expected_generation_omitted_is_unchanged () =
  with_db (fun db ->
    setup db;
    let g = generation db "cnt" in
    let n, cb = counter () in
    Alcotest.(check string)
      "omitting the argument attaches to whatever is live, exactly as before #766"
      (Printf.sprintf "Ok %d" g)
      (reg db ~view_name:"cnt" cb);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "and fires" 1 !n)
;;

(* Precedence: a name that is not live at all reports [`Unknown_view] even when
   an expected generation was supplied. [`Stale_generation] means "live, but a
   different incarnation" and would be actively misleading here — there is no
   incarnation for the caller to retry against. *)
let unknown_view_beats_stale_generation () =
  with_db (fun db ->
    setup db;
    drop_view db;
    Alcotest.(check string)
      "an expected generation does not turn an unknown view into a stale one"
      "`Unknown_view \"cnt\""
      (reg db ~expected_generation:1 ~view_name:"cnt" (fun _ -> Lwt.return_unit)))
;;

(* ------------------------------ accessors, pp ------------------------------ *)

let handle_accessors_agree_with_the_registry () =
  with_db (fun db ->
    setup db;
    let h, g = attach db ~view_name:"cnt" (fun _ -> Lwt.return_unit) in
    Alcotest.(check string) "the handle names its view" "cnt" (Db.view_callback_view h);
    Alcotest.(check int)
      "and its generation, which is what the registry reports right now"
      (generation db "cnt")
      (Db.view_callback_generation h);
    Alcotest.(check int)
      "and what registration returned"
      g
      (Db.view_callback_generation h))
;;

(* #766 added the generation to the rendering. A log line naming only [view#id]
   could not say which incarnation of [view] a handle belonged to, which is the
   very distinction the rest of this file is about. *)
let pp_renders_the_generation () =
  with_db (fun db ->
    setup db;
    let h, g = attach db ~view_name:"cnt" (fun _ -> Lwt.return_unit) in
    let rendered = Format.asprintf "%a" Db.pp_view_callback h in
    let suffix = Printf.sprintf "@%d" g in
    Alcotest.(check bool)
      (Printf.sprintf "%S should render as cnt#<id>@%d" rendered g)
      true
      (String.starts_with ~prefix:"cnt#" rendered
       && String.ends_with ~suffix rendered))
;;

(* ------------------------------- mid-flush -------------------------------- *)

(* #746's snapshot rule, re-pinned against the new API surface: a registration
   made from inside a firing callback — here with [~expected_generation], the
   path that did not exist when that rule was written — still starts firing
   from the NEXT batch, so it cannot make the current batch loop. *)
let expected_generation_registration_mid_flush_starts_next_batch () =
  with_db (fun db ->
    setup db;
    let g = generation db "cnt" in
    let late, late_cb = counter () in
    let once = ref false in
    let outcome = ref "" in
    let cb _ =
      if not !once
      then (
        once := true;
        outcome := reg db ~expected_generation:g ~view_name:"cnt" late_cb);
      Lwt.return_unit
    in
    let _h, _g = attach db ~view_name:"cnt" cb in
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check string)
      "the generation is unchanged mid-flush, so the checked registration succeeds"
      (Printf.sprintf "Ok %d" g)
      !outcome;
    Alcotest.(check int) "the new callback does not fire for the batch it joined" 0 !late;
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "it fires from the next one" 1 !late)
;;

(* The other direction, likewise re-pinned: unregistering from inside a firing
   callback still reports [`Removed] (not something new about generations) and
   still completes the invocation it is in. *)
let self_unregister_mid_flush_reports_removed () =
  with_db (fun db ->
    setup db;
    let fired = ref 0 in
    let slot = ref None in
    let outcome = ref None in
    let cb _ =
      incr fired;
      (match !slot with
       | Some h -> outcome := Some (unreg db h)
       | None -> Alcotest.fail "handle not stored before the first fire");
      Lwt.return_unit
    in
    let h, _ = attach db ~view_name:"cnt" cb in
    slot := Some h;
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "the callback ran the invocation it was in" 1 !fired;
    Alcotest.(check (option string))
      "self-removal mid-flush is accepted and reports `Removed"
      (Some "`Removed")
      !outcome;
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "and it is silent from the next batch on" 1 !fired)
;;

(* ------------------------- the documented recipe --------------------------- *)

(* This is [register_view_callback]'s doc-comment recipe, transcribed. If the
   two ever drift, a camel-side author copying the doc gets code that does not
   compile against this engine — so keeping the transcription literal is the
   point, and this test is what notices. *)
let rewire db h cb =
  let name = Db.view_callback_view h in
  let rec attach_at g =
    match Db.register_view_callback db ~expected_generation:g ~view_name:name cb with
    | Ok (h', (_ : int)) -> Ok h'
    | Error (`Unknown_view v) -> Error (`Unknown_view v)
    (* A FURTHER recreate landed in between; [g'] is the one that is live as of
       this failure, so retry against exactly that. *)
    | Error (`Stale_generation g') -> attach_at g'
  in
  match Db.unregister_view_callback db h with
  (* Both of these mean the view is live at [h]'s OWN generation — [`Removed]
     that [h] was attached to it, [`Not_registered] that it was not — so that
     is the incarnation to reattach to. *)
  | `Removed | `Not_registered -> attach_at (Db.view_callback_generation h)
  | `Stale_generation g -> attach_at g
  | `Unknown_view v -> Error (`Unknown_view v)
;;

let recipe_reattaches_across_a_drop_and_recreate () =
  with_db (fun db ->
    setup db;
    let n, cb = counter () in
    let h0, g0 = attach db ~view_name:"cnt" cb in
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "attached to the first incarnation" 1 !n;
    drop_view db;
    create_view db;
    let h1 =
      match rewire db h0 cb with
      | Ok h -> h
      | Error (`Unknown_view v) -> Alcotest.failf "recipe lost the view %S" v
    in
    let g1 = generation db "cnt" in
    Alcotest.(check bool) "the recreate really did move the generation" true (g1 <> g0);
    Alcotest.(check int)
      "the recipe ended attached to the LIVE incarnation, not the one it started from"
      g1
      (Db.view_callback_generation h1);
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "and the re-registered callback fires again" 2 !n;
    (* Running the recipe again when nothing has changed detaches and reattaches
       against the SAME generation, so the callback count is unchanged rather
       than doubled. *)
    let h2 =
      match rewire db h1 cb with
      | Ok h -> h
      | Error (`Unknown_view v) -> Alcotest.failf "recipe lost the view %S" v
    in
    Alcotest.(check int)
      "a second pass over a current handle keeps the same generation"
      g1
      (Db.view_callback_generation h2);
    exec db "INSERT INTO t VALUES (3, 'c', 30)";
    Alcotest.(check int) "and does not duplicate the callback" 3 !n)
;;

(* The recipe's give-up arm: the view was dropped and not recreated, so there
   is nothing to reattach to and the caller is told the name is gone rather
   than being left believing it is still wired up. *)
let recipe_reports_a_dropped_view () =
  with_db (fun db ->
    setup db;
    let _n, cb = counter () in
    let h, _g = attach db ~view_name:"cnt" cb in
    drop_view db;
    match rewire db h cb with
    | Ok _ -> Alcotest.fail "the recipe should not claim success for a dropped view"
    | Error (`Unknown_view v) -> Alcotest.(check string) "names the lost view" "cnt" v)
;;

(* ------------------------------- the limit --------------------------------- *)

(* THE documented limit, pinned so it cannot quietly become untrue in either
   direction. [`Stale_generation] is decided against THIS handle's registry,
   and a sibling handle's DDL is not visible there (#589/#633/#634). So after a
   SIBLING drops and recreates the view, this handle still believes its own
   entry is current and answers [`Removed] — NOT [`Stale_generation]. A camel
   author must not read the new API as a cross-handle recreate detector; the
   only way to observe a sibling's recreate is still to re-derive this handle's
   view of the store, exactly as for the sibling's other DDL. *)
let a_siblings_recreate_is_not_visible_as_stale () =
  with_db (fun parent ->
    setup parent;
    let worker = run (Db.create_worker_handle parent) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close worker) with
        | _ -> ())
      (fun () ->
         let h, g_worker = attach worker ~view_name:"cnt" (fun _ -> Lwt.return_unit) in
         Alcotest.(check int)
           "the worker sees the same incarnation the parent does (#757)"
           (generation parent "cnt")
           g_worker;
         (* The PARENT recreates the view. The worker's registry is not
            refreshed by that — the per-handle DDL-visibility caveat. *)
         drop_view parent;
         create_view parent;
         Alcotest.(check bool)
           "the parent's generation moved"
           true
           (generation parent "cnt" <> g_worker);
         Alcotest.(check int)
           "but the worker still reports the generation it last learned"
           g_worker
           (generation worker "cnt");
         Alcotest.(check string)
           "so the worker's own unregister reports `Removed, NOT `Stale_generation: this \
            API detects a recreate THIS handle knows about, not a sibling's"
           "`Removed"
           (unreg worker h)))
;;

let () =
  Alcotest.run
    "reactive-view callback identity (#766)"
    [ ( "unregister outcomes"
      , [ Alcotest.test_case
            "`Removed then `Not_registered"
            `Quick
            removed_then_not_registered
        ; Alcotest.test_case
            "`Not_registered for a foreign handle"
            `Quick
            foreign_handle_is_not_registered
        ; Alcotest.test_case
            "`Stale_generation after a drop-and-recreate"
            `Quick
            drop_and_recreate_is_stale_generation
        ; Alcotest.test_case
            "`Unknown_view after a plain drop"
            `Quick
            dropped_view_is_unknown_view
        ] )
    ; ( "expected_generation"
      , [ Alcotest.test_case
            "matching attaches"
            `Quick
            expected_generation_matching_attaches
        ; Alcotest.test_case
            "mismatching attaches nothing"
            `Quick
            expected_generation_mismatch_attaches_nothing
        ; Alcotest.test_case
            "omitted is unchanged"
            `Quick
            expected_generation_omitted_is_unchanged
        ; Alcotest.test_case
            "`Unknown_view beats `Stale_generation"
            `Quick
            unknown_view_beats_stale_generation
        ] )
    ; ( "handle"
      , [ Alcotest.test_case
            "accessors agree with the registry"
            `Quick
            handle_accessors_agree_with_the_registry
        ; Alcotest.test_case "pp renders the generation" `Quick pp_renders_the_generation
        ] )
    ; ( "mid-flush"
      , [ Alcotest.test_case
            "checked registration defers to next batch"
            `Quick
            expected_generation_registration_mid_flush_starts_next_batch
        ; Alcotest.test_case
            "self-unregister reports `Removed"
            `Quick
            self_unregister_mid_flush_reports_removed
        ] )
    ; ( "recipe"
      , [ Alcotest.test_case
            "reattaches across a drop-and-recreate"
            `Quick
            recipe_reattaches_across_a_drop_and_recreate
        ; Alcotest.test_case "reports a dropped view" `Quick recipe_reports_a_dropped_view
        ] )
    ; ( "limit"
      , [ Alcotest.test_case
            "a sibling's recreate is not visible as stale"
            `Quick
            a_siblings_recreate_is_not_visible_as_stale
        ] )
    ]
;;
