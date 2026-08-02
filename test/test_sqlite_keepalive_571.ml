(* #571 — the lint that guards the keep-alive discipline in the
   reference-SQLite comparison benchmarks, and the proof it would have caught
   the crash that motivated it.

   The crash: two of the OCaml sqlite3 bindings' C stubs — the statement
   finalizer and the database closer — do not register their argument as a
   local root, and they read the wrapper struct after releasing the runtime
   system.  Finalizing a statement is the last thing anyone does with it, so
   inside that window the custom block is unreachable from every OCaml root;
   the GC may collect it, run its own finaliser (which finalizes the statement
   and frees the wrapper), and leave the stub reading freed memory and handing
   the garbage to C SQLite.  bench_tpcc died that way at exit 139 — no
   exception, no message — after its pre-run consistency check and before any
   output, which reads as a hung benchmark rather than a crash.

   The remedy is one line per call site and nothing type-checks differently
   without it, so the discipline decays silently.  Hence a lint, and hence the
   "would have caught it" cases below: a lint that only ever passes is
   indistinguishable from no lint.

   These cases read the benchmark sources as text.  Dune copies them into the
   build directory beside this test whether or not the sqlite3 bindings are
   installed, so this runs — and must run — even where those benchmarks are
   not built. *)

module L = Granary_tpc.Tpc_keepalive_lint

let show findings =
  String.concat "; " (List.map (Format.asprintf "%a" L.pp_finding) findings)
;;

(* --- the shipped benchmarks are clean --------------------------------- *)

let sources () = List.map (fun f -> f, L.read_source f) L.comparison_sources

let test_benchmarks_are_clean () =
  List.iter
    (fun (file, src) ->
       let findings = L.check ~file src in
       if findings <> []
       then Alcotest.failf "%s: unguarded binding call: %s" file (show findings))
    (sources ())
;;

(* A lint over sources that contain none of the calls it looks for passes
   trivially, which is the failure mode it exists to prevent.  Strip the
   keep-alive lines out of the real, shipped sources — the tidy-up that
   reintroduces the crash — and require the lint to complain about every one
   of them. *)

let without_keep_alive src =
  let masked = Array.of_list (String.split_on_char '\n' (L.mask_non_code src)) in
  String.split_on_char '\n' src
  |> List.filteri (fun i _ -> not (L.contains masked.(i) L.keep_alive_token))
  |> String.concat "\n"
;;

let test_removing_the_keep_alive_is_caught () =
  List.iter
    (fun (file, src) ->
       let findings = L.check ~file (without_keep_alive src) in
       if findings = []
       then
         Alcotest.failf
           "%s: the lint did not notice its keep-alive being removed — either the file \
            no longer calls the bindings directly, or the lint stopped looking"
           file)
    (sources ())
;;

let test_every_source_is_readable () =
  (* [read_source] raising is the intended behaviour for a missing file; this
     pins that all three are in fact present next to the test. *)
  List.iter
    (fun (file, src) ->
       Alcotest.(check bool) (file ^ " is non-empty") true (String.length src > 0))
    (sources ())
;;

(* --- the heuristic itself --------------------------------------------- *)

let one_finding ~file src =
  match L.check ~file src with
  | [ f ] -> f
  | fs -> Alcotest.failf "expected exactly one finding, got: %s" (show fs)
;;

let src fmt = Printf.sprintf fmt L.binding_module

let test_raw_call_is_flagged () =
  let f = one_finding ~file:"x.ml" (src "let go stmt = %s.finalize stmt\n") in
  Alcotest.(check string) "call" "finalize" f.L.call;
  Alcotest.(check int) "line" 1 f.L.line
;;

let test_keep_alive_on_the_same_line_is_clean () =
  Alcotest.(check (list string))
    "no findings"
    []
    (List.map
       (fun (f : L.finding) -> f.L.call)
       (L.check
          ~file:"x.ml"
          (src "let go s = let r = %s.finalize s in keep_alive s; r\n")))
;;

let test_keep_alive_within_the_window_is_clean () =
  let s = src "let go s =\n  let r = %s.finalize s in\n  ignore r;\n  keep_alive s\n" in
  Alcotest.(check int) "no findings" 0 (List.length (L.check ~file:"x.ml" s))
;;

let test_keep_alive_beyond_the_window_is_flagged () =
  let padding = String.concat "" (List.init (L.window + 1) (fun _ -> "  ignore r;\n")) in
  let s = src "let go s =\n  let r = %s.finalize s in\n" ^ padding ^ "  keep_alive s\n" in
  Alcotest.(check int) "one finding" 1 (List.length (L.check ~file:"x.ml" s))
;;

let test_db_close_is_guarded_too () =
  let f = one_finding ~file:"x.ml" (src "let go db = ignore (%s.db_close db)\n") in
  Alcotest.(check string) "call" "db_close" f.L.call
;;

let test_bare_word_is_not_a_call () =
  (* Without the qualifying dot it is a local function, not the binding. *)
  Alcotest.(check int)
    "no findings"
    0
    (List.length (L.check ~file:"x.ml" "let finalize stmt = ignore stmt\n"))
;;

let test_another_modules_finalize_is_not_a_call () =
  (* granary's own Db.finalize is ordinary OCaml with no C stub behind it, and
     bench_compare.ml calls it four times.  Matching the function name alone
     reported all four. *)
  Alcotest.(check int)
    "no findings"
    0
    (List.length (L.check ~file:"x.ml" "let go s = Db.finalize s\n"))
;;

(* --- comments are prose, not calls ------------------------------------ *)

let test_a_comment_about_the_call_is_not_a_call () =
  Alcotest.(check int)
    "no findings"
    0
    (List.length
       (L.check ~file:"x.ml" (src "(* never call %s.finalize s here *)\nlet x = 1\n")))
;;

let test_nested_comments_are_masked () =
  let s =
    Printf.sprintf
      "(* outer (* inner %s.finalize s *) still comment %s.db_close d *)\nlet x = 1\n"
      L.binding_module
      L.binding_module
  in
  Alcotest.(check int) "no findings" 0 (List.length (L.check ~file:"x.ml" s))
;;

let test_mask_preserves_line_numbers () =
  let s = src "(* a\nb *)\nlet go s = %s.finalize s\n" in
  let f = one_finding ~file:"x.ml" s in
  Alcotest.(check int) "line" 3 f.L.line;
  Alcotest.(check int)
    "length preserved"
    (String.length s)
    (String.length (L.mask_non_code s))
;;

let test_code_after_a_comment_is_still_code () =
  let f = one_finding ~file:"x.ml" (src "let go s = (* note *) %s.finalize s\n") in
  Alcotest.(check string) "call" "finalize" f.L.call
;;

let () =
  Alcotest.run
    "sqlite_keepalive_571"
    [ ( "benchmarks"
      , [ Alcotest.test_case "sources readable" `Quick test_every_source_is_readable
        ; Alcotest.test_case "no unguarded calls" `Quick test_benchmarks_are_clean
        ; Alcotest.test_case
            "removing the keep-alive is caught"
            `Quick
            test_removing_the_keep_alive_is_caught
        ] )
    ; ( "heuristic"
      , [ Alcotest.test_case "raw call flagged" `Quick test_raw_call_is_flagged
        ; Alcotest.test_case
            "same line clean"
            `Quick
            test_keep_alive_on_the_same_line_is_clean
        ; Alcotest.test_case
            "within window clean"
            `Quick
            test_keep_alive_within_the_window_is_clean
        ; Alcotest.test_case
            "beyond window flagged"
            `Quick
            test_keep_alive_beyond_the_window_is_flagged
        ; Alcotest.test_case "db_close guarded" `Quick test_db_close_is_guarded_too
        ; Alcotest.test_case "bare word ignored" `Quick test_bare_word_is_not_a_call
        ; Alcotest.test_case
            "another module ignored"
            `Quick
            test_another_modules_finalize_is_not_a_call
        ] )
    ; ( "comments"
      , [ Alcotest.test_case
            "prose is not a call"
            `Quick
            test_a_comment_about_the_call_is_not_a_call
        ; Alcotest.test_case "nesting" `Quick test_nested_comments_are_masked
        ; Alcotest.test_case "line numbers" `Quick test_mask_preserves_line_numbers
        ; Alcotest.test_case
            "code after a comment"
            `Quick
            test_code_after_a_comment_is_still_code
        ] )
    ]
;;
