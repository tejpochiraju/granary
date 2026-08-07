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

(* sqlite3-policy: names-only — this test NAMES the bindings' module as a bare
   word, in string literals, because [test_binding_module_is_unsplit_in_the_
   source] pins the lint's binding line as SOURCE TEXT and cannot do that
   without spelling the name.  It links nothing: the module never appears as a
   qualifier here, only as data.

   The entry in SQLITE3_NAMES_ONLY_ALLOWLIST is new in #621, and the file did
   not change to earn it — the guard did.  Its pattern was the fixed string
   `Sqlite3.` WITH THE DOT, which missed every mention below along with `open
   Sqlite3` and `module S = Sqlite3` in real code; widening it to the whole word
   is what brought this honest naming into view. *)

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

(* --- strings are data, not calls (#602) -------------------------------- *)

(* [mask_non_code] handled "..." and '.' from the start; it did not know OCaml's
   quoted-string literals, so a single {|...|} containing an unbalanced paren-star
   — which the SQL in these benchmarks has, in COUNT of star-in-parens — opened a
   comment that nothing closed and masked the whole remainder of the file.  The
   lint then reported nothing at all, which is the one outcome a lint must never
   reach quietly.  Proven live during the #602 review: adding such a line to
   bench_tpcc.ml made the lint pass EVEN WITH the keep-alive deleted. *)

let test_quoted_string_does_not_swallow_the_file () =
  let s =
    Printf.sprintf
      "let q = {|SELECT COUNT(*) FROM foo|}\nlet go s = %s.finalize s\n"
      L.binding_module
  in
  let f = one_finding ~file:"x.ml" s in
  Alcotest.(check int) "line" 2 f.L.line
;;

let test_quoted_string_body_is_not_code () =
  let s =
    Printf.sprintf "let q = {|call %s.finalize s here|}\nlet x = 1\n" L.binding_module
  in
  Alcotest.(check int) "no findings" 0 (List.length (L.check ~file:"x.ml" s))
;;

let test_quoted_string_with_a_delimiter_id () =
  let s =
    Printf.sprintf
      "let q = {sql|COUNT(*) and %s.finalize s|sql}\nlet go s = %s.finalize s\n"
      L.binding_module
      L.binding_module
  in
  let f = one_finding ~file:"x.ml" s in
  Alcotest.(check int) "line" 2 f.L.line
;;

let test_a_bare_pipe_brace_does_not_close_a_named_delimiter () =
  (* Inside {sql|...|sql} a bare |} is ordinary text.  Closing on it would end
     the literal early and expose the rest of the SQL as code. *)
  let s =
    Printf.sprintf "let q = {sql|a |} b %s.finalize s|sql}\nlet x = 1\n" L.binding_module
  in
  Alcotest.(check int) "no findings" 0 (List.length (L.check ~file:"x.ml" s))
;;

let test_a_record_literal_is_still_code () =
  (* `{` only opens a quoted string when an identifier and a `|` follow it
     immediately; a record must not be mistaken for one and mask the file. *)
  let s =
    Printf.sprintf "let r = { a = 1 }\nlet go s = %s.finalize s\n" L.binding_module
  in
  let f = one_finding ~file:"x.ml" s in
  Alcotest.(check int) "line" 2 f.L.line
;;

let test_a_digit_cannot_lead_the_delimiter_id () =
  (* OCaml's delimiter identifier is [a-z_][a-z0-9_]*, so [{1|] opens nothing.
     Accepting it would mask real code from there on. *)
  let s = Printf.sprintf "let x = {1|\nlet go s = %s.finalize s\n" L.binding_module in
  Alcotest.(check int) "line" 2 (one_finding ~file:"x.ml" s).L.line
;;

let test_quoted_string_preserves_offsets () =
  let s =
    Printf.sprintf "let q = {|a\nb (* c|}\nlet go s = %s.finalize s\n" L.binding_module
  in
  Alcotest.(check int)
    "length preserved"
    (String.length s)
    (String.length (L.mask_non_code s));
  Alcotest.(check int) "line" 3 (one_finding ~file:"x.ml" s).L.line
;;

let test_unterminated_ordinary_string_does_not_raise () =
  (* A file ending in an open string literal whose last byte is a backslash
     walked [skip_string] past the end of the buffer and raised Invalid_argument
     from [Bytes.set].  "Masks to end of file" was true of the quoted form only.
     A lint that CRASHES on a malformed input is no better than one that passes
     it.  (The offending text is built below rather than written into this
     comment, because OCaml's own lexer reads string literals inside comments and
     would refuse to terminate this one.) *)
  let s = "let s = \"abc\\" in
  Alcotest.(check int) "no findings" 0 (List.length (L.check ~file:"x.ml" s));
  Alcotest.(check int)
    "length preserved"
    (String.length s)
    (String.length (L.mask_non_code s))
;;

let test_unterminated_quoted_string_masks_to_end () =
  (* Fail-safe rather than fail-open is not available here — an unterminated
     literal is not valid OCaml — but it must not crash the lint either. *)
  let s = Printf.sprintf "let q = {|%s.finalize s\n" L.binding_module in
  Alcotest.(check int) "no findings" 0 (List.length (L.check ~file:"x.ml" s))
;;

(* --- literals INSIDE comments (#625) ---------------------------------- *)

(* OCaml's lexer reads string, quoted-string and char literals inside comments,
   so a star-paren sitting inside one does NOT end the comment.  [mask_non_code]
   counted depth without interpreting those bytes: it closed the comment at the
   quoted star-paren, and the depth bookkeeping was off by one from there to the
   end of the file — masking (or unmasking) everything after it.  That is the
   same shape as #602 and the original paren-star bug, and the same failure
   mode: a lint that quietly reports nothing.

   EVERY FIXTURE BELOW IS BUILT AS DATA rather than written into a comment, for
   the reason [test_unterminated_ordinary_string_does_not_raise] gives above:
   this file is OCaml too, and its own lexer would read the literal back out of
   any comment we wrote it into.

   The fix is bounded rather than a straight port of the lexer's rule, because
   [check] also runs over MUTATED text ([without_keep_alive] deletes whole
   lines) which need not compile — so each fixture whose literal does NOT close
   pins the FALLBACK, and would fail against a naive "always skip the literal"
   fix just as surely as against the unfixed masker. *)

let quote = "\""

let test_a_string_in_a_comment_does_not_end_the_comment () =
  (* The issue's own case: a comment whose prose quotes a star-paren.  Before
     #625 the comment closed at the quoted star-paren, the trailing real one
     re-opened nothing, and the quote after it opened a string that swallowed
     the rest of the file — so the call below went unreported. *)
  let s =
    Printf.sprintf
      "(* the closer is spelled %s*)%s in the stub *)\nlet go s = %s.finalize s\n"
      quote
      quote
      L.binding_module
  in
  Alcotest.(check int) "line" 2 (one_finding ~file:"x.ml" s).L.line
;;

let test_a_string_in_a_comment_is_still_not_code () =
  (* The converse of the case above: honouring the literal must not UNMASK it.
     Prose quoting the call is prose. *)
  let s =
    Printf.sprintf
      "(* the stub says %s%s.finalize s%s *)\nlet x = 1\n"
      quote
      L.binding_module
      quote
  in
  Alcotest.(check int) "no findings" 0 (List.length (L.check ~file:"x.ml" s))
;;

let test_an_orphan_quote_in_a_comment_does_not_swallow_the_file () =
  (* Mutated input: a deleted line can leave a comment holding one unpaired
     quote, which the compiler would reject and this lint must survive.  The
     quote has no partner on its line, so the masker falls back to blanking it
     and the comment still ends at its star-paren.  A naive skip-the-string fix
     runs to end of file here and reports nothing. *)
  let s =
    Printf.sprintf
      "(* a 6%s wafer, which the compiler would refuse *)\nlet go s = %s.finalize s\n"
      quote
      L.binding_module
  in
  Alcotest.(check int) "line" 2 (one_finding ~file:"x.ml" s).L.line
;;

let test_a_quoted_string_in_a_comment_does_not_open_a_comment () =
  (* A commented-out line of the benchmarks' own SQL: COUNT of star-in-parens
     inside {|...|}.  Unhonoured, its paren-star opened a nested comment that
     the following star-paren did not balance, and the file stayed masked. *)
  let s =
    Printf.sprintf
      "(* the SQL was {|SELECT COUNT(*) FROM t|} in v1 *)\nlet go s = %s.finalize s\n"
      L.binding_module
  in
  Alcotest.(check int) "line" 2 (one_finding ~file:"x.ml" s).L.line
;;

let test_an_unclosed_quoted_string_in_a_comment_falls_back () =
  (* The quoted-string half of the fallback: no matching closer anywhere in the
     remaining text, so the opener is treated as an ordinary comment byte rather
     than masking to end of file. *)
  let s =
    Printf.sprintf "(* see {|unclosed *)\nlet go s = %s.finalize s\n" L.binding_module
  in
  Alcotest.(check int) "line" 2 (one_finding ~file:"x.ml" s).L.line
;;

let test_a_char_literal_in_a_comment_opens_no_string () =
  (* A comment naming the double-quote CHARACTER, then a string quoting a
     star-paren.  Without char literals being skipped the first quote pairs with
     the wrong partner, the parity flips, and the file is masked from there —
     which is why OCaml's lexer reads char literals inside comments too. *)
  let s =
    Printf.sprintf
      "(* the char '%s' and the closer %s*)%s *)\nlet go s = %s.finalize s\n"
      quote
      quote
      quote
      L.binding_module
  in
  Alcotest.(check int) "line" 2 (one_finding ~file:"x.ml" s).L.line
;;

let test_comment_literals_preserve_offsets () =
  let s =
    Printf.sprintf
      "(* a %s*)%s\n   b *)\nlet go s = %s.finalize s\n"
      quote
      quote
      L.binding_module
  in
  Alcotest.(check int)
    "length preserved"
    (String.length s)
    (String.length (L.mask_non_code s));
  Alcotest.(check int) "line" 3 (one_finding ~file:"x.ml" s).L.line
;;

(* --- #605: the name is one literal, in the SOURCE ---------------------- *)

let test_binding_module_value () =
  Alcotest.(check string) "module name" "Sqlite3" L.binding_module
;;

let test_binding_module_is_unsplit_in_the_source () =
  (* The value test above is not enough and must never be mistaken for enough:
     ["Sqlite" ^ "3"] evaluates to exactly the same string, so it passes while
     the evasion #605 exists to remove is back in the tree.  The property is
     about the SOURCE TEXT, so read the source.

     Match a WHOLE LINE, not a substring.  A [contains] test is satisfied by the
     search string appearing anywhere at all, including in a comment — so

       (* the old spelling was: let binding_module = "Sqlite3" *)
       let binding_module =
         "Sqlite" ^ "3"

     passed this test AND the policy script's tree-wide guard (whose pattern is
     line-oriented, and the line break separates the binding from the
     concatenation).  Verified live before this line was tightened.  The decoy
     half is what the whole-line test kills.

     What this does and does not cover: it pins the CURRENT file's binding line
     and nothing else.  The complementary guard — SPLIT_LITERAL_PATTERN in
     scripts/check-sqlite-policy.sh — is tree-wide, and is what stops a NEW file
     assembling the name to dodge the module guard.  Neither subsumes the other,
     and neither survives someone deleting it: unlike #605's original evasion,
     which needed no accomplice because the prose it hid behind was in the file
     for honest reasons, defeating this pair takes deliberate work at the site. *)
  let src = L.read_source "tpc/tpc_keepalive_lint.ml" in
  let lines = String.split_on_char '\n' src in
  if not (List.exists (String.equal "let binding_module = \"Sqlite3\"") lines)
  then
    Alcotest.fail
      "tpc_keepalive_lint.ml no longer has the unsplit binding as a line of its own \
       (`let binding_module = \"Sqlite3\"`). If the name is being assembled from pieces \
       again, don't: the #370 exemption is an entry in SQLITE3_NAMES_ONLY_ALLOWLIST \
       (#605)."
;;

let test_comparison_sources_is_not_empty () =
  (* #601's cross-check in the policy script is TEXTUAL, so text the compiler
     never sees satisfies it — a list commented out, a shadowing rebinding, a
     [List.filter (fun _ -> false)].  Every test above iterates
     [comparison_sources], so an empty list satisfies all of them vacuously:
     that is #601's own failure mode one level in.  The count is the backstop,
     and it is deliberately exact rather than [> 0]. *)
  Alcotest.(check int) "three comparison sources" 3 (List.length L.comparison_sources)
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
    ; ( "quoted strings"
      , [ Alcotest.test_case
            "does not swallow the file"
            `Quick
            test_quoted_string_does_not_swallow_the_file
        ; Alcotest.test_case "body is not code" `Quick test_quoted_string_body_is_not_code
        ; Alcotest.test_case "delimiter id" `Quick test_quoted_string_with_a_delimiter_id
        ; Alcotest.test_case
            "bare |} inside a named delimiter"
            `Quick
            test_a_bare_pipe_brace_does_not_close_a_named_delimiter
        ; Alcotest.test_case
            "record literal is code"
            `Quick
            test_a_record_literal_is_still_code
        ; Alcotest.test_case
            "a digit cannot lead the delimiter id"
            `Quick
            test_a_digit_cannot_lead_the_delimiter_id
        ; Alcotest.test_case
            "offsets preserved"
            `Quick
            test_quoted_string_preserves_offsets
        ; Alcotest.test_case
            "unterminated quoted"
            `Quick
            test_unterminated_quoted_string_masks_to_end
        ; Alcotest.test_case
            "unterminated ordinary"
            `Quick
            test_unterminated_ordinary_string_does_not_raise
        ] )
    ; ( "literals inside comments (#625)"
      , [ Alcotest.test_case
            "a string does not end the comment"
            `Quick
            test_a_string_in_a_comment_does_not_end_the_comment
        ; Alcotest.test_case
            "a string in a comment is still not code"
            `Quick
            test_a_string_in_a_comment_is_still_not_code
        ; Alcotest.test_case
            "an orphan quote does not swallow the file"
            `Quick
            test_an_orphan_quote_in_a_comment_does_not_swallow_the_file
        ; Alcotest.test_case
            "a quoted string does not open a comment"
            `Quick
            test_a_quoted_string_in_a_comment_does_not_open_a_comment
        ; Alcotest.test_case
            "an unclosed quoted string falls back"
            `Quick
            test_an_unclosed_quoted_string_in_a_comment_falls_back
        ; Alcotest.test_case
            "a char literal opens no string"
            `Quick
            test_a_char_literal_in_a_comment_opens_no_string
        ; Alcotest.test_case
            "offsets preserved"
            `Quick
            test_comment_literals_preserve_offsets
        ] )
    ; ( "the lint's own source"
      , [ Alcotest.test_case "binding module value" `Quick test_binding_module_value
        ; Alcotest.test_case
            "binding module is unsplit in the source"
            `Quick
            test_binding_module_is_unsplit_in_the_source
        ; Alcotest.test_case
            "comparison_sources is not empty"
            `Quick
            test_comparison_sources_is_not_empty
        ] )
    ]
;;
