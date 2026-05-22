# Bugfix #143 — FTS Unicode61 Tokenizer

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the ASCII-only FTS tokenizer (`lib/sql/fts_tokenizer.ml`) with a Unicode-aware tokenizer that mirrors SQLite's `unicode61` default — Unicode-category word boundaries + Unicode case folding.

**Architecture:** Iterate UTF-8 code points (via `Uutf`), classify each via `Uucp.Gc.general_category`, and case-fold via `Uucp.Case.Fold`. Word characters = `L*` (letters), `N*` (numbers), `M*` (marks), and `Pc` (connector punct). Everything else is a separator. Output `token { term; col; pos; start_byte; end_byte }` exactly as before — only the contents of `term` and the byte-offset boundaries change.

**Tech Stack:** OCaml `uucp` (Unicode character properties) + `uutf` (UTF-8 decoder). `uunf` is **not** required for the MVP — we case-fold only, no NFC/NFKC normalization (matches `unicode61` defaults, which don't NFKC by default).

**Why this matters:** German `Größe`, Turkish `İ/ı`, French `Café`, and CJK all tokenize wrong today; query/document divergence means MATCH and snippets are broken outside ASCII. Filed as #143.

**Backward compat:** `.sqlite file format compatibility is NOT a goal — concept port` (per CLAUDE.md). HOWEVER, sqlocaml's own FTS indexes built before this change will not match queries built after, since the on-disk tokens are ASCII-folded bytes and the new tokens are Unicode-folded code points. A re-index is required. Document this in a CHANGELOG note and the issue closure comment.

---

### Task 1: Add Uucp + Uutf opam deps to the container image and dune

**Files:**
- Modify: `Containerfile:5`
- Modify: `lib/sql/dune:3` (libraries list)

- [ ] **Step 1: Add uucp + uutf to the opam install line in `Containerfile`.**

  Replace line 5 of `Containerfile` with:

  ```dockerfile
  RUN opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx bisect_ppx mirage-block mirage-block-unix uutf uucp
  ```

- [ ] **Step 2: Rebuild the podman image.**

  Run on the host (not inside the existing container — the rebuild updates the image itself):

  ```bash
  podman build -t sqlocaml-dev -f Containerfile .
  ```

  Expected: a fresh `sqlocaml-dev` image with `uutf` and `uucp` available. The build downloads + compiles the new packages; expect 5–15 minutes the first time.

- [ ] **Step 3: Add `uutf uucp` to `lib/sql/dune`'s `libraries` field.**

  Change:

  ```
  (libraries lwt sqlocaml.encoding sqlocaml.store sqlocaml.catalog)
  ```

  to:

  ```
  (libraries lwt sqlocaml.encoding sqlocaml.store sqlocaml.catalog uutf uucp)
  ```

- [ ] **Step 4: Confirm the new image picks the deps up.**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | tail
  ```

  Expected: build succeeds. No "library not found" errors for uutf or uucp. (Existing fts_tokenizer.ml still works because we haven't touched it yet.)

- [ ] **Step 5: Commit.**

  ```bash
  git add Containerfile lib/sql/dune
  git commit -m "deps: add uutf + uucp for unicode61 tokenizer (toward #143)

  Co-Authored-By: Claude Sonnet 4.6 (1M context) <noreply@anthropic.com>"
  ```

---

### Task 2: Rewrite fts_tokenizer.ml to use Unicode code-point iteration

**Files:**
- Modify: `lib/sql/fts_tokenizer.ml` (full rewrite, ~50 LoC)

The current tokenizer iterates BYTES; `is_word_char` treats every UTF-8 continuation byte as a word char, so multibyte runs get glued together but never case-folded. We rewrite to iterate CODE POINTS using `Uutf.decode` over a `String.t` source. For each decoded `uchar`:

- Classify via `Uucp.Gc.general_category` — `L*`, `N*`, `M*`, `Pc` ⇒ word char; everything else (whitespace, punctuation other than connector, symbols) ⇒ separator.
- During a word run, accumulate folded output: `Uucp.Case.Fold.fold u` returns either `` `Self `` (no change — append the original uchar UTF-8 bytes) or `` `Uchars us `` (append each in `us` as UTF-8). Build into a `Buffer`.
- Track `start_byte` = byte offset of the first uchar of the run; `end_byte` = byte offset just past the last uchar of the run (use `Uutf.Manual.decoder_byte_count` or accumulate byte counts as we go via `Uutf.decoder_byte_count`).
- On a separator (or EOF), if buffer non-empty: emit a `token` with `term = Buffer.contents buf`, increment `pos`, reset buffer + state.

- [ ] **Step 1: Write failing tests in `test/test_fts.ml`.**

  Add four new test cases (paste verbatim — do not skip the test code):

  ```ocaml
  let test_tokenizer_unicode_german () =
    let toks = Fts_tokenizer.tokenize_string ~col:0 "Größe" in
    Alcotest.(check int) "one token" 1 (List.length toks);
    Alcotest.(check string) "folded term"
      "größe" (List.hd toks).Fts_tokenizer.term

  let test_tokenizer_unicode_french_case () =
    let toks = Fts_tokenizer.tokenize_string ~col:0 "Café CAFÉ" in
    Alcotest.(check int) "two tokens" 2 (List.length toks);
    Alcotest.(check (list string)) "folded match"
      ["café"; "café"]
      (List.map (fun t -> t.Fts_tokenizer.term) toks)

  let test_tokenizer_unicode_turkish_dotted_i () =
    (* SQLite unicode61 folds U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
       to "i̇" (small i + combining dot above) per default Unicode
       case folding. *)
    let toks = Fts_tokenizer.tokenize_string ~col:0 "\xC4\xB0stanbul" in
    Alcotest.(check int) "one token" 1 (List.length toks);
    Alcotest.(check string) "Turkish dotted-I fold"
      "i\xCC\x87stanbul" (List.hd toks).Fts_tokenizer.term

  let test_tokenizer_unicode_cjk_splits () =
    (* CJK ideographs are individually "Lo" — each is a word char, so a
       string of N CJK ideographs without separators should still be a
       single run from SQLite's perspective. (Hanzi parity is approximate;
       this asserts at least that the bytes round-trip and case fold is
       a no-op for ideographs.) *)
    let toks = Fts_tokenizer.tokenize_string ~col:0 "\xE4\xBD\xA0\xE5\xA5\xBD" in
    Alcotest.(check int) "one token" 1 (List.length toks);
    Alcotest.(check string) "no folding of ideographs"
      "\xE4\xBD\xA0\xE5\xA5\xBD" (List.hd toks).Fts_tokenizer.term
  ```

  Register them in the "tokenizer" sub-list at the end of `test_fts.ml`:

  ```ocaml
  Alcotest.test_case "unicode_german"        `Quick test_tokenizer_unicode_german;
  Alcotest.test_case "unicode_french_case"   `Quick test_tokenizer_unicode_french_case;
  Alcotest.test_case "unicode_turkish_i"     `Quick test_tokenizer_unicode_turkish_dotted_i;
  Alcotest.test_case "unicode_cjk_runs"      `Quick test_tokenizer_unicode_cjk_splits;
  ```

- [ ] **Step 2: Run tests and verify they fail (the current ASCII tokenizer will not produce these results).**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build test/test_fts.exe && \
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_fts.exe -- test tokenizer
  ```

  Expected: `unicode_german` and `unicode_french_case` FAIL (terms come out as `Größe` and `Café`/`CAFÉ`, not folded). `unicode_turkish_i` and `unicode_cjk_runs` may pass or fail depending on byte order — log the actual outputs.

- [ ] **Step 3: Implement the Unicode tokenizer.**

  Replace the entirety of `lib/sql/fts_tokenizer.ml` with:

  ```ocaml
  type token = {
    term       : string;
    col        : int;
    pos        : int;
    start_byte : int;
    end_byte   : int;
  }

  (* unicode61-like classification: word chars = letters, numbers, marks,
     connector-punct (e.g. underscore). Everything else is a separator. *)
  let is_word_uchar u =
    match Uucp.Gc.general_category u with
    | `Lu | `Ll | `Lt | `Lm | `Lo
    | `Nd | `Nl | `No
    | `Mn | `Mc | `Me
    | `Pc -> true
    | _   -> false

  let utf8_byte_len u =
    let c = Uchar.to_int u in
    if c < 0x80 then 1
    else if c < 0x800 then 2
    else if c < 0x10000 then 3
    else 4

  let buffer_add_uchar buf u =
    let b = Buffer.create 4 in
    Uutf.Buffer.add_utf_8 b u;
    Buffer.add_string buf (Buffer.contents b)

  let tokenize_string ~col text =
    let n = String.length text in
    let dec = Uutf.decoder ~encoding:`UTF_8 (`String text) in
    let tokens = ref [] in
    let pos    = ref 0 in
    let buf    = Buffer.create 16 in
    let run_start = ref (-1) in (* byte offset of first uchar of current run *)
    let run_end   = ref (-1) in (* byte offset just past last uchar of run *)
    let emit () =
      if !run_start >= 0 then begin
        tokens := { term = Buffer.contents buf; col; pos = !pos;
                    start_byte = !run_start; end_byte = !run_end } :: !tokens;
        incr pos;
        Buffer.clear buf;
        run_start := -1;
        run_end := -1
      end
    in
    let rec loop () =
      let i_before = Uutf.decoder_byte_count dec in
      match Uutf.decode dec with
      | `End | `Malformed _ -> emit ()
      | `Uchar u ->
        let i_after = Uutf.decoder_byte_count dec in
        let _ = i_after in
        if is_word_uchar u then begin
          if !run_start < 0 then run_start := i_before;
          (match Uucp.Case.Fold.fold u with
           | `Self      -> buffer_add_uchar buf u
           | `Uchars us -> List.iter (buffer_add_uchar buf) us);
          run_end := i_before + utf8_byte_len u
        end else
          emit ();
        loop ()
      | `Await -> emit ()
    in
    loop ();
    let _ = n in
    List.rev !tokens

  let tokenize col_texts =
    List.concat_map (fun (col, text) -> tokenize_string ~col text) col_texts
  ```

- [ ] **Step 4: Run tokenizer tests and verify they pass.**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_fts.exe -- test tokenizer
  ```

  Expected: all 10 tokenizer sub-tests pass (6 existing + 4 new).

- [ ] **Step 5: Run the entire test suite to verify no regressions.**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -10
  ```

  Expected: all 580+ e2e + 158 conformance tests still pass. Any failure indicates a downstream caller depends on the old ASCII normalization (most likely a test that uses an ASCII corpus); update the assertion to match the new lowercase output, but do **not** modify the tokenizer to "restore" ASCII-only behavior.

- [ ] **Step 6: Commit.**

  ```bash
  git add lib/sql/fts_tokenizer.ml test/test_fts.ml
  git commit -m "fix(fts): unicode61-compatible tokenizer (closes #143)

  Replaces the byte-iterating ASCII tokenizer with a Uutf-driven
  code-point iterator. Word chars are now Unicode L*/N*/M*/Pc
  categories (via Uucp.Gc); case folding goes through
  Uucp.Case.Fold (covers Turkish dotless-i, German sharp-s, etc.).

  Breaking change for any FTS index built under the old tokenizer:
  the on-disk index stores tokens as folded UTF-8 bytes, and the
  ASCII-folded byte sequences from the previous implementation will
  no longer match queries normalized through the new pipeline. Users
  with persisted FTS data must drop and re-create the table; in-
  memory FTS use is unaffected.

  Co-Authored-By: Claude Sonnet 4.6 (1M context) <noreply@anthropic.com>"
  ```

---

### Task 3: Add SQLite-parity comparison tests for non-ASCII tokenization

**Files:**
- Modify: `test/test_sqlite_compare.ml` (append to the phase35_snippet_parity_cases or add a new array)

- [ ] **Step 1: Add three non-ASCII parity cases.**

  In `test/test_sqlite_compare.ml`, locate `phase35_snippet_parity_cases = [` and append BEFORE the closing `]`:

  ```ocaml
  { name = "fts_match_german_case_fold";
    setup = [
      "CREATE VIRTUAL TABLE t USING fts5(c)";
      "INSERT INTO t VALUES('Größe matters für alle')";
    ];
    query = "SELECT c FROM t WHERE t MATCH 'größe'";
    unordered = false };

  { name = "fts_match_french_accent_case";
    setup = [
      "CREATE VIRTUAL TABLE t USING fts5(c)";
      "INSERT INTO t VALUES('Café CAFÉ café')";
    ];
    query = "SELECT c FROM t WHERE t MATCH 'café'";
    unordered = false };

  { name = "fts_match_turkish_dotted_i";
    setup = [
      "CREATE VIRTUAL TABLE t USING fts5(c)";
      "INSERT INTO t VALUES('İstanbul Istanbul')";
    ];
    query = "SELECT c FROM t WHERE t MATCH 'istanbul'";
    unordered = false };
  ```

- [ ] **Step 2: Run the comparison suite (requires SQLite mounted from host).**

  ```bash
  podman run --rm \
    -v $(pwd):/workspace:Z \
    -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
    -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
    -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
    -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
    -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail
  ```

  Expected: all parity tests pass (the new three included). If any non-ASCII case fails by a single code point (e.g. Turkish folds differently), document the divergence in the test name comment but keep the assertion strict — sqlocaml's `unicode61` should match SQLite's defaults byte-for-byte.

- [ ] **Step 3: Commit.**

  ```bash
  git add test/test_sqlite_compare.ml
  git commit -m "test(fts): non-ASCII tokenizer parity vs SQLite (#143)

  Co-Authored-By: Claude Sonnet 4.6 (1M context) <noreply@anthropic.com>"
  ```

---

### Task 4: Document the breaking change in the project memory file

**Files:**
- Modify: `/home/tej/.claude/projects/-home-tej-projects-sqlite-ocaml-port/memory/project_sqlocaml.md`

- [ ] **Step 1: Add an entry to the "SQL bugs surfaced in 36a" row (or a new "Resolved bugs" row) noting #143 is shipped and that persisted FTS indexes need re-creating.**

  Open the memory file. Find the `SQL bugs surfaced in 36a` row in the roadmap table. Convert it to (or replace with) a single-row entry like:

  ```
  | (FTS follow-ups) | shipped | #142 multi-token phrase ✓, #143 unicode61 tokenizer ✓ — persisted FTS indexes built before this need to be dropped and re-created. |
  ```

- [ ] **Step 2: Push commits to main.**

  ```bash
  git push origin main
  ```

- [ ] **Step 3: Close #143 on Forgejo with a fix-summary comment.**

  ```bash
  ~/.local/bin/forgejo issue edit tej/sqlite_ocaml_port 143 --state=closed
  ~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 143 --body="Fixed by Task 2 of plan 2026-05-22-bugfix-143-unicode61-tokenizer. Tokenizer now iterates Unicode code points (via Uutf), classifies via Uucp.Gc, and case-folds via Uucp.Case.Fold. SQLite-parity tests for German/French/Turkish added in test_sqlite_compare.ml. **Breaking:** persisted FTS indexes built before this commit must be dropped and re-created."
  ```
