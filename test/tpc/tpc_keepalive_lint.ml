(* #571 — enforcement for the keep-alive discipline in the reference-SQLite
   comparison benchmarks.

   Two of the OCaml sqlite3 bindings' C stubs — the one that finalizes a
   prepared statement and the one that closes a database — do NOT register
   their argument as a local root, and they dereference the wrapper struct
   *after* [caml_release_runtime_system].  At those call sites the argument is
   typically dead in the caller's frame (finalizing a statement is the last
   thing anyone does with it), so for the duration of that window the custom
   block is unreachable from every OCaml root.  The pending GC work the
   blocking section runs is then free to collect it and invoke its own
   finaliser, which frees the wrapper and finalizes the statement — and the
   stub resumes by reading the freed struct and handing the garbage it finds
   to C SQLite.  The result is a SIGSEGV with no exception and no message:
   exit 139, which reads as a hung benchmark rather than a crash.

   The fix at each call site is one line: mention the value again after the
   call, through a [keep_alive] that the optimizer may not see through, so the
   caller's frame keeps it live across the window.  That line is invisible —
   nothing type-checks differently without it and the crash is probabilistic —
   which is exactly the kind of discipline that decays.  Hence this lint.

   {b Crude on purpose}, in the same spirit as {!Tpc_sql_lint}: it is a
   line-oriented text scan over the harness's own sources, not an analysis.
   It masks what is not code — comment bodies (nesting, as OCaml's do) and
   string literals — so prose about a call is not mistaken for the call, then
   requires a [keep_alive] mention within a few lines after any line that
   calls a guarded binding function.  It therefore
   cannot see a call split across lines, and it cannot tell a real keep-alive
   from the word appearing nearby.  Both limits are acceptable: the benches
   route every guarded call through one small wrapper each, so what this has
   to notice is a wrapper losing its keep-alive or a new raw call site
   appearing — and it notices both.

   Delete this only when the bindings root their arguments; at that point the
   keep-alive lines go too. *)

let window = 3

type finding =
  { file : string
  ; line : int
  ; call : string
  ; text : string
  }

let pp_finding fmt f =
  Format.fprintf
    fmt
    "%s:%d: %s has no keep-alive within %d lines: %s"
    f.file
    f.line
    f.call
    window
    f.text
;;

(* sqlite3-policy: names-only — this file NAMES the bindings' module in order to
   scan the comparison benchmarks' source text for unguarded Sqlite3.finalize
   and Sqlite3.db_close calls.  It links nothing, and must not: it is compiled
   into granary_tpc, which is deliberately free of any sqlite3 dependency.

   Until #605 the name was written as two concatenated pieces so the #370 policy
   grep would not match it.  That worked silently, recorded no reason anywhere
   the policy could see one, and established "split the literal" as an accepted
   in-tree technique — which is the route a real violation would take next, and
   which the guard cannot tell apart from this honest case.  The exemption is now
   an entry in SQLITE3_NAMES_ONLY_ALLOWLIST in scripts/check-sqlite-policy.sh,
   paired with the marker on the first line above, so it is visible both in the
   policy and at the site.

   Matching on the qualifier and not just the function name is what keeps
   granary's own [Db.finalize] out of the findings. *)
let binding_module = "Sqlite3"
let guarded_calls = [ "finalize"; "db_close" ]
let keep_alive_token = "keep_alive"

(* Cross-checked against SQLITE3_COMPARISON_ALLOWLIST in
   scripts/check-sqlite-policy.sh, which fails if the two diverge in either
   direction (#601).  That script's allowlist is the source of truth: its members
   are exactly the files that link the OCaml Sqlite3 module, which is exactly the
   set that needs the keep-alive guard.  Before that check this was a third
   independent copy of the list, so a new comparison bench could be allowlisted —
   the discoverable step — and silently go unlinted, carrying #571's
   use-after-free with nothing watching. *)
let comparison_sources = [ "bench_tpcc.ml"; "bench_tpch.ml"; "bench_compare.ml" ]

(* OCaml's quoted-string delimiter identifier is [a-z_][a-z0-9_]* — a digit may
   follow but may not lead, so [{1|] is not a literal. *)
let is_ident_start c = (c >= 'a' && c <= 'z') || c = '_'
let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9')

(* Replace every character that is not code — comment bodies and string
   literal bodies — with a space, keeping newlines and every offset intact so
   line numbers still line up.  Comments nest, so this counts depth rather
   than stopping at the first close.

   String literals are skipped rather than scanned, and that is not a nicety:
   the SQL in these benchmarks says COUNT of star-in-parens, whose paren-star
   opened a comment that the following star-paren did not close, so everything
   after the first such query was masked and the lint passed over anything at
   all.  Its own mutation test caught that, which is the argument for having
   one.

   BOTH string forms are handled, and the second one is the same bug over again
   (#602): a quoted-string literal {|...|} — or {id|...|id} — went unrecognised,
   so one line of SQL written that way opened a comment nothing closed and masked
   the whole rest of the file.  A lint that reports nothing is the failure mode
   this file exists to prevent, so the delimiter rule follows OCaml's lexer: '{'
   opens a literal only when an optional lowercase identifier and then '|' follow
   it immediately, and only the matching '|id}' closes it — a bare '|}' inside
   {sql|...|sql} is ordinary text. *)
let mask_non_code src =
  let b = Bytes.of_string src in
  let n = Bytes.length b in
  let at k = if k < n then Bytes.get b k else '\000' in
  (* The bounds test is load-bearing, not defensive noise: [at] answers '\000'
     past the end, so without it a source ending in an open string literal whose
     last byte is a backslash walked [skip_string] one byte too far and raised
     Invalid_argument here. *)
  let blank k = if k < n && at k <> '\n' then Bytes.set b k ' ' in
  let depth = ref 0 in
  let i = ref 0 in
  (* [!i] is the opening quote; blank the body, leave the quotes. *)
  let skip_string () =
    incr i;
    while !i < n && at !i <> '"' do
      if at !i = '\\'
      then (
        blank !i;
        incr i);
      blank !i;
      incr i
    done;
    incr i
  in
  (* '\n' and 'a' are four and three bytes; a lone quote is a type variable. *)
  let skip_char () =
    if at (!i + 1) = '\\' && at (!i + 3) = '\''
    then i := !i + 4
    else if at (!i + 2) = '\''
    then i := !i + 3
    else incr i
  in
  (* [!i] is '{'.  [Some id] if this opens a quoted string {id|...|id}. *)
  let quoted_delim () =
    let j = ref (!i + 1) in
    if !j < n && is_ident_start (at !j)
    then (
      incr j;
      while !j < n && is_ident_char (at !j) do
        incr j
      done);
    if !j < n && at !j = '|'
    then Some (Bytes.sub_string b (!i + 1) (!j - !i - 1))
    else None
  in
  let matches_at k s =
    let m = String.length s in
    let rec go d = d >= m || (at (k + d) = s.[d] && go (d + 1)) in
    k + m <= n && go 0
  in
  (* Blank the body, leave both delimiters.  An unterminated literal is not
     valid OCaml; masking to end of file is the safe answer, not a crash. *)
  let skip_quoted_string id =
    let close = "|" ^ id ^ "}" in
    i := !i + String.length id + 2;
    while !i < n && not (matches_at !i close) do
      blank !i;
      incr i
    done;
    if !i < n then i := !i + String.length close
  in
  let skip_brace () =
    match quoted_delim () with
    | Some id -> skip_quoted_string id
    | None -> incr i
  in
  let open_comment () =
    incr depth;
    blank !i;
    blank (!i + 1);
    i := !i + 2
  in
  let close_comment () =
    decr depth;
    blank !i;
    blank (!i + 1);
    i := !i + 2
  in
  while !i < n do
    let c = at !i
    and c2 = at (!i + 1) in
    if c = '(' && c2 = '*'
    then open_comment ()
    else if c = '*' && c2 = ')' && !depth > 0
    then close_comment ()
    else if !depth > 0
    then (
      blank !i;
      incr i)
    else if c = '"'
    then skip_string ()
    else if c = '\''
    then skip_char ()
    else if c = '{'
    then skip_brace ()
    else incr i
  done;
  Bytes.to_string b
;;

let contains hay needle =
  let hn = String.length hay
  and nn = String.length needle in
  let rec go i =
    i + nn <= hn && (String.equal (String.sub hay i nn) needle || go (i + 1))
  in
  nn = 0 || go 0
;;

let check ~file src =
  let code = Array.of_list (String.split_on_char '\n' (mask_non_code src)) in
  let raw = Array.of_list (String.split_on_char '\n' src) in
  let n = Array.length code in
  let kept_alive_from i =
    let stop = min (n - 1) (i + window) in
    let rec go j = j <= stop && (contains code.(j) keep_alive_token || go (j + 1)) in
    go i
  in
  let findings = ref [] in
  Array.iteri
    (fun i line ->
       List.iter
         (fun call ->
            if
              contains line (binding_module ^ "." ^ call ^ " ") && not (kept_alive_from i)
            then
              findings
              := { file; line = i + 1; call; text = String.trim raw.(i) } :: !findings)
         guarded_calls)
    code;
  List.rev !findings
;;

(* [comparison_sources] are basenames because a test stanza's cwd is its own
   directory.  `dune exec` from the workspace root does not do that, and a lint
   that raises depending on how it was launched is a nuisance rather than a
   guard, so both places are tried before giving up. *)
let read_source file =
  let candidates = [ file; Filename.concat "test" file ] in
  match List.find_opt Sys.file_exists candidates with
  | None -> raise (Sys_error (file ^ ": not found in " ^ String.concat ", " candidates))
  | Some path ->
    let ic = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () -> In_channel.input_all ic)
;;
