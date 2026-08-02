(** A lint against the #571 crash: a binding call that frees its own argument
    while nothing keeps that argument alive.

    Two of the OCaml sqlite3 bindings' C stubs — the statement finalizer and
    the database closer — do not register their argument as a local root and
    dereference the wrapper struct after releasing the runtime system. At such
    a call site the argument is normally dead in the caller's frame, so the GC
    may collect the custom block inside that window, run its own finaliser
    (which frees the wrapper) and leave the stub reading freed memory. The
    process takes SIGSEGV — exit 139, no exception, no message.

    The remedy is one line per call site: mention the value again afterwards
    through a [keep_alive] the optimizer may not see through. Nothing
    type-checks differently without that line and the crash is probabilistic,
    so the discipline decays silently; this lint is what stops it.

    {b Deliberately crude}, like {!Tpc_sql_lint}: a line-oriented text scan
    over the harness's own sources, with OCaml's nesting comments masked out
    so prose about a call is not mistaken for one. It cannot see a call split
    across lines and cannot tell a real keep-alive from the word appearing
    nearby. The benches route every guarded call through one small wrapper
    each, so what it must notice is a wrapper losing its keep-alive or a new
    raw call site appearing — and it notices both.

    Delete it when the bindings root their arguments; the keep-alive lines go
    with it. *)

(* sqlite3-policy: names-only — this interface NAMES Sqlite3.finalize and
   Sqlite3.db_close in its documentation; neither it nor its implementation
   links the module. Allowlisted in scripts/check-sqlite-policy.sh; see #605.
   (The marker sits below the module doc comment rather than above it because
   merlint requires an .mli to OPEN with one.) *)

(** One call to a guarded binding function with no keep-alive after it. *)
type finding =
  { file : string (** the source file the call was found in *)
  ; line : int (** 1-based line number of the call *)
  ; call : string (** which guarded function was called *)
  ; text : string (** the offending source line, trimmed *)
  }

(** [pp_finding fmt f] prints the finding as a one-line file:line diagnostic. *)
val pp_finding : Format.formatter -> finding -> unit

(** The bindings' module name, which a guarded call must be qualified by —
    granary's own [Db.finalize] is not this bug. Written as the plain literal
    since #605; the #370 policy exemption is a recorded entry in
    [SQLITE3_NAMES_ONLY_ALLOWLIST] rather than a split string literal. *)
val binding_module : string

(** The binding functions that free their own argument. *)
val guarded_calls : string list

(** The token a guarded call must be followed by. *)
val keep_alive_token : string

(** How many lines after a guarded call the keep-alive may appear in. *)
val window : int

(** The benchmark sources that use the bindings directly, as basenames relative
    to the test directory. Cross-checked against [SQLITE3_COMPARISON_ALLOWLIST]
    in [scripts/check-sqlite-policy.sh], which fails if the two lists diverge in
    either direction (#601) — that allowlist is the source of truth. *)
val comparison_sources : string list

(** [mask_non_code src] is [src] with every character that is not code —
    comment bodies and string-literal bodies — replaced by a space, preserving
    newlines and every byte offset so line numbers are unchanged.

    Comments nest, as OCaml's do. Both of OCaml's string forms are skipped
    rather than scanned: ordinary ["..."] with its backslash escapes, and
    quoted strings [{|...|}] and [{id|...|id}], where only the matching
    [|id}] closes. Char literals are skipped too, without mistaking a type
    variable for one. Everything else is treated as code — a [{] not followed
    by an optional lowercase identifier and a [|] is a record, not a literal.

    Skipping strings is not a nicety. The SQL in these benchmarks counts
    star-in-parens, whose paren-star opened a comment that nothing closed, and
    the whole rest of the file was masked; the quoted-string form was the same
    bug a second time (#602).

    {b Known limit}: string literals are recognised in code but not {e inside}
    comments, where every byte is blanked without interpretation. OCaml's own
    lexer does read them there, so [(* "*)" *)] is valid and this masks the rest
    of the file — #625. Rare, and [test_removing_the_keep_alive_is_caught] turns
    whole-file masking into a loud failure rather than a silent pass. *)
val mask_non_code : string -> string

(** [contains hay needle] is whether [needle] occurs in [hay]. Exposed because
    the lint's own tests mutate real sources by the same token test it uses. *)
val contains : string -> string -> bool

(** [check ~file src] is every guarded call in [src] that has no keep-alive
    within {!window} lines, in source order. [file] only labels the findings.
    The empty list means the source is clean as far as this lint can tell. *)
val check : file:string -> string -> finding list

(** [read_source file] is the whole contents of [file], looked up relative to
    the current directory and then to [test/] so it works both under a test
    stanza (whose cwd is its own directory) and under [dune exec] from the
    workspace root. Raises [Sys_error] if neither exists: a lint that silently
    skips its subject is no lint. *)
val read_source : string -> string
