(** #661: [ALTER TABLE ... ADD COLUMN ... NOT NULL GENERATED ... VIRTUAL] is no
    longer refused for lacking a DEFAULT it cannot need.

    {1 The defect}

    [Sema.bind_add_column] refuses [ADD COLUMN ... NOT NULL] without a non-NULL
    DEFAULT because existing rows decode SHORT — they were written without the
    new column, so it reads back as a stored NULL, and a DEFAULT is the only
    thing that can supply a value for them. Correct in general, and correct for
    a STORED generated column, which likewise has no read-side recompute for
    rows written before the ALTER.

    Wrong for a VIRTUAL one. There is no stored cell at all:
    [Exec.column_of_col_def] carries [generated_as] through and
    [Exec.compute_virtual_generated_cols] recomputes the column on every read,
    including for pre-existing rows. So the NULL the rule guards against cannot
    occur, and

    {v ALTER TABLE t ADD COLUMN w INTEGER NOT NULL GENERATED ALWAYS AS (v + 1) VIRTUAL; v}

    was refused for a reason that does not apply to it — the same shape as #629
    itself, a check judging a placeholder rather than the value the column will
    actually hold, one gate along from where #629 fixed it.

    {1 Why the exemption is safe rather than merely permissive}

    Because of #629. [Exec.not_null_violation] recomputes the virtuals into a
    copy of the row before judging it, so a NOT NULL VIRTUAL column is genuinely
    ENFORCED at write time. The exemption therefore relaxes {i when} the
    constraint is checked, never {i whether} —
    [the_not_null_is_really_enforced_after_the_alter] is the half of this file
    that makes that claim checkable rather than asserted.

    {1 The judgement call, and which way it went}

    The issue leaves open whether the ALTER should validate the generated
    expression over EXISTING rows. It does not, deliberately:

    - [bind_add_column] is a binder with no store or transaction, so the check
      would have to move into the exec ALTER path and would turn an O(1)
      metadata-only DDL into a full table scan of a table that may be large.
    - It matches how the engine treats pre-existing constraint violations
      generally: they are reported by [PRAGMA not_null_check], not rejected at
      DDL time.

    The cost of that choice is a row that violates its own NOT NULL and says
    nothing until it is next written, and it has no repair surface of its own —
    #629 records that [not_null_scan_cols] deliberately does not cover generated
    columns, because it reports on cells already on disk and a virtual column
    has none. [a_pre_existing_violation_is_not_validated_by_the_alter] pins that
    residual explicitly, including the fact that an UPDATE is where it surfaces
    and that changing the base column is the repair.

    {1 Oracle}

    NOT oracle-checked — the container was frozen when this was written, and
    nothing here is claimed about sqlite3. Granary already diverges from SQLite
    on generated columns and NOT NULL in decided ways (#530, #567, #629); this
    file asserts Granary's behaviour only. *)

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

(** [""] when the statement succeeded, the rendered error otherwise. Covers both
    a returned [Db.error] and an exception raised while executing. *)
let exec_err db sql =
  try
    match run (Db.execute db sql) with
    | Ok () -> ""
    | Error e -> Format.asprintf "%a" Db.pp_error e
  with
  | Failure m -> m
  | e -> Printexc.to_string e
;;

let render = function
  | Db.V_int n -> Int64.to_string n
  | Db.V_real f -> Printf.sprintf "%g" f
  | Db.V_text s -> s
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

let rows db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun r -> String.concat "," (Array.to_list (Array.map render r)))
      (run (Lwt_stream.to_list stream))
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let seed db =
  exec db "CREATE TABLE g (id INTEGER PRIMARY KEY, v INTEGER)";
  exec db "INSERT INTO g (id, v) VALUES (1, 1)";
  exec db "INSERT INTO g (id, v) VALUES (2, 2)"
;;

(* ------------------------------------------------------------------ *)

(** The issue's repro verbatim, and the read that shows the exemption is not
    just a lifted refusal: the column is recomputed for the two rows that were
    written before the ALTER, which is the whole reason it needs no DEFAULT. *)
let the_issues_repro () =
  with_db (fun db ->
    seed db;
    exec db "ALTER TABLE g ADD COLUMN w INTEGER NOT NULL GENERATED ALWAYS AS (v + 1) \
             VIRTUAL";
    Alcotest.(check (list string))
      "pre-existing rows recompute the new column"
      [ "1,1,2"; "2,2,3" ]
      (rows db "SELECT id, v, w FROM g ORDER BY id");
    (* And a row written AFTER the ALTER, through the explicit column list
       #629 requires for a table carrying a generated column. *)
    exec db "INSERT INTO g (id, v) VALUES (3, 10)";
    Alcotest.(check (list string))
      "a row written after the ALTER"
      [ "3,10,11" ]
      (rows db "SELECT id, v, w FROM g WHERE id = 3"))
;;

(** The constraint order is a column-constraint list, so both spellings must
    reach the same exemption. *)
let both_constraint_orders_are_exempt () =
  with_db (fun db ->
    seed db;
    exec db "ALTER TABLE g ADD COLUMN w INTEGER NOT NULL GENERATED ALWAYS AS (v + 1) \
             VIRTUAL";
    exec db "ALTER TABLE g ADD COLUMN x INTEGER GENERATED ALWAYS AS (v * 2) VIRTUAL NOT \
             NULL";
    Alcotest.(check (list string))
      "both added columns read"
      [ "1,2,2"; "2,3,4" ]
      (rows db "SELECT id, w, x FROM g ORDER BY id"))
;;

(** The exemption is VIRTUAL-only. A STORED generated column has no read-side
    recompute, so existing rows really would present a stored NULL, and the
    original rule applies to it unchanged. *)
let stored_is_still_refused () =
  with_db (fun db ->
    seed db;
    let msg =
      exec_err
        db
        "ALTER TABLE g ADD COLUMN z INTEGER NOT NULL GENERATED ALWAYS AS (v + 2) STORED"
    in
    Alcotest.(check bool)
      (Printf.sprintf "STORED still refused for a DEFAULT (got %S)" msg)
      true
      (contains ~needle:"DEFAULT" msg))
;;

(** Control: a plain NOT NULL column with no DEFAULT is refused exactly as
    before. The exemption must not have widened past generated columns. *)
let a_plain_not_null_column_is_still_refused () =
  with_db (fun db ->
    seed db;
    let msg = exec_err db "ALTER TABLE g ADD COLUMN p INTEGER NOT NULL" in
    Alcotest.(check bool)
      (Printf.sprintf "plain NOT NULL still refused (got %S)" msg)
      true
      (contains ~needle:"DEFAULT" msg);
    let msg2 =
      exec_err db "ALTER TABLE g ADD COLUMN p2 INTEGER NOT NULL DEFAULT NULL"
    in
    Alcotest.(check bool)
      (Printf.sprintf "an explicit NULL DEFAULT is still refused (got %S)" msg2)
      true
      (contains ~needle:"DEFAULT" msg2))
;;

(** Control: a NULLABLE virtual generated column never went through the gate at
    all and must be unaffected. *)
let a_nullable_virtual_column_still_works () =
  with_db (fun db ->
    seed db;
    exec db "ALTER TABLE g ADD COLUMN q INTEGER GENERATED ALWAYS AS (v * 3) VIRTUAL";
    Alcotest.(check (list string))
      "nullable virtual column"
      [ "1,3"; "2,6" ]
      (rows db "SELECT id, q FROM g ORDER BY id"))
;;

(** The half that makes the exemption safe rather than permissive: after the
    ALTER the NOT NULL is really enforced, on the COMPUTED value, at write time
    (#629). An INSERT whose base column is NULL makes the expression NULL and
    must be rejected. *)
let the_not_null_is_really_enforced_after_the_alter () =
  with_db (fun db ->
    seed db;
    exec db "ALTER TABLE g ADD COLUMN w INTEGER NOT NULL GENERATED ALWAYS AS (v + 1) \
             VIRTUAL";
    let msg = exec_err db "INSERT INTO g (id, v) VALUES (4, NULL)" in
    Alcotest.(check bool)
      (Printf.sprintf "NULL base column rejected (got %S)" msg)
      true
      (msg <> "");
    Alcotest.(check (list string))
      "and the row was not written"
      []
      (rows db "SELECT id FROM g WHERE id = 4");
    (* A non-NULL base column is accepted, so the refusal above is about the
       computed value and not about the column being generated at all. *)
    exec db "INSERT INTO g (id, v) VALUES (5, 7)";
    Alcotest.(check (list string))
      "a legal row still writes"
      [ "5,8" ]
      (rows db "SELECT id, w FROM g WHERE id = 5"))
;;

(** The accepted residual, pinned so that closing it later is a visible change.
    A row already on disk whose generated expression is NULL does not block the
    ALTER, reads its NULL without complaint, and surfaces only on the next write
    that rewrites it — where [write_row_rekeyed] -> [enforce_not_null]
    recomputes the virtual and raises. Changing the base column is the repair,
    and it is the only one: [PRAGMA not_null_check] deliberately does not cover
    generated columns (#629). *)
let a_pre_existing_violation_is_not_validated_by_the_alter () =
  with_db (fun db ->
    exec db "CREATE TABLE h (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO h (id, v) VALUES (1, NULL)";
    (* Not validated: the ALTER succeeds over a row that violates it. *)
    exec db "ALTER TABLE h ADD COLUMN w INTEGER NOT NULL GENERATED ALWAYS AS (v + 1) \
             VIRTUAL";
    Alcotest.(check (list string))
      "the violating row reads NULL rather than being rejected"
      [ "1,NULL" ]
      (rows db "SELECT id, w FROM h");
    (* It surfaces on the next write. *)
    let msg = exec_err db "UPDATE h SET v = NULL WHERE id = 1" in
    Alcotest.(check bool)
      (Printf.sprintf "an UPDATE that leaves it NULL raises (got %S)" msg)
      true
      (msg <> "");
    (* And the repair is to move the base column. *)
    exec db "UPDATE h SET v = 5 WHERE id = 1";
    Alcotest.(check (list string))
      "repaired by changing the base column"
      [ "1,6" ]
      (rows db "SELECT id, w FROM h"))
;;

let () =
  Alcotest.run
    "ADD COLUMN NOT NULL VIRTUAL GENERATED (#661)"
    [ ( "the exemption"
      , [ Alcotest.test_case "the issue's repro" `Quick the_issues_repro
        ; Alcotest.test_case
            "both constraint orders"
            `Quick
            both_constraint_orders_are_exempt
        ] )
    ; ( "the exemption is narrow"
      , [ Alcotest.test_case "STORED is still refused" `Quick stored_is_still_refused
        ; Alcotest.test_case
            "a plain NOT NULL column is still refused"
            `Quick
            a_plain_not_null_column_is_still_refused
        ; Alcotest.test_case
            "a nullable virtual column still works"
            `Quick
            a_nullable_virtual_column_still_works
        ] )
    ; ( "enforcement and the accepted residual"
      , [ Alcotest.test_case
            "the NOT NULL is really enforced after the ALTER"
            `Quick
            the_not_null_is_really_enforced_after_the_alter
        ; Alcotest.test_case
            "a pre-existing violation is not validated by the ALTER"
            `Quick
            a_pre_existing_violation_is_not_validated_by_the_alter
        ] )
    ]
;;
