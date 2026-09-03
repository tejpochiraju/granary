module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row
module Index_key = Granary_encoding.Index_key

(* Produces uppercase SQLite PRAGMA wire-format strings ("CASCADE", "NO ACTION", etc.)
   Distinct from Cat.fk_action_to_string which uses lowercase for internal serialization. *)
let fk_action_str = function
  | Cat.FA_no_action -> "NO ACTION"
  | Cat.FA_restrict -> "RESTRICT"
  | Cat.FA_cascade -> "CASCADE"
  | Cat.FA_set_null -> "SET NULL"
  | Cat.FA_set_default -> "SET DEFAULT"
;;

let plan_binop : Sema.binop -> Plan.binop = function
  | Sema.Eq -> Plan.Eq
  | Sema.Ne -> Plan.Ne
  | Sema.Lt -> Plan.Lt
  | Sema.Le -> Plan.Le
  | Sema.Gt -> Plan.Gt
  | Sema.Ge -> Plan.Ge
  | Sema.Add -> Plan.Add
  | Sema.Sub -> Plan.Sub
  | Sema.Mul -> Plan.Mul
  | Sema.Div -> Plan.Div
  | Sema.And -> Plan.And
  | Sema.Or -> Plan.Or
  | Sema.Concat -> Plan.Concat
  | Sema.Mod -> Plan.Mod
  | Sema.Bit_and -> Plan.Bit_and
  | Sema.Bit_or -> Plan.Bit_or
  | Sema.Lshift -> Plan.Lshift
  | Sema.Rshift -> Plan.Rshift
  | Sema.Like -> Plan.Like
  | Sema.Glob -> Plan.Glob
;;

let rec plan_expr = function
  | Sema.BE_lit l -> Plan.P_lit l
  | Sema.BE_col i -> Plan.P_col i
  | Sema.BE_binop (op, a, b) -> Plan.P_binop (plan_binop op, plan_expr a, plan_expr b)
  | Sema.BE_not e -> Plan.P_not (plan_expr e)
  | Sema.BE_is_null e -> Plan.P_is_null (plan_expr e)
  | Sema.BE_is_not_null e -> Plan.P_is_not_null (plan_expr e)
  | Sema.BE_neg e -> Plan.P_neg (plan_expr e)
  | Sema.BE_bitnot e -> Plan.P_bitnot (plan_expr e)
  | Sema.BE_between (x, lo, hi) -> Plan.P_between (plan_expr x, plan_expr lo, plan_expr hi)
  | Sema.BE_in (x, vals) -> Plan.P_in (plan_expr x, List.map plan_expr vals)
  | Sema.BE_func (func, args) -> Plan.P_func (func, List.map plan_expr args)
  | Sema.BE_param i -> Plan.P_param i
  | Sema.BE_match _ ->
    failwith "plan_expr: BE_match should be handled at statement level, not as an expr"
  | Sema.BE_subquery inner -> Plan.P_subquery inner
  | Sema.BE_exists inner -> Plan.P_exists inner
  | Sema.BE_in_select (bx, inner) -> Plan.P_in_select (plan_expr bx, inner)
  | Sema.BE_case { scrutinee; branches; else_ } ->
    Plan.P_case
      { scrutinee = Option.map plan_expr scrutinee
      ; branches = List.map (fun (c, r) -> plan_expr c, plan_expr r) branches
      ; else_ = Option.map plan_expr else_
      }
  | Sema.BE_cast (e, ty) -> Plan.P_cast (plan_expr e, ty)
  | Sema.BE_excluded_col i -> Plan.P_excluded_col i
  | Sema.BE_window_slot i -> Plan.P_window_slot i
  (* #489/#490: an output-row column reference.  It only ever appears as a
     whole ORDER BY key on a sort that runs after projection, where the row in
     hand IS the output row — so reading column [i] of it is exactly right.
     [plan_post_agg_sort] additionally has to keep it OUT of its pre-aggregation
     index remapping; see the comment there. *)
  | Sema.BE_out_col i -> Plan.P_col i
  | Sema.BE_collate (be, c) -> Plan.P_collate (plan_expr be, c)
;;

(** Try to recognise an equality predicate of the form [col = v] (or
    [v = col]) where [v] is a literal or a bound parameter, at the top level
    of the WHERE clause.  Returns [Some (col_idx, value_expr)] if matched,
    [None] otherwise.

    A literal [col = NULL] is intentionally NOT matched: [WHERE col = NULL]
    "never matches", and falling back to [Op_filter] (which short-circuits on
    NULL) gives correct behaviour.  A bound parameter ([col = ?]) IS matched —
    this is the common prepared-statement point lookup (#228) — but because the
    bound value is unknown at plan time and may be NULL at run time,
    [Op_index_lookup] execution must return no rows when the value evaluates to
    NULL (see [stream_index_lookup]). *)
let recognise_eq_col_lit = function
  | Sema.BE_binop (Sema.Eq, Sema.BE_col i, (Sema.BE_lit l as e))
  | Sema.BE_binop (Sema.Eq, (Sema.BE_lit l as e), Sema.BE_col i) ->
    (match l with
     | Ast.L_null -> None
     | _ -> Some (i, e))
  | Sema.BE_binop (Sema.Eq, Sema.BE_col i, (Sema.BE_param _ as e))
  | Sema.BE_binop (Sema.Eq, (Sema.BE_param _ as e), Sema.BE_col i) -> Some (i, e)
  | _ -> None
;;

(** A value a range end can be built from: a non-NULL literal, or a bound
    parameter.  A literal NULL is not one, for the same reason [col = NULL] is
    not matched: the comparison is never true, and the filter path already
    handles it. *)
let range_value = function
  | Sema.BE_lit Ast.L_null -> None
  | Sema.BE_lit _ as e -> Some e
  | Sema.BE_param _ as e -> Some e
  | _ -> None
;;

(** #517: recognise a predicate that constrains one or both ends of a column's
    range — an inequality [col <op> v] (or [v <op> col]), or [col BETWEEN lo AND
    hi] (#519) — at the top level of the WHERE clause.  Returns the column and
    the value for each end it constrains.

    Strictness is deliberately dropped: the caller uses this only to narrow the
    span an index seek scans, never to decide whether a row qualifies, so
    treating [>] as [>=] can cost one extra key and can never lose a row.  For
    the same reason a [BETWEEN] whose ends are not both recognisable still
    contributes the end that is — narrowing one side is sound on its own.

    [NOT BETWEEN] parses as a negation wrapping [BE_between], so it does not
    reach this function's [BE_between] case and constrains nothing. *)
let recognise_range_col_lit = function
  | Sema.BE_between (Sema.BE_col i, lo, hi) ->
    (match range_value lo, range_value hi with
     | None, None -> None
     | lo, hi -> Some (i, lo, hi))
  | Sema.BE_binop (op, Sema.BE_col i, e) ->
    (match range_value e, op with
     | None, _ -> None
     | Some v, (Sema.Ge | Sema.Gt) -> Some (i, Some v, None)
     | Some v, (Sema.Le | Sema.Lt) -> Some (i, None, Some v)
     | Some _, _ -> None)
  | Sema.BE_binop (op, e, Sema.BE_col i) ->
    (* [v > col] constrains the column's UPPER end, not its lower one. *)
    (match range_value e, op with
     | None, _ -> None
     | Some v, (Sema.Ge | Sema.Gt) -> Some (i, None, Some v)
     | Some v, (Sema.Le | Sema.Lt) -> Some (i, Some v, None)
     | Some _, _ -> None)
  | _ -> None
;;

(* An index is usable for equality lookup only if all its columns are plain
   (not expressions) and it is not partial: a row absent from a partial index
   may still satisfy the query's WHERE clause, and the optimizer cannot match
   query predicates against expression-index keys. *)
let index_is_seekable (i : Cat.index_info) =
  let is_plain_cols =
    match i.Cat.idx_expr_flags with
    | [] -> true (* old format: no flags = all plain *)
    | flags -> not (List.exists Fun.id flags)
  in
  is_plain_cols && i.Cat.idx_where_sql = None
;;

(** The ordinal of [name] in [meta]'s column list, if it has one. *)
let col_ordinal (meta : Cat.table_meta) name =
  let rec go n = function
    | [] -> None
    | (c : Row.column) :: rest ->
      if String.equal c.Row.name name then Some n else go (n + 1) rest
  in
  go 0 meta.Cat.columns
;;

(** #516: build a nested-loop probe key for one candidate index of the join's
    right table, or [None] if the index cannot serve as a probe.

    The index's columns are walked in order and each is pinned either by the
    join column — whose value comes from the left row, and is preferred when a
    column could be pinned both ways — or by a WHERE equality on the right
    table, which contributes a constant.  The walk stops at the first column
    pinned by neither, so the key always covers a leading prefix.

    A key that pins no left-row value is rejected: every left row would read the
    same range, which is a constant restriction rather than a join probe, and
    the ON predicate would go unenforced by the probe. *)
let probe_key_for_index (right_meta : Cat.table_meta) ~join_col ~left_col ~right_eqs idx =
  let rec walk acc drives = function
    | [] -> acc, drives
    | name :: rest ->
      (match col_ordinal right_meta name with
       | None -> acc, drives
       | Some ord when ord = join_col ->
         walk (Plan.Probe_from_left left_col :: acc) true rest
       | Some ord ->
         (match List.assoc_opt ord right_eqs with
          | Some e -> walk (Plan.Probe_const (plan_expr e) :: acc) drives rest
          | None -> acc, drives))
  in
  if not (index_is_seekable idx)
  then None
  else (
    match walk [] false idx.Cat.idx_columns with
    | _, false -> None
    | parts, true -> Some (List.rev parts))
;;

(** #516: column ordinals in a bound WHERE clause address the combined row, so
    the join's right table owns the [n_right_cols] slots at [right_offset].
    Answers [col_idx]'s ordinal within the right table, or [None] if the column
    belongs to the left side.

    Shared by {!right_table_eqs} and {!right_table_ranges} (#532): the two want
    exactly the same mapping and disagreeing about it would silently mis-address
    a seek. *)
let rebase_right_col ~right_offset ~n_right_cols col_idx =
  if col_idx >= right_offset && col_idx < right_offset + n_right_cols
  then Some (col_idx - right_offset)
  else None
;;

(** #516: the WHERE equalities that pin a column of the join's {i right} table,
    re-based to right-table ordinals. *)
let right_table_eqs ~right_offset ~n_right_cols cs =
  List.filter_map
    (fun c ->
       match recognise_eq_col_lit c with
       | Some (col_idx, e) ->
         Option.map
           (fun ord -> ord, e)
           (rebase_right_col ~right_offset ~n_right_cols col_idx)
       | None -> None)
    cs
;;

(** #532: the WHERE range bounds that constrain a column of the join's {i right}
    table, re-based to right-table ordinals so {!range_for_index} can read them.

    {!right_table_eqs} re-bases by handing back the ordinal {i beside} the value,
    and the chooser never looks at the original AST node again.  A range cannot
    be re-based that way: {!range_for_index} re-recognises whole conjuncts, so
    the ordinal has to move {i inside} the expression.  Rather than rewrite the
    caller's node — which would have to know every spelling
    {!recognise_range_col_lit} accepts, [BETWEEN] and both operand orders
    included — each recognised end is re-emitted as its own canonical
    [col >= v] / [col <= v] conjunct over the re-based ordinal.  Those round-trip
    through [recognise_range_col_lit] to the same ends the original produced, and
    [range_for_index] folds the two ends independently anyway, so splitting a
    [BETWEEN] into its halves loses nothing.

    Emitting [>=]/[<=] for a strict [>]/[<] is sound for the reason given at
    {!recognise_range_col_lit}: that function already drops strictness, because a
    range only narrows the span a seek walks and every row it yields is still
    tested by the whole WHERE clause.

    The narrowing this enables is sound for the invariant #513/#516/#528 rest on:
    [chain_joins] applies the whole WHERE clause to the joined row, so
    restricting what the build side reads cannot change which joined rows
    survive. *)
let right_table_ranges ~right_offset ~n_right_cols cs =
  List.concat_map
    (fun c ->
       match recognise_range_col_lit c with
       | None -> []
       | Some (col_idx, lo, hi) ->
         (match rebase_right_col ~right_offset ~n_right_cols col_idx with
          | None -> []
          | Some ord ->
            let col = Sema.BE_col ord in
            List.filter_map
              Fun.id
              [ Option.map (fun v -> Sema.BE_binop (Sema.Ge, col, v)) lo
              ; Option.map (fun v -> Sema.BE_binop (Sema.Le, col, v)) hi
              ]))
    cs
;;

(** #551: is [meta] backed by a real B-tree the planner may seek or probe?

    The catalog is reached {i by name} — [Cat.indexes_for_table cat ~table:name]
    — but a name is not an identity.  [Cat.register_ephemeral] replaces only the
    {i table} entry and leaves [indexes_by_table] alone, so a CTE that shadows a
    real table answers with that table's indexes while carrying a synthesized
    [table_meta] whose [tree_id] is negative.  Probing the shadowed tree with
    the CTE's shape is a silently wrong answer (zero rows), not a missed
    optimisation.

    Synthesized tables — CTEs, [sqlite_master], [sqlite_sequence] — are marked
    by a negative [tree_id]; a columnar table has no B-tree at all. *)
let meta_is_btree_backed (meta : Cat.table_meta) =
  match meta.Cat.storage with
  | Cat.Row { tree_id; _ } -> tree_id >= 0
  | Cat.Columnar _ -> false
;;

(** Pick the candidate index of [right_meta] whose probe key covers the most
    columns.

    #551: gated on {!meta_is_btree_backed}, for the reason spelled out there —
    without it a CTE shadowing a real table takes a nested-loop probe against
    that table's index tree and returns nothing.  #531 added the same guard on
    the build side; this is its sibling, and it needs a driving side below
    {!nlj_min_driving_rows} to be reached at all. *)
let best_probe cat (right_meta : Cat.table_meta) ~join_col ~left_col ~right_eqs =
  if not (meta_is_btree_backed right_meta)
  then None
  else
    Cat.indexes_for_table cat ~table:right_meta.Cat.name
    |> List.filter_map (fun (i : Cat.index_info) ->
      Option.map
        (fun parts -> i, parts)
        (probe_key_for_index right_meta ~join_col ~left_col ~right_eqs i))
    |> function
    | [] -> None
    | c :: rest ->
      Some
        (List.fold_left
           (fun (bi, bp) (i, p) ->
              if List.length p > List.length bp then i, p else bi, bp)
           c
           rest)
;;

(** Flatten the top-level [AND] spine of a WHERE clause into its conjuncts.
    [OR] and everything else are opaque leaves. *)
let rec conjuncts (e : Sema.bound_expr) =
  match e with
  | Sema.BE_binop (Sema.And, a, b) -> conjuncts a @ conjuncts b
  | e -> [ e ]
;;

(** #508: match [eqs] — the recognised [(conjunct position, col ordinal, value)]
    equalities of the WHERE clause — against one index, longest seekable prefix
    first.  Returns the covered leading columns in INDEX order together with the
    conjunct positions they consumed, or [None] if the index's first column is
    not pinned.

    Position (not column ordinal) identifies a consumed conjunct, so a repeated
    column — [w = 1 AND w = 2] — consumes only the conjunct it actually seeks
    with and leaves the other to the residual filter. *)
let prefix_for_index (meta : Cat.table_meta) (i : Cat.index_info) eqs =
  let col_ordinal name =
    let rec go k = function
      | [] -> None
      | (c : Row.column) :: rest ->
        if String.equal c.Row.name name then Some k else go (k + 1) rest
    in
    go 0 meta.Cat.columns
  in
  let rec go acc used = function
    | [] -> List.rev acc, used
    | idx_col :: rest ->
      (match col_ordinal idx_col with
       | None -> List.rev acc, used
       | Some ord ->
         (match
            List.find_opt (fun (pos, c, _) -> c = ord && not (List.mem pos used)) eqs
          with
          | None -> List.rev acc, used
          | Some (pos, _, v) -> go ((ord, v) :: acc) (pos :: used) rest))
  in
  match go [] [] i.Cat.idx_columns with
  | [], _ -> None
  | prefix, used -> Some (prefix, used)
;;

(** Pick the index giving the longest equality-covered leading prefix. *)
let find_index_for_eqs cat (meta : Cat.table_meta) eqs =
  Cat.indexes_for_table cat ~table:meta.Cat.name
  |> List.filter index_is_seekable
  |> List.filter_map (fun i ->
    Option.map (fun (prefix, used) -> i, prefix, used) (prefix_for_index meta i eqs))
  |> List.fold_left
       (fun best ((_, prefix, _) as cand) ->
          match best with
          | Some (_, bp, _) when List.length bp >= List.length prefix -> best
          | _ -> Some cand)
       None
;;

(** #517: only fixed-width index-key encodings can carry a range bound.  See
    {!Plan.range}. *)
let bounded_type = function
  | Row.Integer | Row.Real -> true
  | Row.Text | Row.Blob -> false
;;

(** #523: what a candidate bound is worth to a same-end fold.

    - [`Orderable] — a numeric literal on a numeric column.  Two of them can be
      compared here, and the winner is one
      {!Granary_sql.Exec.range_seek_bounds} will actually encode.  #527: this
      includes a literal of the {i other} numeric type, which that function
      promotes across the int/real boundary by rounding outward.
    - [`Unknown] — a bound parameter, whose value is not known until run time.
      It may well be the tighter one, so a fold must keep it rather than
      discard it for a literal it cannot compare it against.
    - [`Useless] — a literal that can never bound anything, because
      [range_seek_bounds] has nothing sound to turn it into and leaves that end
      unbounded: a text or blob literal on a numeric column, or a real outside
      int64 range (an infinity included) on an integer one.  A fold should drop
      it in favour of any other candidate.

    A NaN is deliberately [`Unknown] rather than [`Orderable]: it encodes as
    NULL, whose behaviour as a bound is a special case of its own (see
    [range_seek_bounds]).  Keeping it out of the ordering leaves that case
    exactly as it was.

    The int64-range test mirrors {!Granary_sql.Exec.range_bound_key}'s own, so
    the two agree on every value but the last ULP below ±2^63, where that
    function widens by one float step and can decline one end of a value
    classified here as [`Orderable] (it has no [`Lo]/[`Hi] to condition on).
    A residual disagreement there costs at most a narrowing the fold would
    otherwise have kept, never a row: every candidate is individually a sound
    bound and the predicate runs on every yielded row.

    That [9.2233720368547758e18] below is exactly 2^63, and is deliberately the
    same literal as [Granary_sql.Exec.two_pow_63] rather than a reference to it:
    that module depends on this one, not the reverse, so sharing the constant
    would mean exporting an [Int64.of_float]-domain value from this [.mli] for
    one use.  The two MUST stay equal — the one-ULP disagreement described above
    is the {i whole} disagreement only while they are.  See the note at
    [two_pow_63] before changing either. *)
let classify_range_bound (ty : Row.ty) = function
  | Sema.BE_lit (Ast.L_int _) when ty = Row.Integer || ty = Row.Real -> `Orderable
  | Sema.BE_lit (Ast.L_real f) when ty = Row.Real ->
    if Float.is_nan f then `Unknown else `Orderable
  | Sema.BE_lit (Ast.L_real f) when ty = Row.Integer ->
    if Float.is_nan f
    then `Unknown
    else if Float.abs f < 9.2233720368547758e18 (* exactly 2^63 — see above *)
    then `Orderable
    else `Useless
  | Sema.BE_lit _ -> `Useless
  | _ -> `Unknown
;;

(** Order two [`Orderable] bounds for the same column.  Within a single numeric
    type [Float.compare] / [Int64.compare] agree with
    {!Granary_sql.Exec.compare_values} (the residual predicate's order since
    #733) and with {!Granary_encoding.Index_key.encode_value}, so the fold can
    never pick a bound the predicate and the key order disagree about.  #527: a
    mixed int/real pair is ordered here through [Int64.to_float], which above
    2^53 can name the wrong one "tightest" — and since #733 the predicate no
    longer promotes that way, so the two genuinely differ there.  That is a
    perf question, not a soundness one — the fold only ever picks between
    candidates that are each individually a sound bound, and no conjunct is
    marked consumed, so a wrong pick loses a narrowing and never a row. *)
let compare_range_bounds a b =
  match a, b with
  | Sema.BE_lit (Ast.L_int x), Sema.BE_lit (Ast.L_int y) -> Some (Int64.compare x y)
  | Sema.BE_lit (Ast.L_real x), Sema.BE_lit (Ast.L_real y) -> Some (Float.compare x y)
  | Sema.BE_lit (Ast.L_int x), Sema.BE_lit (Ast.L_real y) ->
    Some (Float.compare (Int64.to_float x) y)
  | Sema.BE_lit (Ast.L_real x), Sema.BE_lit (Ast.L_int y) ->
    Some (Float.compare x (Int64.to_float y))
  | _, _ -> None
;;

(** #523: keep whichever of [best] and [cand] constrains [which] end more.  A
    [`Useless] candidate loses to anything else; otherwise only two
    [`Orderable]s can be separated, and every other pairing keeps the incumbent.

    Keeping the incumbent when a parameter is involved makes the result depend
    on the order the conjuncts were written, which is deliberate: with no
    plan-time value there is nothing better to go on, and either choice is a
    sound narrowing because the predicate still runs on every row. *)
let tighter_bound ty which best cand =
  match classify_range_bound ty best, classify_range_bound ty cand with
  | `Useless, (`Orderable | `Unknown) -> cand
  | `Orderable, `Orderable ->
    (match compare_range_bounds best cand, which with
     | None, _ -> best
     | Some n, `Lo -> if n >= 0 then best else cand
     | Some n, `Hi -> if n <= 0 then best else cand)
  | (`Orderable | `Unknown | `Useless), (`Orderable | `Unknown | `Useless) -> best
;;

(** #517: find a range over the index column immediately following the
    equality-covered prefix.  [n_eq] is how many leading index columns the
    equalities pinned, so the bounded column is the index's [n_eq]th.

    The conjuncts are NOT marked consumed: the range narrows the scanned span
    and the predicate is still evaluated on every row, which is what makes an
    inclusive-only bound sound.

    #523: several conjuncts may constrain the same end — [o BETWEEN 100 AND 200
    AND o >= 150], or a plain [o >= 100 AND o >= 150].  Each is individually
    sound, and since every result row satisfies all of them, so is the
    {i extremum}: the greatest lower bound and the least upper bound.  Taking
    the first match instead left the tighter one unused.  See
    {!tighter_bound} for what happens when two candidates cannot be ordered —
    an end whose only candidate is a bound parameter still bounds the seek. *)
let range_for_index (meta : Cat.table_meta) (i : Cat.index_info) ~n_eq conjuncts_list =
  match List.nth_opt i.Cat.idx_columns n_eq with
  | None -> None
  | Some idx_col ->
    (match col_ordinal meta idx_col with
     | None -> None
     | Some ord ->
       let ty = (List.nth meta.Cat.columns ord).Row.ty in
       if not (bounded_type ty)
       then None
       else (
         let end_of which c =
           match recognise_range_col_lit c with
           | Some (col_idx, lo, hi) when col_idx = ord ->
             (match which with
              | `Lo -> lo
              | `Hi -> hi)
           | Some _ | None -> None
         in
         let fold which best c =
           match end_of which c, best with
           | None, _ -> best
           | Some cand, None -> Some cand
           | Some cand, Some best -> Some (tighter_bound ty which best cand)
         in
         let pick which =
           Option.map plan_expr (List.fold_left (fold which) None conjuncts_list)
         in
         match pick `Lo, pick `Hi with
         | None, None -> None
         | r_lo, r_hi -> Some { Plan.r_ty = ty; r_lo; r_hi }))
;;

(** Detect [BE_col a = BE_col b] equality at the top level. *)
let recognise_eq_col_col = function
  | Sema.BE_binop (Sema.Eq, Sema.BE_col a, Sema.BE_col b) -> Some (a, b)
  | _ -> None
;;

(* Negative [tree_id]s are sentinels for synthesized scans with no real B-tree:
   -1 = CTE, -2 = sqlite_master, -3 = sqlite_sequence.  Each is materialized by a
   dedicated plan op rather than a [Op_seq_scan] over a stored tree. *)
(* #635: [alias] is the FROM item's alias, carried onto the leaf scan so
   [Exec.get_outer_scan_metas] can resolve an alias-qualified outer reference.
   The three synthesized scans below take no alias because none of them decodes
   a base table an outer reference could name. *)
let make_scan ~alias (meta : Cat.table_meta) : Plan.op =
  match meta.Cat.storage with
  | Cat.Columnar _ -> Plan.Op_col_seq_scan { table_meta = meta; alias }
  | Cat.Row { tree_id = -1; _ } ->
    Plan.Op_cte_scan { cte_name = meta.Cat.name; n_cols = List.length meta.Cat.columns }
  | Cat.Row { tree_id = -2; _ } -> Plan.Op_sqlite_master
  | Cat.Row { tree_id = -3; _ } -> Plan.Op_sqlite_sequence
  | Cat.Row _ -> Plan.Op_seq_scan { table_meta = meta; alias }
;;

(* Realise a chosen seek as the base-table plan op it reads through. *)
let seek_op ~alias (table_meta : Cat.table_meta) = function
  | Plan.Seek_rowid lookup_val -> Plan.Op_rowid_lookup { table_meta; lookup_val; alias }
  | Plan.Seek_index { idx_tree; keys; range } ->
    (* #550's non-unique-index bail-out budget is a DML-drain concern only, and
       is recomputed at execution time from [idx_tree] rather than carried on
       [Plan.seek] at all (see plan.mli); [Op_index_lookup] has no field for it
       because a SELECT or a hash join's build side streams rows one at a time
       rather than buffering every candidate before reading any of them, so it
       has nothing to "fall back" from mid-walk. *)
    let tree_id_pl, _, _, _ = Cat.row_storage table_meta in
    Plan.Op_index_lookup
      { table_tree = tree_id_pl; idx_tree; keys; range; table_meta; alias }
;;

(* #508: pick an access path from already-recognised equalities.  [eqs] is the
   [(position, column ordinal, value)] triples the caller extracted — position
   identifies which conjunct an equality came from, so a repeated column pins the
   prefix once and leaves the rest to the caller.  [range_conjuncts] are the
   conjuncts a #517 range bound may be read out of; pass [[]] for a caller that
   has none to offer.

   Returns the chosen path with the conjunct positions it consumed.

   #565: gated on {!meta_is_btree_backed}.  This is the single chokepoint
   [plan_base], [choose_access_path] and [plan_dml_seek] all reach, so one guard
   at the entry covers every arm the function has or grows — cheaper than
   remembering to add one per arm.

   #594 corrects the reason #565 gave for the placement.  It said [Seek_rowid]
   was a second leak, reachable via [Cat.rowid_alias_col]; that arm cannot
   actually leak, because [rowid_alias_col] reads the meta's OWN columns rather
   than the catalog, so a CTE's synthesized meta answers [None] there.  The one
   real leak is the index branch: a CTE shadowing a real table picks up that
   table's indexes —
   [find_index_for_eqs] resolves them by name, and [Cat.register_ephemeral]
   replaces only the table entry — and [seek_op] builds an [Op_index_lookup]
   whose [table_tree] is the CTE's sentinel [tree_id = -1].  The result is
   silently empty.  #551 fixed the probe-side instance of exactly this and #531
   the build-side one; this is the third and last site. *)
let access_path_for_eqs cat (table_meta : Cat.table_meta) ~eqs ~range_conjuncts =
  if not (meta_is_btree_backed table_meta)
  then None
  else (
    let alias_eq =
      List.find_opt
        (fun (_, col_idx, _) -> Cat.rowid_alias_col table_meta = Some col_idx)
        eqs
    in
    match alias_eq with
    | Some (pos, _, lit_expr) ->
      (* #243 (T1): the alias column IS the table key — a single rowid seek, no
         index. *)
      Some (Plan.Seek_rowid (plan_expr lit_expr), [ pos ])
    | None ->
      if eqs = []
      then None
      else (
        match find_index_for_eqs cat table_meta eqs with
        | None -> None
        | Some (idx, prefix, consumed) ->
          let keys =
            List.map
              (fun (col_idx, v) ->
                 col_idx, (List.nth table_meta.Cat.columns col_idx).Row.ty, plan_expr v)
              prefix
          in
          let range =
            range_for_index table_meta idx ~n_eq:(List.length prefix) range_conjuncts
          in
          Some (Plan.Seek_index { idx_tree = idx.Cat.idx_tree_id; keys; range }, consumed)))
;;

(* #674 (3 of 3): the column-ordinal sequence a chosen access path is
   guaranteed to produce in ascending order, or [None] if it makes no such
   guarantee. [`All] means "every row is guaranteed sorted, trivially" (a
   single-row lookup); [`Cols ords] means "ascending on this ordinal
   sequence, in order."

   Only [Op_index_lookup] and [Op_rowid_lookup] are handled — those are the
   only two ops [seek_op] ever produces, and both are already guaranteed
   plain-column, non-partial by {!index_is_seekable} (see the comment above
   {!access_path_for_eqs}). *)
let natural_order cat (op : Plan.op) : [ `All | `Cols of int list ] option =
  match op with
  | Plan.Op_rowid_lookup _ -> Some `All
  | Plan.Op_index_lookup { idx_tree; keys; table_meta; _ } ->
    (match
       Cat.indexes_for_table cat ~table:table_meta.Cat.name
       |> List.find_opt (fun (i : Cat.index_info) -> i.Cat.idx_tree_id = idx_tree)
     with
     | None -> None (* defensive; every Op_index_lookup came from a real index *)
     | Some idx ->
       let prefix_len = List.length keys in
       let suffix_names = List.filteri (fun i _ -> i >= prefix_len) idx.Cat.idx_columns in
       (* [index_is_seekable] already guarantees every name resolves to a
          plain table column, so [Option.get] here is safe rather than a
          silent degrade. *)
       Some
         (`Cols (List.map (fun n -> Option.get (col_ordinal table_meta n)) suffix_names)))
  | _ -> None
;;

(* #674 (3 of 3): does [order]'s key sequence match a prefix of [natural]'s
   guarantee?  Every key must be a bare column reference (no expression), ASC
   and NULLS FIRST (explicit or defaulted) — see the design doc
   (docs/superpowers/specs/2026-08-08-674-3-sort-elision-design.md) for why
   both restrictions are load-bearing: DESC has no reverse index walk to
   elide onto, and NULLS LAST disagrees with the index's own NULL-sorts-first
   encoding.

   [table_meta] is the same value the chosen access path carries (and that
   [natural_order] resolved ordinals against); [key_ok] consults it to refuse
   a nullable REAL suffix column (see CLAUDE.md's #536/#578/#579 sections) — a
   NOT NULL REAL column has no NULL to worry about, and a non-REAL column has
   no NaN at all, so both remain eligible.

   #578 gave NaN its own index-key tag (sorting between NULL's and INTEGER's),
   so the index's natural walk now visits NULL entries, then NaN entries, then
   ordinary reals — exactly the order [NULLS FIRST] wants. The refusal
   predates that fix, when NULL and NaN shared one byte and so could interleave
   in an order the walk did not control. Left conservative deliberately: this
   function guards sort ELISION (dropping a real sort step outright), a higher
   bar than #578's own fix, and re-admitting nullable REAL here is an
   unverified optimisation, not part of #578's scope. *)
let order_satisfied_by_natural_order
      (table_meta : Cat.table_meta)
      natural
      (order : Sema.bound_order_key list)
  =
  let key_ok (bkey : Sema.bound_order_key) ord =
    match bkey.Sema.key with
    | Sema.BE_col c ->
      c = ord
      && bkey.Sema.dir = Ast.Asc
      && (bkey.Sema.nulls = None || bkey.Sema.nulls = Some `Nulls_first)
      &&
      let col = List.nth table_meta.Cat.columns ord in
      not (col.Row.ty = Row.Real && not col.Row.not_null)
    | _ -> false
  in
  match natural with
  | Some `All -> true
  | Some (`Cols ords) ->
    let n = List.length order in
    if n > List.length ords
    then false
    else List.for_all2 key_ok order (List.filteri (fun i _ -> i < n) ords)
  | None -> false
;;

(* ------------------------------------------------------------------ *)
(* #520: a crude static cardinality estimate for the join's driving side *)
(* ------------------------------------------------------------------ *)

(** #520: "we have no idea" — the estimate for any shape whose output cannot be
    bounded from the plan alone.  Deliberately larger than any threshold, so an
    unknown driving side always falls on the hash-join side of the choice. *)
let unbounded_rows = max_int

(** #520: the driving-side row count below which a nested-loop probe is taken
    unconditionally, without consulting the right table at all.

    Measured on the #500 W=1 TPC-C population, the StockLevel query
    ([order_line INNER JOIN stock], where [stock] holds 100,000 rows):

    {v
      driving side   nested-loop probe          hash join
      30,240 rows    60,480 examined   2757 ms   130,240 examined   1240 ms
          12 rows        24 examined    0.5 ms   100,012 examined    945 ms
    v}

    Note that rows examined FALL while wall-clock RISES: a B-tree seek costs far
    more than a hash-table lookup, so ~30,000 probes lose to a single
    100,000-row scan, while 12 probes beat it by ~2000x.  {b Rows examined is
    not the cost model.}

    {b The floor is slack, not a win condition.}  It is tempting to read it as
    "a handful of seeks cannot lose to a scan of anything", and that is wrong:
    1000 seeks is not a handful and it does lose to a small right table.
    Measured with the floor set to 0 — D = 1000 against R = 200 costs 2.65 ms
    probing and 2.10 ms hashing, against R = 20 it is 2.52 ms and 1.73 ms.  So
    the floor knowingly accepts up to ~1.5x in that corner.

    It buys two things that are worth more than that corner.  First, it absorbs
    {!range_seek_rows}' deliberate optimism: with the floor at 0 the only test
    that fails is the range-bounded StockLevel shape, whose nominal 100 loses
    the ratio against a 200-row right table even though the real window is 21
    rows.  The floor is the slack that covers a made-up number.  Second, it is
    the only thing that can fire when R is not knowable at all — a WITHOUT ROWID
    or columnar right table usually estimates as {!unbounded_rows}, the ratio
    can never hold, and without the floor such a join could never take a probe.
    (#528 put one crack in that "never": a WITHOUT ROWID right table whose whole
    unique key is pinned by the WHERE clause now seeks to a provable single row,
    so {!estimate_rows} answers 1 rather than {!unbounded_rows} and the ratio
    can hold after all.  A columnar right table has no seek and is still
    unknowable.)

    Those two jobs want different numbers (100 keeps every test green but
    strands the unknowable-R case), so 1000 is a single compromise between them
    — and it stays an order of magnitude above the largest measured win,
    StockLevel's post-#517 230-row driving side.  {b Do not "tune" it as though
    it were a break-even point; it is not one.} *)
let nlj_min_driving_rows = 1000

(** #520: how many right-table rows a single probe is worth.

    The floor above is not enough on its own, because the trade-off is a {b
    ratio}, not an absolute cut: a probe costs D seeks and a hash join costs
    D + R reads, so the same D is the wrong answer against a small R and the
    right one against a large one.  Review of PR #526 measured exactly that —
    1,200 driving rows against TPC-C's 100,000-row [stock] took 153.7 ms as a
    hash join and 4.3 ms as a probe, a 36x regression from a rule that looked at
    D alone.

    The measurements above put a probe at ~91 µs and a hashed right-table row at
    ~9.5 µs, so break-even is around [R / 9.6].  8 is the round number just
    below that, i.e. very slightly biased towards the probe at break-even, where
    by construction the two plans cost about the same and the bias cannot matter
    much either way.  It is a {b heuristic, not a calibrated model}: this engine
    has no ANALYZE, no stored table statistics and no selectivity estimates, so
    there is nothing to calibrate against, and both figures come from one
    workload on one machine.

    Checked against every measured point: 30,240 driving rows against R =
    100,000 gives a cut of 12,500 and takes the hash join, matching the
    measurement; 230 and 12 are under the floor and probe; 1,200 against R =
    100,000 probes, which is the regression this constant fixes. *)
let nlj_probe_cost_ratio = 8

(** #576 tier 3: {!nlj_probe_cost_ratio} for a build side that {b seeks}
    ({!build_side} returned [Op_index_lookup] or [Op_rowid_lookup]) rather
    than scans.

    #576 opened on the suspicion that 8 is simply the wrong number here: it
    was calibrated against a {i scanned} build side at ~9.5 µs/hashed row, and
    #546/#606 separately measured a build-side {i seek} at ~3 pager
    reads/row on the same 100,000-row table — described there as "the same
    order as a probe seek", which would put break-even near 1 rather than 8.

    {b That intuition is about the seek #575 declined, not the one #586
    admits, and the two cost very differently.} #546/#606's ~3 reads/row
    figure was measured with windows from 200 up to 9,999 keys — 2% to 100%
    of the table — which is exactly the band {!build_side_seek_is_unambiguous}
    now REFUSES ([window > table_rows_estimate / build_side_seek_break_even_ratio],
    i.e. > 0.5% of a 100,000-row table declines).  The window this ratio is
    ever consulted for is capped an order of magnitude smaller, and at that
    size a seek's marginal reads/row is nowhere near its large-window figure —
    small windows fit inside a handful of leaf pages, so most of a seek's
    per-entry descent is shared with its neighbours instead of paid fresh.

    Measured directly, [test/bench_nlj_probe_seek_cost_576.ml], 100,000 stock
    rows, cold pager reads, default [GRANARY_PAGE_CACHE]:

    {v
      probe (driving rows scattered across the whole table, matching the
      #520/#526 shape where the join column has no relation to key order):
        D        100    300    600   1,000
        reads     33     89    171     281      marginal 0.276 reads/row

      hash, build side seeking an UNAMBIGUOUS window (#586-admitted, si
      BETWEEN bound, D held fixed at 2,000):
        R         50    150    300     450
        reads     58     60     64      68      marginal 0.025 reads/row
    v}

    A probe costs {b ~11x} an admitted seek's marginal row here — the opposite
    direction from the issue's opening hypothesis, and the reconciliation is
    the point: #546/#606's figure describes the window #575/#586 REFUSE, not
    the one they admit.  Within what {!build_side_seek_is_unambiguous} lets
    through, an admitted seek is closer to a scan's ~0.02 reads/row than to a
    probe's, because the budget that makes it unambiguous also makes it small.

    {b This constant is set from that measurement, not copied from
    {!nlj_probe_cost_ratio}, and it is deliberately conservative rather than
    equal to the measured ~11}: {!build_side_seek_break_even_ratio} could move,
    and a build-side change could someday feed {!probe_is_worth_it} a seek
    outside today's tiny admitted band.  10 is the round number just below
    11 rather than 8, on the same "just below break-even, and the bias cannot
    matter much at break-even" reasoning {!nlj_probe_cost_ratio}'s doc gives.

    {b It changes no decision reachable today.} {!build_side_seek_is_unambiguous}
    caps an admitted [right_rows] at [table_rows_estimate / build_side_seek_break_even_ratio]
    (500 for a 100,000-row table); {!probe_is_worth_it} only reaches either
    ratio once [driving_rows] clears {!nlj_min_driving_rows} = 1000; and
    500 / 10 as well as 500 / 8 are both far below 1000. So for every
    [driving_rows] this function is consulted at, the hash join wins under
    EITHER constant — this is why #532's "row 4" residual (D = 5,000, W =
    20,000) is not an instance of this at all: that window is declined by
    #586 before reaching here, R collapses to [table_rows_estimate], and it is
    {!nlj_probe_cost_ratio} — the scanned constant — that answers it, exactly
    as the #586-review comment on #576 found.  A dedicated seeked-build
    constant is still worth having: it is the answer #576 asked for, it is
    correct where {!nlj_probe_cost_ratio} would not be if the admission budget
    ever widens, and [seeked_build_side_still_takes_the_hash_join] in
    [test/test_join_cost_model_576.ml] pins that today's answer does not move
    by asserting it under BOTH constants at once. *)
let nlj_probe_cost_ratio_seeked_build = 10

let range_seek_rows = 100

(** #532: how many rows a range-bounded seek is estimated to reach.

    {!range_seek_rows} on its own is a flat 100 with no relation to the span the
    range actually covers, and #528/#532 made that constant load-bearing: R is
    now estimated from the build side, and R decides the join strategy.

    Measured on disk, 100,000-row right table, all five rows run both ways by
    spelling the range so the planner cannot recognise it
    ([test/bench_build_side_strategy_532.ml], cold ms):

    {v
      window   driving   hash join   probe   what the flat 100 chose
          21     1,200         6 ms   25 ms   hash — right, 4.2x
       2,000     1,200        21 ms   23 ms   hash — right, 1.1x
      20,000     1,200       281 ms   31 ms   hash — WRONG, 9.0x slower
      20,000     5,000       205 ms  109 ms   hash — wrong, 1.9x slower
      20,000    12,000       242 ms  301 ms   hash — right, 1.2x
    v}

    One constant cannot be right for all five, and the row it is most wrong on
    costs 9x.  The span fixes rows 1-3 and 5.  {b Row 4 it does not fix}: the
    span says R = 20,000, [5,000 <= 20,000/8] fails and the hash join is chosen
    where the probe is ~1.9x better.  That is a real (bounded, ~2x) regression
    against the pre-#532 plan, and it is left standing deliberately rather than
    papered over with a fudge factor, because its cause is elsewhere:
    {!nlj_probe_cost_ratio} was calibrated in #520 against a build side that is
    SCANNED, at ~9.5 µs per hashed row.  A build side reached by an index seek
    costs ~3 pager resolutions per row (#546's measurement), which is the same
    order as a probe seek — so for a seeked build side the true break-even ratio
    is far below 8 and the model overvalues the hash join.  Fixing that means
    either #546's per-entry [rh_get] or a cost model that knows how the build
    side is read; both are bigger than this function.  See the comment on #546.

    So when both ends are integer literals, the number of keys they span is used
    instead.  Everything else — a bound parameter, a non-integer literal, a
    one-ended range — keeps the flat constant, so every plan this function cannot
    speak to is exactly the plan it was before.

    Two deliberate inaccuracies, both in the same direction:

    - a span counts distinct key VALUES, and a non-unique index may hold many
      rows per value, so this can under-state the row count;
    - the estimate never goes {i below} [range_seek_rows], so a genuinely tiny
      window still estimates 100.

    Under-stating R biases towards the hash join and under-stating D biases
    towards the probe, which is what the flat constant already did — this only
    stops it doing so by two orders of magnitude.  Callers cap the result with
    [table_rows_estimate], which is the only real number available.

    #575: the [Some lo, Some hi] arm is the {i only} one that reads anything off
    the query.  Every other shape — a one-ended range, a parameterised bound, a
    non-integer literal — falls to the flat [range_seek_rows], which is a made-up
    constant and not an estimate of anything.  {!range_int_literal_span} is that
    distinction named, so a caller can ask whether this function has something to
    say before believing what it says. *)
let range_int_literal_span (r : Plan.range) =
  let int_lit = function
    | Some (Plan.P_lit (Ast.L_int n)) -> Some n
    | _ -> None
  in
  match int_lit r.Plan.r_lo, int_lit r.Plan.r_hi with
  | Some lo, Some hi -> Some (lo, hi)
  | _ -> None
;;

(** #606: the {i unfloored} key count a both-ends-integer-literal range spans,
    or [None] when the window cannot be sized at all.

    {!range_rows_estimate} answers the same question for the {b cost model}, and
    deliberately never goes below {!range_seek_rows} = 100 — under-stating R
    biases the strategy choice towards the nested-loop probe, and the floor is
    the slack that covers a made-up constant (see {!nlj_min_driving_rows}).

    {b That floor is exactly wrong for a break-even test.}  #606's gate asks
    whether a window is below [table_rows / K] for a measured K in the hundreds,
    so on any table under [100 * K] rows a floored estimate answers "no" for
    every window including a one-key one, and the seek becomes unreachable.  The
    two questions want the same span and opposite treatment of the floor, so
    they are two functions over one {!range_int_literal_span} rather than one
    function with a flag.

    [Some 0] is an empty window (the caller's [hi < lo]); [None] is "the
    estimator has nothing to say", which is both a non-literal bound and an
    [hi - lo] that overflows int64. *)
let range_literal_window_rows (r : Plan.range) =
  match range_int_literal_span r with
  | None -> None
  | Some (lo, hi) ->
    if Int64.compare hi lo < 0
    then Some 0
    else (
      let span = Int64.sub hi lo in
      if Int64.compare span 0L < 0 || Int64.compare span (Int64.of_int max_int) >= 0
      then None
      else Some (Int64.to_int span + 1))
;;

(** #520: how many rows [meta]'s table can hold, from the only real number the
    catalog carries: a rowid table's [next_rowid] high-water mark, which is
    [max(rowid) + 1] over everything ever inserted.

    That makes it an upper bound on the live row count for the ordinary case
    (auto-allocated, non-negative rowids) and an over-estimate after deletes —
    over-estimating biases the choice towards the hash join, which is the safer
    direction: choosing hash wrongly costs a bounded multiple of the right
    table's size, while choosing nested-loop wrongly grows without limit in the
    driving side.  An explicitly inserted negative rowid would break the bound; that is
    accepted, since the consequence is only a strategy choice.

    WITHOUT ROWID and columnar tables carry no such counter and are unbounded. *)
let table_rows_estimate (meta : Cat.table_meta) =
  match meta.Cat.storage with
  | Cat.Columnar _ -> unbounded_rows
  | Cat.Row { without_rowid = true; _ } -> unbounded_rows
  | Cat.Row { next_rowid; _ } when Int64.equal next_rowid Cat.empty_next_rowid -> 0
  | Cat.Row { next_rowid; _ } ->
    if Int64.compare next_rowid (Int64.of_int unbounded_rows) >= 0
    then unbounded_rows
    else max 0 (Int64.to_int next_rowid - 1)
;;

(** #606/#546: how much smaller than the whole table a build-side window has to
    be before seeking it beats scanning the table.

    #575 shipped the range carve-out gated on
    [range_rows_estimate r < table_rows_estimate meta] — "the window is smaller
    than the table" — which admits everything from 0% to 99.999% selectivity.
    #606 is that residual: [si BETWEEN 0 AND 9998] over 10,000 rows passed the
    gate and cost 4.19x the pager reads of the scan it replaced, while
    [si BETWEEN 0 AND 1000000] — one literal further out — was declined.  The
    cliff sat at the table's row count, nowhere near break-even.

    {b The break-even is a ratio of two per-row costs, and both were measured
    directly} with [test/bench_build_side_seek_546.ml], 100% population,
    [B546_EXTRA='si BETWEEN 0 AND <hi>'], cold pager reads.  Reads are the
    load-bearing figure — they are what a MirageOS target pays and they are
    near-deterministic, reproducing {i to the digit} across hosts; the wall-clock
    column moves with whatever else the box is doing and is quoted only where it
    agrees.

    {v
      10,000 stock / 3,000 driving — scanned build side reads 6,299
        window      1     50    100    150    200    500   1,000  5,000   9,999
        S:reads  6103   6202   6304   6406   6508   7116   8132  16253   26401
        marginal    -   2.02   2.04   2.04   2.04   2.03   2.03   2.03    2.03

      100,000 stock / 30,000 driving — scanned build side reads 93,067
        window    200    500  1,000
        S:reads 91533  92441  93957      marginal 3.03 reads per entry
    v}

    The [window = 1] cell is the seek's own intercept, 6,103 — {i not} the
    scan's 6,299, which is a different plan and appears only in the header line.
    Confusing the two is the one substitution that would make the marginal column
    look non-linear, in a table whose whole authority is that it is not.

    Read those two blocks as a pair of straight lines and the constant falls out
    of them:

    - a {b seeked} entry costs 2.04 pager reads at 10,000 rows and 3.03 at
      100,000 — the per-entry descent from the index root, so it grows with tree
      depth, i.e. with log N;
    - a {b scanned} row costs 0.020 reads at 10,000 (2.04 x 97.5 read-parity
      window / 10,000) and 0.021 at 100,000 (3.03 x ~706 / 100,000) — one page
      per ~47 rows, flat in N as a sequential leaf walk must be.

    Read-parity — the window where the two lines cross — is therefore {b 1/103
    of the table at 10,000 rows and 1/141 at 100,000} (interpolating 93,067
    between the 500- and 1,000-key rows gives 707), and it tightens with N
    because only the seek's side of the ratio grows.  Extrapolating one more
    decade of tree depth puts 1,000,000 rows near 1/190.

    {b Reads and wall-clock cross over at very different windows, and the gate
    follows the reads deliberately.}  Measured on the same runs:

    {v
                        read parity   wall parity   gate cuts at
      10,000 rows            97          ~4,000          50
      100,000 rows          705       ~20,000-50,000    500
    v}

    Wall time agrees with the reads only at the far end — the 9,999-key window is
    2.66-2.77x slower than the scan it replaced, reproducing #546's 2.6-3.5x and
    #606's 2.63x — and disagrees across everything below it.  A 5,000-key window
    reads 2.58x more and {i is} 1.42x cold / 1.30x warm slower, so that one
    agrees too; the genuine disagreement runs from about 1/100 of the table down
    to about 1/2.5 of it, where a seeked build side is faster in wall time
    despite reading more, because a declined build side has to hash every row of
    the table.

    So {b the gate declines the entire disagreement band, and that costs up to
    ~2.2x in wall time on this harness under the plan it now picks}.  That is
    accepted knowingly, not overlooked.  The reason is that this bench is
    page-cache-warm: a "read" here is a [Wal_read] resolved from memory and costs
    almost nothing, so wall time is measuring CPU — the hashing — with the I/O
    term set to zero.  On any real block device the I/O term dominates and the
    ordering inverts, and a real block device is the MirageOS target this engine
    exists for.  Choosing the counter that survives that change of hardware is
    the point; do not "fix" the gate by re-tuning it against this file's wall
    clock.

    [200] is the round number just past that extrapolation.  The bias it carries
    is deliberate and asymmetric: over-stating the ratio {i declines} windows
    that would have won, costing the wall-clock band described below, while
    under-stating it {i admits} windows that cost up to the 4.19x of pager reads
    #606 measured.  #575's option B already made that trade once — decline the
    unmeasurable, keep only what is unambiguous — and this is the same trade with
    a measured number instead of a table-sized one.

    {b It stops being the conservative direction somewhere above ~1.1M rows.}
    True parity tightens as log N while the constant does not, so the two cross:
    at 10,000,000 rows parity is near 1/240 and the gate still admits up to
    1/200, i.e. it would then be admitting windows measurably on the losing side
    rather than declining ones on the winning side.  Nothing in the tree is near
    that scale, and the honest fix at that point is a ratio that grows with the
    index's depth rather than a larger constant — but a future reader raising
    this number to buy back the wall-clock band should know the error already
    changes sign in the other direction.

    {b This is not the statistic #576 is about and does not close it.}  It is a
    per-row cost ratio measured on one engine, not a selectivity estimate: for
    the range arm it can only be consulted where the window is {i literally}
    readable off the query and the index is unique, which is why
    {!build_side_seek_is_unambiguous} still asks {!index_is_unique} and
    {!range_literal_window_rows} first there.  A parameterised bound still has
    no window to compare and is still declined.  Since #576 tier 1 this ratio
    (via {!table_seek_budget}) is {i also} consulted in a fourth place —
    {!build_side_seek_is_unambiguous}'s bare-equality-prefix arm, where the
    index is explicitly {b non-unique} but carries an analyzed leading-column
    stat instead of a literal window.

    {b Correction to #546's published figure.}  That issue derived break-even
    "near 1/390" from a scanned-row cost of 0.008 reads — an estimate of one page
    per ~130 rows, never measured.  The direct measurement above is 0.021, one
    page per ~47 rows, on the same harness and the same populations, at two
    scales that agree.  1/390 is the tighter and therefore safe direction, but it
    is not what the engine does, and a threshold of 390 would decline a band this
    branch measures as a win. *)
let build_side_seek_break_even_ratio = 200

(** How many entries a seek over [meta]'s table may read before it stops being
    cheaper than a full scan, or [None] when the table's row count cannot be
    judged at all.

    {!dml_seek_bail_out_at} and {!build_side_seek_is_unambiguous} both need
    exactly this quantity — "how many rows is [1 / build_side_seek_break_even_ratio]
    of the table" — and used to compute it separately, once as a floor test
    written as its own negation ([rows < build_side_seek_break_even_ratio]) and
    once inline as part of a longer boolean chain. Sharing it means a future
    change to the ratio or to {!unbounded_rows}'s sentinel can't apply to only
    one of the two callers.

    [None] covers both the input {!table_rows_estimate} cannot size at all (a
    WITHOUT ROWID or columnar table, which answers {!unbounded_rows}) and a
    table too small for [1 / build_side_seek_break_even_ratio] of it to be
    worth even one entry: [Some 0] would bail a walk out on its very first
    candidate regardless of selectivity, which is wrong in the same direction
    as admitting an unbounded table would be wrong in the other. *)
let table_seek_budget (meta : Cat.table_meta) =
  let rows = table_rows_estimate meta in
  if rows >= unbounded_rows
  then None
  else (
    let budget = rows / build_side_seek_break_even_ratio in
    if budget = 0 then None else Some budget)
;;

(** #593: [meta]'s index living in tree [idx_tree], if it has one.

    The catalog is keyed by table name and answers a list, so every question
    about "the index this seek reads" starts by finding it.  Three callers ask
    different questions of the same answer — {!seek_is_unique_point},
    {!index_is_unique} and {!build_side_seek_is_unambiguous} — and the lookup is
    shared rather than open-coded three times, which is how the second of those
    call sites came to disagree with the first. *)
let index_by_tree cat (meta : Cat.table_meta) ~idx_tree =
  Cat.indexes_for_table cat ~table:meta.Cat.name
  |> List.find_opt (fun (i : Cat.index_info) -> i.Cat.idx_tree_id = idx_tree)
;;

(** #576 tier 2: the smallest index [i] into [boundaries] such that
    [boundaries.(i) >= key] (byte order) — [Array.length boundaries] if
    [key] is greater than every boundary. A standard lower-bound binary
    search; [boundaries] is assumed sorted ascending, which
    [Exec.build_histogram] guarantees. *)
let histogram_lower_bound (boundaries : string array) (key : string) =
  let n = Array.length boundaries in
  let rec go lo hi =
    if lo >= hi
    then lo
    else (
      let mid = lo + ((hi - lo) / 2) in
      if String.compare boundaries.(mid) key < 0 then go (mid + 1) hi else go lo mid)
  in
  go 0 n
;;

(** #576 tier 2 (corrected): estimate a literal [Integer]/[Real] range's row
    count from [idx_tree]'s histogram at column position [n_eq] -- the
    position [Plan.range]'s doc comment guarantees a range always describes
    (the first column after an [n_eq]-long equality-covered prefix; see
    docs/superpowers/specs/2026-08-11-576-tier2-per-position-histograms-design.md's
    "The bug" section for the full reachability proof). [None] when no
    histogram is available at that position (unanalyzed index, position
    out of range, capped, or the range has no literal bound to look up --
    a bound parameter, or a bound of any type other than [L_int]/[L_real];
    [Plan.range] only exists for [Integer]/[Real] columns in the first
    place). [None] on EITHER end falls back whole to
    {!range_int_literal_span}, same as before.

    A literal is coerced to [r.Plan.r_ty] -- the bounded column's OWN
    declared type -- before it is encoded into a probe key, because the AST
    literal's own syntactic spelling does not always match: an ordinary
    integer literal against a REAL column (`v BETWEEN 0 AND 199`, no decimal
    point) is exactly as common as one with a decimal point, but the
    histogram's boundaries were built in [r_ty], and [Index_key.encode_value]
    tags [IK_int] and [IK_real] with different leading bytes, so comparing
    across the mismatch silently defeats the lookup rather than raising.

    Deliberately BUCKET-GRANULARITY, not linear interpolation -- see the
    original tier-2 design doc's "Consumption" section, unchanged by this
    fix; only WHICH histogram is looked up changed.

    MARGINAL, not CONDITIONAL -- read this before wiring the result into a
    new consumer. [range_histograms.(n_eq)] is built UNCONDITIONALLY over
    the whole table by {!Exec.execute_create_index}'s walk: it is an
    equi-depth histogram of that column's values across every row of the
    table, with no knowledge of any equality prefix. The caller's actual
    question, though, is conditional -- "how many rows match BOTH the
    [n_eq]-long equality prefix AND this range" -- and the histogram alone
    cannot answer it. The result is a systematic OVER-estimate, by roughly
    the equality prefix's own selectivity factor: for `w = 1 AND v BETWEEN
    ...` where [w] has 1000 distinct values, the histogram-based estimate
    for [v]'s range is the row count across ALL 1000 values of [w], i.e.
    ~1000x the true row count for [w = 1] specifically.

    Three things worth being explicit about:
    (a) this is not a new blind spot -- the pre-existing flat
        {!range_int_literal_span} fallback this replaces had the identical
        one: it also estimated a range's span with no knowledge of any
        equality prefix;
    (b) the error direction is the SAFE one -- an over-estimate biases the
        caller toward the hash join, and {!table_rows_estimate}'s own doc
        comment already argues that is the direction to err in when unsure;
    (c) this is DELIBERATELY not corrected by combining with the equality
        prefix's own {!index_leading_distinct_count} (dividing the estimate
        by that count would assume the range's rows are spread evenly
        across the prefix's distinct values -- an independence assumption,
        not a fact this histogram or that stat encodes). That combination
        is a plausible future improvement, not something silently missing
        today -- name it as such if you touch this function, rather than
        treating the current number as more exact than it is. This matters
        because a future reader wiring this estimate into another consumer
        (e.g. a build-side gate for {!range_literal_window_rows}, per #606)
        could otherwise be misled by how "real" the number looks now that
        it comes from stored per-value data instead of a flat constant. *)
let range_histogram_estimate cat (meta : Cat.table_meta) ~idx_tree ~n_eq (r : Plan.range) =
  match index_by_tree cat meta ~idx_tree with
  | Some { Cat.idx_stats = Some { Cat.range_histograms; rows_at_analysis; _ }; _ }
    when n_eq >= 0 && n_eq < Array.length range_histograms ->
    (match range_histograms.(n_eq) with
     | Some { Cat.boundaries } when Array.length boundaries >= 2 ->
       let lit_key = function
         | Some (Plan.P_lit (Ast.L_int n)) ->
           (match r.Plan.r_ty with
            | Granary_encoding.Row.Real ->
              (* The histogram's boundaries were built in the column's OWN
                 declared type. An ordinary integer literal against a REAL
                 column (`v BETWEEN 0 AND 199`) must be coerced to [IK_real]
                 before lookup -- [IK_int] and [IK_real] are different tag
                 bytes ([0x02] vs [0x03] in {!Index_key.encode_value}), so an
                 un-coerced [IK_int] key sorts below EVERY [IK_real] boundary
                 regardless of its numeric value, always landing at bucket 0. *)
              Some
                (Bytes.to_string
                   (Index_key.encode_value (Index_key.IK_real (Int64.to_float n))))
            | _ -> Some (Bytes.to_string (Index_key.encode_value (Index_key.IK_int n))))
         | Some (Plan.P_lit (Ast.L_real f)) ->
           (match r.Plan.r_ty with
            | Granary_encoding.Row.Integer ->
              (* The rarer direction (`WHERE intcol BETWEEN 1.5 AND 10.5`):
                 round to the nearest int. This feeds a bucket-granularity
                 estimate, not a scan boundary, so a simple round is enough --
                 unlike {!Exec.range_bound_key}'s careful ceil/floor/pred/succ
                 handling of the same cross-type problem for an actual seek
                 bound, which this function does not need. *)
              Some
                (Bytes.to_string
                   (Index_key.encode_value
                      (Index_key.IK_int (Int64.of_float (Float.round f)))))
            | _ -> Some (Bytes.to_string (Index_key.encode_value (Index_key.IK_real f))))
         | None ->
           None (* unbounded end: use the histogram's own extreme, handled below *)
         | Some _ ->
           None (* a parameter, or any other expr shape: no literal to look up *)
       in
       let n_buckets = Array.length boundaries - 1 in
       let lo_i =
         match r.Plan.r_lo with
         | None -> Some 0
         | Some _ as e ->
           (match lit_key e with
            | Some k -> Some (histogram_lower_bound boundaries k)
            | None -> None)
       in
       let hi_i =
         match r.Plan.r_hi with
         | None -> Some n_buckets
         | Some _ as e ->
           (match lit_key e with
            | Some k -> Some (histogram_lower_bound boundaries k)
            | None -> None)
       in
       (match lo_i, hi_i with
        | Some lo_i, Some hi_i
          when Option.is_some r.Plan.r_lo || Option.is_some r.Plan.r_hi ->
          (* Clamp to [n_buckets]: [hi_i] can be [Array.length boundaries]
             (one past the last valid bucket index) when the upper bound is
             unbounded or past the histogram's own max, which with [lo_i = 0]
             would otherwise give [span_buckets = n_buckets + 1] -- one more
             bucket-width than actually exists, and enough to push the
             product above [rows_at_analysis]. *)
          let span_buckets = min n_buckets (max 0 (hi_i - lo_i)) in
          Some (max range_seek_rows (rows_at_analysis * span_buckets / n_buckets))
        | _ -> None)
     | _ -> None)
  | _ -> None
;;

(** #576 tier 2 (corrected): [~n_eq] is the equality-prefix length -- the
    column position [r] describes (see {!range_histogram_estimate}'s doc
    comment). [cat]/[meta]/[idx_tree] identify the seeked index, exactly as
    {!estimate_rows_from_stats} already does. *)
let range_rows_estimate cat (meta : Cat.table_meta) ~idx_tree ~n_eq (r : Plan.range) =
  match range_histogram_estimate cat meta ~idx_tree ~n_eq r with
  | Some est -> est
  | None ->
    (match range_int_literal_span r with
     | Some (lo, hi) ->
       let span = Int64.sub hi lo in
       (* [hi < lo] is an empty window; a [span] that came out negative for the
          other reason — [hi - lo] overflowing int64 — is as unbounded as a
          range gets.  Both are handled by the sign test, in the direction each
          wants. *)
       if Int64.compare span 0L < 0
       then if Int64.compare hi lo < 0 then range_seek_rows else unbounded_rows
       else if Int64.compare span (Int64.of_int unbounded_rows) >= 0
       then unbounded_rows
       else max range_seek_rows (Int64.to_int span + 1)
     | None -> range_seek_rows)
;;

(** #576 tier 1: [idx_tree]'s leading-column distinct-value count, if the
    index was analyzed at [CREATE INDEX] time and the count is positive.
    [None] covers "never analyzed" (every index created before this shipped,
    every UNIQUE index, WITHOUT ROWID/columnar/expression indexes — see
    [Exec.execute_create_index]) uniformly with "analyzed but somehow zero" --
    the latter cannot happen for a table with at least one row, but a zero
    denominator must never reach the division in {!estimate_rows_from_stats}. *)
let index_leading_distinct_count cat (meta : Cat.table_meta) ~idx_tree =
  match index_by_tree cat meta ~idx_tree with
  | None -> None
  | Some i ->
    (match i.Cat.idx_stats with
     | Some stats when stats.Cat.distinct_count > 0 -> Some stats.Cat.distinct_count
     | _ -> None)
;;

(** #576 tier 1: estimate a non-unique equality-prefix seek's row count from
    [idx_tree]'s analyzed leading-column cardinality, or [None] when no usable
    stat exists (today's exact behavior applies unchanged in that case).
    [table_rows_estimate meta] uses the CURRENT row count; [distinct_count] is
    from analysis time -- see the design doc's "Consumption" section for why
    mixing the two is the right call. Shared by {!estimate_rows} and
    {!build_side_seek_is_unambiguous} (Task 5) so the two questions -- "how
    many rows" and "is that seek worth taking" -- never answer from different
    numbers. *)
let estimate_rows_from_stats cat (meta : Cat.table_meta) ~idx_tree =
  match index_leading_distinct_count cat meta ~idx_tree with
  | None -> None
  | Some distinct_count ->
    let total = table_rows_estimate meta in
    Some (min (total / distinct_count) total)
;;

(** #575: is the index [idx_tree] holds a UNIQUE one?

    Weaker than {!seek_is_unique_point} on purpose, and the two are not
    interchangeable — see that function's [all_not_null] discussion.  This is the
    precondition for reading a {!range_rows_estimate} as a row count at all: that
    estimate counts distinct KEY VALUES, so comparing it against
    {!table_rows_estimate}'s ROW count is only meaningful when one key value is
    one row. *)
let index_is_unique cat (meta : Cat.table_meta) ~idx_tree =
  match index_by_tree cat meta ~idx_tree with
  | Some i -> i.Cat.idx_unique
  | None -> false
;;

(** #550: how many index entries a DML (UPDATE/DELETE) seek over [idx_tree] may
    walk before abandoning the seek for a full table scan, or [None] to walk
    unconditionally.

    A UNIQUE index needs no guard: [test_bounded_drain_514.ml] already pins an
    always-seek, buffer-then-fetch contract for a UNIQUE index's worst case (a
    strict prefix matching half the table), and #541 measured that contract as
    the right one there — one extra key column narrows the WORST case to "one
    row per distinct value of the pinned prefix," which is bounded by
    definition. A NON-UNIQUE index carries no such bound: its leading columns
    can match an unbounded fraction of the table, which is exactly the
    [WHERE tenant_id = 1] shape #550 is about, and the planner has no
    per-index cardinality statistic (#576) to size that fraction ahead of
    time. So a non-unique prefix gets a RUNTIME budget instead of a plan-time
    answer: {!build_side_seek_break_even_ratio} is the same per-entry seek
    cost #546/#606 measured for the hash join's build side, reused here
    because the DML drain pays the identical cost (one B-tree descent to fetch
    the table row behind each index entry) — only consulted at run time
    rather than plan time, because an equality prefix has no window a literal
    range does.

    {!table_seek_budget} answers [None] both for a table {!table_rows_estimate}
    cannot size at all (a WITHOUT ROWID or columnar table) — dividing an
    unknown row count by the ratio would produce a budget with no basis, and
    "decline what cannot be judged" is the same call
    {!build_side_seek_is_unambiguous} makes for the same input — and for a
    table under {!build_side_seek_break_even_ratio} rows, where the division
    floors to 0 and would bail a walk out on its very first candidate
    regardless of how selective the prefix actually is —
    [test_composite_seek_508.ml]'s [dml_seeks_through_a_secondary_index] is a
    60-row table seeking a full [(a, b)] equality prefix through a genuinely
    non-unique secondary index, and it must still seek. A table this small has
    no meaningful "1/200 of it" greater than zero, and its absolute cost is
    trivial either way (#546's own measurements start at 10,000 rows), so there
    is nothing here for the guard to protect against.

    {b This is called at EXECUTION time, once per execution, and [meta] must be
    a freshly read [table_meta], never one carried across executions of a
    prepared statement.} The sole caller is [Exec.seek_index_candidates],
    which re-reads [meta] from the catalog
    ([Granary_catalog.Catalog.find_table_cached]) on every call rather than
    reusing whatever [table_meta] the plan itself carries.  That distinction is
    the whole fix for a staleness bug this function used to have: an earlier
    revision called this at PLAN time ({!plan_dml_seek}) and stamped its answer
    onto [Plan.seek] as a [bail_out_at] field, and [Plan.op] is cached and
    reused for the life of a prepared statement ([Db.prepare]'s [stmt.plan];
    [Db.run]/[Db.iter] never re-plan). A budget baked in once, from whatever the
    table's row count was at PREPARE time, would then silently keep answering
    for that same count forever, however large the table grew afterwards — a
    statement prepared while the table was small (or below this function's own
    floor, giving no guard at all) reproduces #550's exact O(n) pessimization
    for the remaining life of the prepared statement. Calling this fresh on
    every execution, from [idx_tree] (stable across a statement's life; a
    dropped/recreated index is a separate, pre-existing DDL-visibility
    limitation — see [Plan.seek]'s doc) and a live [table_meta], is what keeps
    the guard tracking the table rather than a snapshot of it. *)
let dml_seek_bail_out_at cat (meta : Cat.table_meta) ~idx_tree =
  if index_is_unique cat meta ~idx_tree then None else table_seek_budget meta
;;

(** #575: is every column of [idx_tree]'s UNIQUE index pinned by [keys]?

    {!seek_is_unique_point} without its [all_not_null] test, which is deliberate
    and is the whole reason this exists separately — see that function. *)
let index_full_unique_pin cat (meta : Cat.table_meta) ~idx_tree ~keys =
  match index_by_tree cat meta ~idx_tree with
  | Some i -> i.Cat.idx_unique && List.length i.Cat.idx_columns = List.length keys
  | None -> false
;;

(** #520: does [idx_tree] belong to a UNIQUE index of [meta] whose every column
    is pinned by [keys]?  Such a seek reaches at most one row.

    A UNIQUE index permits any number of NULLs, so a full-key seek whose key
    includes one can reach many index entries — hence the [not_null] check,
    which keeps the estimate from answering 1 in the direction this module's
    bias argues against.

    #526 had to exempt the implicit PRIMARY KEY index from that check, because
    [Sema.mark_table_pk] marked only a table-level [PRIMARY KEY] naming a SINGLE
    column: both columns of [PRIMARY KEY (w, o)] carried [primary_key = false]
    and hence [not_null = false], so the composite-PK point lookup — precisely
    the #508/#516 shape this series exists for — fell out of the one-row class
    and lost its probe (201 rows examined against 2). #530 fixed the marking at
    its source and the exemption is gone: the columns of a table-level PRIMARY
    KEY are now DECLARED NOT NULL, so the PK index satisfies [all_not_null] on
    its own merits. #533 made [Catalog.open_] re-derive those flags from the
    implicit PK index on load, so a file written before #530 — whose stored
    flags are all false — reaches the one-row class too, instead of silently
    reverting the #513 StockLevel win on every pre-existing database.

    {b [not_null] is a declaration, not an invariant.} #567 added encode-time
    enforcement, so no write this engine performs can land a NULL in a column
    declared NOT NULL any more — but a file written before #530 can already
    hold one, and #533's re-derivation is what makes its schema claim
    otherwise ([PRAGMA not_null_check] reports those rows, #563). The estimate
    survives that for the same reason the check is coarse in the first place:
    see below.

    The residual exposure on a [`User] or [`Implicit_unique] index is smaller
    than it looks, because a key is only ever pinned by [col = value] and that
    comparison is never true for NULL: a seek whose key is NULL returns no rows
    at all, so the under-estimate can only mis-cost a query that was going to be
    empty.  The check is kept there anyway — it is free, and "empty result" is
    not the same as "cheap". *)
let seek_is_unique_point cat (meta : Cat.table_meta) ~idx_tree ~keys =
  let all_not_null =
    List.for_all
      (fun (col_idx, _, _) -> (List.nth meta.Cat.columns col_idx).Row.not_null)
      keys
  in
  index_full_unique_pin cat meta ~idx_tree ~keys && all_not_null
;;

(** #520: estimate how many rows [op] produces, statically.

    Two callers reach here, and they see different shapes.

    As the {i driving}-side estimate, only three shapes are possible:
    [chain_joins] hands {!plan_join} either the base access path — which
    [plan_base] builds as a bare scan or seek when the query has joins, never
    wrapped in a filter — or a previous join.

    #528 added the second caller: the {i build}-side estimate, whose op comes
    from {!build_side}.  That adds the synthesized and columnar scans
    {!make_scan} can produce — [Op_cte_scan], [Op_sqlite_master],
    [Op_sqlite_sequence], [Op_col_seq_scan] — all of which fall to the
    [unbounded_rows] arm.  That is the same answer {!table_rows_estimate} gives
    for a columnar table, and a pessimistic one for the three synthesized
    shapes; it is inert because {!best_probe} answers [None] for all of them, so
    the strategy lands on the hash join whatever [right_rows] says.  That was
    once argued as "finds no index on them", which was false for a CTE shadowing
    a real table — {!best_probe} reaches the catalog by name and found the
    shadowed table's indexes (#551).  It now carries the {!meta_is_btree_backed}
    guard, so the claim holds by construction rather than by luck.

    Every other op answers {!unbounded_rows} rather than adding an arm no test
    can reach: an [Op_aggregate] with no GROUP BY is exactly one row and an
    [Op_limit] is capped by its limit, both of which would be easy to classify
    and neither of which can occur below a join today.

    A join's output is {!unbounded_rows} for a different reason — without
    selectivity estimates its fan-out is genuinely unknown, and the asymmetry of
    the two errors (see {!table_rows_estimate}) says to answer "large" when in
    doubt.  The practical effect is that only the first join of a chain can take
    a probe. *)
let estimate_rows cat (op : Plan.op) =
  match op with
  | Plan.Op_rowid_lookup _ -> 1
  | Plan.Op_index_lookup { idx_tree; keys; range; table_meta; _ } ->
    let seek =
      if seek_is_unique_point cat table_meta ~idx_tree ~keys
      then 1
      else (
        match range with
        | Some r ->
          range_rows_estimate cat table_meta ~idx_tree ~n_eq:(List.length keys) r
        | None ->
          (* #576 tier 1: a non-unique equality prefix with no range used to
             be pure unbounded_rows; now it consults the leading column's
             analyzed distinct-value count when one exists. *)
          (match estimate_rows_from_stats cat table_meta ~idx_tree with
           | Some est -> est
           | None -> unbounded_rows))
    in
    min seek (table_rows_estimate table_meta)
  | Plan.Op_seq_scan { table_meta; _ } -> table_rows_estimate table_meta
  | _ -> unbounded_rows
;;

(** #575: is [seek]'s reach something the planner can actually see?

    The whole of {!build_side}'s guard, factored out so its cases can be read
    next to each other.  Three qualify:

    - [Seek_rowid] — the rowid alias is the table key, so this addresses exactly
      one row.
    - [Seek_index] on a UNIQUE index with {i every} key column pinned — one
      entry, one row.  A strict prefix of a unique index does {b not} qualify:
      that is precisely the shape [bench_build_side_seek_546] measured at
      2.6-3.5x slower when it selects the whole table, and nothing in the catalog
      distinguishes that from the 1% case (#576).  Nor does a full pin of a
      non-unique index, which reaches an unbounded run of duplicates.

      This asks {!index_full_unique_pin}, which is {!seek_is_unique_point} {i
      without} its [all_not_null] test, and the omission is deliberate (#593).
      Folding this arm into [seek_is_unique_point] outright — as an intermediate
      version did — imports a check calibrated for a different question and
      declines a full-key pin of a UNIQUE index over NULLABLE columns, turning a
      one-[rh_get] point seek into a full table scan.  That is an ordinary
      schema ([CREATE UNIQUE INDEX ix ON t (a, b)]) and it was a regression
      against pre-#575 behaviour, invisible to every test in
      [test_hash_join_build_seek_528.ml] because they all pin a PRIMARY KEY,
      whose columns are implicitly NOT NULL since #530.
      [nullable_unique_key_pin_still_seeks] is the case that now covers it.

      Why the omission is right: [seek_is_unique_point]'s own doc says
      [all_not_null] is there to stop the {i cardinality estimate} answering 1
      anti-conservatively, and explains in the same breath that reachability does
      not need it — a key is only ever pinned by [col = value], never true of
      NULL, so a NULL-keyed seek returns no rows.  NULLs cannot make a seek reach
      {i more} than one row, and "at most one row" is the only thing this gate
      asks.  The two functions share {!index_by_tree} so they cannot drift on the
      part they do agree about.
    - [Seek_index] carrying a #532 range bound {b whose window the estimator can
      read and puts below the measured break-even} (#606).  A range is a strict
      prefix, so
      it needs an argument the bare prefix does not have, and that argument is
      the estimate: #546's table measures the {i open-ended} prefix walk, and a
      range that really stops short of the end of the prefix is a different
      access path — measured separately on disk by
      [test/bench_build_side_strategy_532.ml] at 4.2x faster for a 21-row window.

      {b Both halves of that have to be tested, and the first version of this
      function tested neither.}  It admitted any [range = Some _], on the
      strength of the sentence above rather than of anything the code checked:

      - "a range stops that walk at a bound" is false for a {b one-ended} range.
        {!range_for_index} answers [Some] when {i either} end is present, so
        [sw = 1 AND si >= 0] yields [{r_lo = Some 0; r_hi = None}] and walks from
        the start of the pinned prefix to the end of it.  That is #546's
        open-ended walk exactly, and it was measured on #546's own 100,000-row
        population at {b 3.6x slower} than the scan (1.00-1.06 s against 0.28 s)
        — worse than the 2.6-3.5x #575 exists to remove, reachable by appending a
        tautology to a WHERE clause.
      - "it has an estimate the planner can consult" is false for {b anything but
        two integer literals}.  {!range_rows_estimate} answers the flat
        {!range_seek_rows} for a one-ended or parameterised bound, which is a
        made-up constant; consulting it there is consulting nothing.

      So the gate asks three things, and all three are load-bearing:

      + {!index_is_unique} — {b without it the other two are incommensurable}.
        {!range_rows_estimate} counts distinct KEY VALUES; {!table_rows_estimate}
        counts ROWS.  Comparing them means something only when one value is one
        row.  On a non-unique [INDEX (sw, cat)] with [cat] taking ten values
        across 500 rows per warehouse, [cat BETWEEN 0 AND 9] estimates ten and
        reaches five hundred — measured on disk at 100,000 rows as {b 4.2x}
        slower than the scan.  An intermediate version of this gate asked
        [seek_is_unique_point] here instead, which is {i dead code} in this arm:
        a range exists only when [range_for_index] found an index column at
        position [n_eq], i.e. [length idx_columns > length keys], and
        [seek_is_unique_point] requires them equal.  It read as a uniqueness
        check and was not one.
      + [length idx_columns = length keys + 1] — whether the bounded column is
        the {b last} one in the index.  {b Without it the window is not a bound
        on anything.}  {!range_literal_window_rows} counts distinct values of the
        BOUNDED column; the gate below compares that against a count of ROWS, and
        the two are commensurable only when one bounded value is one entry.
        {!index_is_unique} makes the {i whole} key unique, not the prefix through
        position [n_eq], and {!range_for_index} places the range at index column
        [n_eq] with nothing requiring it to be last — so on a three-or-more-column
        key every column {i after} the bounded one multiplies the entries a
        window reaches, invisibly to the estimate.  Measured:

        {v
          stock (sw, si, sub, qty)  PRIMARY KEY (sw, si, sub)
          4,000 rows, si 1..20, sub 1..200
          ... JOIN stock ON sub = i_id WHERE w = 1 AND sw = 1
                        AND si BETWEEN 1 AND 20

          gate budget = 4000/200 = 20 ; window = 20  ->  ADMITTED
            seek : rows_examined 5100   index_entries 5100   (all 4,000 entries)
            foil : rows_examined 5100   index_entries 1100
        v}

        A window the estimate sized at 20 reached the entire table — 200x, 100%
        selectivity, exactly what #606 was filed to stop, and [rows_examined]
        reports the same 5,100 either way, which is #546's whole point.  The
        shape is not exotic: TPC-C's [order_line] key is
        [(ol_w_id, ol_d_id, ol_o_id, ol_number)].

        Requiring the bounded column to be last is what turns the {i assumption}
        "entries per bounded value = 1" into a checked fact, and with it the
        "#576 is untouched" argument below becomes a consequence rather than a
        claim.  It costs nothing on any shape in the tree, because a range is
        only ever read off the column after the pinned prefix and every existing
        case has that column last.
      + [r_ty = Row.Integer] — whether the bounded column is an INTEGER.  {b The
        window is a count of integers; only an integer column makes that a count
        of entries.}  {!range_int_literal_span} inspects the two BOUNDS and never
        consults [r_ty], while {!bounded_type} admits [Row.Real] — so on a REAL
        last column the estimate counts the integers in [lo, hi] and the seek
        walks the distinct REALS in it, which is unbounded.  Measured:

        {v
          stock (sw INTEGER, sr REAL, qty INTEGER)  PRIMARY KEY (sw, sr)
          4,000 rows, sr spread strictly inside (0,1)
          ... WHERE sw = 1 AND sr BETWEEN 0 AND 1

          budget = 4000/200 = 20 ; window computed = 2  ->  ADMITTED (pre-#606 B2)
            seek : rows_examined 5200   index_entries 5200   (all 4,000 entries)
            foil : rows_examined 5200   index_entries 1200
        v}

        2,000x the budget at 100% selectivity — #606's headline pathology and
        #546's counter split, verbatim, one {i type} away from the position
        premise above.  A REAL trailing key column is the canonical time-series
        shape ([PRIMARY KEY (sensor, ts)], [ts BETWEEN <day> AND <day+1>]), not a
        corner.

        {b This restricts one decision, not the feature.}  This function gates
        {!build_side} alone — the hash join's build side.  A REAL range bound
        still narrows a base scan, a DML seek and (since #570) a nested-loop
        probe, none of which route through here, because none of them replaces a
        sequential scan of the whole table and so none of them needs the window
        to bound anything.  What is declined is the {i build-side seek} on a REAL
        column, and only because that is the one place a mis-sized window costs
        more than it saves.

        {b With this conjunct the bound stops being an assumption and becomes a
        theorem.}  [Index_key.encode_value] tags NULL [0x00], NaN [0x01] (#578),
        integers [0x02] and reals [0x03], so NULL and NaN entries both sort
        strictly below any integer lower bound and cannot inflate the walk; a
        unique full key means
        one key value is one entry; the bounded column being last means no
        further column multiplies it; and an integer column means the window's
        unit and the walk's unit are the same.  Take away any one of the four and
        the comparison is between two different quantities again.
      + {!range_literal_window_rows} — whether the estimator can read the window
        at all, rather than falling back to its flat constant.
      + [window * build_side_seek_break_even_ratio <= table_rows_estimate meta] —
        whether the window is below the {b measured break-even}.

      {2 #606: the third conjunct used to be "smaller than the table"}

      It read [range_rows_estimate r < table_rows_estimate meta], and that
      declined only windows numerically {i larger} than the whole table, so
      0%-99.999% selectivity still seeked.  [si BETWEEN 0 AND 9998] over 10,000
      rows passed it and cost 4.19x the scan's pager reads, while
      [si BETWEEN 0 AND 1000000] — one literal further out — was declined: the
      cliff sat at the row count, not at break-even.  #606 recorded that residual
      and expected closing it to need #576's per-index cardinality statistic.

      It does not, because {b this particular question is not a selectivity
      question}.  The window's size is already known exactly — it is read off two
      integer literals — so what is missing is not a distribution but the price
      of a seeked entry against a scanned row, and that is a property of the
      engine's own read path, measurable to two significant figures without any
      stored statistic.  {!build_side_seek_break_even_ratio} carries the
      measurement (two scales, agreeing) and the reason 200 rather than #546's
      published 390.

      That distinction is the whole reason this is not the fudge #576 forbids: a
      constant standing in for a distribution the engine cannot see is a fudge; a
      constant measured directly, twice, over the quantity it actually names is a
      calibration.  A parameterised bound and a non-integer literal still have no
      distribution to consult and are still declined, unchanged.  A non-unique
      index's prefix is {b no longer} declined unconditionally: since #576
      tier 1 (see {!build_side_seek_is_unambiguous}) it is admitted when the
      leading column carries an analyzed distinct-value count whose resulting
      estimate fits within {!table_seek_budget}, and declined only when the
      index is unanalyzed or the estimate exceeds budget.

      Two edges the ratio brings with it:

      - [table_rows_estimate] answers {!unbounded_rows} for a WITHOUT ROWID or
        columnar table, i.e. "no row count".  Dividing that by the ratio would
        admit any window at all on exactly the tables whose size is unknown, so
        an unbounded row count declines instead — the same "decline what cannot
        be judged" that #575 settled on, applied to the other operand.
      - the comparison is written as a division rather than a multiplication
        because [window * 200] overflows for a window near [max_int], and it
        would overflow towards {i admitting}.

      A justification naming a property, attached to a gate that does not test
      for it, is the fudge-factor wedge #561 and #576 warn about.  This function
      has now been wrong that way {b five} times:

      + admitting any range at all;
      + appearing to check uniqueness through a call that could never be true;
      + calling a table-sized threshold a break-even;
      + sizing a window in key VALUES while comparing it against ROWS — the
        {i position} premise, unstated;
      + and doing the same one type over — the {i type} premise, unstated, which
        survived the fix for the fourth because that fix argued position at
        length and left type implicit.

      So each conjunct above now names the measurement that would fire without
      it, and the four premises are listed together where the theorem is stated,
      rather than being discoverable one defect at a time.

      The last two are the instructive ones.  In both cases the sentence that
      predicted the defect was already in this comment when it shipped past —
      {!index_is_unique}'s own doc states the invariant the fourth broke — and in
      both cases the fix for one premise read as a fix for the class.  {b Reading
      a warning is not testing for the thing it warns about, and closing one
      instance of a class is not closing the class.}

      {b #576 tier 1 adds a fifth admission below (the [Seek_index] arm with
      [range = None] on a non-unique index), and it rests on a premise of its
      own that must be named here rather than left to be found the same way the
      other five were.}  {!estimate_rows_from_stats} computes
      [table_rows_estimate / distinct_count] — the {i mean} rows per leading-
      column value — which assumes the column's values are roughly uniformly
      distributed under the pinned prefix.  A skewed column breaks that
      silently and in the {i admitting} direction: 500 distinct values across
      100,000 rows estimates 200 (well inside a 500-row budget) whether the
      values are even or whether one value alone holds 40,000 of them, and the
      seek that admission takes then reads 40,000 entries — 80x over budget,
      exactly the #575/#546 regression class this whole function exists to
      keep out.  This is accepted as tier-1 scope, not fixed here: a per-column
      histogram (tier 2) is what would size a skewed column correctly, and
      {!Cat.idx_stats}'s [rows_at_analysis] does not help in the meantime — it
      records headcount at analysis time, not shape, so it cannot distinguish a
      uniform column from a skewed one. *)
let build_side_seek_is_unambiguous cat (meta : Cat.table_meta) = function
  | Plan.Seek_rowid _ -> true
  | Plan.Seek_index { idx_tree; keys; range = None } ->
    index_full_unique_pin cat meta ~idx_tree ~keys
    ||
    (* #576 tier 1: a non-unique equality prefix used to be declined
       unconditionally here. Admit it when the leading column's analyzed
       distinct-value count puts the estimated row count within
       table_seek_budget -- the same budget dml_seek_bail_out_at consults.

       Premise this arm rests on, named per this function's own doc-block
       convention (see its "wrong that way five times" paragraph above):
       estimate_rows_from_stats assumes the leading column's values are
       roughly UNIFORMLY distributed, since it divides the table's row count
       by the distinct-value count to get a mean. A skewed column can make
       this estimate arbitrarily wrong in the ADMITTING direction -- few
       distinct values with one value holding most of the rows still looks
       small on average. Tier 2 (per-column histograms) is what would fix
       this; rows_at_analysis does not help, since it records headcount at
       analysis time, not shape. *)
    (match estimate_rows_from_stats cat meta ~idx_tree with
      | None -> false
      | Some est ->
        (match table_seek_budget meta with
         | None -> false
         | Some budget -> est <= budget))
  | Plan.Seek_index { idx_tree; keys; range = Some r } ->
    index_is_unique cat meta ~idx_tree
    && (match index_by_tree cat meta ~idx_tree with
        (* [None] is unreachable: [index_is_unique] just called [index_by_tree]
           with these arguments and answered [false] on [None], so the [&&] has
           already short-circuited.  Kept because the alternative is an
           [assert false] or a partial match on a lookup that is fallible by
           type; do not write a test for this arm, there is no query that
           reaches it. *)
        | Some i -> List.length i.Cat.idx_columns = List.length keys + 1
        | None -> false)
    && (match r.Plan.r_ty with
        (* The window is a count of INTEGERS; only an integer column makes that
           a count of entries.  See the doc's third conjunct. *)
        | Row.Integer -> true
        | Row.Real | Row.Text | Row.Blob -> false)
    &&
      (match range_literal_window_rows r with
      | None -> false
      | Some window ->
        (* #550 review: shares {!table_seek_budget} with
           {!dml_seek_bail_out_at} rather than re-deriving "rows / ratio"
           inline — see that function's doc. *)
        (match table_seek_budget meta with
         | None -> false
         | Some budget -> window <= budget))
;;

(** #528: the plan op a join's right table is read through when it is the {i
    build} side of a hash join.

    [make_scan] was unconditional here, so a hash join always read the whole
    right table even when the WHERE clause pinned a leading prefix of one of its
    indexes — the TPC-C StockLevel shape, where [s_w_id = ?] makes one
    warehouse's [stock] directly seekable out of 100,000 rows across every
    warehouse.  [right_eqs] is the same re-based equality list {!best_probe}
    consumes, so the two strategies now narrow the right table from the same
    facts.

    Soundness is the invariant #513 and #516 already rest on: [chain_joins]
    applies the whole WHERE clause to the joined row, so restricting what the
    build side reads cannot change which joined rows survive.  This holds for
    LEFT JOIN for the reason #516 settled on — a narrowed build side null-extends
    left rows a full scan would have matched, those rows carry NULL in the very
    column the narrowing conjunct tests, [col = value] is never true of NULL, and
    the post-join filter drops them exactly as it dropped the wider rows they
    replaced.

    #532: [right_ranges] is the range half of the same re-basing, from
    {!right_table_ranges}.  It was empty when this function landed, so a build
    side whose prefix was pinned by equalities still walked to the end of that
    prefix even when the WHERE clause bounded the {i next} index column — the
    [sw = 1 AND si BETWEEN 20 AND 40] shape, which seeked warehouse 1 and then
    read all of it.  Feeding it through {!access_path_for_eqs} stops the walk at
    the upper bound.

    That feeds back into the strategy choice, {b in the direction that costs a
    probe rather than the one that buys one}.  {!estimate_rows} answers
    {!range_seek_rows} for a seek carrying a range where it answered
    [table_rows_estimate] for a bare prefix pin, so R shrinks — and
    {!probe_is_worth_it} takes the probe iff [driving_rows <= right_rows / 8], so
    a {i smaller} R makes the probe {i harder} to justify.  A join above
    {!nlj_min_driving_rows} whose right table holds N rows therefore moves from
    nested-loop probe to hash join across the whole band D <= N/8 (12,500 for
    N = 100,000), because R/8 falls from 12,500 to 12.

    Which way the flip goes is worth stating once more because it reads
    backwards: shrinking R moves joins towards the {i hash join}, taking probes
    away.  Whether that is an improvement depends entirely on how wide the window
    is, and a flat {!range_seek_rows} cannot tell — measured on disk, it was
    right for a 21-row window (4.2x faster) and 9x WRONG for a 20,000-row one.
    {!range_rows_estimate} reads the span off literal bounds for that reason;
    its doc carries the measurements and the one band still left mis-costed.

    #575 runs that mechanism {i in reverse}, and it is easy to miss.  Declining a
    seek makes the build side an [Op_seq_scan], so R rises from
    {!range_seek_rows} (or from 1) to [table_rows_estimate] — and a {i larger} R
    makes the probe {i easier} to justify.  Some joins therefore move from hash
    join back to nested-loop probe, not merely from a seeked build side to a
    scanned one.  [a_parameterised_window_keeps_the_flat_estimate] in
    [test/test_build_side_range_532.ml] is that case: 1,200 driving rows against
    20,000 stock rows go from a 21,200-row hash join to a 2,400-row probe, which
    is the plan #520's measurements prefer.  The strategy change is a consequence
    of the access-path change, and #575's issue text did not anticipate it.

    {2 #575: the seek is conditional}

    #528 took the seek unconditionally whenever a prefix was pinned, because
    there is no selectivity estimate to consult.  Where the prefix selects nearly
    the whole table that trades one ordered leaf walk for an index traversal plus
    a row fetch per entry, which in-memory measurement cannot see — {b measured
    on disk} by [test/bench_build_side_seek_546.ml] at 100,000 build-side rows:

    {v
      pinned prefix selects   seek reads   scan reads   cold
      100% (TPC-C W=1)           393,980       93,067   2.6-3.5x SLOWER
       25%                       166,691       93,03x   1.06-1.37x slower
        6%                       109,869       93,049   0.72x faster
        1%                        93,960       93,103   0.6-1.1x
    v}

    The seek costs ~3 pager resolutions per row fetched against the sequential
    walk's ~0.008, so break-even is around 1/390 of the table.  That figure
    barely moves when index order is scrambled against rowid order — the cost is
    the per-entry descent itself, so the ordered-fetch remedy #541 landed for the
    DML drain would not recover it.

    #575 decided what to do about that {i now}, rather than waiting for the
    per-index leading-column cardinality statistic (#576) that could tell the
    100% row from the 1% one — those two are the same plan shape over the same
    sized table with the same [table_rows_estimate], differing only in a value
    distribution nothing in the catalog records.  The decision was {b option B}:
    take the seek only where it is unambiguously right and decline the
    unmeasurable middle, which is what {!build_side_seek_is_unambiguous} tests.

    Four access paths qualify.  The first two reach {b at most one row}:

    - [Seek_rowid] — the rowid alias IS the table key, so the seek addresses one
      row and cannot lose to a scan.
    - [Seek_index] on a {b unique} index whose {b every} key column is pinned by
      an equality.  One entry, one [rh_get].

    The other two are bounded rather than a point:

    - [Seek_index] carrying a #532 range bound.  Not a point, but not part of
      what #546 measured either: the table above is the {i open-ended} prefix
      walk, and a range stops it at a bound.  It is also the one case with a
      selectivity estimate to consult ({!range_rows_estimate}), so #575's premise
      — "no selectivity estimate" — does not hold for it.
    - [Seek_index] on a bare equality prefix of a {b non-unique} index, admitted
      since #576 tier 1 when the leading column was analyzed at [CREATE INDEX]
      time: {!estimate_rows_from_stats} turns that column's stored
      distinct-value count into a row-count estimate, and the seek is taken
      only when that estimate fits {!table_seek_budget}.  This is the case
      #575 originally declined outright — see the history above — because at
      the time there was no selectivity estimate for a non-unique prefix at
      all; #576 tier 1 is what supplies one.

    Everything else — a strict prefix of a unique index, or a non-unique
    prefix that is unanalyzed or whose stats-based estimate exceeds budget —
    is still declined and scans.  Before #576 tier 1 that was every row of the
    table above, so the trade below was unconditional; now it is the residual
    left after the fourth arm above has had its chance to admit the seek.

    {b What this does NOT cost: TPC-C StockLevel.}  That query is the one #528
    and the table above are written around, so the natural reading is that #575
    gives it up.  It does not, because StockLevel never reaches this function:
    it plans as [NestedLoopJoin(stock)] over [IndexLookup(order_line)], not as a
    hash join, and it does so identically before and after #575.
    {!probe_is_worth_it} short-circuits on {!nlj_min_driving_rows} — the driving
    [order_line] seek carries a range and estimates {!range_seek_rows} = 100,
    [100 <= 1000], so the probe wins unconditionally.  StockLevel's 20-order
    window can never push the driving side over that floor.  #513's own
    resolution agrees: stock_level went 1,580 ms to 19.8 ms on #516's probe, not
    on #528's build side.

    So the cost of #575 is the 6% and 1% rows of the table above, and nothing
    else.  Verified by EXPLAIN on the real W=1 schema rather than argued from the
    shape of the SQL, because the shape of the SQL is exactly what makes the
    wrong reading tempting.

    {!Granary_sql.Exec.query_stats}'s [index_entries] is what makes this
    testable: [rows_examined] reports 130,000 for {i both} plans in the 100%
    case, so no assertion on it could tell a declined seek from a taken one.

    Synthesized right tables — CTEs, [sqlite_master], [sqlite_sequence], marked
    by a negative [tree_id] — and columnar tables have no B-tree to seek and are
    left to {!make_scan}.  For a CTE the guard is load-bearing, not defensive:
    {!access_path_for_eqs} reaches the catalog {i by name}, so a CTE that
    shadows a real table would otherwise pick up that table's indexes and plan
    an [Op_index_lookup] against a tree it has nothing to do with — a wrong
    answer, pinned by [cte_shadowing_a_real_table_still_scans].

    #565: that guard used to live here as an inline copy of
    {!meta_is_btree_backed}'s two-line test.  It now lives inside
    {!access_path_for_eqs} itself, which covers this caller and the base-scan and
    DML-seek ones at once.  Two independent copies of the same guard is precisely
    what let #551 exist unnoticed while this one was correct. *)
let build_side cat (right_meta : Cat.table_meta) ~alias ~right_eqs ~right_ranges =
  let eqs = List.mapi (fun pos (col_idx, v) -> pos, col_idx, v) right_eqs in
  match access_path_for_eqs cat right_meta ~eqs ~range_conjuncts:right_ranges with
  | Some (seek, _consumed) when build_side_seek_is_unambiguous cat right_meta seek ->
    seek_op ~alias right_meta seek
  | Some _ | None -> make_scan ~alias right_meta
;;

(** #520/#576: is a nested-loop probe worth it, given [driving_rows] estimated
    left rows and a right table of [right_rows], read through a build side
    that either scans or seeks?

    A probe costs one seek per driving row; the hash join it replaces costs one
    read per right-table row, plus the same driving rows either way.  So below
    {!nlj_min_driving_rows} the probe always wins, and above it the comparison
    is against the right table's size scaled by a per-row-cost ratio —
    {!nlj_probe_cost_ratio} when [build_side_is_seek] is [false] (the build
    side is [Op_seq_scan] or one of {!make_scan}'s synthesized/columnar arms),
    {!nlj_probe_cost_ratio_seeked_build} when it is [true] ([Op_index_lookup]
    or [Op_rowid_lookup] — see {!build_side}).  #520 calibrated the first
    against a scanned build side; #576 calibrated the second directly, rather
    than reusing a constant measured for a different physical operation.  See
    {!nlj_probe_cost_ratio_seeked_build}'s doc for why the two numbers turned
    out close instead of an order of magnitude apart, and for why the choice
    between them changes no decision reachable today.

    The ratio is written as a division rather than [driving_rows * ratio >
    right_rows] because [driving_rows] can be {!unbounded_rows} = [max_int],
    which that multiplication would overflow into a negative number and silently
    invert the test.  An unbounded estimate on {i either} side also fails the
    comparison outright, so "we have no idea how big this is" lands on the hash
    join, which is the bounded-loss choice. *)
let probe_is_worth_it ~driving_rows ~right_rows ~build_side_is_seek =
  let ratio =
    if build_side_is_seek then nlj_probe_cost_ratio_seeked_build else nlj_probe_cost_ratio
  in
  driving_rows <= nlj_min_driving_rows
  || (driving_rows < unbounded_rows
      && right_rows < unbounded_rows
      && driving_rows <= right_rows / ratio)
;;

(** #552: the join for an ON predicate the hash keys cannot express — anything
    that is not a [col = col] equality between the two sides.  Shared by the
    catalog path ({!plan_join}) and the no-catalog one ({!chain_joins_no_cat}),
    which had the same defect because they had the same shape.

    The op is a cartesian hash join ([left_key = -1]) and the ON predicate has
    to be applied somewhere above the pairing.  {b Where} is not a free choice:

    - [`Inner]: above the join, as an [Op_filter].  Equivalent to filtering
      inside it, and it keeps the shape every other part of the planner and its
      tests already expect.
    - [`Left]: inside the join, as [on_pred].  A filter above the join is
      {i wrong} here — the ON predicate is the match test, so a left row that
      satisfies it for no right row must still be emitted null-extended, and
      that null-extended row is exactly what a post-join filter rejects
      (typically because the predicate is NULL on it, as [b > a] is).  Before
      #552 every left row was paired with every right row, [any] was set for all
      of them, the null-extension never fired, and the filter then dropped the
      unmatched left row entirely — [SELECT a, b FROM l LEFT JOIN r ON b > a]
      lost its unmatched left rows (#539) and [ON si IS NULL] returned nothing
      at all (#552).

    The WHERE clause is unaffected either way: [chain_joins] still applies it to
    the joined row, above the join, which is where SQL puts it — a WHERE
    conjunct on a right-table column legitimately drops null-extended rows. *)
let general_on_join ~left_op ~right_op ~on ~join_kind ~right_offset ~n_right_cols
  : Plan.op
  =
  let pred = plan_expr on in
  let on_pred =
    match join_kind with
    | `Left -> Some pred
    | `Inner -> None
  in
  let cart =
    Plan.Op_hash_join
      { left = left_op
      ; right = right_op
      ; left_key = -1
      ; right_key = -1
      ; on_pred
      ; join_kind
      ; right_col_offset = right_offset
      ; n_right_cols
      }
  in
  match join_kind with
  | `Left -> cart
  | `Inner -> Plan.Op_filter { pred; child = cart }
;;

(** Plan a JOIN.  [left_op] produces left-table rows; we wrap it with
    either Op_nested_loop_join (when the right table has an index the join can
    probe) or Op_hash_join (otherwise).  If the ON predicate is not a simple
    equality between a left and a right column, fall back to
    {!general_on_join}'s hash cartesian product.

    #516: [where_conjuncts] are the top-level [AND] conjuncts of the query's
    WHERE clause.  Equalities among them that pin a right-table column can
    complete a multi-column probe key whose remaining column is the join column
    — the TPC-C shape, where [stock] is keyed on [(s_w_id, s_i_id)], joined on
    [s_i_id], and [s_w_id] is fixed by the WHERE clause.  This is only sound
    because the caller still applies the whole WHERE clause to the joined row;
    see {!Plan.probe_part}.

    #520: having a probe is not on its own a reason to use one.  A probe costs a
    B-tree seek per driving row where a hash join costs one scan of the right
    table, so the choice turns on how many driving rows there are — see
    {!probe_is_worth_it} for the rule, {!nlj_probe_cost_ratio} for the
    measurements behind it, and {!estimate_rows} for the static estimate that
    stands in for statistics this engine does not keep.
    Both strategies return the same rows; this is only ever a cost decision. *)
let plan_join cat ~where_conjuncts (bj : Sema.bound_join) (left_op : Plan.op) n_left
  : Plan.op
  =
  let join_kind =
    match bj.kind with
    | Ast.Inner -> `Inner
    | Ast.Left -> `Left
  in
  let right_offset = bj.right_col_offset in
  let n_right_cols = List.length bj.right_meta.Cat.columns in
  let right_eqs = right_table_eqs ~right_offset ~n_right_cols where_conjuncts in
  (* #532: the range half of the same re-basing.  #570: BOTH strategies take it
     — the build side through [build_side], the nested-loop probe through
     [range_for_index] at the probe key's length.  Until #570 only the former
     did, so the same range on the same table narrowed the read or not depending
     on which strategy the cost model happened to pick. *)
  let right_ranges = right_table_ranges ~right_offset ~n_right_cols where_conjuncts in
  (* #528: the build side of a hash join, narrowed by the same WHERE equalities
     that would complete a probe key.  It does not depend on the strategy chosen
     — but the cost model's [right_rows] depends on IT, so build it first. *)
  let right_op =
    build_side cat bj.right_meta ~alias:bj.Sema.right_alias ~right_eqs ~right_ranges
  in
  (* #520: neither side of the cost comparison depends on which strategy is
     chosen or on which column the ON predicate resolves to — compute both once,
     outside the match. *)
  let driving_rows = estimate_rows cat left_op in
  (* #528: R in the cost comparison is what the hash join would actually read,
     so it is estimated from the (possibly narrowed) build-side op rather than
     from the whole table.

     Be precise about how much that moves.  {!estimate_rows} answers
     [table_rows_estimate] for the [Op_seq_scan] case, and for an
     [Op_index_lookup] it answers [unbounded_rows] — hence [table_rows_estimate]
     after the [min] — unless {!seek_is_unique_point} holds, the seek carries a
     range, or (#576 tier 1) the seek is a bare equality prefix on a non-unique
     index with an analyzed leading-column stat.  So R collapses to 1 for a
     full-unique-key or rowid-alias pin, to [range_seek_rows] once #532 gives the
     seek a range bound, to {!estimate_rows_from_stats}'s estimate for a
     stats-backed non-unique prefix, and is otherwise unchanged (still
     [table_rows_estimate]) for a partial prefix pin with no usable stat.
     [range_seek_rows] is a made-up constant, not a selectivity estimate; do not
     read this as one.

     {b Which way a smaller R pushes the choice is the opposite of what it looks
     like.}  {!probe_is_worth_it} takes the probe iff
     [driving_rows <= right_rows / 8] — R is on the RIGHT of the comparison — so
     shrinking R makes the probe HARDER to justify, not easier.  Every collapse
     above therefore moves joins TOWARDS the hash join: the point-seek cases have
     done so since #528, and #532's range case moves the whole band
     [nlj_min_driving_rows < D <= table_rows_estimate / 8] with it.  See
     {!build_side} for the measurement that says the range case is faster for
     having moved.  A change that makes R more accurate will move plans; check
     which side of that comparison it lands on before assuming the direction. *)
  let right_rows = estimate_rows cat right_op in
  (* #576: which per-row cost {!probe_is_worth_it} should charge the hash join
     — [right_op] already says whether {!build_side} took the seek or declined
     it, so this is read off the op it returned rather than re-deriving it. *)
  let build_side_is_seek =
    match right_op with
    | Plan.Op_index_lookup _ | Plan.Op_rowid_lookup _ -> true
    | _ -> false
  in
  let mk_with_left_col_right_col left_col right_col : Plan.op =
    let probe =
      if probe_is_worth_it ~driving_rows ~right_rows ~build_side_is_seek
      then best_probe cat bj.right_meta ~join_col:right_col ~left_col ~right_eqs
      else None
    in
    match probe with
    | Some (idx, probe) ->
      (* #570: the probe key covers a leading prefix of [idx]'s columns and
         stops at the first column pinned by neither the left row nor a WHERE
         equality.  That is precisely [range_for_index]'s [~n_eq] position, so
         the range half of the re-basing narrows the probe by the same call the
         build side already makes — no new recogniser and no new spelling to
         keep in step.

         Unlike the build side this needs no #575/#606 gate.  Those exist
         because a build-side seek REPLACES a sequential scan of the whole
         table, so a wide window can cost more than what it displaced.  A probe
         is a seek either way: the range only moves its start key forward and
         stops it early, so the worst case is the pre-#570 probe exactly and
         there is nothing to decline.  It also does not feed the cost model —
         the strategy is already chosen by the time this runs. *)
      let probe_range =
        range_for_index bj.right_meta idx ~n_eq:(List.length probe) right_ranges
      in
      Plan.Op_nested_loop_join
        { left = left_op
        ; right_meta = bj.right_meta
        ; right_alias = bj.Sema.right_alias
        ; idx_tree = idx.Cat.idx_tree_id
        ; probe
        ; probe_range
        ; join_kind
        ; right_col_offset = right_offset
        ; n_right_cols
        }
    | None ->
      Plan.Op_hash_join
        { left = left_op
        ; right = right_op
        ; left_key = left_col
        ; right_key = right_col
        ; on_pred = None
        ; join_kind
        ; right_col_offset = right_offset
        ; n_right_cols
        }
  in
  match recognise_eq_col_col bj.on with
  | Some (a, b) when a < n_left && b >= right_offset ->
    mk_with_left_col_right_col a (b - right_offset)
  | Some (a, b) when b < n_left && a >= right_offset ->
    mk_with_left_col_right_col b (a - right_offset)
  | _ ->
    general_on_join ~left_op ~right_op ~on:bj.on ~join_kind ~right_offset ~n_right_cols
;;

let sema_agg_to_plan (a : Sema.agg_spec) : Plan.agg_spec =
  { Plan.func = a.func
  ; col_ord = a.col_ord
  ; (* #488: the argument expression addresses the INPUT row, exactly like any
       other bound expression over a scanned row, so it needs no remapping. *)
    arg_expr = Option.map plan_expr a.arg_expr
  ; distinct = a.distinct
  }
;;

let sema_agg_proj_to_plan : Sema.agg_proj_item -> Plan.proj_item = function
  | Sema.AP_group_col i -> Plan.PI_group_col i
  | Sema.AP_agg_slot i -> Plan.PI_agg_slot i
  | Sema.AP_window_slot i -> Plan.PI_window_slot i
  | Sema.AP_expr e -> Plan.PI_expr (plan_expr e)
;;

let plan_window_item (ws : Sema.window_sema) : Plan.window_plan_item =
  { Plan.func = ws.Sema.func
  ; args = List.map plan_expr ws.Sema.args
  ; partition_by = List.map plan_expr ws.Sema.partition_by
  ; order_by =
      List.map
        (fun (bk : Sema.bound_order_key) ->
           let dir =
             match bk.Sema.dir with
             | Ast.Asc -> `Asc
             | Ast.Desc -> `Desc
           in
           let nulls =
             match bk.Sema.nulls with
             | Some `Nulls_first -> `Nulls_first
             | Some `Nulls_last -> `Nulls_last
             | None ->
               (match dir with
                | `Asc -> `Nulls_first
                | `Desc -> `Nulls_last)
           in
           plan_expr bk.Sema.key, dir, nulls)
        ws.Sema.order_by
  ; frame = ws.Sema.frame
  }
;;

let rec substitute_window_slots ~n_input_cols (e : Plan.expr) : Plan.expr =
  let go = substitute_window_slots ~n_input_cols in
  match e with
  | Plan.P_window_slot i -> Plan.P_col (n_input_cols + i)
  | Plan.P_binop (op, a, b) -> Plan.P_binop (op, go a, go b)
  | Plan.P_not e -> Plan.P_not (go e)
  | Plan.P_is_null e -> Plan.P_is_null (go e)
  | Plan.P_is_not_null e -> Plan.P_is_not_null (go e)
  | Plan.P_neg e -> Plan.P_neg (go e)
  | Plan.P_bitnot e -> Plan.P_bitnot (go e)
  | Plan.P_between (x, lo, hi) -> Plan.P_between (go x, go lo, go hi)
  | Plan.P_in (x, vs) -> Plan.P_in (go x, List.map go vs)
  | Plan.P_func (f, args) -> Plan.P_func (f, List.map go args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    Plan.P_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Plan.P_cast (e, ty) -> Plan.P_cast (go e, ty)
  | Plan.P_collate (e, c) -> Plan.P_collate (go e, c)
  (* Leaves, and the three subquery-bearing nodes — a window slot inside a
     subquery's own [Ast.stmt] is that statement's to substitute, not this
     one's.  Exhaustive rather than [| e' -> e'] for the reason #670 gives: a
     catch-all is what lets a newly added constructor fall through a walker
     silently. *)
  | Plan.P_lit _
  | Plan.P_col _
  | Plan.P_param _
  | Plan.P_excluded_col _
  | Plan.P_subquery _
  | Plan.P_exists _
  | Plan.P_in_select _ -> e
;;

(* #507: an aggregated projection item may be an expression over the aggregate
   output row.  A window function inside such an expression is bound to a slot
   number, which only becomes a column index once the aggregate list is final —
   the window results sit after [group_cols @ aggs] in the row the executor
   builds.  Hence the substitution belongs here rather than in [Sema], which
   binds the projection before HAVING has contributed its own aggregates. *)
let plan_agg_proj ~group_by ~aggs agg_proj =
  let n_input_cols = List.length group_by + List.length aggs in
  List.map
    (fun item ->
       match sema_agg_proj_to_plan item with
       | Plan.PI_expr e -> Plan.PI_expr (substitute_window_slots ~n_input_cols e)
       | other -> other)
    agg_proj
;;

(* Wrap [child] in a filter for the conjuncts an access path did not consume. *)
let residual_filter ~consumed all_conjuncts child =
  let left =
    List.filteri (fun pos _ -> not (List.mem pos consumed)) all_conjuncts
    |> List.map plan_expr
  in
  match left with
  | [] -> child
  | p :: rest ->
    let pred = List.fold_left (fun a b -> Plan.P_binop (Plan.And, a, b)) p rest in
    Plan.Op_filter { pred; child }
;;

(* Choose an access path for [where] over a single table.  The clause is split on
   its top-level [AND] spine (#508); equality conjuncts of the form
   [col = literal|?] can pin either the rowid alias (a single table seek) or the
   leading columns of an index (an encoded-prefix seek).  Returns the chosen path
   together with the conjunct positions it consumed — everything else must still
   be evaluated by the caller. *)
let choose_access_path cat (table_meta : Cat.table_meta) conjuncts_list =
  let eqs =
    conjuncts_list
    |> List.mapi (fun pos c ->
      Option.map (fun (col_idx, v) -> pos, col_idx, v) (recognise_eq_col_lit c))
    |> List.filter_map Fun.id
  in
  access_path_for_eqs cat table_meta ~eqs ~range_conjuncts:conjuncts_list
;;

(* #513: the conjuncts of a joined query that speak only about the driving
   table.  Column ordinals in a bound WHERE clause address the {i combined} row,
   whose leading [n_base] slots are the driving table's own columns, so anything
   at or beyond that boundary belongs to a joined table and cannot pin the base
   table's index.  Only the recognised parts of a surviving conjunct are ever
   read — its subject column, checked here, and value sides that are literals or
   bound parameters, never columns.  A #519 [BETWEEN] whose other end mentions a
   joined column therefore survives with that end simply unrecognised, and
   contributes only the end that is a value. *)
let base_only_conjuncts ~n_base cs =
  List.filter
    (fun c ->
       match recognise_eq_col_lit c with
       | Some (col_idx, _) -> col_idx < n_base
       | None ->
         (* #517: inequalities (and #519's BETWEEN) are kept too — they cannot
            pin the prefix, but they can bound the column after it. *)
         (match recognise_range_col_lit c with
          | Some (col_idx, _, _) -> col_idx < n_base
          | None -> false))
    cs
;;

(* The base access path for a SELECT: the chosen seek as a plan op, with the
   unconsumed conjuncts left behind as a residual filter; a filtered seq scan
   when nothing is seekable.

   #513: a join no longer forfeits the access path.  It does change what the
   seek is allowed to assume — [chain_joins] applies the whole WHERE clause to
   the joined row, so here the seek narrows what the driving table reads and
   nothing more.  That is why the joined case drops the residual filter (the
   post-join filter already covers every conjunct, including the consumed ones)
   and why it plans only from [base_only_conjuncts]: it is a pure restriction of
   the base input, exactly like the #508 DML seek. *)
let plan_base cat ~table_meta ~alias ~where ~has_joins =
  match where with
  | None -> make_scan ~alias table_meta
  | Some e ->
    let cs = conjuncts e in
    if has_joins
    then (
      let n_base = List.length table_meta.Cat.columns in
      match choose_access_path cat table_meta (base_only_conjuncts ~n_base cs) with
      | None -> make_scan ~alias table_meta
      | Some (seek, _consumed) -> seek_op ~alias table_meta seek)
    else (
      let fallback () =
        Plan.Op_filter { pred = plan_expr e; child = make_scan ~alias table_meta }
      in
      match choose_access_path cat table_meta cs with
      | None -> fallback ()
      | Some (seek, consumed) ->
        residual_filter ~consumed cs (seek_op ~alias table_meta seek))
;;

(* #508: the narrowing path for a DML WHERE clause.  Unlike [plan_base] this
   discards which conjuncts were consumed — the write path always re-evaluates
   the whole predicate on every candidate row, so the seek is a pure
   restriction of what gets read.

   #550's non-unique-index bail-out budget is NOT computed here any more (see
   plan.mli's [seek] and {!dml_seek_bail_out_at}'s staleness note): a [Plan.op]
   built by this function can be cached and reused for the life of a prepared
   statement, so a row-count-derived budget baked in here would go stale as the
   table grows.  [Exec.seek_index_candidates] calls {!dml_seek_bail_out_at}
   itself, once per execution, against a freshly re-read [table_meta] — this
   function hands it nothing but [idx_tree] to do that with, exactly what
   [access_path_for_eqs] already produces. *)
let plan_dml_seek cat ~table_meta ~where =
  match cat, where with
  | Some c, Some e -> Option.map fst (choose_access_path c table_meta (conjuncts e))
  | _ -> None
;;

(* #495: the sort direction and NULL placement of one bound ORDER BY key.  A
   missing NULLS clause follows the direction: NULLs first ascending, last
   descending. *)
let order_dir_nulls (bkey : Sema.bound_order_key) =
  let dir =
    match bkey.Sema.dir with
    | Ast.Asc -> `Asc
    | Ast.Desc -> `Desc
  in
  let nulls =
    match bkey.Sema.nulls with
    | Some `Nulls_first -> `Nulls_first
    | Some `Nulls_last -> `Nulls_last
    | None ->
      (match dir with
       | `Asc -> `Nulls_first
       | `Desc -> `Nulls_last)
  in
  dir, nulls
;;

(* Build ORDER BY sort keys, substituting window slots into the key
   expressions when window functions are present. *)
let plan_sort_keys ~order ~windows ~n_input_cols =
  List.map
    (fun (bkey : Sema.bound_order_key) ->
       let dir =
         match bkey.dir with
         | Ast.Asc -> `Asc
         | Ast.Desc -> `Desc
       in
       let nulls =
         match bkey.nulls with
         | Some `Nulls_first -> `Nulls_first
         | Some `Nulls_last -> `Nulls_last
         | None ->
           (match dir with
            | `Asc -> `Nulls_first
            | `Desc -> `Nulls_last)
       in
       let e = plan_expr bkey.key in
       let e' = if windows = [] then e else substitute_window_slots ~n_input_cols e in
       e', dir, nulls)
    order
;;

(* Build the projection operator: aggregate, expression-project (with window
   slot substitution), or plain ordinal project. *)
let plan_projection
      ~is_aggregated
      ~after_sort
      ~group_by
      ~aggs
      ~having
      ~agg_proj
      ~agg_windows
      ~expr_proj
      ~proj
      ~windows
      ~n_input_cols
  =
  if is_aggregated
  then
    Plan.Op_aggregate
      { child = after_sort
      ; group_cols = group_by
      ; aggs = List.map sema_agg_to_plan aggs
      ; having = Option.map plan_expr having
      ; proj = plan_agg_proj ~group_by ~aggs agg_proj
      ; windows = List.map plan_window_item agg_windows
      }
  else if expr_proj <> []
  then
    Plan.Op_expr_project
      { exprs =
          List.map
            (fun (be, alias) ->
               let e = plan_expr be in
               let e' =
                 if windows = [] then e else substitute_window_slots ~n_input_cols e
               in
               e', alias)
            expr_proj
      ; child = after_sort
      }
  else Plan.Op_project { ordinals = proj; child = after_sort }
;;

(* #495: ORDER BY over an aggregate expression.  Sema bound these keys over the
   aggregate OUTPUT row ([group_cols @ aggs]), but the sort runs over the
   PROJECTED row, which need not contain the aggregate at all
   ([... GROUP BY k ORDER BY SUM(v)] projects no SUM).  So each key is appended
   to [Op_aggregate]'s own projection as a hidden column — where its ordinals
   mean what they were bound to mean — sorted on by position, and trimmed away
   again by an [Op_project] wrapped around the sort.

   Trimming here, before [finalize_select] applies DISTINCT and LIMIT, is what
   keeps the hidden columns invisible to everything downstream; the statement's
   output width comes from [Sema]'s [agg_proj] and never sees them.

   This path is taken for the WHOLE clause or none of it, which is why the
   [remap_e] below cannot collide with it: a key bound in output space would be
   indistinguishable from a pre-aggregation ordinal that happens to have the
   same number. *)
let plan_agg_order_hidden ~agg_order_keys ~projected =
  match projected with
  | Plan.Op_aggregate r ->
    let n_visible = List.length r.proj in
    let hidden =
      List.map
        (fun (bkey : Sema.bound_order_key) -> Plan.PI_expr (plan_expr bkey.Sema.key))
        agg_order_keys
    in
    let child = Plan.Op_aggregate { r with proj = r.proj @ hidden } in
    let keys =
      List.mapi
        (fun j bkey ->
           let dir, nulls = order_dir_nulls bkey in
           Plan.P_col (n_visible + j), dir, nulls)
        agg_order_keys
    in
    Plan.Op_project
      { ordinals = List.init n_visible Fun.id; child = Plan.Op_sort { keys; child } }
  | _ ->
    (* Unreachable: [agg_order_keys] is non-empty only for an aggregated
       SELECT, and [plan_projection] then always returns [Op_aggregate].  This
       fails loudly rather than returning the child unchanged, because the
       symptom of the latter would be silently UNSORTED rows — the hidden
       columns have nowhere to go, so the whole ORDER BY would evaporate. *)
    failwith
      "plan_agg_order_hidden: ORDER BY over an aggregate needs an Op_aggregate projection"
;;

(* Post-aggregation ORDER BY: ORDER BY col indices are in pre-aggregation
   space, so remap each P_col to its position in the aggregated output. *)
let plan_post_agg_sort_input_space ~group_by ~agg_proj ~order ~projected =
  let plan_proj = List.map sema_agg_proj_to_plan agg_proj in
  let find_idx pred lst =
    let rec go k = function
      | [] -> None
      | x :: rest -> if pred x then Some k else go (k + 1) rest
    in
    go 0 lst
  in
  (* #663: RECURSIVE, not a single top-level match.  This used to rewrite only a
     bare [P_col] at the root of a key, so an ORDER BY over an EXPRESSION of a
     grouped column — [ORDER BY nm || 'q'] — kept the pre-aggregation index
     inside the expression and evaluated it against the aggregated OUTPUT row.
     On a table where the grouped column is not at input index 0 that reads a
     different output column entirely (the aggregate slot), silently, or
     indexes past the end of the row; it only looked right because the usual
     fixture groups by the first column, where the two indices coincide.

     Every remaining [P_col] under an aggregated ORDER BY key IS a grouped
     column, because [Sema.bind_select_order] now refuses the key outright if
     any column it names is not grouped (#663's other half).  So descending
     cannot mis-fire on a column that should have been left alone: there are
     none.

     {b But being GROUPED is not enough to be remappable, and that gap was the
     first revision's blocker.}  The inner search needs the column to appear in
     [agg_proj] as an [AP_group_col] — i.e. to be PROJECTED as a bare group
     column.  [SELECT COUNT( * ) FROM g GROUP BY nm ORDER BY nm] groups by [nm]
     and projects only the count, so the search failed, the [None] arm kept the
     PRE-AGGREGATION index, and the sort read past the end of a one-column
     output row: [Invalid_argument] escaping the executor mid-query, for an
     idiomatic shape.  With [UPPER(nm)] projected instead of [nm] it did not
     crash but sorted by the aggregate.  Making the walk recursive WIDENED the
     reach of that arm rather than narrowing it, since it now fires inside
     expressions too.

     So a grouped column that is not projected gets a HIDDEN output slot, the
     same mechanism [plan_agg_order_hidden] uses for #495's aggregate keys: the
     column is appended to [Op_aggregate]'s own projection where its ordinal
     means what it was bound to mean, sorted on by position, and trimmed away
     by an [Op_project] around the sort so nothing downstream sees it.  Slots
     are allocated at most once per grouped column, so [ORDER BY nm, nm || 'q']
     adds one, not two.

     The three subquery-bearing nodes stay opaque — the columns inside them
     belong to their own from-list, not to this GROUP BY. *)
  let n_visible =
    match projected with
    | Plan.Op_aggregate r -> Some (List.length r.proj)
    | _ -> None
  in
  (* Group-column positions given a hidden slot, in allocation order. *)
  let hidden = ref [] in
  let alloc_hidden gc_pos =
    match n_visible with
    | None -> None
    | Some n ->
      (match find_idx (( = ) gc_pos) !hidden with
       | Some k -> Some (n + k)
       | None ->
         hidden := !hidden @ [ gc_pos ];
         Some (n + List.length !hidden - 1))
  in
  let rec remap_e e =
    match e with
    | Plan.P_col i ->
      (match find_idx (( = ) i) group_by with
       | None -> e
       | Some gc_pos ->
         (match
            find_idx
              (function
                | Plan.PI_group_col k -> k = gc_pos
                | _ -> false)
              plan_proj
          with
          | Some out_pos -> Plan.P_col out_pos
          | None ->
            (match alloc_hidden gc_pos with
             | Some out_pos -> Plan.P_col out_pos
             | None -> e)))
    | Plan.P_binop (op, a, b) -> Plan.P_binop (op, remap_e a, remap_e b)
    | Plan.P_not a -> Plan.P_not (remap_e a)
    | Plan.P_is_null a -> Plan.P_is_null (remap_e a)
    | Plan.P_is_not_null a -> Plan.P_is_not_null (remap_e a)
    | Plan.P_neg a -> Plan.P_neg (remap_e a)
    | Plan.P_bitnot a -> Plan.P_bitnot (remap_e a)
    | Plan.P_between (x, lo, hi) -> Plan.P_between (remap_e x, remap_e lo, remap_e hi)
    | Plan.P_in (x, vs) -> Plan.P_in (remap_e x, List.map remap_e vs)
    | Plan.P_func (f, args) -> Plan.P_func (f, List.map remap_e args)
    | Plan.P_case { scrutinee; branches; else_ } ->
      Plan.P_case
        { scrutinee = Option.map remap_e scrutinee
        ; branches = List.map (fun (c, r) -> remap_e c, remap_e r) branches
        ; else_ = Option.map remap_e else_
        }
    | Plan.P_cast (x, ty) -> Plan.P_cast (remap_e x, ty)
    | Plan.P_collate (x, c) -> Plan.P_collate (remap_e x, c)
    (* Opaque by decision (subqueries) or genuinely leaves.  Exhaustive rather
       than [| _ -> e]: a new constructor carrying an expression must be a
       compile error here, since missing one is precisely the defect above. *)
    | Plan.P_subquery _ | Plan.P_exists _ | Plan.P_in_select _ -> e
    | Plan.P_lit _ | Plan.P_param _ | Plan.P_excluded_col _ | Plan.P_window_slot _ -> e
  in
  let keys =
    List.map
      (fun (bkey : Sema.bound_order_key) ->
         let dir =
           match bkey.dir with
           | Ast.Asc -> `Asc
           | Ast.Desc -> `Desc
         in
         let nulls =
           match bkey.nulls with
           | Some `Nulls_first -> `Nulls_first
           | Some `Nulls_last -> `Nulls_last
           | None ->
             (match dir with
              | `Asc -> `Nulls_first
              | `Desc -> `Nulls_last)
         in
         (* #489/#490: [BE_out_col] already addresses the aggregated OUTPUT
            row, so it must bypass [remap_e], which exists to translate
            *pre-aggregation* input column indices.  Feeding it through would
            silently re-point the key at whichever GROUP BY column happens to
            share its index — an ORDER BY on an aggregate alias would then sort
            by the grouping column instead, which is the same class of silent
            wrong answer #489 was filed about. *)
         let e' =
           match bkey.key with
           | Sema.BE_out_col i -> Plan.P_col i
           | k -> remap_e (plan_expr k)
         in
         e', dir, nulls)
      order
  in
  if keys = []
  then projected
  else (
    match !hidden, projected, n_visible with
    | [], _, _ | _, _, None -> Plan.Op_sort { keys; child = projected }
    | hs, Plan.Op_aggregate r, Some n ->
      let child =
        Plan.Op_aggregate
          { r with proj = r.proj @ List.map (fun k -> Plan.PI_group_col k) hs }
      in
      Plan.Op_project
        { ordinals = List.init n Fun.id; child = Plan.Op_sort { keys; child } }
    | _, child, _ -> Plan.Op_sort { keys; child })
;;

(* The post-aggregation sort: #495's output-space keys when the ORDER BY
   mentioned an aggregate, the pre-aggregation-space remap otherwise.  Sema
   guarantees the two lists are never both non-empty. *)
let plan_post_agg_sort ~group_by ~agg_proj ~order ~agg_order_keys ~projected =
  if agg_order_keys <> []
  then plan_agg_order_hidden ~agg_order_keys ~projected
  else plan_post_agg_sort_input_space ~group_by ~agg_proj ~order ~projected
;;

(* Apply DISTINCT then LIMIT/OFFSET to a planned SELECT body. *)
let finalize_select ~distinct ~limit ~offset sorted =
  let after_distinct = if distinct then Plan.Op_distinct { child = sorted } else sorted in
  match limit with
  | None -> after_distinct
  | Some n ->
    let off = Option.value ~default:0 offset in
    Plan.Op_limit { limit = n; offset = off; child = after_distinct }
;;

(* Catalog path: chain joins left-to-right via plan_join, then apply WHERE to
   the combined row (single-table WHERE is already folded into [base]). *)
let chain_joins cat ~(table_meta : Cat.table_meta) ~base ~joins ~where =
  let where_conjuncts =
    match where with
    | None -> []
    | Some e -> conjuncts e
  in
  let after_joins, _ =
    List.fold_left
      (fun (op, n_left) (bj : Sema.bound_join) ->
         let joined = plan_join cat ~where_conjuncts bj op n_left in
         joined, n_left + List.length bj.Sema.right_meta.Cat.columns)
      (base, List.length table_meta.Cat.columns)
      joins
  in
  if joins <> []
  then (
    match where with
    | None -> after_joins
    | Some e -> Plan.Op_filter { pred = plan_expr e; child = after_joins })
  else after_joins
;;

(* No-catalog path: chain joins as hash joins, recognising equi-join keys and
   falling back to a cartesian product + filter. *)
let chain_joins_no_cat ~(table_meta : Cat.table_meta) ~table_alias ~joins =
  let base = make_scan ~alias:table_alias table_meta in
  fst
    (List.fold_left
       (fun (op, n_left) (bj : Sema.bound_join) ->
          let n_right_cols = List.length bj.right_meta.Cat.columns in
          let right_offset = bj.right_col_offset in
          let join_kind =
            match bj.kind with
            | Ast.Inner -> `Inner
            | Ast.Left -> `Left
          in
          let joined =
            match recognise_eq_col_col bj.on with
            | Some (a, b) when a < n_left && b >= right_offset ->
              Plan.Op_hash_join
                { left = op
                ; right = make_scan ~alias:bj.Sema.right_alias bj.right_meta
                ; left_key = a
                ; right_key = b - right_offset
                ; on_pred = None
                ; join_kind
                ; right_col_offset = right_offset
                ; n_right_cols
                }
            | Some (a, b) when b < n_left && a >= right_offset ->
              Plan.Op_hash_join
                { left = op
                ; right = make_scan ~alias:bj.Sema.right_alias bj.right_meta
                ; left_key = b
                ; right_key = a - right_offset
                ; on_pred = None
                ; join_kind
                ; right_col_offset = right_offset
                ; n_right_cols
                }
            | _ ->
              (* #552: shared with the catalog path, which had the same defect
                 because it had the same shape. *)
              general_on_join
                ~left_op:op
                ~right_op:(make_scan ~alias:bj.Sema.right_alias bj.right_meta)
                ~on:bj.on
                ~join_kind
                ~right_offset
                ~n_right_cols
          in
          joined, n_left + n_right_cols)
       (base, List.length table_meta.columns)
       joins)
;;

let plan_select
      cat
      ~table_meta
      ~table_alias
      ~proj
      ~expr_proj
      ~where
      ~order
      ~limit
      ~offset
      ~joins
      ~group_by
      ~aggs
      ~having
      ~agg_proj
      ~distinct
      ~windows
      ~agg_windows
      ~agg_order_keys
  =
  let has_joins = joins <> [] in
  let n_input_cols =
    List.length table_meta.Cat.columns
    + List.fold_left
        (fun acc (bj : Sema.bound_join) ->
           acc + List.length bj.Sema.right_meta.Cat.columns)
        0
        joins
  in
  let base = plan_base cat ~table_meta ~alias:table_alias ~where ~has_joins in
  let after_where = chain_joins cat ~table_meta ~base ~joins ~where in
  let is_aggregated = aggs <> [] || group_by <> [] in
  (* Insert Op_window after scan+filter+joins when windows are present. *)
  let after_window =
    if windows = []
    then after_where
    else
      Plan.Op_window
        { child = after_where; windows = List.map plan_window_item windows; n_input_cols }
  in
  (* For non-aggregate queries: sort BEFORE projection so col_idx correctly
     addresses the original table schema (pre-projection row layout).
     For aggregate queries: sort AFTER aggregation because ORDER BY refers
     to the aggregated output row layout. *)
  let make_sort child =
    let keys = plan_sort_keys ~order ~windows ~n_input_cols in
    if keys = []
    then child
    else if
      (not has_joins)
      && windows = []
      &&
      (* [plan_base] wraps its chosen seek in a residual [Op_filter] whenever
         some conjunct — e.g. a #517 range bound, which narrows the seek's
         span but is never marked "consumed" — is left for the caller to
         re-check.  [Op_filter] only drops rows; it never reorders survivors,
         so unwrapping one layer of it here to reach the seek underneath is
         sound, and is exactly what lets the range-bounded case elide too. *)
      match base with
      | Plan.Op_index_lookup { table_meta; _ }
      | Plan.Op_rowid_lookup { table_meta; _ }
      | Plan.Op_filter
          { child =
              ( Plan.Op_index_lookup { table_meta; _ }
              | Plan.Op_rowid_lookup { table_meta; _ } )
          ; _
          } ->
        let seek =
          match base with
          | Plan.Op_filter { child; _ } -> child
          | b -> b
        in
        order_satisfied_by_natural_order table_meta (natural_order cat seek) order
      | _ -> false
    then
      (* #674 (3 of 3): the chosen access path already produces this order —
         eliding a redundant Op_sort is what lets #677's Op_limit early-stop
         reach the scanner directly, instead of draining it into a sort
         buffer first. *)
      child
    else Plan.Op_sort { keys; child }
  in
  let after_sort = if is_aggregated then after_window else make_sort after_window in
  let projected =
    plan_projection
      ~is_aggregated
      ~after_sort
      ~group_by
      ~aggs
      ~having
      ~agg_proj
      ~agg_windows
      ~expr_proj
      ~proj
      ~windows
      ~n_input_cols
  in
  (* Post-aggregation sort (only for aggregated queries). *)
  let sorted =
    if is_aggregated
    then plan_post_agg_sort ~group_by ~agg_proj ~order ~agg_order_keys ~projected
    else projected
  in
  finalize_select ~distinct ~limit ~offset sorted
;;

(* ORDER BY sort keys without window-slot substitution (used by UPDATE,
   DELETE, compound queries, and the no-catalog SELECT path). *)
let plan_order_keys order =
  List.map
    (fun (bk : Sema.bound_order_key) ->
       let dir =
         match bk.dir with
         | Ast.Asc -> `Asc
         | Ast.Desc -> `Desc
       in
       let nulls =
         match bk.nulls with
         | Some `Nulls_first -> `Nulls_first
         | Some `Nulls_last -> `Nulls_last
         | None ->
           (match dir with
            | `Asc -> `Nulls_first
            | `Desc -> `Nulls_last)
       in
       plan_expr bk.key, dir, nulls)
    order
;;

(* Catalog indexes for a table, or [] when no catalog is available. *)
let indexes_of cat (table_meta : Cat.table_meta) =
  match cat with
  | Some c -> Cat.indexes_for_table c ~table:table_meta.Cat.name
  | None -> []
;;

(* #653: shared by both INSERT shapes, so a future change to how an upsert's
   assignments are planned cannot reach one spelling and miss the other. *)
let plan_upsert_update upsert_update =
  match upsert_update with
  | None -> None
  | Some (cols, assigns) -> Some (cols, List.map (fun (i, e) -> i, plan_expr e) assigns)
;;

let plan_insert ~table_meta ~ordinals ~values ~on_conflict ~returning ~upsert_update =
  let plan_upsert = plan_upsert_update upsert_update in
  Plan.Op_insert
    { table_meta
    ; ordinals
    ; values = List.map (List.map plan_expr) values
    ; on_conflict
    ; returning = List.map plan_expr returning
    ; upsert_update = plan_upsert
    }
;;

let plan_create_index
      ~name
      ~table_meta
      ~col_sqls
      ~col_expr_flags
      ~where_expr
      ~where_ast
      ~unique
      ~if_not_exists
  =
  let tree_id_ci, _, _, _ = Cat.row_storage table_meta in
  Plan.Op_create_index
    { name
    ; table = table_meta.Cat.name
    ; tree_id = tree_id_ci
    ; col_sqls
    ; col_expr_flags
    ; where_expr = Option.map plan_expr where_expr
    ; where_sql = Option.map Ast.expr_to_sql where_ast
    ; unique
    ; columns = table_meta.Cat.columns
    ; if_not_exists
    }
;;

let plan_update cat ~table_meta ~assignments ~where ~order ~limit ~offset ~returning =
  Plan.Op_update
    { table_meta
    ; assignments = List.map (fun (i, e) -> i, plan_expr e) assignments
    ; where = Option.map plan_expr where
    ; seek = plan_dml_seek cat ~table_meta ~where
    ; order = plan_order_keys order
    ; limit
    ; offset
    ; indexes = indexes_of cat table_meta
    ; returning = List.map plan_expr returning
    }
;;

let plan_delete cat ~table_meta ~where ~order ~limit ~offset ~returning =
  Plan.Op_delete
    { table_meta
    ; where = Option.map plan_expr where
    ; seek = plan_dml_seek cat ~table_meta ~where
    ; order = plan_order_keys order
    ; limit
    ; offset
    ; indexes = indexes_of cat table_meta
    ; returning = List.map plan_expr returning
    }
;;

(* SELECT planning without a catalog: no index lookups and no index-based NLJ;
   builds a hash-join + filter chain manually. *)
let plan_select_no_cat
      ~table_meta
      ~table_alias
      ~proj
      ~expr_proj
      ~where
      ~order
      ~limit
      ~offset
      ~joins
      ~group_by
      ~aggs
      ~having
      ~agg_proj
      ~distinct
      ~windows
      ~agg_windows
      ~agg_order_keys
  =
  let after_joins = chain_joins_no_cat ~table_meta ~table_alias ~joins in
  let filtered =
    match where with
    | None -> after_joins
    | Some e -> Plan.Op_filter { pred = plan_expr e; child = after_joins }
  in
  let n_input_cols_no_cat =
    List.length table_meta.Cat.columns
    + List.fold_left
        (fun acc (bj : Sema.bound_join) ->
           acc + List.length bj.Sema.right_meta.Cat.columns)
        0
        joins
  in
  let after_window_no_cat =
    if windows = []
    then filtered
    else
      Plan.Op_window
        { child = filtered
        ; windows = List.map plan_window_item windows
        ; n_input_cols = n_input_cols_no_cat
        }
  in
  let is_aggregated = aggs <> [] || group_by <> [] in
  let make_sort child =
    let keys = plan_order_keys order in
    if keys = [] then child else Plan.Op_sort { keys; child }
  in
  let after_sort =
    if is_aggregated then after_window_no_cat else make_sort after_window_no_cat
  in
  let projected =
    plan_projection
      ~is_aggregated
      ~after_sort
      ~group_by
      ~aggs
      ~having
      ~agg_proj
      ~agg_windows
      ~expr_proj
      ~proj
      ~windows
      ~n_input_cols:n_input_cols_no_cat
  in
  let sorted =
    if not is_aggregated
    then projected
    else if agg_order_keys <> []
    then plan_agg_order_hidden ~agg_order_keys ~projected
    else make_sort projected
  in
  finalize_select ~distinct ~limit ~offset sorted
;;

(* PRAGMA table_info rows: one row per column (cid, name, type, notnull,
   dflt_value, pk). *)
let pragma_table_info_rows cat table_name =
  match cat with
  | None -> []
  | Some c ->
    (match Cat.find_table_cached c ~name:table_name with
     | None -> []
     | Some meta ->
       List.mapi
         (fun i (col : Row.column) ->
            [| Row.V_int (Int64.of_int i)
             ; Row.V_text col.name
             ; Row.V_text
                 (match col.ty with
                  | Row.Integer -> "INTEGER"
                  | Row.Text -> "TEXT"
                  | Row.Real -> "REAL"
                  | Row.Blob -> "BLOB")
             ; Row.V_int (if col.not_null then 1L else 0L)
             ; Row.V_null
             ; (* dflt_value — simplified *)
               Row.V_int (if col.primary_key then 1L else 0L)
            |])
         meta.columns)
;;

(* PRAGMA foreign_key_list rows, in SQLite column order: id, seq, table
   (parent), from (local), to (parent col), on_update, on_delete, match. *)
let pragma_fk_list_rows cat table_name =
  let fks =
    match cat with
    | None -> []
    | Some c ->
      (match Cat.find_table_cached c ~name:table_name with
       | None -> []
       | Some meta -> meta.Cat.fk_constraints)
  in
  List.mapi
    (fun i (fk : Cat.fk_constraint) ->
       [| Row.V_int (Int64.of_int i)
        ; Row.V_int 0L
        ; (* seq: always 0 for single-col FKs *)
          Row.V_text fk.Cat.fk_parent_table
        ; Row.V_text (String.concat "," fk.Cat.fk_local_cols)
        ; Row.V_text (String.concat "," fk.Cat.fk_parent_cols)
        ; Row.V_text (fk_action_str fk.Cat.fk_on_update)
        ; Row.V_text (fk_action_str fk.Cat.fk_on_delete)
        ; Row.V_text "NONE"
       |]
       (* match: always NONE *))
    fks
;;

(* Rows for the result-producing PRAGMAs (those not handled as Op_pragma_*
   in [plan_pragma]). *)
let plan_pragma_rows cat kind =
  match kind with
  | Ast.Pragma_table_info table_name -> pragma_table_info_rows cat table_name
  | Ast.Pragma_index_list table_name ->
    let idxs =
      match cat with
      | None -> []
      | Some c -> Cat.indexes_for_table c ~table:table_name
    in
    List.mapi
      (fun i (idx : Cat.index_info) ->
         [| Row.V_int (Int64.of_int i)
          ; Row.V_text idx.idx_name
          ; Row.V_int (if idx.idx_unique then 1L else 0L)
         |])
      idxs
  | Ast.Pragma_foreign_key_list table_name -> pragma_fk_list_rows cat table_name
  | Ast.Pragma_journal_mode -> [ [| Row.V_text "delete" |] ]
  | Ast.Pragma_set _ -> [] (* no-op setter: return empty result *)
  | Ast.Pragma_user_version
  | Ast.Pragma_user_version_set _
  | Ast.Pragma_integrity_check
  | Ast.Pragma_not_null_check
  | Ast.Pragma_not_null_repair
  | Ast.Pragma_foreign_keys
  | Ast.Pragma_foreign_keys_set _
  | Ast.Pragma_recursive_triggers
  | Ast.Pragma_recursive_triggers_set _
  | Ast.Pragma_defer_foreign_keys
  | Ast.Pragma_defer_foreign_keys_set _
  | Ast.Pragma_wal_checkpoint
  | Ast.Pragma_checkpoint_status
  | Ast.Pragma_wal_replay_check
  | Ast.Pragma_wal_autocheckpoint
  | Ast.Pragma_wal_autocheckpoint_set _
  | Ast.Pragma_synchronous
  | Ast.Pragma_synchronous_set _
  | Ast.Pragma_wal_batch_commits
  | Ast.Pragma_wal_batch_commits_set _
  | Ast.Pragma_wal_batch_interval_ms
  | Ast.Pragma_wal_batch_interval_ms_set _
  | Ast.Pragma_database_list
  | Ast.Pragma_active_database
  | Ast.Pragma_active_database_set _ ->
    assert false (* handled by outer match in plan_pragma *)
;;

let plan_pragma cat kind =
  match kind with
  | Ast.Pragma_user_version -> Plan.Op_pragma_get_user_version
  | Ast.Pragma_user_version_set v -> Plan.Op_pragma_set_user_version { version = v }
  | Ast.Pragma_integrity_check -> Plan.Op_pragma_integrity_check
  (* #563 *)
  | Ast.Pragma_not_null_check -> Plan.Op_pragma_not_null_check
  | Ast.Pragma_not_null_repair -> Plan.Op_pragma_not_null_repair
  | Ast.Pragma_foreign_keys -> Plan.Op_pragma_get_fk
  | Ast.Pragma_foreign_keys_set on -> Plan.Op_pragma_set_fk { on }
  | Ast.Pragma_recursive_triggers -> Plan.Op_pragma_get_recursive_triggers
  | Ast.Pragma_recursive_triggers_set on -> Plan.Op_pragma_set_recursive_triggers { on }
  | Ast.Pragma_defer_foreign_keys -> Plan.Op_pragma_get_defer_fk
  | Ast.Pragma_defer_foreign_keys_set on -> Plan.Op_pragma_set_defer_fk { on }
  | Ast.Pragma_wal_checkpoint -> Plan.Op_pragma_wal_checkpoint
  | Ast.Pragma_checkpoint_status -> Plan.Op_pragma_checkpoint_status
  | Ast.Pragma_wal_replay_check -> Plan.Op_pragma_wal_replay_check
  | Ast.Pragma_wal_autocheckpoint -> Plan.Op_pragma_get_wal_autocheckpoint
  | Ast.Pragma_wal_autocheckpoint_set n -> Plan.Op_pragma_set_wal_autocheckpoint { n }
  | Ast.Pragma_synchronous -> Plan.Op_pragma_get_synchronous
  | Ast.Pragma_synchronous_set mode -> Plan.Op_pragma_set_synchronous { mode }
  | Ast.Pragma_wal_batch_commits -> Plan.Op_pragma_get_wal_batch_commits
  | Ast.Pragma_wal_batch_commits_set n -> Plan.Op_pragma_set_wal_batch_commits { n }
  | Ast.Pragma_wal_batch_interval_ms -> Plan.Op_pragma_get_wal_batch_interval_ms
  | Ast.Pragma_wal_batch_interval_ms_set n ->
    Plan.Op_pragma_set_wal_batch_interval_ms { n }
  | Ast.Pragma_database_list -> Plan.Op_database_list
  | Ast.Pragma_active_database -> Plan.Op_active_database_get
  | Ast.Pragma_active_database_set s -> Plan.Op_active_database_set { schema = s }
  | _ -> Plan.Op_pragma_rows { rows = plan_pragma_rows cat kind }
;;

let rec plan ?cat = function
  | Sema.BS_col_create_table { name; columns; if_not_exists } ->
    Plan.Op_col_create_table { name; columns; if_not_exists }
  | Sema.BS_create_table
      { name
      ; columns
      ; uniq_idxs
      ; if_not_exists
      ; fk_constraints
      ; without_rowid
      ; autoincrement
      } ->
    Plan.Op_create_table
      { name
      ; columns
      ; uniq_idxs
      ; if_not_exists
      ; fk_constraints
      ; without_rowid
      ; autoincrement
      }
  | Sema.BS_insert_select { table_meta; ordinals; source; on_conflict; upsert_update } ->
    Plan.Op_insert_select
      { table_meta
      ; ordinals
      ; source = plan ?cat source
      ; on_conflict
      ; upsert_update = plan_upsert_update upsert_update
      }
  | Sema.BS_insert { table_meta; ordinals; values; on_conflict; returning; upsert_update }
    -> plan_insert ~table_meta ~ordinals ~values ~on_conflict ~returning ~upsert_update
  | Sema.BS_select
      { distinct
      ; table_meta
      ; table_alias
      ; proj
      ; expr_proj
      ; where
      ; order
      ; limit
      ; offset
      ; joins
      ; group_by
      ; aggs
      ; having
      ; agg_proj
      ; windows
      ; agg_windows
      ; agg_order_keys
      ; agg_out_aliases = _
      } ->
    (match cat with
     | Some cat ->
       plan_select
         cat
         ~table_meta
         ~table_alias
         ~proj
         ~expr_proj
         ~where
         ~order
         ~limit
         ~offset
         ~joins
         ~group_by
         ~aggs
         ~having
         ~agg_proj
         ~distinct
         ~windows
         ~agg_windows
         ~agg_order_keys
     | None ->
       (* Backwards-compatible path: no catalog → no index lookup, and
          (for JOIN) no index-based NLJ. *)
       plan_select_no_cat
         ~table_meta
         ~table_alias
         ~proj
         ~expr_proj
         ~where
         ~order
         ~limit
         ~offset
         ~joins
         ~group_by
         ~aggs
         ~having
         ~agg_proj
         ~distinct
         ~windows
         ~agg_windows
         ~agg_order_keys)
  | Sema.BS_create_index
      { name
      ; table_meta
      ; col_sqls
      ; col_expr_flags
      ; where_expr
      ; where_ast
      ; unique
      ; if_not_exists
      } ->
    plan_create_index
      ~name
      ~table_meta
      ~col_sqls
      ~col_expr_flags
      ~where_expr
      ~where_ast
      ~unique
      ~if_not_exists
  | Sema.BS_update { table_meta; assignments; where; order; limit; offset; returning } ->
    plan_update cat ~table_meta ~assignments ~where ~order ~limit ~offset ~returning
  | Sema.BS_delete { table_meta; where; order; limit; offset; returning } ->
    plan_delete cat ~table_meta ~where ~order ~limit ~offset ~returning
  | Sema.BS_seq_write (Sema.Seq_set { table; seq }) -> Plan.Op_seq_set { table; seq }
  | Sema.BS_seq_write (Sema.Seq_reset { table }) -> Plan.Op_seq_reset { table }
  | Sema.BS_drop_table { table_meta; _ } ->
    Plan.Op_drop_table { table_meta; indexes = indexes_of cat table_meta }
  | Sema.BS_drop_index { idx_info; _ } -> Plan.Op_drop_index { idx_info }
  | Sema.BS_alter_table { table_meta; action } ->
    Plan.Op_alter_table { table_meta; action }
  | Sema.BS_begin -> Plan.Op_begin
  | Sema.BS_commit -> Plan.Op_commit
  | Sema.BS_rollback -> Plan.Op_rollback
  | Sema.BS_savepoint name -> Plan.Op_savepoint name
  | Sema.BS_release name -> Plan.Op_release name
  | Sema.BS_rollback_to name -> Plan.Op_rollback_to name
  | Sema.BS_create_fts_table { name; columns } ->
    Plan.Op_create_fts_table { name; columns }
  | Sema.BS_fts_insert { fts_meta; col_names; col_values; rowid_value } ->
    Plan.Op_fts_insert
      { fts_meta
      ; col_names
      ; col_values = List.map plan_expr col_values
      ; rowid_value = Option.map plan_expr rowid_value
      }
  | Sema.BS_fts_delete { fts_meta; where } ->
    Plan.Op_fts_delete { fts_meta; where = Option.map plan_expr where }
  | Sema.BS_fts_seq_scan { fts_meta; where; limit; offset } ->
    let base = Plan.Op_fts_seq_scan { fts_meta; where = Option.map plan_expr where } in
    finalize_select ~distinct:false ~limit ~offset base
  | Sema.BS_fts_match_scan
      { fts_meta; query; proj; include_rank; snippets; limit; offset } ->
    (* #687: limit/offset are threaded into the op itself rather than
       wrapped on with [finalize_select]'s [Op_limit] — [stream_fts_match_scan]
       slices the score-sorted match list to the [offset, offset+limit) window
       BEFORE fetching content for each match, so an outer [Op_limit] would
       only slice a stream whose expensive work was already fully paid for. *)
    Plan.Op_fts_match_scan
      { fts_meta; query; proj; include_rank; snippets; limit; offset }
  | Sema.BS_compound { op; left; right; order; limit; offset } ->
    plan_compound ?cat ~op ~left ~right ~order ~limit ~offset ()
  | Sema.BS_const_select { exprs } ->
    (match exprs with
     | [ (Sema.BE_func (Ast.Fn_changes, []), _) ] -> Plan.Op_changes
     | [ (Sema.BE_func (Ast.Fn_last_insert_rowid, []), _) ] -> Plan.Op_last_insert_rowid
     | [ (Sema.BE_func (Ast.Fn_total_changes, []), _) ] -> Plan.Op_total_changes
     | _ ->
       Plan.Op_const_select
         { exprs = List.map (fun (e, alias) -> plan_expr e, alias) exprs })
  | Sema.BS_pragma { kind } -> plan_pragma cat kind
  | Sema.BS_with_cte { name; def; query; recursive } ->
    Plan.Op_with_cte
      { cte_name = name; def = plan ?cat def; query = plan ?cat query; recursive }
  | Sema.BS_create_view { name; query } -> Plan.Op_create_view { name; query }
  | Sema.BS_create_reactive_view { name; query; refresh } ->
    Plan.Op_create_reactive_view { name; query; refresh }
  | Sema.BS_drop_view { name } -> Plan.Op_drop_view { name }
  | Sema.BS_drop_reactive_view { name; if_exists } ->
    Plan.Op_drop_reactive_view { name; if_exists }
  | Sema.BS_create_trigger { name; timing; event; table; when_; body } ->
    Plan.Op_create_trigger { name; timing; event; table; when_; body }
  | Sema.BS_drop_trigger { name } -> Plan.Op_drop_trigger { name }
  | Sema.BS_no_op -> Plan.Op_no_op
  | Sema.BS_explain { analyze; inner } ->
    Plan.Op_explain { analyze; inner = plan ?cat inner }
  | Sema.BS_vacuum -> Plan.Op_vacuum
  | Sema.BS_attach { path; schema } -> Plan.Op_attach { path; schema }
  | Sema.BS_detach { schema } -> Plan.Op_detach { schema }

(* Plan a set operation (UNION/INTERSECT/EXCEPT), recursively planning each
   side, then applying ORDER BY / LIMIT.  Part of [plan]'s recursive group. *)
and plan_compound ?cat ~op ~left ~right ~order ~limit ~offset () =
  let l = plan ?cat left in
  let r = plan ?cat right in
  let base =
    match op with
    | Ast.Union -> Plan.Op_union { all = false; left = l; right = r }
    | Ast.Union_all -> Plan.Op_union { all = true; left = l; right = r }
    | Ast.Intersect -> Plan.Op_intersect { left = l; right = r }
    | Ast.Except -> Plan.Op_except { left = l; right = r }
  in
  let sorted =
    if order = []
    then base
    else Plan.Op_sort { keys = plan_order_keys order; child = base }
  in
  match limit with
  | None -> sorted
  | Some n ->
    let off = Option.value ~default:0 offset in
    Plan.Op_limit { limit = n; offset = off; child = sorted }
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
