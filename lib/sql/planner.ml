module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row

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

(** #516: the WHERE equalities that pin a column of the join's {i right} table.
    Column ordinals in a bound WHERE clause address the combined row, so the
    right table's own columns are the [n_right_cols] slots at [right_offset];
    the result re-bases them to right-table ordinals. *)
let right_table_eqs ~right_offset ~n_right_cols cs =
  List.filter_map
    (fun c ->
       match recognise_eq_col_lit c with
       | Some (col_idx, e)
         when col_idx >= right_offset && col_idx < right_offset + n_right_cols ->
         Some (col_idx - right_offset, e)
       | Some _ | None -> None)
    cs
;;

(** Pick the candidate index of [right_meta] whose probe key covers the most
    columns. *)
let best_probe cat (right_meta : Cat.table_meta) ~join_col ~left_col ~right_eqs =
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
         (fun (bi, bp) (i, p) -> if List.length p > List.length bp then i, p else bi, bp)
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

(** Order two [`Orderable] bounds for the same column.  [Float.compare] is the
    same total order {!Granary_sql.Exec.compare_values} applies in the residual
    predicate and {!Granary_encoding.Index_key.encode_value} encodes, so the
    fold can never pick a bound the predicate and the key order disagree
    about.  #527: a mixed int/real pair is ordered through [Int64.to_float],
    which above 2^53 can name the wrong one "tightest".  That is a perf
    question, not a soundness one — the fold only ever picks between candidates
    that are each individually a sound bound, and no conjunct is marked
    consumed, so a wrong pick loses a narrowing and never a row. *)
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
let make_scan (meta : Cat.table_meta) : Plan.op =
  match meta.Cat.storage with
  | Cat.Columnar _ -> Plan.Op_col_seq_scan { table_meta = meta }
  | Cat.Row { tree_id = -1; _ } ->
    Plan.Op_cte_scan { cte_name = meta.Cat.name; n_cols = List.length meta.Cat.columns }
  | Cat.Row { tree_id = -2; _ } -> Plan.Op_sqlite_master
  | Cat.Row { tree_id = -3; _ } -> Plan.Op_sqlite_sequence
  | Cat.Row _ -> Plan.Op_seq_scan { table_meta = meta }
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
    or columnar right table estimates as {!unbounded_rows}, the ratio can never
    hold, and without the floor such a join could never take a probe.

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

let range_seek_rows = 100

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

(** #520: does [idx_tree] belong to a UNIQUE index of [meta] whose every column
    is pinned by [keys]?  Such a seek reaches at most one row.

    A UNIQUE index permits any number of NULLs, so a full-key seek whose key
    includes one can reach many index entries — hence the [not_null] check,
    which keeps the estimate from answering 1 in the direction this module's
    bias argues against.

    {b The implicit PRIMARY KEY index is exempt from it.}  [Sema.mark_table_pk]
    marks only a table-level [PRIMARY KEY] naming a SINGLE column, so both
    columns of [PRIMARY KEY (w, o)] carry [primary_key = false] and hence
    [not_null = false] — and the composite-PK point lookup is precisely the
    #508/#516 shape this whole series exists for.  Without the exemption a seek
    that reaches exactly one row was estimated at the table's high-water mark
    and lost its probe: review of PR #526 measured 201 rows examined against 2.
    The PK index is the table's identity, so its full-key seek is the one-row
    class whether or not the columns happen to be marked.

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
  Cat.indexes_for_table cat ~table:meta.Cat.name
  |> List.exists (fun (i : Cat.index_info) ->
    i.Cat.idx_tree_id = idx_tree
    && i.Cat.idx_unique
    && List.length i.Cat.idx_columns = List.length keys
    && (all_not_null || i.Cat.idx_origin = `Implicit_pk))
;;

(** #520: estimate how many rows [op] produces, statically.

    Only the three shapes a driving side can actually take are classified.
    [chain_joins] hands {!plan_join} either the base access path — which
    [plan_base] builds as a bare scan or seek when the query has joins, never
    wrapped in a filter — or a previous join, so nothing else reaches here.
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
      else if Option.is_some range
      then range_seek_rows
      else unbounded_rows
    in
    min seek (table_rows_estimate table_meta)
  | Plan.Op_seq_scan { table_meta } -> table_rows_estimate table_meta
  | _ -> unbounded_rows
;;

(** #520: is a nested-loop probe worth it, given [driving_rows] estimated left
    rows and a right table of [right_rows]?

    A probe costs one seek per driving row; the hash join it replaces costs one
    read per right-table row, plus the same driving rows either way.  So below
    {!nlj_min_driving_rows} the probe always wins, and above it the comparison
    is against the right table's size scaled by {!nlj_probe_cost_ratio}.

    The ratio is written as a division rather than [driving_rows * ratio >
    right_rows] because [driving_rows] can be {!unbounded_rows} = [max_int],
    which that multiplication would overflow into a negative number and silently
    invert the test.  An unbounded estimate on {i either} side also fails the
    comparison outright, so "we have no idea how big this is" lands on the hash
    join, which is the bounded-loss choice. *)
let probe_is_worth_it ~driving_rows ~right_rows =
  driving_rows <= nlj_min_driving_rows
  || (driving_rows < unbounded_rows
      && right_rows < unbounded_rows
      && driving_rows <= right_rows / nlj_probe_cost_ratio)
;;

(** Plan a JOIN.  [left_op] produces left-table rows; we wrap it with
    either Op_nested_loop_join (when the right table has an index the join can
    probe) or Op_hash_join (otherwise).  If the ON predicate is not a simple
    equality between a left and a right column, fall back to a hash cartesian
    product wrapped in an Op_filter.

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
  (* #520: neither side of the cost comparison depends on which strategy is
     chosen or on which column the ON predicate resolves to — compute both once,
     outside the match. *)
  let driving_rows = estimate_rows cat left_op in
  let right_rows = table_rows_estimate bj.right_meta in
  let mk_with_left_col_right_col left_col right_col : Plan.op =
    let probe =
      if probe_is_worth_it ~driving_rows ~right_rows
      then best_probe cat bj.right_meta ~join_col:right_col ~left_col ~right_eqs
      else None
    in
    match probe with
    | Some (idx, probe) ->
      Plan.Op_nested_loop_join
        { left = left_op
        ; right_meta = bj.right_meta
        ; idx_tree = idx.Cat.idx_tree_id
        ; probe
        ; join_kind
        ; right_col_offset = right_offset
        ; n_right_cols
        }
    | None ->
      Plan.Op_hash_join
        { left = left_op
        ; right = make_scan bj.right_meta
        ; left_key = left_col
        ; right_key = right_col
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
    (* General ON predicate: cartesian hash-join + post-filter. *)
    let cart =
      Plan.Op_hash_join
        { left = left_op
        ; right = make_scan bj.right_meta
        ; left_key = -1
        ; right_key = -1
        ; join_kind
        ; right_col_offset = right_offset
        ; n_right_cols
        }
    in
    Plan.Op_filter { pred = plan_expr bj.on; child = cart }
;;

let sema_agg_to_plan (a : Sema.agg_spec) : Plan.agg_spec =
  { Plan.func = a.func; col_ord = a.col_ord }
;;

let sema_agg_proj_to_plan : Sema.agg_proj_item -> Plan.proj_item = function
  | Sema.AP_group_col i -> Plan.PI_group_col i
  | Sema.AP_agg_slot i -> Plan.PI_agg_slot i
  | Sema.AP_window_slot i -> Plan.PI_window_slot i
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
  | e' -> e'
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
          range_for_index table_meta idx ~n_eq:(List.length prefix) conjuncts_list
        in
        Some (Plan.Seek_index { idx_tree = idx.Cat.idx_tree_id; keys; range }, consumed))
;;

(* Realise a chosen seek as the base-table plan op it reads through. *)
let seek_op (table_meta : Cat.table_meta) = function
  | Plan.Seek_rowid lookup_val -> Plan.Op_rowid_lookup { table_meta; lookup_val }
  | Plan.Seek_index { idx_tree; keys; range } ->
    let tree_id_pl, _, _, _ = Cat.row_storage table_meta in
    Plan.Op_index_lookup { table_tree = tree_id_pl; idx_tree; keys; range; table_meta }
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
let plan_base cat ~table_meta ~where ~has_joins =
  match where with
  | None -> make_scan table_meta
  | Some e ->
    let cs = conjuncts e in
    if has_joins
    then (
      let n_base = List.length table_meta.Cat.columns in
      match choose_access_path cat table_meta (base_only_conjuncts ~n_base cs) with
      | None -> make_scan table_meta
      | Some (seek, _consumed) -> seek_op table_meta seek)
    else (
      let fallback () =
        Plan.Op_filter { pred = plan_expr e; child = make_scan table_meta }
      in
      match choose_access_path cat table_meta cs with
      | None -> fallback ()
      | Some (seek, consumed) -> residual_filter ~consumed cs (seek_op table_meta seek))
;;

(* #508: the narrowing path for a DML WHERE clause.  Unlike [plan_base] this
   discards which conjuncts were consumed — the write path always re-evaluates
   the whole predicate on every candidate row, so the seek is a pure
   restriction of what gets read. *)
let plan_dml_seek cat ~table_meta ~where =
  match cat, where with
  | Some c, Some e -> Option.map fst (choose_access_path c table_meta (conjuncts e))
  | _ -> None
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
      ; proj = List.map sema_agg_proj_to_plan agg_proj
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

(* Post-aggregation ORDER BY: ORDER BY col indices are in pre-aggregation
   space, so remap each P_col to its position in the aggregated output. *)
let plan_post_agg_sort ~group_by ~agg_proj ~order ~projected =
  let plan_proj = List.map sema_agg_proj_to_plan agg_proj in
  let find_idx pred lst =
    let rec go k = function
      | [] -> None
      | x :: rest -> if pred x then Some k else go (k + 1) rest
    in
    go 0 lst
  in
  let remap_e e =
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
          | None -> e))
    | _ -> e
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
         let e = plan_expr bkey.key in
         let e' = remap_e e in
         e', dir, nulls)
      order
  in
  if keys = [] then projected else Plan.Op_sort { keys; child = projected }
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
let chain_joins_no_cat ~(table_meta : Cat.table_meta) ~joins =
  let base = make_scan table_meta in
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
                ; right = make_scan bj.right_meta
                ; left_key = a
                ; right_key = b - right_offset
                ; join_kind
                ; right_col_offset = right_offset
                ; n_right_cols
                }
            | Some (a, b) when b < n_left && a >= right_offset ->
              Plan.Op_hash_join
                { left = op
                ; right = make_scan bj.right_meta
                ; left_key = b
                ; right_key = a - right_offset
                ; join_kind
                ; right_col_offset = right_offset
                ; n_right_cols
                }
            | _ ->
              let cart =
                Plan.Op_hash_join
                  { left = op
                  ; right = make_scan bj.right_meta
                  ; left_key = -1
                  ; right_key = -1
                  ; join_kind
                  ; right_col_offset = right_offset
                  ; n_right_cols
                  }
              in
              Plan.Op_filter { pred = plan_expr bj.on; child = cart }
          in
          joined, n_left + n_right_cols)
       (base, List.length table_meta.columns)
       joins)
;;

let plan_select
      cat
      ~table_meta
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
  let base = plan_base cat ~table_meta ~where ~has_joins in
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
    if keys = [] then child else Plan.Op_sort { keys; child }
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
    then plan_post_agg_sort ~group_by ~agg_proj ~order ~projected
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

let plan_insert ~table_meta ~ordinals ~values ~on_conflict ~returning ~upsert_update =
  let plan_upsert =
    match upsert_update with
    | None -> None
    | Some (cols, assigns) -> Some (cols, List.map (fun (i, e) -> i, plan_expr e) assigns)
  in
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
  =
  let after_joins = chain_joins_no_cat ~table_meta ~joins in
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
  let sorted = if is_aggregated then make_sort projected else projected in
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
  | Ast.Pragma_foreign_keys
  | Ast.Pragma_foreign_keys_set _
  | Ast.Pragma_recursive_triggers
  | Ast.Pragma_recursive_triggers_set _
  | Ast.Pragma_defer_foreign_keys
  | Ast.Pragma_defer_foreign_keys_set _
  | Ast.Pragma_wal_checkpoint
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
  | Ast.Pragma_foreign_keys -> Plan.Op_pragma_get_fk
  | Ast.Pragma_foreign_keys_set on -> Plan.Op_pragma_set_fk { on }
  | Ast.Pragma_recursive_triggers -> Plan.Op_pragma_get_recursive_triggers
  | Ast.Pragma_recursive_triggers_set on -> Plan.Op_pragma_set_recursive_triggers { on }
  | Ast.Pragma_defer_foreign_keys -> Plan.Op_pragma_get_defer_fk
  | Ast.Pragma_defer_foreign_keys_set on -> Plan.Op_pragma_set_defer_fk { on }
  | Ast.Pragma_wal_checkpoint -> Plan.Op_pragma_wal_checkpoint
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
  | Sema.BS_insert_select { table_meta; ordinals; source; on_conflict } ->
    Plan.Op_insert_select { table_meta; ordinals; source = plan ?cat source; on_conflict }
  | Sema.BS_insert { table_meta; ordinals; values; on_conflict; returning; upsert_update }
    -> plan_insert ~table_meta ~ordinals ~values ~on_conflict ~returning ~upsert_update
  | Sema.BS_select
      { distinct
      ; table_meta
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
      } ->
    (match cat with
     | Some cat ->
       plan_select
         cat
         ~table_meta
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
     | None ->
       (* Backwards-compatible path: no catalog → no index lookup, and
          (for JOIN) no index-based NLJ. *)
       plan_select_no_cat
         ~table_meta
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
         ~agg_windows)
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
  | Sema.BS_fts_seq_scan { fts_meta; where } ->
    Plan.Op_fts_seq_scan { fts_meta; where = Option.map plan_expr where }
  | Sema.BS_fts_match_scan { fts_meta; query; proj; include_rank; snippets } ->
    Plan.Op_fts_match_scan { fts_meta; query; proj; include_rank; snippets }
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
