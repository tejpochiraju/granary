(* The 22 TPC-H queries, transcribed from the specification's functional query
   definitions (§2.4.1–§2.4.22) with the substitution parameters bound to the
   spec's *validation* values, so that every run is comparable and the SQLite
   cross-check has a stable target.

   Two deliberate, documented departures from the spec text:

   - Date arithmetic of the form [DATE '1998-12-01' - INTERVAL '90' DAY] is
     constant-folded to the literal date it evaluates to.  This is arithmetic on
     the spec's own constants, not a dialect workaround.  Our schema stores dates
     as TEXT 'YYYY-MM-DD', which orders correctly under lexicographic
     comparison.
   - Q15's CREATE VIEW is carried in [setup] rather than in [sql], because the
     spec itself defines Q15 as three statements of which only the middle one is
     the measured query.  The trailing DROP VIEW is omitted from [sql]; the
     database is NOT discarded between queries, so the harness drops the view
     itself — see {!drop_setup_sql} below for the mechanism and why it runs
     both before setup and after the query.

   Task 8a then applied four *mechanical, meaning-preserving* rewrites, each
   verified against reference SQLite by the runner's answer cross-check:

   - [FROM a, b WHERE a.x = b.y] becomes [FROM a INNER JOIN b ON a.x = b.y].
     granary's FROM clause takes one table plus explicit JOIN clauses
     (lib/sql/parser.mly:812, :831), so no comma-separated FROM list parsed
     (#486).  Join predicates moved to ON; filter predicates stayed in WHERE.
   - [EXTRACT(YEAR FROM d)] becomes [CAST(strftime('%Y', d) AS INTEGER)], and
     [SUBSTRING(s FROM 1 FOR 2)] becomes [substr(s, 1, 2)].  Both spec forms are
     rejected by reference SQLite as well, so neither is a granary-only gap.
     Dates are TEXT 'YYYY-MM-DD', so the strftime form is exact.
   - An outer column referenced from inside a correlated subquery is written
     qualified ([orders.o_orderkey], not [o_orderkey]).  granary silently fails
     to bind the unqualified form to the outer row and returns a wrong answer
     (#485); the qualified form means the same thing to any conforming engine.
   - Q15's view column list moved into the SELECT's own aliases; granary's
     grammar had no [CREATE VIEW v (c1, c2) AS ...] form.  THIS ONE HAS BEEN
     WITHDRAWN: #491 added the form, and Q15's setup now carries the spec's
     own column list again.  It is left on the list because the other three
     are still applied, and because a reader comparing this file against the
     spec should be able to see which differences were once deliberate.

   These rewrites were applied to every query they touch, including queries for
   which granary still does not produce the right answer.  Such a query carries
   [Rewritten_pending] — CSV token ["rewritten-not-yet-running"] — never
   [Native], so that neither a reader of the benchmark CSV nor a later reviewer
   can mistake its transformed SQL for untouched spec text.

   Task 8b then applied one further lever and settled every verdict.  A view
   named in FROM position is rewritten into a CTE by [Sema.bind_internal]
   (lib/sql/sema.ml:3777), so a [CREATE VIEW] in a query's [setup] can carry
   constructs the query itself may not use:

   - a row-level measure such as [l_extendedprice * (1 - l_discount)] projected
     under a name, so the query aggregates a plain column reference (#488);
   - the spec's own derived table, hoisted whole, since granary's FROM takes no
     subquery (#486).

   The rule observed throughout: **a setup view may project row-level
   expressions, never an aggregate the query is supposed to compute.**  Setup
   runs outside the timed section, so pre-aggregating there would move the
   measured work out of the measurement.  The single exception is Q13, whose
   spec-defined derived table *is* a per-customer COUNT — and Q13 is Skipped
   regardless.  Q15's [revenue0] is likewise the spec's own view.

   After Task 8b there is no [Native] query left.  Q1, Q6 and Q13 were the last
   verbatim spec SQL and none of the three ran; Q1 and Q6 now run through setup
   views, Q13 is Skipped.

   Verdicts were established at **SF 0.01**, not SF 0.001.  At SF 0.001 most
   queries return 0 rows on both engines, and an all-empty answer cross-checks
   [ok] for reasons that have nothing to do with correctness: Q2 and Q20 passed
   at SF 0.001 and are WRONG at SF 0.01 (#492).  A [Rewritten] verdict here
   therefore means "verified against reference SQLite at SF 0.01 on a non-empty
   result" — with one flagged exception, Q18, whose rationale says so outright.

   Nothing here is adjusted in a way that changes a query's meaning. *)

type verdict =
  | Native
  | Rewritten of string
  | Rewritten_pending of string
  | Skipped of string

type query =
  { number : int
  ; sql : string
  ; setup : string list
  ; verdict : verdict
  }

(* Q1 — Pricing Summary Report.  DELTA = 90, so
   DATE '1998-12-01' - INTERVAL '90' DAY = '1998-09-02'. *)
let q1 =
  { number = 1
  ; setup =
      [ {|CREATE VIEW q1_lineitem AS
      SELECT l_returnflag,
             l_linestatus,
             l_shipdate,
             l_quantity,
             l_extendedprice,
             l_discount,
             l_extendedprice * (1 - l_discount) AS disc_price,
             l_extendedprice * (1 - l_discount) * (1 + l_tax) AS charge
      FROM lineitem|}
      ]
  ; verdict =
      Rewritten
        "the two row-level measures l_extendedprice * (1 - l_discount) and ... * (1 + \
         l_tax) are projected as named columns by a setup view, so the query aggregates \
         plain column references: granary rejects an aggregate whose argument is an \
         expression (#488). No aggregation is moved into the view — the eight \
         aggregates, the GROUP BY and the ORDER BY are all still computed by the \
         measured query. Verified against SQLite at SF 0.01 on a 4-row result."
  ; sql =
      {|SELECT l_returnflag, l_linestatus,
       SUM(l_quantity) AS sum_qty,
       SUM(l_extendedprice) AS sum_base_price,
       SUM(disc_price) AS sum_disc_price,
       SUM(charge) AS sum_charge,
       AVG(l_quantity) AS avg_qty,
       AVG(l_extendedprice) AS avg_price,
       AVG(l_discount) AS avg_disc,
       COUNT(*) AS count_order
FROM q1_lineitem
WHERE l_shipdate <= '1998-09-02'
GROUP BY l_returnflag, l_linestatus
ORDER BY l_returnflag, l_linestatus|}
  }
;;

(* Q2 — Minimum Cost Supplier.  SIZE = 15, TYPE = 'BRASS', REGION = 'EUROPE'.
   The spec asks for the first 100 rows. *)
let q2 =
  { number = 2
  ; setup = []
  ; verdict =
      Rewritten_pending
        "implicit joins rewritten as explicit INNER JOIN ... ON (#486); the correlated \
         MIN subquery's outer reference written as part.p_partkey (#485). Runs on \
         granary but returns the WRONG ANSWER: 0 rows where SQLite returns 4 at SF 0.01. \
         A correlated subquery matches nothing when the outer query's FROM is a join, \
         even with the reference qualified (#492). This read 'ok' at SF 0.001 only \
         because both engines legitimately return 0 rows there."
  ; sql =
      {|SELECT s_acctbal, s_name, n_name, p_partkey, p_mfgr, s_address, s_phone, s_comment
FROM part
INNER JOIN partsupp ON p_partkey = ps_partkey
INNER JOIN supplier ON s_suppkey = ps_suppkey
INNER JOIN nation ON s_nationkey = n_nationkey
INNER JOIN region ON n_regionkey = r_regionkey
WHERE p_size = 15
  AND p_type LIKE '%BRASS'
  AND r_name = 'EUROPE'
  AND ps_supplycost = (
        SELECT MIN(ps_supplycost)
        FROM partsupp
        INNER JOIN supplier ON s_suppkey = ps_suppkey
        INNER JOIN nation ON s_nationkey = n_nationkey
        INNER JOIN region ON n_regionkey = r_regionkey
        WHERE part.p_partkey = ps_partkey
          AND r_name = 'EUROPE')
ORDER BY s_acctbal DESC, n_name, s_name, p_partkey
LIMIT 100|}
  }
;;

(* Q3 — Shipping Priority.  SEGMENT = 'BUILDING', DATE = '1995-03-15'. *)
let q3 =
  { number = 3
  ; setup = []
  ; verdict =
      Skipped
        "ORDER BY on a computed measure has no expressible form — #490 (alias), #495 \
         (aggregate expression), #489 (ordinal, silently ignored). The SQL below carries \
         the #486 join rewrite and the SUM over an expression could be cleared by a \
         setup view as in Q1 (#488), but ORDER BY revenue DESC cannot be written at all"
  ; sql =
      {|SELECT l_orderkey,
       SUM(l_extendedprice * (1 - l_discount)) AS revenue,
       o_orderdate,
       o_shippriority
FROM customer
INNER JOIN orders ON c_custkey = o_custkey
INNER JOIN lineitem ON l_orderkey = o_orderkey
WHERE c_mktsegment = 'BUILDING'
  AND o_orderdate < '1995-03-15'
  AND l_shipdate > '1995-03-15'
GROUP BY l_orderkey, o_orderdate, o_shippriority
ORDER BY revenue DESC, o_orderdate
LIMIT 10|}
  }
;;

(* Q4 — Order Priority Checking.  DATE = '1993-07-01'; the spec's
   DATE + INTERVAL '3' MONTH folds to '1993-10-01'. *)
let q4 =
  { number = 4
  ; setup = []
  ; verdict =
      Rewritten
        "correlated EXISTS outer reference written as orders.o_orderkey: granary never \
         binds an unqualified outer column to the outer row and silently returned 0 rows \
         instead of 5 (#485)"
  ; sql =
      {|SELECT o_orderpriority, COUNT(*) AS order_count
FROM orders
WHERE o_orderdate >= '1993-07-01'
  AND o_orderdate < '1993-10-01'
  AND EXISTS (
        SELECT *
        FROM lineitem
        WHERE l_orderkey = orders.o_orderkey
          AND l_commitdate < l_receiptdate)
GROUP BY o_orderpriority
ORDER BY o_orderpriority|}
  }
;;

(* Q5 — Local Supplier Volume.  REGION = 'ASIA', DATE = '1994-01-01';
   DATE + INTERVAL '1' YEAR folds to '1995-01-01'. *)
let q5 =
  { number = 5
  ; setup = []
  ; verdict =
      Skipped
        "ORDER BY revenue DESC ranks a computed measure, which has no expressible form — \
         #490 (alias), #495 (aggregate expression), #489 (ordinal, silently ignored). \
         The SQL below carries the #486 join rewrite; the SUM over an expression would \
         yield to a setup view as in Q1 (#488), the ordering will not"
  ; sql =
      {|SELECT n_name, SUM(l_extendedprice * (1 - l_discount)) AS revenue
FROM customer
INNER JOIN orders ON c_custkey = o_custkey
INNER JOIN lineitem ON l_orderkey = o_orderkey
INNER JOIN supplier ON l_suppkey = s_suppkey AND c_nationkey = s_nationkey
INNER JOIN nation ON s_nationkey = n_nationkey
INNER JOIN region ON n_regionkey = r_regionkey
WHERE r_name = 'ASIA'
  AND o_orderdate >= '1994-01-01'
  AND o_orderdate < '1995-01-01'
GROUP BY n_name
ORDER BY revenue DESC|}
  }
;;

(* Q6 — Forecasting Revenue Change.  DATE = '1994-01-01' (+1 year =
   '1995-01-01'), DISCOUNT = 0.06, QUANTITY = 24.  The DISCOUNT +/- 0.01 band is
   left unfolded, exactly as the spec writes it. *)
let q6 =
  { number = 6
  ; setup =
      [ {|CREATE VIEW q6_lineitem AS
      SELECT l_shipdate,
             l_discount,
             l_quantity,
             l_extendedprice * l_discount AS disc_revenue
      FROM lineitem|}
      ]
  ; verdict =
      Rewritten
        "the row-level measure l_extendedprice * l_discount is projected as a named \
         column by a setup view so that the query's SUM takes a plain column reference \
         (#488). The SUM itself and every filter stay in the measured query. Verified \
         against SQLite at SF 0.01 on a 1-row result."
  ; sql =
      {|SELECT SUM(disc_revenue) AS revenue
FROM q6_lineitem
WHERE l_shipdate >= '1994-01-01'
  AND l_shipdate < '1995-01-01'
  AND l_discount BETWEEN 0.06 - 0.01 AND 0.06 + 0.01
  AND l_quantity < 24|}
  }
;;

(* Q7 — Volume Shipping.  NATION1 = 'FRANCE', NATION2 = 'GERMANY'. *)
let q7 =
  { number = 7
  ; setup =
      [ {|CREATE VIEW q7_shipping AS
      SELECT n1.n_name AS supp_nation,
             n2.n_name AS cust_nation,
             CAST(strftime('%Y', l_shipdate) AS INTEGER) AS l_year,
             l_extendedprice * (1 - l_discount) AS volume
      FROM supplier
      INNER JOIN lineitem ON s_suppkey = l_suppkey
      INNER JOIN orders ON o_orderkey = l_orderkey
      INNER JOIN customer ON c_custkey = o_custkey
      INNER JOIN nation n1 ON s_nationkey = n1.n_nationkey
      INNER JOIN nation n2 ON c_nationkey = n2.n_nationkey
      WHERE ((n1.n_name = 'FRANCE' AND n2.n_name = 'GERMANY')
          OR (n1.n_name = 'GERMANY' AND n2.n_name = 'FRANCE'))
        AND l_shipdate BETWEEN '1995-01-01' AND '1996-12-31'|}
      ]
  ; verdict =
      Rewritten
        "implicit joins rewritten as explicit INNER JOIN ... ON (#486); EXTRACT(YEAR \
         FROM l_shipdate) rewritten as CAST(strftime('%Y', l_shipdate) AS INTEGER), \
         which reference SQLite rejects in spec form too; the spec's derived table \
         'shipping' hoisted verbatim into a setup view, since granary has no subquery in \
         FROM (#486). The view is the spec's own sub-select, row-level only — the SUM, \
         GROUP BY and ORDER BY are still the measured query. Verified against SQLite at \
         SF 0.01 on a 4-row result."
  ; sql =
      {|SELECT supp_nation, cust_nation, l_year, SUM(volume) AS revenue
FROM q7_shipping
GROUP BY supp_nation, cust_nation, l_year
ORDER BY supp_nation, cust_nation, l_year|}
  }
;;

(* Q8 — National Market Share.  NATION = 'BRAZIL', REGION = 'AMERICA',
   TYPE = 'ECONOMY ANODIZED STEEL'. *)
let q8 =
  { number = 8
  ; setup = []
  ; verdict =
      Skipped
        "the projection SUM(CASE ...) / SUM(volume) is arithmetic over aggregate \
         results, which granary rejects as a complex expression in an aggregated \
         projection — #494. The derived table itself would hoist into a setup view \
         exactly as Q7's and Q9's do (#486), and granary's strftime is confirmed working \
         (Q7), so #494 is the sole remaining blocker"
  ; sql =
      {|SELECT o_year,
       SUM(CASE WHEN nation = 'BRAZIL' THEN volume ELSE 0 END) / SUM(volume) AS mkt_share
FROM (
      SELECT CAST(strftime('%Y', o_orderdate) AS INTEGER) AS o_year,
             l_extendedprice * (1 - l_discount) AS volume,
             n2.n_name AS nation
      FROM part
      INNER JOIN lineitem ON p_partkey = l_partkey
      INNER JOIN supplier ON s_suppkey = l_suppkey
      INNER JOIN orders ON l_orderkey = o_orderkey
      INNER JOIN customer ON o_custkey = c_custkey
      INNER JOIN nation n1 ON c_nationkey = n1.n_nationkey
      INNER JOIN region ON n1.n_regionkey = r_regionkey
      INNER JOIN nation n2 ON s_nationkey = n2.n_nationkey
      WHERE r_name = 'AMERICA'
        AND o_orderdate BETWEEN '1995-01-01' AND '1996-12-31'
        AND p_type = 'ECONOMY ANODIZED STEEL') AS all_nations
GROUP BY o_year
ORDER BY o_year|}
  }
;;

(* Q9 — Product Type Profit Measure.  COLOR = 'green'. *)
let q9 =
  { number = 9
  ; setup =
      [ {|CREATE VIEW q9_profit AS
      SELECT n_name AS nation,
             CAST(strftime('%Y', o_orderdate) AS INTEGER) AS o_year,
             l_extendedprice * (1 - l_discount) - ps_supplycost * l_quantity AS amount
      FROM part
      INNER JOIN lineitem ON p_partkey = l_partkey
      INNER JOIN supplier ON s_suppkey = l_suppkey
      INNER JOIN partsupp ON ps_suppkey = l_suppkey AND ps_partkey = l_partkey
      INNER JOIN orders ON o_orderkey = l_orderkey
      INNER JOIN nation ON s_nationkey = n_nationkey
      WHERE p_name LIKE '%green%'|}
      ]
  ; verdict =
      Skipped
        "granary is OOM-killed on this query at SF 0.01 — #498. The rewrite below is \
         complete and faithful (joins made explicit per #486, EXTRACT as \
         CAST(strftime('%Y', ...) AS INTEGER), the spec's 'profit' derived table hoisted \
         verbatim into the setup view), and reference SQLite answers it in ~0.05 s with \
         168 rows; granary consumes the machine's ~25 GB of free memory on a six-way \
         join over ~60 k lineitem rows and is SIGKILLed. Marked Skipped rather than \
         pending so that the harness is not killed mid-run"
  ; sql =
      {|SELECT nation, o_year, SUM(amount) AS sum_profit
FROM q9_profit
GROUP BY nation, o_year
ORDER BY nation, o_year DESC|}
  }
;;

(* Q10 — Returned Item Reporting.  DATE = '1993-10-01'; +3 months =
   '1994-01-01'.  The spec asks for the first 20 rows. *)
let q10 =
  { number = 10
  ; setup = []
  ; verdict =
      Skipped
        "ORDER BY revenue DESC ranks a computed measure, which has no expressible form — \
         #490 (alias), #495 (aggregate expression), #489 (ordinal, silently ignored). \
         The SQL below carries the #486 join rewrite; the SUM over an expression would \
         yield to a setup view as in Q1 (#488), the ordering will not"
  ; sql =
      {|SELECT c_custkey,
       c_name,
       SUM(l_extendedprice * (1 - l_discount)) AS revenue,
       c_acctbal,
       n_name,
       c_address,
       c_phone,
       c_comment
FROM customer
INNER JOIN orders ON c_custkey = o_custkey
INNER JOIN lineitem ON l_orderkey = o_orderkey
INNER JOIN nation ON c_nationkey = n_nationkey
WHERE o_orderdate >= '1993-10-01'
  AND o_orderdate < '1994-01-01'
  AND l_returnflag = 'R'
GROUP BY c_custkey, c_name, c_acctbal, c_phone, n_name, c_address, c_comment
ORDER BY revenue DESC
LIMIT 20|}
  }
;;

(* Q11 — Important Stock Identification.  NATION = 'GERMANY',
   FRACTION = 0.0001. *)
let q11 =
  { number = 11
  ; setup = []
  ; verdict =
      Skipped
        "ORDER BY value DESC ranks a computed measure, which has no expressible form — \
         #490 (alias), #495 (aggregate expression), #489 (ordinal, silently ignored). \
         The SQL below carries the #486 join rewrite in both the outer query and the \
         HAVING subquery; SUM(ps_supplycost * ps_availqty) would yield to a setup view \
         as in Q1 (#488), the ordering will not"
  ; sql =
      {|SELECT ps_partkey, SUM(ps_supplycost * ps_availqty) AS value
FROM partsupp
INNER JOIN supplier ON ps_suppkey = s_suppkey
INNER JOIN nation ON s_nationkey = n_nationkey
WHERE n_name = 'GERMANY'
GROUP BY ps_partkey
HAVING SUM(ps_supplycost * ps_availqty) > (
        SELECT SUM(ps_supplycost * ps_availqty) * 0.0001
        FROM partsupp
        INNER JOIN supplier ON ps_suppkey = s_suppkey
        INNER JOIN nation ON s_nationkey = n_nationkey
        WHERE n_name = 'GERMANY')
ORDER BY value DESC|}
  }
;;

(* Q12 — Shipping Modes and Order Priority.  SHIPMODE1 = 'MAIL',
   SHIPMODE2 = 'SHIP', DATE = '1994-01-01' (+1 year = '1995-01-01'). *)
let q12 =
  { number = 12
  ; setup =
      [ {|CREATE VIEW q12_lines AS
      SELECT l_shipmode,
             l_commitdate,
             l_receiptdate,
             l_shipdate,
             CASE WHEN o_orderpriority = '1-URGENT' OR o_orderpriority = '2-HIGH'
                  THEN 1 ELSE 0 END AS high_line,
             CASE WHEN o_orderpriority <> '1-URGENT' AND o_orderpriority <> '2-HIGH'
                  THEN 1 ELSE 0 END AS low_line
      FROM orders
      INNER JOIN lineitem ON o_orderkey = l_orderkey|}
      ]
  ; verdict =
      Rewritten
        "implicit joins rewritten as explicit INNER JOIN ... ON (#486); the two \
         row-level CASE expressions are projected as named columns by a setup view so \
         that the query's SUMs take plain column references (#488). The view performs no \
         aggregation and no filtering — both SUMs, the GROUP BY, every WHERE predicate \
         and the ORDER BY remain in the measured query. Verified against SQLite at SF \
         0.01 on a 2-row result."
  ; sql =
      {|SELECT l_shipmode,
       SUM(high_line) AS high_line_count,
       SUM(low_line) AS low_line_count
FROM q12_lines
WHERE l_shipmode IN ('MAIL', 'SHIP')
  AND l_commitdate < l_receiptdate
  AND l_shipdate < l_commitdate
  AND l_receiptdate >= '1994-01-01'
  AND l_receiptdate < '1995-01-01'
GROUP BY l_shipmode
ORDER BY l_shipmode|}
  }
;;

(* Q13 — Customer Distribution.  WORD1 = 'special', WORD2 = 'requests'. *)
let q13 =
  { number = 13
  ; setup =
      [ {|CREATE VIEW q13_c_orders AS
      SELECT c_custkey AS c_custkey, COUNT(o_orderkey) AS c_count
      FROM customer LEFT OUTER JOIN orders
        ON c_custkey = o_custkey
       AND o_comment NOT LIKE '%special%requests%'
      GROUP BY c_custkey|}
      ]
  ; verdict =
      Skipped
        "ORDER BY custdist DESC ranks a computed measure, which has no expressible form \
         — #490 (alias), #495 (aggregate expression), #489 (ordinal, silently ignored). \
         Everything else is now rewritten and reference SQLite runs it (25 rows at SF \
         0.01): the spec's derived table is hoisted into the setup view below and its \
         trailing column list (c_custkey, c_count) moved into the sub-select's own \
         aliases, since granary has neither a subquery in FROM (#486) nor a \
         column-list-carrying alias — reference SQLite rejects the latter too. granary \
         fails with 'unknown column: q13_c_orders.custdist'"
  ; sql =
      {|SELECT c_count, COUNT(*) AS custdist
FROM q13_c_orders
GROUP BY c_count
ORDER BY custdist DESC, c_count DESC|}
  }
;;

(* Q14 — Promotion Effect.  DATE = '1995-09-01'; +1 month = '1995-10-01'. *)
let q14 =
  { number = 14
  ; setup = []
  ; verdict =
      Skipped
        "the projection 100.00 * SUM(CASE ...) / SUM(...) is arithmetic over aggregate \
         results, which granary rejects as a complex expression in an aggregated \
         projection — #494. The SQL below carries the #486 join rewrite; a setup view \
         can clear the aggregates' own expression arguments (#488) but not the division \
         of one aggregate by another"
  ; sql =
      {|SELECT 100.00 * SUM(CASE WHEN p_type LIKE 'PROMO%'
                         THEN l_extendedprice * (1 - l_discount)
                         ELSE 0 END) / SUM(l_extendedprice * (1 - l_discount)) AS promo_revenue
FROM lineitem
INNER JOIN part ON l_partkey = p_partkey
WHERE l_shipdate >= '1995-09-01'
  AND l_shipdate < '1995-10-01'|}
  }
;;

(* Q15 — Top Supplier.  DATE = '1996-01-01'; +3 months = '1996-04-01'.  The spec
   defines Q15 as CREATE VIEW / SELECT / DROP VIEW, of which only the SELECT is
   the measured query, so the view creation belongs in [setup]. *)
let q15 =
  { number = 15
  ; setup =
      [ {|CREATE VIEW revenue0 (supplier_no, total_revenue) AS
      SELECT l_suppkey,
             SUM(l_extendedprice * (1 - l_discount))
      FROM lineitem
      WHERE l_shipdate >= '1996-01-01'
        AND l_shipdate < '1996-04-01'
      GROUP BY l_suppkey|}
      ]
  ; verdict =
      Skipped
        "the spec's own WHERE total_revenue = (SELECT MAX(total_revenue) FROM revenue0) \
         names the view inside a subquery's FROM, which granary silently answers with 0 \
         rows — #496. Two other gaps were cleared and are recorded in the SQL below: the \
         implicit join is now explicit (#486), and the view's SUM over an expression \
         would need the Q1 treatment (#488). A third was cleared in the engine instead: \
         #491 added CREATE VIEW v (c1, c2) AS, so the setup below carries the spec's own \
         column list again rather than the aliases it was rewritten into. granary would \
         also need the FROM operands swapped, because a view is only resolved in leading \
         FROM position (#497)"
  ; sql =
      {|SELECT s_suppkey, s_name, s_address, s_phone, total_revenue
FROM supplier
INNER JOIN revenue0 ON s_suppkey = supplier_no
WHERE total_revenue = (SELECT MAX(total_revenue) FROM revenue0)
ORDER BY s_suppkey|}
  }
;;

(* Q16 — Parts/Supplier Relationship.  BRAND = 'Brand#45',
   TYPE = 'MEDIUM POLISHED', SIZE1..8 = 49, 14, 23, 45, 19, 3, 36, 9. *)
let q16 =
  { number = 16
  ; setup = []
  ; verdict =
      Skipped
        "ORDER BY supplier_cnt DESC has no expressible form — #490/#495/#489. \
         COUNT(DISTINCT ps_suppkey) was the other blocker and is gone: #491 added \
         DISTINCT as an aggregate argument, so the SQL below is now blocked only by the \
         ordering. It also carries the #486 join rewrite"
  ; sql =
      {|SELECT p_brand, p_type, p_size, COUNT(DISTINCT ps_suppkey) AS supplier_cnt
FROM partsupp
INNER JOIN part ON p_partkey = ps_partkey
WHERE p_brand <> 'Brand#45'
  AND p_type NOT LIKE 'MEDIUM POLISHED%'
  AND p_size IN (49, 14, 23, 45, 19, 3, 36, 9)
  AND ps_suppkey NOT IN (
        SELECT s_suppkey
        FROM supplier
        WHERE s_comment LIKE '%Customer%Complaints%')
GROUP BY p_brand, p_type, p_size
ORDER BY supplier_cnt DESC, p_brand, p_type, p_size|}
  }
;;

(* Q17 — Small-Quantity-Order Revenue.  BRAND = 'Brand#23',
   CONTAINER = 'MED BOX'. *)
let q17 =
  { number = 17
  ; setup = []
  ; verdict =
      Skipped
        "two independent blockers. SUM(l_extendedprice) / 7.0 divides an aggregate \
         result by a constant, which granary rejects as a complex expression in an \
         aggregated projection (#494); and the correlated subquery sits under a joined \
         outer FROM, where granary silently matches nothing (#492), so clearing #494 \
         alone would only turn the error into a wrong answer. The SQL below carries the \
         #486 join rewrite and the #485 outer-reference qualification"
  ; sql =
      {|SELECT SUM(l_extendedprice) / 7.0 AS avg_yearly
FROM lineitem
INNER JOIN part ON p_partkey = l_partkey
WHERE p_brand = 'Brand#23'
  AND p_container = 'MED BOX'
  AND l_quantity < (
        SELECT 0.2 * AVG(l_quantity)
        FROM lineitem
        WHERE l_partkey = part.p_partkey)|}
  }
;;

(* Q18 — Large Volume Customer.  QUANTITY = 300.  The spec asks for the first
   100 rows. *)
let q18 =
  { number = 18
  ; setup = []
  ; verdict =
      Rewritten
        "implicit joins rewritten as explicit INNER JOIN ... ON: granary's FROM clause \
         takes a single table (#486). NOT VERIFIED: granary runs it and agrees with \
         SQLite, but both return 0 rows at SF 0.001 AND at SF 0.01 — no customer reaches \
         the spec's HAVING SUM(l_quantity) > 300 at these scales — so the cross-check is \
         satisfied by anything that returns nothing, including a wrong join. This is the \
         exact failure mode that made Q2 and Q20 look correct for two rounds of review. \
         Confirming Q18 needs SF 0.1, which #493 puts out of reach."
  ; sql =
      {|SELECT c_name, c_custkey, o_orderkey, o_orderdate, o_totalprice, SUM(l_quantity)
FROM customer
INNER JOIN orders ON c_custkey = o_custkey
INNER JOIN lineitem ON o_orderkey = l_orderkey
WHERE o_orderkey IN (
        SELECT l_orderkey
        FROM lineitem
        GROUP BY l_orderkey
        HAVING SUM(l_quantity) > 300)
GROUP BY c_name, c_custkey, o_orderkey, o_orderdate, o_totalprice
ORDER BY o_totalprice DESC, o_orderdate
LIMIT 100|}
  }
;;

(* Q19 — Discounted Revenue.  QUANTITY1/2/3 = 1, 10, 20;
   BRAND1/2/3 = 'Brand#12', 'Brand#23', 'Brand#34'. *)
let q19 =
  { number = 19
  ; setup =
      [ {|CREATE VIEW q19_lineitem AS
      SELECT l_partkey,
             l_quantity,
             l_shipmode,
             l_shipinstruct,
             l_extendedprice * (1 - l_discount) AS disc_price
      FROM lineitem|}
      ]
  ; verdict =
      Rewritten
        "the join predicate p_partkey = l_partkey lifted into an explicit INNER JOIN ... \
         ON (#486); the WHERE clause is left byte-identical, so the repeated predicate \
         is redundant rather than moved. The row-level measure l_extendedprice * (1 - \
         l_discount) is projected as a named column by a setup view so the SUM takes a \
         plain column reference (#488); the SUM and the whole three-way disjunction stay \
         in the measured query. Verified against SQLite at SF 0.01 on a 1-row result."
  ; sql =
      {|SELECT SUM(disc_price) AS revenue
FROM q19_lineitem
INNER JOIN part ON p_partkey = l_partkey
WHERE (
        p_partkey = l_partkey
        AND p_brand = 'Brand#12'
        AND p_container IN ('SM CASE', 'SM BOX', 'SM PACK', 'SM PKG')
        AND l_quantity >= 1 AND l_quantity <= 1 + 10
        AND p_size BETWEEN 1 AND 5
        AND l_shipmode IN ('AIR', 'AIR REG')
        AND l_shipinstruct = 'DELIVER IN PERSON')
   OR (
        p_partkey = l_partkey
        AND p_brand = 'Brand#23'
        AND p_container IN ('MED BAG', 'MED BOX', 'MED PKG', 'MED PACK')
        AND l_quantity >= 10 AND l_quantity <= 10 + 10
        AND p_size BETWEEN 1 AND 10
        AND l_shipmode IN ('AIR', 'AIR REG')
        AND l_shipinstruct = 'DELIVER IN PERSON')
   OR (
        p_partkey = l_partkey
        AND p_brand = 'Brand#34'
        AND p_container IN ('LG CASE', 'LG BOX', 'LG PACK', 'LG PKG')
        AND l_quantity >= 20 AND l_quantity <= 20 + 10
        AND p_size BETWEEN 1 AND 15
        AND l_shipmode IN ('AIR', 'AIR REG')
        AND l_shipinstruct = 'DELIVER IN PERSON')|}
  }
;;

(* Q20 — Potential Part Promotion.  COLOR = 'forest', DATE = '1994-01-01'
   (+1 year = '1995-01-01'), NATION = 'CANADA'. *)
let q20 =
  { number = 20
  ; setup = []
  ; verdict =
      Rewritten_pending
        "implicit joins rewritten as explicit INNER JOIN ... ON (#486); the innermost \
         correlated subquery's outer references written as partsupp.ps_partkey / \
         partsupp.ps_suppkey (#485). Runs on granary but returns the WRONG ANSWER: 0 \
         rows where SQLite returns 3 at SF 0.01, because a correlated subquery matches \
         nothing when the enclosing scope's FROM is a join (#492). This read 'ok' at SF \
         0.001 only because both engines legitimately return 0 rows there."
  ; sql =
      {|SELECT s_name, s_address
FROM supplier
INNER JOIN nation ON s_nationkey = n_nationkey
WHERE s_suppkey IN (
        SELECT ps_suppkey
        FROM partsupp
        WHERE ps_partkey IN (
                SELECT p_partkey
                FROM part
                WHERE p_name LIKE 'forest%')
          AND ps_availqty > (
                SELECT 0.5 * SUM(l_quantity)
                FROM lineitem
                WHERE l_partkey = partsupp.ps_partkey
                  AND l_suppkey = partsupp.ps_suppkey
                  AND l_shipdate >= '1994-01-01'
                  AND l_shipdate < '1995-01-01'))
  AND n_name = 'CANADA'
ORDER BY s_name|}
  }
;;

(* Q21 — Suppliers Who Kept Orders Waiting.  NATION = 'SAUDI ARABIA'.  The spec
   asks for the first 100 rows. *)
let q21 =
  { number = 21
  ; setup = []
  ; verdict =
      Skipped
        "ORDER BY numwait DESC ranks a computed measure, which has no expressible form — \
         #490 (alias), #495 (aggregate expression), #489 (ordinal, silently ignored). \
         The two correlated subqueries also sit under a joined outer FROM, so #492 would \
         make the answer wrong even if the ordering were expressible. The SQL below \
         carries the #486 join rewrite"
  ; sql =
      {|SELECT s_name, COUNT(*) AS numwait
FROM supplier
INNER JOIN lineitem l1 ON s_suppkey = l1.l_suppkey
INNER JOIN orders ON o_orderkey = l1.l_orderkey
INNER JOIN nation ON s_nationkey = n_nationkey
WHERE o_orderstatus = 'F'
  AND l1.l_receiptdate > l1.l_commitdate
  AND EXISTS (
        SELECT *
        FROM lineitem l2
        WHERE l2.l_orderkey = l1.l_orderkey
          AND l2.l_suppkey <> l1.l_suppkey)
  AND NOT EXISTS (
        SELECT *
        FROM lineitem l3
        WHERE l3.l_orderkey = l1.l_orderkey
          AND l3.l_suppkey <> l1.l_suppkey
          AND l3.l_receiptdate > l3.l_commitdate)
  AND n_name = 'SAUDI ARABIA'
GROUP BY s_name
ORDER BY numwait DESC, s_name
LIMIT 100|}
  }
;;

(* Q22 — Global Sales Opportunity.  I1..I7 = '13', '31', '23', '29', '30',
   '18', '17'. *)
let q22 =
  { number = 22
  ; setup =
      [ {|CREATE VIEW q22_custsale AS
      SELECT substr(c_phone, 1, 2) AS cntrycode, c_acctbal
      FROM customer
      WHERE substr(c_phone, 1, 2) IN ('13', '31', '23', '29', '30', '18', '17')
        AND c_acctbal > (
              SELECT AVG(c_acctbal)
              FROM customer
              WHERE c_acctbal > 0.00
                AND substr(c_phone, 1, 2) IN ('13', '31', '23', '29', '30', '18', '17'))
        AND NOT EXISTS (
              SELECT *
              FROM orders
              WHERE o_custkey = customer.c_custkey)|}
      ]
  ; verdict =
      Rewritten
        "SUBSTRING(c_phone FROM 1 FOR 2) rewritten as substr(c_phone, 1, 2), which \
         reference SQLite rejects in spec form too; the correlated NOT EXISTS outer \
         reference written as customer.c_custkey (#485); the spec's derived table \
         'custsale' hoisted verbatim into a setup view, since granary has no subquery in \
         FROM (#486). The view's own FROM is the single table customer, which is why its \
         correlated NOT EXISTS is not hit by #492. The view is row-level only; COUNT(*), \
         SUM(c_acctbal), the GROUP BY and the ORDER BY are still the measured query. \
         Verified against SQLite at SF 0.01 on a 7-row result."
  ; sql =
      {|SELECT cntrycode, COUNT(*) AS numcust, SUM(c_acctbal) AS totacctbal
FROM q22_custsale
GROUP BY cntrycode
ORDER BY cntrycode|}
  }
;;

let all =
  [ q1
  ; q2
  ; q3
  ; q4
  ; q5
  ; q6
  ; q7
  ; q8
  ; q9
  ; q10
  ; q11
  ; q12
  ; q13
  ; q14
  ; q15
  ; q16
  ; q17
  ; q18
  ; q19
  ; q20
  ; q21
  ; q22
  ]
;;

let find n = List.find_opt (fun q -> q.number = n) all

let verdict_label = function
  | Native -> "native"
  | Rewritten _ -> "rewritten"
  | Rewritten_pending _ -> "rewritten-not-yet-running"
  | Skipped _ -> "skipped"
;;

(* A query's [setup] creates views (Q15's revenue0) on a database that outlives
   the query, because reloading the dataset per query is not affordable.  The
   spec's trailing DROP VIEW was omitted on the assumption that the database is
   discarded between queries; that assumption does not hold, so the harness
   drops the view itself: before setup, so setup is re-runnable across repeats
   and engines, and after the query, so no view leaks into a later one.

   This is the single place that rationale is written down, and {!drop_setup_sql}
   below is the single implementation of the drop — both the benchmark runner and
   the smoke test call it.  The rationale was previously copied into three files
   and the logic written twice, which is exactly how the stale header comment at
   the top of this file went stale (#503). *)
let view_name_of_setup stmt =
  let words =
    String.split_on_char
      ' '
      (String.map
         (function
           | '\n' | '\r' | '\t' -> ' '
           | c -> c)
         stmt)
    |> List.filter (fun w -> w <> "")
  in
  let rec scan = function
    | a :: b :: name :: _
      when String.uppercase_ascii a = "CREATE" && String.uppercase_ascii b = "VIEW" ->
      (* the view may carry an explicit column list: "revenue0 (supplier_no,…" *)
      Some (List.hd (String.split_on_char '(' name))
    | _ :: rest -> scan rest
    | [] -> None
  in
  scan words
;;

let drop_setup_sql q =
  List.filter_map
    (fun stmt ->
       Option.map (Printf.sprintf "DROP VIEW IF EXISTS %s") (view_name_of_setup stmt))
    q.setup
;;
