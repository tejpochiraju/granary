# Mined SQLite Corpus

SQL test cases adapted from SQLite's own test suite (https://sqlite.org/src,
files under `test/`). Each case has been transcribed to a plain `.sql` format
so it can be replayed against granary without needing a TCL interpreter.

## File format

Each file is a sequence of SQL statements. Lines beginning with `-- expect:`
specify the expected result of the most recent SELECT, as rows separated by
`||` and columns separated by `|`. NULL is the literal string `NULL`.

Example:
```sql
CREATE TABLE t (n INTEGER);
INSERT INTO t VALUES (1), (2), (3);
SELECT SUM(n) FROM t;
-- expect: 6
```

Multiple rows:
```sql
SELECT n FROM t ORDER BY n;
-- expect: 1||2||3
```

Lines beginning with `--` (without `expect:`) are ignored as comments. Blank
lines are ignored. Statements are terminated by `;` at end of line.

## Provenance

| File | Source SQLite test | Notes |
|------|-------------------|-------|
| select_basic.sql | select1.test (excerpts) | Basic SELECT with WHERE, ORDER BY |
| select_join.sql  | join.test (excerpts)    | INNER, LEFT JOINs |
| aggregates.sql   | aggregate.test (excerpts) | COUNT, SUM, GROUP BY |
| subquery.sql     | subquery.test (excerpts) | scalar, EXISTS, IN |
| dml.sql          | insert.test, update.test, delete.test | basic DML |
| txn.sql          | trans.test (excerpts) | BEGIN/COMMIT/ROLLBACK |
| index.sql        | index.test (excerpts) | CREATE INDEX, UNIQUE |
| trigger.sql      | trigger1.test (excerpts) | BEFORE/AFTER |
| view.sql         | view.test (excerpts) | CREATE VIEW |
| cte.sql          | with1.test (excerpts) | non-recursive CTE |

Excerpts are minimal SQL fragments selected for behavior that granary claims
to support. They are NOT verbatim copies — they have been simplified and
re-expressed to match granary's accepted SQL surface.
