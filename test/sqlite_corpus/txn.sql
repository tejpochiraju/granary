-- txn.sql — Adapted from sqlite/test/trans.test
CREATE TABLE t (n INTEGER);
INSERT INTO t (n) VALUES (1);
BEGIN;
INSERT INTO t (n) VALUES (2);
INSERT INTO t (n) VALUES (3);
COMMIT;
SELECT n FROM t ORDER BY n;
-- expect: 1||2||3
BEGIN;
INSERT INTO t (n) VALUES (99);
ROLLBACK;
SELECT n FROM t ORDER BY n;
-- expect: 1||2||3
