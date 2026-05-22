-- cte.sql — Adapted from sqlite/test/with1.test
CREATE TABLE t (n INTEGER);
INSERT INTO t (n) VALUES (1);
INSERT INTO t (n) VALUES (2);
INSERT INTO t (n) VALUES (3);
WITH doubled AS (SELECT n * 2 AS d FROM t) SELECT d FROM doubled ORDER BY d;
-- expect: 2||4||6
