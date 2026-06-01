-- Adapted from sqlite/test/aggregate.test
CREATE TABLE t (g INTEGER, v INTEGER);
INSERT INTO t (g, v) VALUES (1, 10);
INSERT INTO t (g, v) VALUES (1, 20);
INSERT INTO t (g, v) VALUES (2, 30);
INSERT INTO t (g, v) VALUES (2, 40);
INSERT INTO t (g, v) VALUES (3, 50);

SELECT COUNT(*) FROM t;
-- expect: 5

SELECT g, COUNT(*) FROM t GROUP BY g ORDER BY g;
-- expect: 1|2||2|2||3|1

SELECT g, SUM(v) FROM t GROUP BY g HAVING SUM(v) >= 50 ORDER BY g;
-- expect: 2|70||3|50

SELECT MIN(v), MAX(v) FROM t;
-- expect: 10|50
