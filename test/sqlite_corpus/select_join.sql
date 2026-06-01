-- Adapted from sqlite/test/join.test
CREATE TABLE t1 (a INTEGER, b INTEGER);
CREATE TABLE t2 (b INTEGER, c INTEGER);
INSERT INTO t1 (a, b) VALUES (1, 10);
INSERT INTO t1 (a, b) VALUES (2, 20);
INSERT INTO t1 (a, b) VALUES (3, 30);
INSERT INTO t2 (b, c) VALUES (10, 100);
INSERT INTO t2 (b, c) VALUES (30, 300);

SELECT t1.a, t2.c FROM t1 INNER JOIN t2 ON t1.b = t2.b ORDER BY t1.a;
-- expect: 1|100||3|300

SELECT t1.a, t2.c FROM t1 LEFT JOIN t2 ON t1.b = t2.b ORDER BY t1.a;
-- expect: 1|100||2|NULL||3|300
