-- Adapted from sqlite/test/select1.test
CREATE TABLE t1 (a INTEGER, b INTEGER, c INTEGER);
INSERT INTO t1 (a, b, c) VALUES (1, 2, 3);
INSERT INTO t1 (a, b, c) VALUES (4, 5, 6);
INSERT INTO t1 (a, b, c) VALUES (7, 8, 9);

SELECT a FROM t1 ORDER BY a;
-- expect: 1||4||7

SELECT a, b FROM t1 WHERE a > 1 ORDER BY a;
-- expect: 4|5||7|8

SELECT c FROM t1 WHERE b = 5;
-- expect: 6

SELECT a FROM t1 ORDER BY a DESC LIMIT 2;
-- expect: 7||4

SELECT a FROM t1 WHERE a BETWEEN 3 AND 7 ORDER BY a;
-- expect: 4||7
