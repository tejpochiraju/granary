-- index.sql — Adapted from sqlite/test/index.test
CREATE TABLE t (a INTEGER, b TEXT);
CREATE INDEX idx_a ON t(a);
CREATE UNIQUE INDEX idx_b ON t(b);
INSERT INTO t (a, b) VALUES (1, 'x');
INSERT INTO t (a, b) VALUES (2, 'y');
INSERT INTO t (a, b) VALUES (3, 'z');
SELECT a, b FROM t WHERE a = 2;
-- expect: 2|y
SELECT a FROM t WHERE b = 'z';
-- expect: 3
