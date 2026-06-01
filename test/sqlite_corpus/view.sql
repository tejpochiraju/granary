-- view.sql — Adapted from sqlite/test/view.test
CREATE TABLE t (id INTEGER, v INTEGER);
INSERT INTO t (id, v) VALUES (1, 10);
INSERT INTO t (id, v) VALUES (2, 20);
INSERT INTO t (id, v) VALUES (3, 30);
CREATE VIEW big_v AS SELECT id, v FROM t WHERE v > 15;
SELECT id, v FROM big_v ORDER BY id;
-- expect: 2|20||3|30
