-- dml.sql — Adapted from sqlite/test/insert.test, update.test, delete.test
CREATE TABLE t (id INTEGER, v INTEGER);
INSERT INTO t (id, v) VALUES (1, 10);
INSERT INTO t (id, v) VALUES (2, 20);
INSERT INTO t (id, v) VALUES (3, 30);
UPDATE t SET v = v * 2 WHERE id = 2;
SELECT id, v FROM t ORDER BY id;
-- expect: 1|10||2|40||3|30
DELETE FROM t WHERE v > 20;
SELECT id, v FROM t ORDER BY id;
-- expect: 1|10
