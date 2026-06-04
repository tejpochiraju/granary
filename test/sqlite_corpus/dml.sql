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

-- #250: INTEGER PRIMARY KEY autoincrement seeds from the literal max rowid,
-- including a below-start (negative) explicit id: NULL after -5 gets max+1 = -4.
CREATE TABLE seedt (id INTEGER PRIMARY KEY);
INSERT INTO seedt VALUES (-5);
INSERT INTO seedt (id) VALUES (NULL);
SELECT id FROM seedt ORDER BY id;
-- expect: -5||-4
-- explicit 0 into a fresh table seeds max+1 = 1.
CREATE TABLE seedz (id INTEGER PRIMARY KEY);
INSERT INTO seedz VALUES (0);
INSERT INTO seedz (id) VALUES (NULL);
SELECT id FROM seedz ORDER BY id;
-- expect: 0||1
