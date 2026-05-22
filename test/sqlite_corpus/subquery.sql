-- subquery.sql — Adapted from sqlite/test/subquery.test
CREATE TABLE a (id INTEGER, n INTEGER);
CREATE TABLE b (id INTEGER, n INTEGER);
INSERT INTO a (id, n) VALUES (1, 10);
INSERT INTO a (id, n) VALUES (2, 20);
INSERT INTO b (id, n) VALUES (1, 100);
INSERT INTO b (id, n) VALUES (3, 300);
SELECT id FROM a WHERE EXISTS (SELECT 1 FROM b WHERE b.id=a.id);
-- expect: 1
SELECT id, (SELECT n FROM b WHERE b.id=a.id) FROM a ORDER BY id;
-- expect: 1|100||2|NULL
