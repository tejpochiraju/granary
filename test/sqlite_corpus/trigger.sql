-- trigger.sql — Adapted from sqlite/test/trigger1.test
-- Note: table renamed from `log` to `audit_log` to dodge issue #146
-- (granary currently treats LOG as a reserved keyword).
CREATE TABLE t (n INTEGER);
CREATE TABLE audit_log (msg TEXT);
CREATE TRIGGER trg AFTER INSERT ON t BEGIN INSERT INTO audit_log (msg) VALUES ('hit'); END;
INSERT INTO t (n) VALUES (1);
INSERT INTO t (n) VALUES (2);
SELECT COUNT(*) FROM audit_log;
-- expect: 2
