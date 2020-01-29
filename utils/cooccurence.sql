-- Generate an Edge Table for Gephi:
-- Edge weights indicate how often two Authors posted in the same thread.
--
-- USAGE: sqlite3 data.db < cooccurence.sql
--
-- Note: Use threadview.sql before!
.separator "\t"
.headers on

SELECT a.name AS Source, b.name AS Target, count(DISTINCT a.thread) AS Weight
FROM comments AS a
CROSS JOIN comments AS b
WHERE a.thread = b.thread AND a.name != b.name
GROUP BY Source, Target ORDER BY Weight DESC;
