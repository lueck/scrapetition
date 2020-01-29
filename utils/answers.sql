-- Generate an Edge Table for Gephi:
-- Edges indicate how often one Author answered to an other author.
--
-- USAGE: sqlite3 data.db < answers.sql
--
-- Note: This requires threadview.sql before!
.separator "\t"
.headers on

SELECT child.name AS Source, parent.name AS Target, count(child.id) AS Weight
FROM comments AS child
CROSS JOIN comments AS parent
WHERE child.parent = parent.id
GROUP BY parent.user, child.user ORDER BY Weight;
