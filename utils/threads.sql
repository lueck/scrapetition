-- 1. assign thread to comments without a parent (starters)
UPDATE comments SET thread = id WHERE parent IS NULL AND domain LIKE '%www.zeit.de%';

-- 2. recursively assign thread ID and calculate the height
WITH RECURSIVE
     dis (id, parent, thread, height, name) AS (
     SELECT id, parent, thread, 0, name FROM comments WHERE parent is NULL
     UNION
     SELECT comments.id, comments.parent, dis.thread, dis.height+1, comments.name FROM comments, dis WHERE comments.parent = dis.id)
SELECT * FROM dis;


