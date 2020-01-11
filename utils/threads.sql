-- Add thread IDs to comments that have none. This does work on
-- PostgreSQL, but not on SQLite3.
WITH RECURSIVE
     dis (comment_id, id, parent, newthread) AS (
     SELECT comment_id, id, parent, id FROM comment
     WHERE parent is NULL AND thread is NULL
     UNION
     SELECT comment.comment_id, comment.id, comment.parent, dis.newthread
     FROM comment, dis
     WHERE comment.parent = dis.id AND comment.thread is NULL)
UPDATE comment SET thread = dis.newthread
FROM dis
WHERE comment.comment_id = dis.comment_id;
