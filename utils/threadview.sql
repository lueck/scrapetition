-- This creates a view named comments on the comment table. It
-- generates missing thread IDs recursively. It works on SQLite3 and
-- even with PostgreSQL, but for PostgreSQL you should use threads.sql
-- to propagate the thread IDs directly into the comment table.
--
-- USAGE with sqlite3:
--
-- sqlite3 data.db < threadview.sql
CREATE VIEW comments
-- This is a view on comments with thread IDs propagated.
AS
WITH RECURSIVE
     dis (comment_id,
     	  id,
     	  domain,
	  text,
	  title,
	  user_id,
	  name,
	  date_informal,
	  date,
	  article_id,
	  parent,
	  thread,
	  up_votes,
	  down_votes,
	  url_id,
	  height)
     AS (
     SELECT comment_id, id, domain, text, title, user_id, name, date_informal, date, article_id,
     	    parent, id, up_votes, down_votes, url_id, 0
	    FROM comment WHERE parent is NULL
     UNION
     SELECT comment.comment_id, comment.id, comment.domain, comment.text, comment.title,
     	    comment.user_id, comment.name, comment.date_informal,
	    comment.date, comment.article_id,
	    comment.parent, dis.thread, comment.up_votes,
	    comment.down_votes, comment.url_id, dis.height+1
     FROM comment, dis WHERE comment.parent = dis.id)
SELECT * FROM dis
GO
