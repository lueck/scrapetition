-- recursively assign thread ID and calculate the height
WITH RECURSIVE
     dis (id, parent, thread, height, name, title) AS (
     SELECT id, parent, id, 0, name, title FROM comments WHERE parent is NULL
     UNION
     SELECT comments.id, comments.parent, dis.thread, dis.height+1, comments.name, comments.title
     FROM comments, dis WHERE comments.parent = dis.id)
SELECT * FROM dis;


-- same as above, but regenerate the whole table
WITH RECURSIVE
     dis (id, domain, text, title, user, name, date_informal, date, parent, thread, up_votes, down_votes, url, scrape_url, scraper, height) AS (
     SELECT id, domain, text, title, user, name, date_informal, date, parent, id, up_votes, down_votes, url, scrape_date, scraper, 0 FROM comments WHERE parent is NULL
     UNION
     SELECT comments.id, comments.domain, comments.text, comments.title, comments.user, comments.name, comments.date_informal, comments.date, comments.parent, dis.thread, comments.up_votes, comments.down_votes, comments.url, comments.scrape_date, comments.scraper, dis.height+1
     FROM comments, dis WHERE comments.parent = dis.id)
SELECT * FROM dis;

