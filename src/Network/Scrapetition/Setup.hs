module Network.Scrapetition.Setup where

-- | This module gathers functions for setting up databases.

import Database.HDBC

import Network.Scrapetition.URL
import Network.Scrapetition.Comment
import Network.Scrapetition.User
import Network.Scrapetition.Vote
import Network.Scrapetition.Article


-- | For SQLite, create all tables with standard table names used by
-- the SQL statements of this library.
setupSqlite :: IConnection conn => conn -> IO ()
setupSqlite conn = do
  run conn (createUrlTableSqlite "url") []
  run conn (createUrlSourceTableSqlite "url_scraped") []
  run conn (createArticleTableSqlite "article" "url") []
  run conn (createCommentTable "comment") []
  run conn (createUserTable "user") []
  run conn (createVotingTable "comment" "user" "comment_voting") []
  return ()
