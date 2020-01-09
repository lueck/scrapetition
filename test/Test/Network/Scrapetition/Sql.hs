{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.Scrapetition.Sql where

-- | The correctness of SQL statements and the presence of tables and
-- columns can't asserted by GHC. So unit tests are needed.

import Test.Framework
import Database.HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Data.Map as Map
import System.IO.Temp
import System.IO
import System.Directory
import qualified Data.Text as T

import Network.Scrapetition.Env
import Network.Scrapetition.Item
import Network.Scrapetition.Sql
import Network.Scrapetition.URL
import Network.Scrapetition.Comment
import Network.Scrapetition.User
import Network.Scrapetition.Vote
import Network.Scrapetition.Article



dbName :: IO FilePath
dbName = emptySystemTempFile "test-scrapetitionXXXX.db"

getStmt :: String -> Map.Map String String -> IO String
getStmt dbms stmts = do
  case (Map.lookup dbms stmts) of
    Nothing -> fail $ "SQL statement not found for dbms " ++ dbms
    Just stmt -> return stmt

count :: [SqlValue] -> Int
count (x:[]) = (fromSql x) :: Int
count _ = -1

strFromSql :: [SqlValue] -> String
strFromSql (x:[]) = (fromSql x) :: String
strFromSql x = fail $ "Unexpected result " ++ show x

maybeIntFromSql :: [SqlValue] -> Maybe Int
maybeIntFromSql (x:[]) = (fromSql x) :: Maybe Int
maybeIntFromSql _ = Just (-1)

urls :: [String]
urls =
  [ "http://start.it"
  , "http://www.feu.de/ksw/ndl"
  , "http://www.feu.de/mi/fachschaft"
  ]

articles :: [Article]
articles =
  [ Article "http://www.feu.de/ksw/ndl" (Just "Institut") Nothing (Just "http://start.it") Nothing (Just "test")
  , Article "http://not.present" Nothing Nothing Nothing Nothing Nothing
  , Article "http://www.feu.de/ksw/ndl" Nothing Nothing (Just "http://not.present") Nothing Nothing
  ]

users :: [User]
users =
  [ User "u0" (Just "Tom") (Just "http://www.feu.de/ksw/ndl") Nothing (Just "test")
  , User "u1" (Just "Bob") (Just "http://not.present") Nothing (Just "test")
  ]

comments :: [Comment]
comments =
  [ mkC "c0" "http://www.feu.de/ksw/ndl" "http://www.feu.de/ksw/ndl" Nothing
  , mkC "c1" "http://www.feu.de/ksw/ndl" "http://www.feu.de/ksw/ndl" (Just "u0")
  , mkC "c2" "http://not.present" "http://www.feu.de/ksw/ndl" Nothing
  , mkC "c3" "http://www.feu.de/ksw/ndl" "http://not.present" Nothing -- will fail to insert
  , mkC "c4" "http://www.feu.de/ksw/ndl" "http://www.feu.de/ksw/ndl" (Just "not-present")
  ]

mkC :: T.Text -> T.Text -> T.Text -> Maybe T.Text -> Comment
mkC ident art url usr =
  Comment "Hallo Welt" (Just "Titel") usr (Just "Name") (Just "a year ago") Nothing
  ident (Just "parent") (Just "thread") (Just 10) (Just 3)
  (Just art) (Just 42) (Just (-1)) (Just url) Nothing (Just "test")

-- | Setup SQLite3 database, connect to it and run the test
-- function. Teardown the database afterwards.
run_sqlite_test test_fun = do
  -- setup
  db <- dbName
  conn <- Sqlite3.connectSqlite3 db
  run conn (createUrlTableSqlite "url") []
  run conn (createUrlSourceTableSqlite "url_scraped") []
  run conn (createArticleTableSqlite "article" "url") []
  run conn (createCommentTable "comment") []
  run conn (createUserTable "user") []
  run conn (createVotingTable "comment" "user" "comment_voting") []
  commit conn
  -- run the test function
  test_fun conn sqlite3Drv
  -- tear down
  disconnect conn
  removeFile db


test_url_sqlite = run_sqlite_test _test_url

_test_url conn dbms = do
  -- test insertion of urls
  stmt <- getStmt dbms urlInsertStmt >>= prepare conn
  executeMany stmt $ map ((:[]) . toSql) urls
  commit conn
  rows <- quickQuery conn "SELECT url FROM url" []
  assertEqual urls (map strFromSql rows)

  -- insertion of duplicate urls will add nothing
  stmt <- getStmt dbms urlInsertStmt >>= prepare conn
  executeMany stmt $ map ((:[]) . toSql) urls
  commit conn
  rows <- quickQuery conn "SELECT url FROM url" []
  assertEqual urls (map strFromSql rows)

  -- selection of seen urls
  stmt <- getStmt dbms urlSeenSelectStmt -- >>= prepare conn
  rows <- quickQuery conn stmt []
  assertEqual 0 (length $ map strFromSql rows)

  -- selection of non-seen urls
  stmt <- getStmt dbms urlNotSeenSelectStmt -- >>= prepare conn
  rows <- quickQuery conn stmt []
  assertEqual 3 (length $ map strFromSql rows)

  -- update of last seen date
  stmt <- getStmt dbms urlSeenDateUpdateStmt >>= prepare conn
  executeMany stmt $ map ((:[]) . toSql) $ [head urls]
  commit conn

  -- selection of seen urls
  stmt <- getStmt dbms urlSeenSelectStmt -- >>= prepare conn
  rows <- quickQuery conn stmt []
  assertEqual 1 (length $ map strFromSql rows)

  -- selection of non-seen urls
  stmt <- getStmt dbms urlNotSeenSelectStmt -- >>= prepare conn
  rows <- quickQuery conn stmt []
  assertEqual (length urls - 1) (length $ map strFromSql rows)

  -- insertion of scraped urls (start.it was visited)
  stmt <- getStmt dbms urlSourceInsertStmt >>= prepare conn
  executeMany stmt $ map (\url -> [toSql $ head urls, toSql url]) $ tail urls
  -- insertion of duplicates will add nothing
  executeMany stmt $ map (\url -> [toSql $ head urls, toSql url]) $ tail urls
  commit conn
  rows <- quickQuery conn "SELECT count(*) FROM url_scraped" []
  assertEqual (length urls - 1) (head $ map count rows)


test_article_sqlite = run_sqlite_test _test_article

_test_article conn dbms = do
  -- setup: add some urls first
  stmt <- getStmt dbms urlInsertStmt >>= prepare conn
  executeMany stmt $ map ((:[]) . toSql) urls
  commit conn

  -- insert article
  stmt <- getStmt dbms articleInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues articles
  rows <- quickQuery conn "SELECT count(*) FROM article" []
  assertEqual 1 (head $ map count rows)


test_user_sqlite = run_sqlite_test _test_user

_test_user conn dbms = do
  -- setup: add some urls first
  stmt <- getStmt dbms urlInsertStmt >>= prepare conn
  executeMany stmt $ map ((:[]) . toSql) urls
  commit conn
  
  -- insert user
  stmt <- getStmt dbms userInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues users
  rows <- quickQuery conn "SELECT count(*) FROM user" []
  assertEqual 1 (head $ map count rows)


test_commentBare_sqlite = run_sqlite_test _test_commentBare

_test_commentBare conn dbms = do
  -- inserting comment if there are no urls, articles etc. fails
  stmt <- getStmt dbms commentInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues comments
  rows <- quickQuery conn "SELECT id FROM comment" []
  assertEqual [] (map strFromSql rows)

test_commentWithUrlOnly_sqlite =
  run_sqlite_test _test_commentWithUrlOnly

_test_commentWithUrlOnly conn dbms = do
  -- setup: add some urls first
  stmt <- getStmt dbms urlInsertStmt >>= prepare conn
  executeMany stmt $ map ((:[]) . toSql) urls
  
  -- insert comment
  stmt <- getStmt dbms commentInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues comments
  -- duplicates will add nothing
  executeMany stmt $ map toSqlValues comments
  rows <- quickQuery conn "SELECT id FROM comment" []
  assertEqual ["c0", "c1", "c2", "c4"] (map strFromSql rows)
  rows <- quickQuery conn "SELECT url_id FROM comment" []
  -- FIXME: 2 for http://www.feu.de/ksw/ndl is not garanteed
  assertEqual (replicate 4 $ Just 2) (map maybeIntFromSql rows)
  
test_commentWithUrlAndArticleOnly_sqlite =
  run_sqlite_test _test_commentWithUrlAndArticleOnly

_test_commentWithUrlAndArticleOnly conn dbms = do
  -- setup: add some urls first
  stmt <- getStmt dbms urlInsertStmt >>= prepare conn
  executeMany stmt $ map ((:[]) . toSql) urls

  -- insert comment
  stmt <- getStmt dbms commentInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues comments
  rows <- quickQuery conn "SELECT article_id FROM comment" []
  assertEqual (replicate 4 Nothing) (map maybeIntFromSql rows)

  -- further setup: add an article
  stmt <- getStmt dbms articleInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues articles

  -- delete and insert comment again
  prepare conn "DELETE FROM comment" >>= flip execute []
  stmt <- getStmt dbms commentInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues comments
  rows <- quickQuery conn "SELECT article_id FROM comment" []
  assertEqual [Just 1, Just 1, Nothing, Just 1] (map maybeIntFromSql rows)

  
test_commentWithUrlAndUserOnly_sqlite = run_sqlite_test _test_commentWithUrlAndUserOnly

_test_commentWithUrlAndUserOnly conn dbms = do
  -- setup: add some urls first
  stmt <- getStmt dbms urlInsertStmt >>= prepare conn
  executeMany stmt $ map ((:[]) . toSql) urls

  -- insert comment
  stmt <- getStmt dbms commentInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues comments
  rows <- quickQuery conn "SELECT user_id FROM comment" []
  assertEqual (replicate 4 Nothing) (map maybeIntFromSql rows)

  -- further setup: add a user
  stmt <- getStmt dbms userInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues users
  commit conn
  
  -- delete and insert comment again
  prepare conn "DELETE FROM comment" >>= flip execute []
  stmt <- getStmt dbms commentInsertStmt >>= prepare conn
  executeMany stmt $ map toSqlValues comments
  rows <- quickQuery conn "SELECT user_id FROM comment" []
  assertEqual [Nothing, Just 1, Nothing, Nothing] (map maybeIntFromSql rows)

