{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.Scrapetition.SqlInjection where

-- | Web-Scraping means gathering loads of untrusted content into your
-- database. Attacking web scrapers may sound weired, but is
-- definitively possible. With the tests below we assert that the
-- values inserted into the database are properly quoted, so that
-- there is no risk of SQL injection.
--
-- The only thing we must test here: Is text input properly quoted by
-- HDBC? If the quotation is generically done by HDBC we are fine
-- off. Haskell's type safety does the rest for the other types of
-- input.

import Test.Framework
import Database.HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Data.Map as Map
import System.IO.Temp
import System.IO
import System.Directory

import Network.Scrapetition.Env
import Network.Scrapetition.URL
import Network.Scrapetition.Sql

dbName :: IO FilePath
dbName = emptySystemTempFile "test-scrapetitionXXXX.db"

urls :: [String]
urls =
  [ "http://attact.org"
  , "http://bttact.org'--comment"
  , "http://cttact.org';delete from url where true--"
  , "http://dttact.org';delete from url where true;--"
  ]

countUrlsStmt :: String
countUrlsStmt = "SELECT url FROM url"

-- | This test asserts that SQL input is properly quoted by HDBC.
test_sqlInjectionUrl = do
  case (Map.lookup sqlite3Drv urlInsertStmt) of
    Nothing -> do
      fail "Insert statement not found"
    Just stmt -> do
      db <- dbName
      conn <- Sqlite3.connectSqlite3 db
      run conn (createUrlTableSqlite "url") []
      commit conn
      stmt' <- prepare conn stmt
      executeMany stmt' $ map ((:[]) . toSql) urls
      commit conn
      rows <- quickQuery conn countUrlsStmt []
      assertEqual (length urls) (length $ map count rows)
      assertEqual urls (map count rows)
      -- Disconnection must defered until the tests are performed
      -- because of lazy evaluation.
      disconnect conn
      removeFile db
  where
    count :: [SqlValue] -> String
    count (x:[]) = (fromSql x) :: String
    count x = fail $ "Unexpected result " ++ show x
