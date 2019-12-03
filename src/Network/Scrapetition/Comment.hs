{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Comment
  where

-- | This module defines a record for scraping discussion comments
-- from social media.

import Control.Lens
import Control.Applicative
import Database.HDBC
import Data.Time
import Data.Maybe

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql
import Network.Scrapetition.User


-- * Data type for comments.

-- | A record for comments on social media.
data Comment = Comment
  { _comment_text :: String
  , _comment_user :: Maybe String
  , _comment_name :: Maybe String
  , _comment_dateInformal :: Maybe String -- ^ informal date information like "3 weeks ago"
  , _comment_date :: Maybe UTCTime
  , _comment_id :: String
  , _comment_parent :: Maybe String
  , _comment_thread :: Maybe String
  , _comment_upVotes :: Maybe Int
  , _comment_downVotes :: Maybe Int
  , _comment_url :: Maybe String
  , _comment_scrapeDate :: Maybe UTCTime
  , _comment_scraper :: Maybe String
  } deriving (Eq, Show)

makeLenses ''Comment


instance Item Comment where
  itemId c = _comment_id c

instance HasMeta Comment where
  itemUrl c = _comment_url c
  setItemUrl c url = c & comment_url .~ url
  itemScrapeDate c = _comment_scrapeDate c
  setItemScrapeDate c date = c & comment_scrapeDate .~ date
  itemScraper c = _comment_scraper c
  setItemScraper c scraper = c & comment_scraper .~ scraper

instance ThreadItem Comment where
  itemParent c = _comment_parent c
  itemThread c = _comment_thread c
  setItemThread c t = c & comment_thread .~ t

instance HasUser Comment where
  itemUser c = _comment_user c
  itemName c = _comment_name c


-- | Generate an identifier for a 'Comment'.
commentIdentifier :: Maybe String -> Maybe String -> Comment -> String
commentIdentifier = identifier "/comment/"


-- * HDBC

-- | Prepares the insert statement.
commentInsertStmt :: String            -- ^ table name
                  -> String
commentInsertStmt tName =
  "INSERT OR IGNORE INTO " ++ tName ++ " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

commentToSql :: Comment -> [SqlValue]
commentToSql (Comment txt usr name dateInf date id_ parent thread upVotes downVotes url scrD scr) =
  [ toSql id_
  , toSql $ fromMaybe "UNKOWN" $ domain url
  , toSql txt
  , toSql usr
  , toSql name
  , toSql dateInf
  , toSql date
  , toSql parent
  , toSql thread
  , toSql upVotes
  , toSql downVotes
  , toSql url
  , toSql scrD
  , toSql scr
  ]

instance ToSqlValues Comment where
  toSqlValues = commentToSql


-- * SQL Strings 

-- | SQL string for creating a table for 'Comment' items.
createCommentTable :: String -> String
createCommentTable tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "id TEXT NOT NULL,\n" ++
  "domain TEXT NOT NULL,\n" ++
  "text TEXT NOT NULL,\n" ++
  "user TEXT,\n" ++
  "name TEXT,\n" ++
  "date_informal TEXT,\n" ++
  "date TEXT,\n" ++
  "parent TEXT,\n" ++
  "thread TEXT,\n" ++
  "up_votes TEXT,\n" ++
  "down_votes TEXT,\n" ++
  "url TEXT,\n" ++
  "scrape_date TEXT,\n" ++
  "scraper TEXT,\n" ++
  "PRIMARY KEY (id, domain))\n"
