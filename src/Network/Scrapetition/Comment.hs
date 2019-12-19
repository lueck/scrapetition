{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Network.Scrapetition.Comment
  where

-- | This module defines a record for scraping discussion comments
-- from social media.

import Control.Lens
import Control.Applicative
import Database.HDBC
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as Map

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql
import Network.Scrapetition.User
import Network.Scrapetition.Env


-- * Data type for comments.

-- | A record for comments on social media.
data Comment = Comment
  { _comment_text :: T.Text
  , _comment_title :: Maybe T.Text
  , _comment_user :: Maybe T.Text
  , _comment_name :: Maybe T.Text
  , _comment_dateInformal :: Maybe T.Text -- ^ informal date information like "3 weeks ago"
  , _comment_date :: Maybe UTCTime
  , _comment_id :: T.Text
  , _comment_parent :: Maybe T.Text
  , _comment_thread :: Maybe T.Text
  , _comment_upVotes :: Maybe Int
  , _comment_downVotes :: Maybe Int
  , _comment_url :: Maybe T.Text
  , _comment_scrapeDate :: Maybe UTCTime
  , _comment_scraper :: Maybe T.Text
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
commentIdentifier :: Maybe T.Text -> Maybe T.Text -> Comment -> T.Text
commentIdentifier = identifier "/comment/"


-- * HDBC

-- | Prepares the insert statement.
commentInsertStmt :: Map.Map String String
commentInsertStmt = Map.fromList
  [ (sqlite3Drv, "INSERT OR IGNORE INTO comment " ++ ever)
  , (pgDrv, "INSERT INTO comment " ++ ever ++ " ON CONFLICT DO NOTHING")
  ]
  where
    ever = "(id, domain, text, title, user_id, date_informal, date, parent, thread, up_votes, down_votes, url_id, scraper) VALUES (?, ?, ?, ?, (SELECT user_id FROM \"user\" WHERE user = ? AND domain = ?), ?, ?, ?, ?, ?, ?, (SELECT url_id FROM url WHERE url = ?), ?)"

commentToSql :: Comment -> [SqlValue]
commentToSql (Comment txt tit usr name dateInf date id_ parent thread upVotes downVotes url scrD scr) =
  [ toSql id_
  , d
  , toSql txt
  , toSql tit
  , toSql usr
  , d
  , toSql dateInf
  , toSql date
  , toSql parent
  , toSql thread
  , toSql upVotes
  , toSql downVotes
  , toSql url
  , toSql scr
  ]
  where
    d = toSql $ fromMaybe "UNKOWN" $ domainT url

instance ToSqlValues Comment where
  toSqlValues = commentToSql


-- * SQL Strings 

-- | SQL string for creating a table for 'Comment' items.
createCommentTable :: String -> String
createCommentTable tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "comment_id INTEGER PRIMARY KEY AUTOINCREMENT,\n" ++
  "id TEXT NOT NULL,\n" ++
  "domain TEXT NOT NULL,\n" ++
  "text TEXT NOT NULL,\n" ++
  "title TEXT,\n" ++
  "user_id TEXT NOT NULL REFERENCES user(user_id),\n" ++
  "name TEXT,\n" ++
  "date_informal TEXT,\n" ++
  "date TEXT,\n" ++
  "parent TEXT,\n" ++
  "thread TEXT,\n" ++
  "up_votes TEXT,\n" ++
  "down_votes TEXT,\n" ++
  "url_id INTEGER NOT NULL REFERENCES url(url_id),\n" ++
  "-- time when first/last found this url on a scraped page:\n" ++
  "first_scraped timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "last_scraped  timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "scraper TEXT,\n" ++
  "CONSTRAINT unique_in_domain UNIQUE (id, domain))\n"
