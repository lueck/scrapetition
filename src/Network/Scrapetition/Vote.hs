{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Network.Scrapetition.Vote
  where

-- | This modules defines a record for scraping up and down votes on
-- discussion items from social media.

import Control.Lens
import Control.Applicative
import Database.HDBC
import Data.Maybe
import Control.Monad
import Data.Time
import qualified Data.Text as T

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
-- import Network.Scrapetition.User
import Network.Scrapetition.Sql


-- * Data type for votes.

-- | A record for votes on social media.
data Vote = Vote
  { _vote_user :: T.Text
  , _vote_item :: T.Text
  , _vote_value :: Int
  , _vote_url :: Maybe T.Text
  , _vote_scrapeDate :: Maybe UTCTime
  , _vote_scraper :: Maybe T.Text
  } deriving (Eq, Show)

makeLenses ''Vote


instance HasMeta Vote where
  itemUrl c = _vote_url c
  setItemUrl c url = c & vote_url .~ url
  itemScrapeDate c = _vote_scrapeDate c
  setItemScrapeDate c date = c & vote_scrapeDate .~ date
  itemScraper c = _vote_scraper c
  setItemScraper c scraper = c & vote_scraper .~ scraper



-- * HDBC

-- | Prepares the insert statement.
voteInsertStmt :: String            -- ^ table name
               -> String
voteInsertStmt tName =
  "INSERT OR IGNORE INTO " ++ tName ++ " VALUES (?, ?, ?, ?, ?, ?, ?)"

voteToSql :: Vote -> [SqlValue]
voteToSql (Vote usr itm val url scrDate scr) =
  [ toSql $ fromMaybe "UNKOWN" $ domainT url
  , toSql $ usr
  , toSql $ itm
  , toSql $ val
  , toSql $ url
  , toSql $ scrDate
  , toSql $ scr
  ]

instance ToSqlValues Vote where
  toSqlValues = voteToSql
  insertStmt _ = voteInsertStmt



-- * SQL Strings 


-- | SQL string for creating a crossing table for votes on 'Vote'
-- items by 'User'.
createVotingTable :: String -> String -> String -> String
createVotingTable itemsName usersName tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "domain TEXT NOT NULL,\n" ++
  "user TEXT NOT NULL,\n" ++
  "item TEXT NOT NULL,\n" ++
  "vote INTEGER,\n" ++
  "url TEXT,\n" ++
  "scrape_date TEXT,\n" ++
  "scraper TEXT,\n" ++
  "CONSTRAINT unique_vote UNIQUE (domain, user, item),\n" ++
  "CONSTRAINT fk_users FOREIGN KEY (domain, item) REFERENCES " ++ itemsName ++ "(domain, id),\n" ++
  "CONSTRAINT fk_users FOREIGN KEY (domain, user) REFERENCES " ++ usersName ++ "(domain, user))\n"
