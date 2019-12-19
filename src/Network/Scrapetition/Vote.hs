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
import Data.Monoid
import qualified Data.Map as Map

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql
import Network.Scrapetition.Env


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


instance Item Vote where
  itemId c = _vote_user c <> _vote_item c

instance ThreadItem Vote where
  itemParent c = Nothing
  itemThread c = Nothing
  setItemThread c _ = c

instance HasMeta Vote where
  itemUrl c = _vote_url c
  setItemUrl c url = c & vote_url .~ url
  itemScrapeDate c = _vote_scrapeDate c
  setItemScrapeDate c date = c & vote_scrapeDate .~ date
  itemScraper c = _vote_scraper c
  setItemScraper c scraper = c & vote_scraper .~ scraper



-- * HDBC

-- | Prepares the insert statement.
voteInsertStmt :: Map.Map String String
voteInsertStmt = Map.fromList
  [ (sqlite3Drv, "INSERT OR IGNORE INTO comment_voting " ++ ever)
  , (pgDrv, "INSERT INTO comment_voting " ++ ever ++ " ON CONFLICT DO NOTHING")
  ]
  where
    -- FIXME: reduce nested selects
    ever = "(domain, user_id, comment_id, vote, url_id, scraper) VALUES (?, (SELECT user_id FROM \"user\" WHERE \"user\" = ? AND domain = ?), (SELECT comment_id FROM comment WHERE id = ? AND domain = ?), ?, (SELECT url_id FROM url WHERE url = ?), ?)"

voteToSql :: Vote -> [SqlValue]
voteToSql (Vote usr itm val url scrDate scr) =
  [ d
  , toSql $ usr
  , d
  , toSql $ itm
  , d
  , toSql $ val
  , toSql $ url
  , toSql $ scr
  ]
  where
    d = toSql $ fromMaybe "UNKOWN" $ domainT url

instance ToSqlValues Vote where
  toSqlValues = voteToSql


-- * SQL Strings 

-- | SQL string for creating a crossing table for votes on 'Vote'
-- items by 'User'.
createVotingTable :: String -> String -> String -> String
createVotingTable itemsName usersName tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "domain TEXT NOT NULL,\n" ++
  "user_id INTEGER NOT NULL REFERENCES " ++ usersName ++ "(user_id),\n" ++
  "comment_id INTEGER NOT NULL REFERENCES " ++ itemsName ++ "(comment_id),\n" ++ -- FIXME: more flex?
  "vote INTEGER,\n" ++
  "url_id INTEGER NOT NULL REFERENCES url(url_id),\n" ++
  "-- time when first/last found this url on a scraped page:\n" ++
  "first_scraped timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "last_scraped  timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "scraper TEXT,\n" ++
  "CONSTRAINT unique_vote UNIQUE (domain, user_id, comment_id))\n"

  -- "CONSTRAINT fk_users FOREIGN KEY (domain, item) REFERENCES " ++ itemsName ++ "(domain, id),\n" ++
  -- "CONSTRAINT fk_users FOREIGN KEY (domain, user) REFERENCES " ++ usersName ++ "(domain, user))\n"
