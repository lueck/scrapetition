{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Network.Scrapetition.User
  where

-- | This modules defines a record for scraping users from social
-- media websites.

import Control.Lens
import Control.Applicative
import Database.HDBC
import Data.Maybe
import Data.Time
import qualified Data.Text as T
import qualified Data.Map as Map

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql
import Network.Scrapetition.Env


-- * Data type for users.

-- | A record for users on social media.
data User = User
  { _user_user :: T.Text
  , _user_name :: Maybe T.Text
  , _user_url :: Maybe T.Text
  , _user_scrapeDate :: Maybe UTCTime
  , _user_scraper :: Maybe T.Text
  } deriving (Eq, Show)

makeLenses ''User


instance Item User where
  itemId c = _user_user c

instance HasMeta User where
  itemUrl c = _user_url c
  setItemUrl c url = c & user_url .~ url
  itemScrapeDate u = _user_scrapeDate u
  setItemScrapeDate c date = c & user_scrapeDate .~ date
  itemScraper u = _user_scraper u
  setItemScraper c scraper = c & user_scraper .~ scraper


-- | A type class. Every item, that has a user information, should
-- implement this.
class HasUser item where
  itemUser :: item -> Maybe T.Text
  itemName :: item -> Maybe T.Text

  
-- | Create a user from an user's item. At least there must be a user
-- ID in the comment. Otherwise Nothing is returned.
contributor :: (HasUser i, Item i, HasMeta i) => i -> Maybe User
contributor item
  | isJust $ itemUser item
  = Just $ User (fromMaybe "never" $ itemUser item) (itemName item) (itemUrl item) (itemScrapeDate item) (itemScraper item)
  | otherwise = Nothing


-- | Generate an identifier for a 'User'.
userIdentifier :: Maybe T.Text  -- ^ domain name
               -> User          -- ^ the user
               -> T.Text
userIdentifier domain = identifier "/user/" domain Nothing




-- * HDBC

-- | Prepares the insert statement.
userInsertStmt :: Map.Map String String
userInsertStmt = Map.fromList
  [ (sqlite3Drv, "INSERT OR IGNORE INTO user " ++ ever)
  , (pgDrv, "INSERT INTO \"user\" " ++ ever ++ " ON CONFLICT DO NOTHING")
  ]
  where
    ever = "(\"user\", domain, name, url_id, scraper) VALUES (?, ?, ?, (SELECT url_id FROM url WHERE url = ?), ?)"

userToSql :: User -> [SqlValue]
userToSql (User usr name url scrD scr) =
  [ toSql usr
  , toSql $ fromMaybe "UNKNOWN" $ domainT url
  , toSql name
  , toSql url
  , toSql scr
  ]

instance ToSqlValues User where
  toSqlValues = userToSql


-- * SQL Strings 

-- | SQL string for creating a table for 'User'.
createUserTable :: String -> String
createUserTable tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "user_id INTEGER PRIMARY KEY AUTOINCREMENT,\n" ++
  "user TEXT NOT NULL,\n" ++
  "domain TEXT NOT NULL,\n" ++
  "name TEXT,\n" ++
  "url_id INTEGER NOT NULL REFERENCES url(url_id),\n" ++
  "-- time when first/last found this url on a scraped page:\n" ++
  "first_scraped timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "last_scraped  timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "scraper TEXT,\n" ++
  "CONSTRAINT unique_in_domain UNIQUE (user, domain))\n"
