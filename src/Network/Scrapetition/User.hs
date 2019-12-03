{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.User
  where

-- | This modules defines a record for scraping users from social
-- media websites.

import Control.Lens
import Control.Applicative
import Database.HDBC
import Data.Maybe
import Data.Time

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql


-- * Data type for users.

-- | A record for users on social media.
data User = User
  { _user_user :: String
  , _user_name :: Maybe String
  , _user_url :: Maybe String
  , _user_scrapeDate :: Maybe UTCTime
  , _user_scraper :: Maybe String
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

instance ThreadItem User where
  itemParent c = Nothing
  itemThread c = Nothing
  setItemThread c _ = c


-- | A type class. Every item, that has a user information, should
-- implement this.
class HasUser item where
  itemUser :: item -> Maybe String
  itemName :: item -> Maybe String

  
-- | Create a user from an user's item. At least there must be a user
-- ID in the comment. Otherwise Nothing is returned.
contributor :: (HasUser i, Item i, HasMeta i) => i -> Maybe User
contributor item
  | isJust $ itemUser item
  = Just $ User (fromMaybe "never" $ itemUser item) (itemName item) (itemUrl item) (itemScrapeDate item) (itemScraper item)
  | otherwise = Nothing


-- | Generate an identifier for a 'User'.
userIdentifier :: Maybe String  -- ^ domain name
               -> User          -- ^ the user
               -> String
userIdentifier domain = identifier "/user/" domain Nothing




-- * HDBC

-- | Prepares the insert statement.
userInsertStmt :: String            -- ^ table name
                  -> String
userInsertStmt tName =
  "INSERT OR IGNORE INTO " ++ tName ++ " VALUES (?, ?, ?, ?, ?, ?)"

userToSql :: User -> [SqlValue]
userToSql (User usr name url scrD scr) =
  [ toSql usr
  , toSql $ fromMaybe "UNKNOWN" $ domain url
  , toSql name
  , toSql url
  , toSql scrD
  , toSql scr
  ]

instance ToSqlValues User where
  toSqlValues = userToSql
  insertStmt _ = userInsertStmt


-- * SQL Strings 

-- | SQL string for creating a table for 'User'.
createUserTable :: String -> String
createUserTable tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "user TEXT,\n" ++
  "domain TEXT,\n" ++
  "name TEXT,\n" ++
  "url TEXT,\n" ++
  "scrape_date TEXT,\n" ++
  "scraper TEXT,\n" ++
  "PRIMARY KEY (user, domain))\n"
