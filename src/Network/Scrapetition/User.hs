{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.User
  where

-- | This modules defines a record for scraping users from social
-- media websites.

import Control.Lens
import Control.Applicative
import Database.HDBC

import Network.Scrapetition.Item
import Network.Scrapetition.Utils


-- * Data type for users.

class HasUser item where
  itemUser :: item -> Maybe String
  itemName :: item -> Maybe String
  

-- | A record for users on social media.
data User = User
  { _user_user :: String
  , _user_name :: Maybe String
  -- , _user_scrapeDate :: String
  -- , _user_scrapeMethod :: String
  , _user_url :: Maybe String
  } deriving (Eq, Show)

makeLenses ''User


instance Item User where
  itemUrl c = _user_url c
  setItemUrl c url = c & user_url .~ url
  itemId c = _user_user c
  identifyItem c = userIdentifier Nothing c


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
  "INSERT OR IGNORE INTO " ++ tName ++ " VALUES (?, ?, ?, ?)"

userToSql :: (User -> String) -> User -> [SqlValue]
userToSql f c =
  [ toSql $ f c
  , toSql $ c^.user_user
  , toSql $ c^.user_name
  , toSql $ c^.user_url
  ]


-- * SQL Strings 

-- | SQL string for creating a table for 'User'.
createUserTable :: String -> String
createUserTable tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "key TEXT PRIMARY KEY,\n" ++
  "user TEXT,\n" ++
  "name TEXT,\n" ++
  "url TEXT)"
