{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.User
  where

-- | This modules defines a record for scraping users from social
-- media websites.

import Control.Lens
import Control.Applicative
import Database.HDBC
import Data.Maybe

import Network.Scrapetition.Item
import Network.Scrapetition.Utils


-- * Data type for users.

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


-- | A type class. Every item, that has a user information, should
-- implement this.
class HasUser item where
  itemUser :: item -> Maybe String
  itemName :: item -> Maybe String

  
-- | Create a user from an user's item. At least there must be a user
-- ID in the comment. Otherwise Nothing is returned.
contributor :: (HasUser i, Item i) => i -> Maybe User
contributor item
  | isJust $ itemUser item
  = Just $ User (fromMaybe "never" $ itemUser item) (itemName item) (itemUrl item)
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
