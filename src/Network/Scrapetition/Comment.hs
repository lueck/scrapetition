{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Comment
  where

-- | This modules defines a record for scraping discussion comments
-- from social media.

import Control.Lens
import Control.Applicative
import Database.HDBC

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.User
import Network.Scrapetition.Vote


-- * Data type for comments.

-- | A record for comments on social media.
data Comment = Comment
  { _comment_text :: String
  , _comment_user :: Maybe String
  , _comment_name :: Maybe String
  , _comment_date :: Maybe String
  , _comment_id :: String
  , _comment_parent :: Maybe String
  , _comment_thread :: Maybe String
  , _comment_upVotes :: Maybe Int
  , _comment_downVotes :: Maybe Int
  , _comment_upVoters :: Maybe [(String, Int)]   -- FIXME: rename to votings
  , _comment_downVoters :: Maybe [(String, Int)] -- FIXME: delete
  , _comment_url :: Maybe String
  -- , _comment_scrapeDate :: String
  -- , _comment_scrapeMethod :: String
  } deriving (Eq, Show)

makeLenses ''Comment


instance Item Comment where
  itemUrl c = _comment_url c
  setItemUrl c url = c & comment_url .~ url
  itemId c = _comment_id c
  identifyItem c = commentIdentifier Nothing Nothing c

instance ThreadItem Comment where
  itemParent c = _comment_parent c
  itemThread c = _comment_thread c
  setItemThread c t = c & comment_thread .~ t

instance HasUser Comment where
  itemUser c = _comment_user c
  itemName c = _comment_name c

instance HasVoters Comment where
  votes c = _comment_upVoters c


-- | Generate an identifier for a 'Comment'.
commentIdentifier :: Maybe String -> Maybe String -> Comment -> String
commentIdentifier = identifier "/comment/"


-- * HDBC

-- | Prepares the insert statement.
commentInsertStmt :: String            -- ^ table name
                  -> String
commentInsertStmt tName =
  "INSERT OR IGNORE INTO " ++ tName ++ " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

commentToSql :: (Comment -> String) -> Comment -> [SqlValue]
commentToSql f c =
  [ toSql $ f c
  , toSql $ c^.comment_text
  , toSql $ c^.comment_user
  , toSql $ c^.comment_name
  , toSql $ c^.comment_date
  , toSql $ c^.comment_id
  , toSql $ c^.comment_parent
  , toSql $ c^.comment_thread
  , toSql $ c^.comment_upVotes
  , toSql $ c^.comment_downVotes
  , toSql $ c^.comment_url
  ]


-- * SQL Strings 

-- | SQL string for creating a table for 'Comment' items.
createCommentTable :: String -> String
createCommentTable tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "key TEXT PRIMARY KEY,\n" ++
  "text TEXT NOT NULL,\n" ++
  "user TEXT,\n" ++
  "name TEXT,\n" ++
  "dateStr TEXT,\n" ++
  "id TEXT NOT NULL,\n" ++
  "parent TEXT,\n" ++
  "thread TEXT,\n" ++
  "up_votes TEXT,\n" ++
  "down_votes TEXT,\n" ++
  -- "up_voters TEXT,\n" ++
  -- "down_voters TEXT,\n" ++
  "url TEXT)"
