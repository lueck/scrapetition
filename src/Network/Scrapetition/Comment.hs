{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Comment
  where

import Control.Lens
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Network.Scrapetition.Item
-- import Network.Scrapetition.SQLite


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
  , _comment_upVoters :: Maybe [String]
  , _comment_downVoters :: Maybe [String]
  , _comment_url :: Maybe String
  -- , _comment_scrapeDate :: String
  -- , _comment_scrapeMethod :: String
  } deriving (Eq, Show)

makeLenses ''Comment

instance Item Comment where
  itemUrl c = _comment_url c
  setItemUrl c url = c & comment_url .~ url
  itemId c = _comment_id c
  identifyItem c = _comment_id c -- FIXME

instance ThreadItem Comment where
  itemParent c = _comment_parent c
  itemThread c = _comment_thread c
  setItemThread c t = c & comment_thread .~ t


instance FromRow Comment where
  fromRow = Comment
    <$> field -- _comment_text
    <*> field -- _commant_user
    <*> field -- _commant_name
    <*> field -- _commant_date
    <*> field -- _commant_id
    <*> field -- _commant_parent
    <*> field -- _commant_thread
    <*> field -- _commant_upVotes
    <*> field -- _commant_downVotes
    <*> (pure $ Nothing) -- _commant_upVoters
    <*> (pure $ Nothing) -- _commant_downVoters
    <*> field -- _commant_url

instance ToRow Comment where
  toRow c@(Comment t u n d i p th uv dv _ _ url) =
    -- upVoters and downVoters can not be serialized to a row. There
    -- must be a crossing table for this kind of data.
    toRow (t, u, n, d, i, p, th, uv, dv, url)
    -- FIXME: toRow must generate an identifier! (identify "|comment|" Nothing Nothing c)

    
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


createVotingTable :: String -> String -> String -> String
createVotingTable commentsName usersName tName =
  "CREATE TABEL IF NOT EXISTS " ++ tName ++ " (\n" ++
  "user TEXT NOT NULL REFERENCES " ++ usersName ++ "(user),\n" ++
  "comment TEXT NOT NULL REFERENCES " ++ commentsName ++ "(key),\n" ++
  "vote TEXT,\n" ++
  "CONSTRAINT unique_vote UNIQUE (user, comment, vote))\n"

