{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Vote
  where

-- | This modules defines a record for scraping up and down votes on
-- discussion items from social media.

import Control.Lens
import Control.Applicative
import Database.HDBC

import Network.Scrapetition.Item
import Network.Scrapetition.Utils


-- * Data type for votes.

class HasVoters item where
  upVoters :: item -> Maybe [String]
  downVoters :: item -> Maybe [String]


-- | A record for votes on social media.
data Vote = Vote
  { _vote_user :: Maybe String
  , _vote_item :: String
  , _vote_value :: Int
  -- , _vote_scrapeDate :: String
  -- , _vote_scrapeMethod :: String
  } deriving (Eq, Show)

makeLenses ''Vote


-- * HDBC

-- | Prepares the insert statement.
voteInsertStmt :: String            -- ^ table name
               -> String
voteInsertStmt tName =
  "INSERT OR IGNORE INTO " ++ tName ++ " VALUES (?, ?, ?)"

voteToSql :: (Vote -> String) -> Vote -> [SqlValue]
voteToSql f c =
  [ toSql $ c^.vote_user
  , toSql $ c^.vote_item
  , toSql $ c^.vote_value
  ]


-- * SQL Strings 


-- | SQL string for creating a crossing table for votes on 'Vote'
-- items by 'User'.
createVotingTable :: String -> String -> String -> String
createVotingTable votesName usersName tName =
  "CREATE TABLE IF NOT EXISTS " ++ tName ++ " (\n" ++
  "user TEXT NOT NULL REFERENCES " ++ usersName ++ "(key),\n" ++
  "item TEXT NOT NULL REFERENCES " ++ votesName ++ "(key),\n" ++
  "vote INTEGER,\n" ++
  "CONSTRAINT unique_vote UNIQUE (user, item))\n"

