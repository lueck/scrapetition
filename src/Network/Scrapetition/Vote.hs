{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Vote
  where

-- | This modules defines a record for scraping up and down votes on
-- discussion items from social media.

import Control.Lens
import Control.Applicative
import Database.HDBC
import Data.Maybe
import Control.Monad

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.User


-- * Data type for votes.

class HasVoters item where
  votes :: item -> Maybe [(String, Int)]


-- | A record for votes on social media.
data Vote = Vote
  { _vote_user :: String
  , _vote_item :: String
  , _vote_value :: Int
  -- , _vote_scrapeDate :: String
  -- , _vote_scrapeMethod :: String
  } deriving (Eq, Show)

makeLenses ''Vote


votings :: (Item i, HasVoters i, HasUser i) =>
           (i -> String -> String) -- ^ a function that generates user
                                   -- identifiert from item and a
                                   -- string giving the user.
        -> i                       -- ^ the item
        -> [Vote]
votings uidFun item
  | (isJust $ contributor item) && (isJust $ votes item) -- FIXME: do we need a contributor?
  = fromMaybe [] $ fmap (map (genVote (uidFun item) itmId)) $ votes item
  | otherwise = []
  where
    -- usr = fmap userIdentifier $ contributor item
    -- usrId = fromMaybe "never" usr
    itmId = identifyItem item
    genVote :: (String -> String) -> String -> (String, Int) -> Vote
    genVote uidFun' iid (who, value) = Vote (uidFun' who) iid value


votingUserIdentifier :: (Item i, HasVoters i, HasUser i) =>
                        i       -- ^ the item voted
                     -> String  -- ^ the voting user
                     -> String
votingUserIdentifier i u =
  identifier "/user/" Nothing (Just u) i


-- votingUsers :: (Item i, HasVoters i, HasUser i) =>
--            (i -> String -> String) -- ^ a function that generates user
--                                    -- identifier from item and a
--                                    -- string giving the user.
--         -> i                       -- ^ the item
--         -> [User]
-- votingUsers uidFun item
votingUsers :: (Item i, HasVoters i, HasUser i) =>
               i                -- ^ the item
            -> [User]
votingUsers item
  | (isJust $ contributor item) && (isJust $ votes item)
  -- = join $ fmap ((fmap concat) . (fmap (map (genVote (uidFun item) itmId))) . votes) item
  = fromMaybe [] $ fmap (map (genUser (itemUrl item))) $ votes item
  | otherwise = []
  where
    -- genUser :: (String -> String) -> Maybe String -> (String, Int) -> Vote
    -- genUser uidFun' url (who, _) = User (uidFun' who) Nothing url
    genUser :: Maybe String -> (String, Int) -> User
    genUser url (who, _) = User who Nothing url



-- * HDBC

-- | Prepares the insert statement.
voteInsertStmt :: String            -- ^ table name
               -> String
voteInsertStmt tName =
  "INSERT OR IGNORE INTO " ++ tName ++ " VALUES (?, ?, ?)"

voteToSql :: Vote -> [SqlValue]
voteToSql c =
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

