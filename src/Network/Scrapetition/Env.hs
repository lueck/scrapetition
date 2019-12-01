{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Env
  where

import Control.Lens
import Control.Lens.TH
import Database.HDBC
import Text.HTML.Scalpel
import System.IO

import Network.Scrapetition.Item
import Network.Scrapetition.User
import Network.Scrapetition.Vote


data Env c i = Env
  { _env_conn :: Maybe c
  , _env_scraper :: Scraper String ([i], [URL]) -- ^ the scraper
  , _env_commentIdentifier :: (Maybe String -> Maybe String -> i -> String)
  , _env_threadItemToSql :: (i -> [SqlValue])
  , _env_insertItemStmt :: String
  , _env_userIdentifier :: (Maybe String -> User -> String)
  , _env_userToSql :: (User -> [SqlValue])
  , _env_insertUserStmt :: String
  , _env_voteToSql :: (Vote -> [SqlValue])
  , _env_insertVoteStmt :: String
  , _env_logger :: Handle
  }

makeLenses ''Env
