{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Env
  where

-- | This module defines a record for configuration data passed around
-- in the app.

import Control.Lens
import System.IO
import qualified Data.Map as Map

import Network.Scrapetition.Item
import Network.Scrapetition.Dispatcher

data Env c = Env
  { _env_conn :: Maybe c
  , _env_dispatchers :: [Dispatcher]
  , _env_logger :: Handle
  , _env_startDomain :: String
  , _env_crossDomain :: Bool
  , _env_lifo :: Bool
  , _env_insertUrlStmt :: Map.Map String String
  , _env_insertUrlSourceStmt :: Map.Map String String
    -- ^ HDBC insert statement for URLs. E.g. @INSERT INO url_source
    -- VALUES (?, ?)@.
  , _env_updateUrlSeenDateStmt :: Map.Map String String
  }

makeLenses ''Env

-- | Driver name of Database.HDBC.Sqlite3
sqlite3Drv :: String
sqlite3Drv = "sqlite3"

-- | Driver name of Database.HDBC.PostgreSQL
pgDrv :: String
pgDrv = "postgresql"
