{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Env
  where

-- | This module defines a record for configuration data passed around
-- in the app.

import Control.Lens
import System.IO

import Network.Scrapetition.Item
import Network.Scrapetition.Dispatcher

data Env c = Env
  { _env_conn :: Maybe c
  , _env_dispatchers :: [Dispatcher]
  , _env_logger :: Handle
  }

makeLenses ''Env
