module Network.Scrapetition.AppType
  where

-- | This module defines the type for the App and it's monad
-- transformer stack.

import Control.Monad.Reader

import Network.Scrapetition.Env


-- | The type variables must be bound to 'DB.IConnection' and 'Item'
-- and 'ThreadItem' like in 'runScraper'.
type App c a = ReaderT (Env c) IO a

