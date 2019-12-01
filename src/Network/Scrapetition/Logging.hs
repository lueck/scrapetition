module Network.Scrapetition.Logging
  where

import Control.Monad.Reader

import Network.Scrapetition.AppType


log :: String -> App c i ()
log mesg = do
  liftIO $ print mesg
