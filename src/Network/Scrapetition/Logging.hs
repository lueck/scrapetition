module Network.Scrapetition.Logging
  where

import Control.Monad.Reader
import System.IO

import Network.Scrapetition.AppType
import Network.Scrapetition.Env


log :: String -> App c i ()
log mesg = do
  env <- ask
  liftIO $ hPutStrLn (_env_logger env) mesg
