module Network.Scrapetition.Logging
  where

import Control.Monad.Reader
import System.IO

import Network.Scrapetition.AppType
import Network.Scrapetition.Env


data LogLevel = Info | Warning | Error


log :: LogLevel -> String -> App c ()
log level mesg = do
  env <- ask
  liftIO $ hPutStrLn (_env_logger env) mesg
