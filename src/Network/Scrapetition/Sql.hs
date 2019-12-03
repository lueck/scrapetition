module Network.Scrapetition.Sql
  where

import Database.HDBC
import Control.Monad
import Control.Monad.Reader

import Network.Scrapetition.AppType
import Network.Scrapetition.Env
import Network.Scrapetition.Item
import Network.Scrapetition.Dispatcher


insertScrapedItems :: (IConnection c) =>
                      Dispatcher    -- ^ the current dispatcher
                   -> [ScrapedItem]         -- ^ the items to insert
                   -> [[SqlValue]]
                   -> App c ()
insertScrapedItems _ [] _ = return ()
insertScrapedItems dispatcher items values = do
  conf <- ask
  case (_env_conn conf) of
    Just conn -> do
      -- insert items
      stmt <- liftIO $ prepare conn ((insStmt $ head items) (_dptchr_tableName dispatcher))
      liftIO $ executeMany stmt values --   $ map toSqlValues items
      liftIO $ commit conn
    Nothing -> return ()
  where
    insStmt (MkScrapedItem i) = insertStmt i
