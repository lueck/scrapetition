module Network.Scrapetition.Sql
  where

import Database.HDBC
import Control.Monad
import Control.Monad.Reader

import Network.Scrapetition.AppType
import Network.Scrapetition.Env

class ToSqlValues i where
  toSqlValues :: i -> [SqlValue]


insertScrapedItems :: (IConnection c,
                       ToSqlValues i) =>
                      Blower i    -- ^ the current blower
                   -> [i]         -- ^ the items to insert
                   -> App c i ()
insertScrapedItems _ [] = return ()
insertScrapedItems blower items = do
  conf <- ask
  case (_env_conn conf) of
    Just conn -> do
      -- insert items
      stmt <- liftIO $ prepare conn $ ((_blwr_insertItemStmt blower) (_blwr_tableName blower))
      liftIO $ executeMany stmt $ map toSqlValues items
    Nothing -> return ()
