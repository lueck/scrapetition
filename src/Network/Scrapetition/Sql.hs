module Network.Scrapetition.Sql
  where

import Database.HDBC
import Control.Monad
import Control.Monad.Reader
import Text.HTML.Scalpel (URL)
import qualified Data.Map as Map

import Network.Scrapetition.AppType
import Network.Scrapetition.Env
import Network.Scrapetition.Item
import Network.Scrapetition.Dispatcher
import qualified Network.Scrapetition.Logging as L

-- | Insert scraped items into the SQL database.
insertScrapedItems :: (IConnection c) =>
                      Dispatcher    -- ^ the current dispatcher
                   -> [ScrapedItem] -- ^ the items to insert
                   -> [[SqlValue]]
                   -> App c ()
insertScrapedItems _ [] _ = return ()
insertScrapedItems dispatcher items values = do
  conf <- ask
  case (_env_conn conf) of
    Nothing -> return ()
    Just conn -> do
      case (Map.lookup (hdbcDriverName conn) $ _dptchr_insertItemStmt dispatcher) of
        Nothing -> do
          L.log L.Error $
            "Could not find the SQL statement for inserting \"" ++
            _dptchr_itemName dispatcher ++
            "\" into the \"" ++
            hdbcDriverName conn ++ "\" database"
        Just stmt' -> do
          -- insert items
          stmt <- liftIO $ prepare conn $ stmt' -- ((insStmt $ head items) (_dptchr_tableName dispatcher))
          liftIO $ executeMany stmt values --   $ map toSqlValues items
          liftIO $ commit conn
  -- where
  --   insStmt (MkScrapedItem i) = insertStmt i


-- | Insert URLs into the SQL database.
insertUrls :: (IConnection c) =>
             [URL]
          -> App c ()
insertUrls urls = do
  conf <- ask
  case (_env_conn conf) of
    Nothing -> return ()
    Just conn -> do
      case (Map.lookup (hdbcDriverName conn) $ _env_insertUrlStmt conf) of
        Nothing -> do
          L.log L.Error $
            "Could not find the SQL statement for inserting a URL into the " ++
            hdbcDriverName conn ++ " database"
        Just stmt' -> do
          stmt <- liftIO $ prepare conn stmt'
          liftIO $ executeMany stmt $ map ((:[]) . toSql) urls
          liftIO $ commit conn

-- | Update the last seen date of an URL.
updateUrlSeenDate :: (IConnection c) =>
                 URL
              -> App c ()
updateUrlSeenDate url = do
  conf <- ask
  case (_env_conn conf) of
    Nothing -> return ()
    Just conn -> do
      case (Map.lookup (hdbcDriverName conn) $ _env_updateUrlSeenDateStmt conf) of
        Nothing -> do
          L.log L.Error $
            "Could not find the SQL statement for inserting a URL into the " ++
            hdbcDriverName conn ++ " database"
        Just stmt' -> do
          stmt <- liftIO $ prepare conn stmt'
          liftIO $ execute stmt $ [toSql url]
          liftIO $ commit conn


-- | Insert scraped URLs with URL where found into the SQL database.
insertScrapedUrls :: (IConnection c) =>
                     URL        -- ^ the URL visited with the scraper
                  -> [URL]      -- ^ the URLs found by the scraper
                  -> App c ()
insertScrapedUrls seenUrl urls = do
  conf <- ask
  case (_env_conn conf) of
    Nothing -> return ()
    Just conn -> do
      case (Map.lookup (hdbcDriverName conn) $ _env_insertUrlSourceStmt conf) of
        Nothing -> do
          L.log L.Error $
            "Could not find the SQL statement for inserting a URL into the " ++
            hdbcDriverName conn ++ " database"
        Just stmt' -> do
          stmt <- liftIO $ prepare conn stmt'
          liftIO $ executeMany stmt $ map (\url -> [toSql seenUrl, toSql url]) urls
          liftIO $ commit conn


-- | Get scraped URLs from database.
selectUrlsSeen :: (IConnection c) =>
                     App c ([URL])
selectUrlsSeen = do
  conf <- ask
  case (_env_conn conf) of
    Nothing -> return []
    Just conn -> do
      case (Map.lookup (hdbcDriverName conn) $ _env_selectUrlSeenStmt conf) of
        Nothing -> do
          L.log L.Error $
            "Could not find SQL statement for selecting scraped URLs from the " ++
            hdbcDriverName conn ++ " database"
          return []
        Just stmt' -> do
          -- stmt <- liftIO $ prepare conn stmt'
          urls <- liftIO $ quickQuery conn stmt' []
          return $ map convRow urls
            where
              convRow :: [SqlValue] -> URL
              convRow (x:[]) = (fromSql x) :: URL
              convRow x = fail $ "Unexpected result: " ++ show x
