module Network.Scrapetition.Sql
  where

import Database.HDBC
import Control.Monad
import Control.Monad.Reader
import Text.HTML.Scalpel (URL)
import qualified Data.Map as Map
import Data.Maybe

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


-- | Get URLs from database that were already visited.
selectUrlsSeen :: (IConnection c) => App c ([URL])
selectUrlsSeen = selectUrls (_env_selectUrlSeenStmt) Nothing []

-- | Get URLs from database that were never visited.
selectUrlsNotSeen :: (IConnection c) => App c ([URL])
selectUrlsNotSeen = selectUrls (_env_selectUrlNotSeenStmt) Nothing []

-- | Select URLs from database based on a flexible WHERE clause.
selectUrlsWhere :: (IConnection c) =>
                   String       -- ^ WHERE clause
                -> App c ([URL])
selectUrlsWhere whereClause = selectUrls (_env_selectUrlWhereStmt) (Just whereClause) []


-- | Generic function to get URLs from database.
selectUrls :: (IConnection c) =>
              (Env c -> Map.Map String String) -- ^ the getter for the select statement
           -> Maybe String -- ^ Suffix (WHERE clause) to be appended to the select statement
           -> [SqlValue] -- ^ sql values to insert into the select statement
           -> App c ([URL])
selectUrls stmtGetter whereClause vals = do
  conf <- ask
  case (_env_conn conf) of
    Nothing -> return []
    Just conn -> do
      case (Map.lookup (hdbcDriverName conn) $ stmtGetter conf) of
        Nothing -> do
          L.log L.Error $
            "Could not find SQL statement for selecting scraped URLs from the " ++
            hdbcDriverName conn ++ " database"
          return []
        Just stmt' -> do
          urls <- liftIO $ quickQuery conn (stmt' ++ fromMaybe "" whereClause) vals
          return $ map convRow urls
            where
              convRow :: [SqlValue] -> URL
              convRow (x:[]) = (fromSql x) :: URL
              convRow x = fail $ "Unexpected result: " ++ show x
