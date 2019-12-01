module Network.Scrapetition.App
  where

import Control.Concurrent
import Text.HTML.Scalpel
import Data.Maybe
import Control.Lens
import qualified Database.HDBC as DB
import Control.Monad.Reader

import Network.Scrapetition.Env
import Network.Scrapetition.Item
import Network.Scrapetition.Utils


-- | The type variables must be bound to 'DB.IConnection' and 'Item'
-- and 'ThreadItem' like in 'runScraper'.
type App c i a = ReaderT (Env c i) IO a


-- | Run a scraper and call it recursively on the scraped URLs. Items
-- are collected and may be stored in a database, depending of 'Env'
-- passed in ReaderT.
runScraper :: (DB.IConnection c, Item i, ThreadItem i) =>
              [URL]                    -- ^ URLs to scrape
           -> [URL]                    -- ^ URLs done
           -> App c i ([i]) -- same as -> ReaderT (Env c i) IO ([i])
runScraper urls seen = do
  conf <- ask
  let maybeNext = nextUrl urls seen
      threadItemIdentifier = _env_commentIdentifier conf
      threadItemToSql = _env_threadItemToSql conf
      insertItemStmt = _env_insertItemStmt conf
      userToSql = _env_userToSql conf
      insertUserStmt = _env_insertUserStmt conf
      userFromItem = _env_userFromItem conf
      scraper = _env_scraper conf
  case maybeNext of
    Nothing -> do
      liftIO $ print "All URLs seen."
      return []
    Just next -> do
      liftIO $ print $ "Scraping " ++ next
      result <- liftIO $ scrapeURL next scraper
      let comments' = fromMaybe [] $ fmap ((map (flip setItemUrl $ Just next)) . fst) result
          comments = propagateThreads (threadItemIdentifier Nothing) comments'
          newUrls = fromMaybe [] $ fmap snd result
      liftIO $ print $ "Found " ++ (show $ length comments) ++ " items, and "
        ++ (show $ length newUrls) ++ " URLs."
      case (_env_conn conf) of
        Just conn -> do
          stmtI <- liftIO $ DB.prepare conn insertItemStmt
          liftIO $ DB.executeMany stmtI $ map threadItemToSql comments
          stmtU <- liftIO $ DB.prepare conn insertUserStmt
          liftIO $ DB.executeMany stmtU $
            map userToSql $
            catMaybes $
            map userFromItem comments
          liftIO $ DB.commit conn
        Nothing -> do
          return ()
      liftIO $ threadDelay 2000000
      moreComments <- runScraper (urls++newUrls) (next:seen)
      return $ comments ++ moreComments


nextUrl :: [URL] -> [URL] -> Maybe URL
nextUrl [] _ = Nothing
nextUrl (x:xs) seen
  | x `elem` seen = nextUrl xs seen
  | otherwise = Just x

