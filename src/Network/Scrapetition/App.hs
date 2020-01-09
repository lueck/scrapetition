module Network.Scrapetition.App
  where

import Control.Concurrent hiding (forkIO)
import Control.Concurrent.Forkable (forkIO)
import Text.HTML.Scalpel (URL)
import Data.Maybe
import Control.Lens
import qualified Database.HDBC as DB
import Control.Monad.Reader
import Data.Time
import System.Environment
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Text.Encoding
import Data.List

import Network.Scrapetition.AppType
import Network.Scrapetition.Env
import Network.Scrapetition.Dispatcher
import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql
import Network.Scrapetition.Logging as L


-- | Run a scraper and call it recursively on the scraped URLs.
runScrapers :: (DB.IConnection c) =>
               [URL]                    -- ^ URLs to scrape
            -> [URL]                    -- ^ URLs done
            -> App c ()
runScrapers urls seen = do
  conf <- ask
  maybeNext <- nextUrl urls seen
  let dispatchers = _env_dispatchers conf
  now <- liftIO getCurrentTime
  appString <- getAppString
  case maybeNext of
    Nothing -> do
      L.log L.Info "All URLs seen."
      return ()
    Just next -> do
      L.log L.Info $ (show $ length urls) ++ " URLs left to scrape."
      L.log L.Info $ "Scraping " ++ next
      body <- liftIO $ getUrl next
      updateUrlSeenDate next -- fork thread
      -- run scrapers
      newUrls <- forM (filter (dispatch next) dispatchers) 
        (scrape urls seen next body)
      let startDomain = Just $ _env_startDomain conf
          newUrls' = nub $
            if (_env_crossDomain conf)
            then (map (mkAbsolute next) $ concat newUrls)
            else (filter ((==startDomain) . domain . Just) $
                  map (mkAbsolute next) $ concat newUrls)
          seen' = next:seen
          unionOfUrls = if (_env_lifo conf)
            then ((reverse newUrls') `union` urls)
            else (urls `union` newUrls')
      liftIO $ threadDelay 2000000
      runScrapers (unionOfUrls \\ seen') seen'

-- | Run a scraper given in dispatcher parameter over the current
-- URL. Scraped items are written to a database.
scrape :: (DB.IConnection c) =>
          [URL]
       -> [URL]
       -> URL
       -> T.Text
       -> Dispatcher
       -> App c ([URL])
scrape urls seen url body dispatcher = do
  let items = (_dptchr_scraper dispatcher) body
      newUrls' = (_dptchr_urlScraper dispatcher) body
  let newUrls = nub $ map (mkAbsolute url) $ fromMaybe [] newUrls'
  appString <- getAppString
  now <- liftIO getCurrentTime
  let items' = fromMaybe [] items
  L.log L.Info $ "Found " ++ (show $ length items') ++ " items, and "
    ++ (show $ length newUrls) ++ " URLs."
  insertUrls newUrls            -- not to be done in forked thread
  insertScrapedUrls url newUrls -- fork thread
  insertScrapedItems dispatcher items' $
    map (unpackToSql appString now) items' -- fork thread
  return newUrls
  where
    unpackToSql appString now (MkScrapedItem i) =
      toSqlValues $ addMeta (T.pack appString) now (T.pack url) i


nextUrl :: [URL] -> [URL] -> App c (Maybe URL)
nextUrl [] _ = return Nothing
nextUrl us seen = do
  return $ nextUrl' us seen
  where
    nextUrl' :: [URL] -> [URL] -> Maybe URL
    nextUrl' [] _ = Nothing
    nextUrl' (x:xs) seen
      | x `elem` seen = nextUrl' xs seen
      | otherwise = Just x


getUrl :: String -> IO T.Text
getUrl url = do
  body <- simpleHttp url
  -- FIXME: get encoding from response
  
  -- FIXME: Should we use Data.Text.Lazy? Since the same text is
  -- scraped several times with different scrapers, it seems better to
  -- use strict text.
  return $ decodeUtf8 $ L.toStrict body


getAppString :: App c (String)
getAppString = liftIO getProgName
