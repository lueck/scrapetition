module Network.Scrapetition.App
  where

import Control.Concurrent
import Text.HTML.Scalpel hiding (scrape)
import Data.Maybe
import Control.Lens
import qualified Database.HDBC as DB
import Control.Monad.Reader
import Data.Time
import System.Environment
import Network.HTTP

import Network.Scrapetition.AppType
import Network.Scrapetition.Env
import Network.Scrapetition.Dispatcher
import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql
import Network.Scrapetition.Logging as L


-- | Run a scraper and call it recursively on the scraped URLs. Items
-- are collected and may be stored in a database, depending on 'Env'
-- passed in ReaderT.
runScrapers :: (DB.IConnection c) =>
               [URL]                    -- ^ URLs to scrape
            -> [URL]                    -- ^ URLs done
            -> App c () -- same as -> ReaderT (Env c i) IO ([i])
runScrapers urls seen = do
  conf <- ask
  let maybeNext = nextUrl urls seen
      dispatchers = _env_dispatchers conf
  now <- liftIO getCurrentTime
  appString <- getAppString
  case maybeNext of
    Nothing -> do
      L.log "All URLs seen."
      return ()
    Just next -> do
      L.log $ "Scraping " ++ next
      -- body <- liftIO $ getUrl next
      -- run scrapers
      newUrls <- forM (filter (dispatch next) dispatchers) 
        (scrape urls seen next "asdf") -- FIXME: return urls
      -- let newUrls = fromMaybe [] $ fmap snd result
      liftIO $ threadDelay 2000000
      runScrapers (urls++(concat newUrls)) (next:seen)

scrape :: (DB.IConnection c) =>
          [URL]
       -> [URL]
       -> URL
       -> String
       -> Dispatcher
       -> App c ([URL])
scrape urls seen url body dispatcher = do
  items <- liftIO $ scrapeURL url (_dptchr_scraper dispatcher)
  newUrls' <- liftIO $ scrapeURL url (_dptchr_urlScraper dispatcher)
  let newUrls = fromMaybe [] newUrls'
  appString <- getAppString
  now <- liftIO getCurrentTime
  let items' = fromMaybe [] items
  L.log $ "Found " ++ (show $ length items') ++ " items, and "
    ++ (show $ length newUrls) ++ " URLs."
  insertScrapedItems dispatcher items' $ map (unpackToSql appString now) items'
  return newUrls
  where
    unpackToSql appString now (MkScrapedItem i) =
      toSqlValues $ addMeta appString now url i


nextUrl :: [URL] -> [URL] -> Maybe URL
nextUrl [] _ = Nothing
nextUrl (x:xs) seen
  | x `elem` seen = nextUrl xs seen
  | otherwise = Just x


getUrl :: String -> IO String
getUrl url = do
  rsp <- simpleHTTP (getRequest url)
  body <- getResponseBody rsp
  return body


getAppString :: App c (String)
getAppString = liftIO getProgName
