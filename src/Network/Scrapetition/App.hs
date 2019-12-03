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
import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql
import Network.Scrapetition.Logging as L


-- | Run a scraper and call it recursively on the scraped URLs. Items
-- are collected and may be stored in a database, depending on 'Env'
-- passed in ReaderT.
runScrapers :: (DB.IConnection c,
                Item i, ThreadItem i, HasMeta i, ToSqlValues i) =>
               [URL]                    -- ^ URLs to scrape
            -> [URL]                    -- ^ URLs done
            -> App c i () -- same as -> ReaderT (Env c i) IO ([i])
runScrapers urls seen = do
  conf <- ask
  let maybeNext = nextUrl urls seen
      blowers = _env_blowers conf
  now <- liftIO getCurrentTime
  appString <- getAppString
  case maybeNext of
    Nothing -> do
      L.log "All URLs seen."
      return ()
    Just next -> do
      L.log $ "Scraping " ++ next
      -- body <- liftIO $ getUrl next
      newUrls <- forM blowers (scrape urls seen next "asdf") -- FIXME: return urls
      -- let newUrls = fromMaybe [] $ fmap snd result
      liftIO $ threadDelay 2000000
      runScrapers (urls++(concat newUrls)) (next:seen)
      return ()

scrape :: (DB.IConnection c,
           Item i, ThreadItem i, HasMeta i, ToSqlValues i) =>
          [URL]
       -> [URL]
       -> URL
       -> String
       -> Blower i
       -> App c i ([URL])
scrape urls seen url body blower = do
  -- let items = scrapeStringLike body (_blwr_scraper blower)
  --     newUrls = fromMaybe [] $ scrapeStringLike body (_blwr_urlScraper blower)
  items <- liftIO $ scrapeURL url (_blwr_scraper blower)
  newUrls' <- liftIO $ scrapeURL url (_blwr_urlScraper blower)
  let newUrls = fromMaybe [] newUrls'
  appString <- getAppString
  now <- liftIO getCurrentTime
  -- add meta data to items
  let items' = fromMaybe [] $
        fmap ((map
               ((flip setItemScraper $ Just appString) .
                (flip setItemScrapeDate $ Just now) .
                (flip setItemUrl $ Just url)))
             ) items
      -- items'' = propagateThreads (threadItemIdentifier Nothing) items'
  L.log $ "Found " ++ (show $ length items') ++ " items, and "
    ++ (show $ length newUrls) ++ " URLs."
  insertScrapedItems blower items'
  return newUrls


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


getAppString :: App c i (String)
getAppString = liftIO getProgName
