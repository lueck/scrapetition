{-# LANGUAGE OverloadedStrings #-}
module Network.Scrapetition.Scrapers.Generic where

-- | Generic scrapers.

import Text.HTML.Scalpel
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Time
import Data.Maybe
import Data.List

import Network.Scrapetition.Env
import Network.Scrapetition.Dispatcher
import Network.Scrapetition.Utils
import Network.Scrapetition.Item
import Network.Scrapetition.Comment


-- | Scrape all URLs which are given in the href attribute of an html
-- anchor.
links :: Scraper T.Text [URL]
links =
  chroots ("a" @: [match (\k _ -> k=="href")]) link -- (fmap (mkAbsolute url) link)

-- | Return the href attribute of an html anchor.
link :: Scraper T.Text URL
link = fmap T.unpack $ attr "href" $ "a"

-- | Return all URLs which are given in href attribute of an html
-- anchor, but not the ones starting with '#', i.e. links to a
-- fragment of the same document.
nonSameFragLinks :: Scraper T.Text [URL]
nonSameFragLinks =
  chroots ("a" @: [match (\k v -> k=="href" && (fromMaybe False $ fmap ((/='#') . fst) $ uncons v))]) link


-- | Scraper that returns no URLs at all.
noUrls :: Scraper T.Text [URL]
noUrls = pure []

-- | Scraper that returns no items at all.
packedEmpty :: Scraper T.Text [ScrapedItem]
packedEmpty = pure []

-- | A dispatcher for collecting URLs only.
urlsCollectingDispatcher :: Dispatcher
urlsCollectingDispatcher = Dispatcher
  { _dptchr_urlScheme = ".*"
  , _dptchr_scraper = const Nothing
  , _dptchr_urlScraper = flip scrapeStringLike nonSameFragLinks
  , _dptchr_insertItemStmt = Map.empty
  , _dptchr_itemName = "no item (just urls)"
  }

-- | Parse a time string like "2020-01-09T14:15:58+01:00".
parseJsonDatetime :: String -> Maybe UTCTime
parseJsonDatetime s = id
  <$> (parseTimeM True defaultTimeLocale "%FT%T%z" s :: Maybe UTCTime)


-- * Dispatcher for scraping just URLs

-- | A dispatcher for scraping URLs
allLinksDispatcher :: Dispatcher
allLinksDispatcher = Dispatcher
  { _dptchr_urlScheme = ".*"
  , _dptchr_scraper = const Nothing
  , _dptchr_urlScraper = flip scrapeStringLike links
  , _dptchr_insertItemStmt = Map.empty
  , _dptchr_itemName = "url"
  }

-- | Collect all links, pure fragment identifiers, too.
allLinksDispatchers :: [Dispatcher]
allLinksDispatchers = [allLinksDispatcher]
