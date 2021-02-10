{-# LANGUAGE OverloadedStrings #-}
module Network.Scrapetition.Scrapers.Generic where

-- | Generic scrapers.

import Text.HTML.Scalpel
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Time
import Data.List

import Network.Scrapetition.Dispatcher
import Network.Scrapetition.Item


-- | Test if an attribute with the given name is present. (Did I
-- overlock that in scalpel's API?)
hasAttr :: String -> AttributePredicate
hasAttr name = match (\k _ -> k==name)

-- | Scrape all URLs which are given in the href attribute of an html
-- anchor.
links :: Scraper T.Text [URL]
links = links_ link

-- | Return all URLs which are given in href attribute of an html
-- anchor, but not the ones starting with '#', i.e. links to a
-- fragment of the same document.
linksDropSameFrag :: Scraper T.Text [URL]
linksDropSameFrag = nonSameFragLinks_ link

-- | Return all URLs which are given in the href attribute of an html
-- anchor, but drop their fragment identifiers. Links targeting
-- fragments of the same document are dropped, too.
linksDropFrag :: Scraper T.Text [URL]
linksDropFrag = nonSameFragLinks_ linkDropFrag

-- | Construct an URL scraper for html anchors with a "href" attribute.
links_ :: Scraper T.Text URL -> Scraper T.Text [URL]
links_ scrpr = chroots ("a" @: [hasAttr "href"]) scrpr

-- | Construct an URL scraper for html anchors with a "href"
-- attribute, but drop it when the link target starts with "#". Links
-- from the protocols "mailto:" and "javascript:" are dropped, too.
nonSameFragLinks_ :: Scraper T.Text URL -> Scraper T.Text [URL]
nonSameFragLinks_ scrpr =
  chroots ("a" @: [match (\k v ->
                            k=="href" &&
                            (not $ "#" `isPrefixOf` v) &&
                            (not $ "mailto:" `isPrefixOf` v) &&
                            (not $ "javascript:" `isPrefixOf` v)
                         )]) scrpr

-- | Return the href attribute of an html anchor.
link :: Scraper T.Text URL
link = fmap T.unpack $ attr "href" $ "a"

-- | Return the href attribute of an html anchor, drop the fragment identifier.
linkDropFrag :: Scraper T.Text URL
linkDropFrag = fmap (T.unpack . T.takeWhile (/='#')) $ attr "href" $ "a"


-- | Scraper that returns no URLs at all.
noUrls :: Scraper T.Text [URL]
noUrls = pure []

-- | Scraper that returns no items at all.
packedEmpty :: Scraper T.Text [ScrapedItem]
packedEmpty = pure []

-- | A dispatcher for collecting URLs only. This uses the
-- 'linksDropFrag' scraper, so fragment identifiers are dropped.
urlsCollectingDispatcher :: Dispatcher
urlsCollectingDispatcher = Dispatcher
  { _dptchr_urlScheme = ".*"
  , _dptchr_scraper = const Nothing
  , _dptchr_urlScraper = flip scrapeStringLike linksDropFrag
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
