module Network.Scrapetition.Dispatcher
  where

-- | This module defines a data type for the scraper dispatcher. It is
-- up to this dispatcher to decide which scrapers to apply to an URL.

import Text.HTML.Scalpel
import Text.Regex.TDFA

import Network.Scrapetition.Item


-- | Data type for dispatcher information. It defines which scraper
-- for items and URLs is to be used for an URL scheme. The scrapers
-- are run, if the current URL matches the scheme, given by a regular
-- expression. The name of the SQL table for storing the scraped items
-- and the insert statement is also given here.
data Dispatcher = Dispatcher
  { _dptchr_urlScheme :: String                  -- ^ regular expression
  , _dptchr_scraper :: Scraper String ([ScrapedItem]) -- ^ the scraper
  , _dptchr_urlScraper :: Scraper String ([URL]) -- ^ the URL scraper
  , _dptchr_insertItemStmt :: String -> String   -- ^ the SQL statement for inserting
  , _dptchr_tableName :: String                  -- ^ the name of the SQL table
  }
  
-- | Filter by urlScheme if a scraper should be run.
dispatch :: String -> Dispatcher -> Bool
dispatch url dptchr = url =~ (_dptchr_urlScheme dptchr)
