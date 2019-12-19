{-# LANGUAGE GADTs, OverloadedStrings #-}
module Network.Scrapetition.Item
  where

import Data.Time
import Database.HDBC
import qualified Data.Text as T

type Identifier = T.Text

-- | A scraped item.
class Item i where
  itemId :: i -> Identifier

-- | Meta data of a scraped item.
class HasMeta i where
  itemUrl :: i -> Maybe T.Text
  setItemUrl :: i -> Maybe T.Text -> i
  itemScrapeDate :: i -> Maybe UTCTime
  setItemScrapeDate :: i ->  Maybe UTCTime -> i
  itemScraper :: i -> Maybe T.Text
  setItemScraper :: i -> Maybe T.Text -> i

-- | Add meta data to an instance of 'HasMeta'.
addMeta :: HasMeta i => T.Text -> UTCTime -> T.Text -> i -> i
addMeta appString now u =
  (flip setItemScraper (Just appString)) .
  (flip setItemScrapeDate (Just now)) .
  (flip setItemUrl (Just u))

-- | A scraped item with a parent like a comment in a discussion.
class ThreadItem i where
  itemParent :: i -> Maybe Identifier
  itemThread :: i -> Maybe Identifier
  setItemThread :: i -> Maybe Identifier -> i

class ToSqlValues i where
  toSqlValues :: i -> [SqlValue]



-- | 'ScrapedItem' is GADT wrapper for scraped item types. Wrapping is
-- needed for defining dispatchers for scrapers for inhomogenous
-- types. Your scalpel scrapers' return types must be packed into this
-- type.
--
-- Example: @yourScraperPacked = fmap (map MkScrapedItem) yourScraper
data ScrapedItem
  where
    MkScrapedItem :: (Item i, HasMeta i, ThreadItem i, ToSqlValues i) => i -> ScrapedItem
