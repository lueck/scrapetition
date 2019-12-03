{-# LANGUAGE GADTs #-}
module Network.Scrapetition.Item
  where

import Data.Time
import Database.HDBC

type Identifier = String

-- | A scraped item.
class Item i where
  itemId :: i -> Identifier

-- | Meta data of a scraped item.
class HasMeta i where
  itemUrl :: i -> Maybe String
  setItemUrl :: i -> Maybe String -> i
  itemScrapeDate :: i -> Maybe UTCTime
  setItemScrapeDate :: i ->  Maybe UTCTime -> i
  itemScraper :: i -> Maybe String
  setItemScraper :: i -> Maybe String -> i

-- | Add meta data to an instance of 'HasMeta'.
addMeta :: HasMeta i => String -> UTCTime -> String -> i -> i
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
  insertStmt :: i -> (String -> String)



-- | 'ScrapedItem' is GADT wrapper for scraped item types. Wrapping is
-- needed for defining dispatchers for scrapers for inhomogenous
-- types. Your scalpel scrapers' return types must be packed into this
-- type.
--
-- Example: @yourScraperPacked = fmap (map MkScrapedItem) yourScraper
data ScrapedItem
  where
    MkScrapedItem :: (Item i, HasMeta i, ThreadItem i, ToSqlValues i) => i -> ScrapedItem
