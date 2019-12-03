module Network.Scrapetition.Item
  where

import Data.Time

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

-- | A scraped item with a parent like a comment in a discussion.
class ThreadItem i where
  itemParent :: i -> Maybe Identifier
  itemThread :: i -> Maybe Identifier
  setItemThread :: i -> Maybe Identifier -> i
