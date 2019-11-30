module Network.Scrapetition.Item
  where

type Identifier = String

-- | A scraped item.
class Item i where
  itemUrl :: i -> Maybe String
  setItemUrl :: i -> Maybe String -> i
  itemId :: i -> Identifier
  identifyItem :: i -> String

-- | A scraped item with a parent like a comment in a discussion.
class ThreadItem i where
  itemParent :: i -> Maybe Identifier
  itemThread :: i -> Maybe Identifier
  setItemThread :: i -> Maybe Identifier -> i
