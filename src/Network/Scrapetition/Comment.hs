{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Comment
  where

import Control.Lens

data Comment = Comment
  { _comment_text :: String
  , _comment_user :: Maybe String
  , _comment_name :: Maybe String
  , _comment_date :: Maybe String
  , _comment_id :: String
  , _comment_parent :: Maybe String
  , _comment_thread :: Maybe String
  , _comment_upVotes :: Maybe Int
  , _comment_downVotes :: Maybe Int
  , _comment_upVoters :: Maybe [String]
  , _comment_downVoters :: Maybe [String]
  , _comment_url :: Maybe String
  -- , _comment_scrapeDate :: String
  -- , _comment_scrapeMethod :: String
  } deriving (Eq, Show)

makeLenses ''Comment

