{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Env
  where

import Control.Lens
import Control.Lens.TH
import Database.HDBC

import Network.Scrapetition.Item

data (IConnection c, Item i) => Env c i = Env
  { envConn :: Maybe c
  , envCommentIdentifier :: (Maybe String -> Maybe String -> i -> String)
  , envThreadItemToSql :: (i -> [SqlValue])
  , envInsertItemStmt :: String
  }

-- makeLensesWith camelCaseFields ''Env
