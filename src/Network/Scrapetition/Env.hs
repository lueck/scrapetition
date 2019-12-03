{-# LANGUAGE TemplateHaskell #-}
module Network.Scrapetition.Env
  where

import Control.Lens
import Control.Lens.TH
import Database.HDBC
import Text.HTML.Scalpel
import System.IO

import Network.Scrapetition.Item


data Blower i = Blower
  { _blwr_urlScheme :: String                  -- ^ regular expression
  , _blwr_scraper :: Scraper String ([i])      -- ^ the scraper
  , _blwr_urlScraper :: Scraper String ([URL]) -- ^ the URL scraper
  , _blwr_insertItemStmt :: String -> String   -- ^ the SQL statement for inserting
  , _blwr_tableName :: String                  -- ^ the name of the SQL table
  , _blwr_toSql :: (i -> [SqlValue])           -- ^ function for converting to SQL values
  }
  
data Env c i = Env
  { _env_conn :: Maybe c
  , _env_blowers :: [Blower i]
  , _env_logger :: Handle
  }

makeLenses ''Env


