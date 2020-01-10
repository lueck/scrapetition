{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Network.Scrapetition.Article where

-- | This module defines a record for scraping articles from websites.

import Control.Lens
import Control.Applicative
import Database.HDBC
import Data.Maybe
import Data.Time
import qualified Data.Text as T
import qualified Data.Map as Map

import Network.Scrapetition.Item
import Network.Scrapetition.Utils
import Network.Scrapetition.Sql
import Network.Scrapetition.Env


-- | A record for articles, i.e. the redactional item of a
-- website. E.g. an news article on a news site, or an item for sale
-- in a shopping site.
--
-- Note: There is actually no field for the content of the article,
-- but only for some meta data. There is also meta data about the
-- scraping.
data Article = Article
  { _artcl_canonical :: T.Text   -- ^ the canonical URL of the article
  , _artcl_title :: Maybe T.Text -- ^ the title or name of the article
  , _artcl_description :: Maybe T.Text -- ^ a short description or subtitle
  , _artcl_author :: Maybe T.Text -- ^ the author, creator or copyright holder
  , _artcl_date :: Maybe UTCTime  -- ^ the publication date
  , _artcl_url :: Maybe T.Text -- ^ scraping meta data, needn't be the canonical URL
  , _artcl_scrapeDate :: Maybe UTCTime
  , _artcl_scraper :: Maybe T.Text
  } deriving (Eq, Show)

makeLenses ''Article


instance Item Article where
  itemId c = _artcl_canonical c

instance HasMeta Article where
  itemUrl c = _artcl_url c
  setItemUrl c url = c & artcl_url .~ url
  itemScrapeDate u = _artcl_scrapeDate u
  setItemScrapeDate c date = c & artcl_scrapeDate .~ date
  itemScraper u = _artcl_scraper u
  setItemScraper c scraper = c & artcl_scraper .~ scraper

instance ThreadItem Article where
  itemParent c = Nothing
  itemThread c = Nothing
  setItemThread c _ = c


-- | An item that is an instance of the RefersToArticle must provide
-- a canonical URL of an 'Article'.
class RefersToArticle item where
  articleCanonicalUrl :: item -> Maybe T.Text

-- | Create an 'Article' from an item which refers to an article by a
-- canonical URL. This is for generating minimal 'Article' records
-- from e.g. a 'Comment'.
articleWithCanonicalUrl :: (RefersToArticle i, HasMeta i) => i -> Maybe Article
articleWithCanonicalUrl item
  | isJust $ articleCanonicalUrl item
  = Just $ Article (fromMaybe "never" $ articleCanonicalUrl item) Nothing Nothing Nothing Nothing (itemUrl item) (itemScrapeDate item) (itemScraper item)
  | otherwise = Nothing



-- * HDBC SQL Statements

-- | Prepares the insert statement.
articleInsertStmt :: Map.Map String String
articleInsertStmt = Map.fromList
  [ (sqlite3Drv, "INSERT OR IGNORE INTO article " ++ ever)
  , (pgDrv, "INSERT INTO \"article\" " ++ ever ++ " ON CONFLICT DO NOTHING")
  ]
  where
    ever = "(canonical, domain, title, description, author, date, url_id, scraper) VALUES ((SELECT url_id FROM url WHERE url = ?), ?, ?, ?, ?, ?, (SELECT url_id FROM url WHERE url = ?), ?)"

articleToSql :: Article -> [SqlValue]
articleToSql (Article canonical tit desc au date url scrD scr) =
  [ toSql canonical
  , toSql $ fromMaybe "UNKNOWN" $ domainT url
  , toSql tit
  , toSql desc
  , toSql au
  , toSql date
  , toSql url
  , toSql scr
  ]

instance ToSqlValues Article where
  toSqlValues = articleToSql


-- * Create table statement for SQLite3

createArticleTableSqlite :: String -> String -> String
createArticleTableSqlite tableName urlTable =
  "CREATE TABLE IF NOT EXISTS " ++ tableName ++ "(\n" ++
  "article_id INTEGER PRIMARY KEY AUTOINCREMENT,\n" ++
  "canonical TEXT NOT NULL REFERENCES " ++ urlTable ++ "(url_id),\n" ++
  "domain TEXT,\n" ++
  "title TEXT,\n" ++
  "description TEXT,\n" ++
  "author TEXT,\n" ++
  "date TIMESTAMP,\n" ++
  -- meta data
  "url_id TEXT NOT NULL REFERENCES " ++ urlTable ++ "(url_id),\n" ++
  "first_scraped TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "last_scraped  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "scraper TEXT,\n" ++
  "CONSTRAINT article_unique UNIQUE (canonical))\n"
