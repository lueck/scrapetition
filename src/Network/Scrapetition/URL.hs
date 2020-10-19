module Network.Scrapetition.URL where

import qualified Data.Map as Map

import Network.Scrapetition.Env


-- * HDBC SQL Statements

-- | Insert a URL into the url table.
urlInsertStmt :: Map.Map String String
urlInsertStmt = Map.fromList
  [ (sqlite3Drv, "INSERT OR IGNORE INTO url (url) VALUES (?)")
  , (pgDrv, "INSERT INTO url (url) VALUES (?) ON CONFLICT DO NOTHING") 
  ]

-- | Update the date when visited a URL. 
urlSeenDateUpdateStmt :: Map.Map String String
urlSeenDateUpdateStmt = Map.fromList
  [ (sqlite3Drv, ever)
  , (pgDrv, ever)
  ]
  where
    ever =
      "UPDATE url SET (first_seen, last_seen, status) = (coalesce(first_seen, CURRENT_TIMESTAMP), CURRENT_TIMESTAMP, ?) WHERE url = ?"

-- | Select the URLs already seen (visited).
urlSeenSelectStmt :: Map.Map String String
urlSeenSelectStmt = Map.fromList
  [ (sqlite3Drv, ever)
  , (pgDrv, ever)
  ]
  where
    ever = "SELECT url FROM url WHERE first_seen IS NOT NULL"

-- | Select the URLs never seen (visited) so far.
urlNotSeenSelectStmt :: Map.Map String String
urlNotSeenSelectStmt = Map.fromList
  [ (sqlite3Drv, ever)
  , (pgDrv, ever)
  ]
  where
    ever = "SELECT url FROM url WHERE first_seen IS NULL"

-- | Select the URLs with WHERE clause. There is no 'SqlValue' here to
-- be inserted, but the where clause has to be appended as a string.
urlSelectWhereStmt :: Map.Map String String
urlSelectWhereStmt = Map.fromList
  [ (sqlite3Drv, ever)
  , (pgDrv, ever)
  ]
  where
    ever = "SELECT url FROM url WHERE "

-- | Insert the source of a scraped url into the url_scraped table.
urlSourceInsertStmt :: Map.Map String String
urlSourceInsertStmt = Map.fromList
  [ (sqlite3Drv, "INSERT OR IGNORE INTO " ++ ever)
  , (pgDrv, "INSERT INTO " ++ ever ++ " ON CONFLICT DO NOTHING")
  ]
  where
    ever = "url_scraped (source, target) VALUES ((SELECT url_id FROM url WHERE url = ?), (SELECT url_id FROM url WHERE url = ?))"


-- * Create statements for SQLite3


createUrlTableSqlite :: String -> String
createUrlTableSqlite tableName =
  "CREATE TABLE IF NOT EXISTS " ++ tableName ++ "(\n" ++
  "url_id INTEGER PRIMARY KEY AUTOINCREMENT,\n" ++
  "url TEXT NOT NULL,\n" ++
  "status INTEGER,\n" ++
  "-- time when first/last visited this url:\n" ++
  "first_seen timestamp,\n" ++
  "last_seen  timestamp,\n" ++
  "-- time when first/last found this url on a scraped page:\n" ++
  "first_scraped timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "last_scraped  timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "scraper TEXT,\n" ++
  "CONSTRAINT url_unique UNIQUE (url))\n"
  

createUrlSourceTableSqlite :: String -> String
createUrlSourceTableSqlite tableName =
  "CREATE TABLE IF NOT EXISTS " ++ tableName ++ " (\n" ++
  "source INTEGER NOT NULL REFERENCES url(url_id),\n" ++
  "target INTEGER NOT NULL REFERENCES url(url_id),\n" ++
  "-- time when first/last found this url on a scraped page:\n" ++
  "first_scraped timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "last_scraped  timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,\n" ++
  "scraper TEXT,\n" ++
  "CONSTRAINT unique_source UNIQUE (source, target))\n"
