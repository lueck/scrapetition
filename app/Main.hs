module Main where

import Text.HTML.Scalpel (scrapeURL, scrapeStringLike)
import Options.Applicative
import Data.Monoid ((<>))
import Network.URI
import qualified Data.HashMap as Map
import Control.Lens hiding (argument)
import qualified Database.HDBC as DB
import Control.Monad.Reader
import System.IO
import Data.Maybe

import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC.PostgreSQL as PostgreSQL

import Network.Scrapetition.AppType
import Network.Scrapetition.Item
import Network.Scrapetition.Comment
import Network.Scrapetition.User
import Network.Scrapetition.Vote
import Network.Scrapetition.Article
import Network.Scrapetition.Env
import Network.Scrapetition.App
import Network.Scrapetition.Utils
import Network.Scrapetition.Dispatcher
import Network.Scrapetition.URL
import Network.Scrapetition.Sql
import Network.Scrapetition.Setup
import Network.Scrapetition.Scrapers.Generic

import qualified Network.Scrapetition.Scrapers.ZeitDe as ZeitDe



data Opts = Opts
  { source :: UrlSource
  , followLinks :: Bool
  , crossDomain :: Bool
  , visitAgain :: Bool
  , lifo :: Bool
  , scraper :: ScraperSelector
  , output :: OutputMethod
  , logfile :: Maybe String
  , itemsTable :: String
  , usersTable :: String
  , votingsTable :: String
  }

data UrlSource
  = SingleUrl String
  | NotSeenFromDB String -- ^ with domain restriction

data ScraperSelector
  = ZeitDeComments

data OutputMethod
  = SQLite
  { file :: String
  }
  | Postgres
  { connection :: String
  }
  | Raw


opts_ :: Parser Opts
opts_ = Opts
  <$> ((SingleUrl <$> strOption
        (long "url"
         <> short 'u'
         <> metavar "URL"
         <> help "A URL to start with."))
       <|>
       (NotSeenFromDB <$> strOption
        (long "db-not-seen"
         <> short 'd'
         <> help "Visit the URLs from the database, that were no not visited yet. A domain must be given to which the crawling of URLs is restricted. This restriction can be overridden with the  --cross-domain  option."
         <> metavar "DOMAIN")))
  <*> switch (long "follow-links"
              <> short 'f'
              <> help "Follow all links found on a page. Use this in combination with  --cross-domain  for cross-domain scraping.")
  <*> switch (long "cross-domain"
              <> short 'x'
              <> help "Follow links pointing outside of the domain of the start URL.")
  <*> switch (long "visit-again"
              <> short 'a'
              <> help "Visit URLs again. Without this option, URLs will not be visited and scraped, if they are already in the database and marked as visited.")
  <*> switch (long "lifo"
              <> short 'l'
              <> help "Last in, first out handling of URLs: With this switch the last found URL is scraped first. By default, the first found URL is scraped first.")
  <*> (flag ZeitDeComments ZeitDeComments
        (long "wwwZeitDe-comments"
         <> help "Scraper for discussion on articles at http://www.zeit.de. This is the default scraper -- and the only one so far."))

  <*> ((SQLite <$>
        (strOption (short 's'
                    <> long "sqlite"
                    <> help "Output to SQLite3 database. This is the default output method."
                    <> value "data.db"
                    <> showDefault
                    <> metavar "DATABASE")))
        <|>
        (Postgres <$>
         (strOption (short 'p'
                     <> long "postgresql"
                     <> help "Output to PostgreSQL database given by the connection string. See  http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT for info about the connection string.\nExample: '-p \"host=localhost dbname=scrapetition_test user=me password=mine\"'"
                     <> metavar "CONNECTION")))
        <|>
        (flag' Raw
         (short 'r'
          <> long "raw"
          <> help "Output raw data."))
      )
  <*> (optional $ strOption (long "logfile"
                             <> help "Specify a file for logging messages. By default, messages are logged to stderr."
                             <> metavar "LOGFILE"))
  <*> strOption (long "items-table"
                 <> help "Table name for scraped items."
                 <> value "comments"
                 <> showDefault
                 <> metavar "ITEMTABLE")
  <*> strOption (long "users-table"
                 <> help "Table name for scraped users."
                 <> value "users"
                 <> showDefault
                 <> metavar "USERTABLE")
  <*> strOption (long "voting-table"
                 <> help "Table name for voting by users about items."
                 <> value "comment_voting"
                 <> showDefault
                 <> metavar "VOTINGTABLE")


-- scraperRegistry ZeitDeComments = ZeitDe.commentsThreadsAndNext

-- evalOpts :: Opts -> Env c i
evalOpts opts@(Opts source follow cross _ lifo scrapper _ logfile itemTab userTab votingTab) = do
  logHandle <- getLogger logfile
  return $ Env
    { _env_conn = Nothing::Maybe Sqlite3.Connection
    , _env_dispatchers = (followLinkDispatchers follow) ++
                         (genDispatchers scrapper)
    , _env_logger = logHandle
    , _env_startDomain = domainRestriction source
    , _env_crossDomain = cross
    , _env_lifo = lifo
    , _env_insertUrlStmt = urlInsertStmt
    , _env_insertUrlSourceStmt = urlSourceInsertStmt
    , _env_updateUrlSeenDateStmt = urlSeenDateUpdateStmt
    , _env_selectUrlSeenStmt = urlSeenSelectStmt
    , _env_selectUrlNotSeenStmt = urlNotSeenSelectStmt
    , _env_selectUrlWhereStmt = urlSelectWhereStmt
    }
  where
    followLinkDispatchers True = [urlsCollectingDispatcher]
    followLinkDispatchers False = []
    genDispatchers ZeitDeComments = ZeitDe.zeitDeDispatchers
    genDispatchers _ = []

domainRestriction :: UrlSource -> String
domainRestriction (SingleUrl url) = fromMaybe "unkown" $ domain $ Just url
domainRestriction (NotSeenFromDB d) = d

getLogger :: Maybe String -> IO Handle
getLogger Nothing = do { return stderr }
getLogger (Just fname) = openFile fname WriteMode


closeLogger :: Env c -> IO ()
closeLogger env = do
  let logger = _env_logger env
  if  logger == stderr then return() else hClose logger
  return ()


main = execParser opts >>= run
  where opts = info (helper <*> opts_)
          (fullDesc
           <> progDesc
           "scrapetition scrapes discussions from social media web sites. It starts with a URL given by the user and tries to scrape all comments on this URL and subsequent URL."
           <> header "scrapetition  - scrape comments (discussions) from social media web sites.")

-- | Evaluate commandline options and run the scraper.
run :: Opts -> IO ()
run opts@(Opts source _ _ again _ _ (SQLite fname) _ _ _ _) = do
  env <- evalOpts opts
  conn <- Sqlite3.connectSqlite3 fname
  setupSqlite conn
  cs <- flip runReaderT (env & env_conn .~ (Just (conn::Sqlite3.Connection))) $ do
    crawl opts
    -- seen <- selectUrlsSeen
    -- insertUrls [url]
    -- runScrapers [url] (if again then [] else seen)
  -- report env cs
  DB.disconnect conn
  closeLogger env
run opts@(Opts source _ _ again _ _ (Postgres connection) _ _ _ _) = do
  env <- evalOpts opts
  conn <- PostgreSQL.connectPostgreSQL connection
  -- prepareSql opts conn
  cs <- flip runReaderT (env & env_conn .~ (Just (conn::PostgreSQL.Connection))) $ do
    crawl opts
    -- seen <- selectUrlsSeen
    -- insertUrls [url]
    -- runScrapers [url] (if again then [] else seen)
  -- report env cs
  DB.disconnect conn
  closeLogger env
run opts@(Opts (SingleUrl url) _ _ _ _ _ Raw _ _ _ _) = do
  env <- evalOpts opts
  cs <- runReaderT (runScrapers [url] []) env
  -- print cs
  -- report env cs
  closeLogger env
run _ = do
  print "Invalid combination of options: -d and -r"

-- | Get URLs depending on command line arguments an run scrapers.
crawl :: (DB.IConnection c) =>Opts -> App c ()
crawl opts@(Opts (SingleUrl url) _ _ again _ _ _ _ _ _ _) = do
  seen <- selectUrlsSeen
  insertUrls [url]
  runScrapers [url] (if again then [] else seen)
crawl opts@(Opts (NotSeenFromDB _) _ _ again _ _ _ _ _ _ _) = do
  seen <- selectUrlsSeen
  urls <- selectUrlsNotSeen
  runScrapers urls (if again then [] else seen)


-- report :: (Item i) => Env c i -> [i] -> IO ()
-- report env cs = do
--   hPutStrLn (_env_logger env) $ "Scraped " ++ (show $ length cs) ++ " comments"
--   let cs' = Map.fromList(zip (map (identifier "/comment/" Nothing Nothing) cs) cs)
--   hPutStrLn (_env_logger env) $ (show $ length $ Map.keys cs') ++ " are different."
