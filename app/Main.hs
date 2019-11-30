module Main where

import Text.HTML.Scalpel (scrapeURL, scrapeStringLike)
import Options.Applicative
import Data.Monoid ((<>))
import Network.URI
import qualified Data.HashMap as Map
--import Control.Lens
import qualified Database.HDBC as DB
import Database.HDBC.Sqlite3

import Network.Scrapetition.Comment
import Network.Scrapetition.Env
import Network.Scrapetition.App
import qualified Network.Scrapetition.Scrapers.ZeitDe as ZeitDe (commentsThreadsAndNext, identifierZeitDe)
import qualified Network.Scrapetition.Scrapers.ZeitDe as ZeitDe (comments, comments')


--url = "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache/komplettansicht"


data Opts = Opts
  { url :: String
  , output :: OutputMethod
  }

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
  <$> argument str (metavar "URL"
                    <> help "An URL to start with.")
  <*> ((SQLite <$>
        (strOption (short 's'
                    <> long "sqlite"
                    <> help "Output to SQLite3 database. (Default)"
                    <> value "data.db"
                    <> showDefault
                    <> metavar "DATABASE")))
        <|>
        (Postgres <$>
         (strOption (short 'p'
                     <> long "postgresql"
                     <> help "Output to PostgreSQL database."
                     <> metavar "CONNECTION")))
        <|>
        (flag' Raw
         (short 'r'
          <> long "raw"
          <> help "Output raw haskell values."))
      )

main = execParser opts >>= run
  where opts = info (helper <*> opts_)
          (fullDesc
           <> progDesc
           "scrapetition scrapes discussions from social media web sites. It starts with a URL given by the user and tries to scrape all comments on this URL and subsequent URL."
           <> header "scrapetition  - scrape comments (discussions) from social media web sites.")

-- | Evaluate commandline options and run the scraper.
run :: Opts -> IO ()
run opts@(Opts url (SQLite fname)) = do
  conn <- connectSqlite3 fname
  prepareSql opts conn
  cs <- runScraper (Env
                    (Just conn)
                    commentIdentifier
                    (commentToSql (commentIdentifier Nothing Nothing))
                    (commentInsertStmt "comments")
                   ) ZeitDe.commentsThreadsAndNext [url] []
  report cs
  DB.disconnect conn
run (Opts url (Postgres _)) = do
  print "Postgres output is not yet implemented"
run opts@(Opts url Raw) = do
  cs <- runScraper (Env
                    (Nothing::Maybe Connection)
                    commentIdentifier
                    (commentToSql (commentIdentifier Nothing Nothing))
                    (commentInsertStmt "comments")
                   ) ZeitDe.commentsThreadsAndNext [url] []
  print cs
  report cs

prepareSql :: DB.IConnection conn => Opts -> conn -> IO ()
prepareSql opts conn = do
  DB.run conn (createCommentTable "comments") []
  DB.run conn (createUserTable "users") []
  DB.run conn (createVotingTable "comments" "users" "comment_voting") []
  DB.commit conn

report :: [Comment] -> IO ()
report cs = do
  print $ "Scraped " ++ (show $ length cs) ++ " comments"
  let cs' = Map.fromList(zip (map (ZeitDe.identifierZeitDe Nothing) cs) cs)
  print $ (show $ length $ Map.keys cs') ++ " are different."
