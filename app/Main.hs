module Main where

import Text.HTML.Scalpel (scrapeURL, scrapeStringLike)
import Options.Applicative
import Data.Monoid ((<>))
import Network.URI
import qualified Data.HashMap as Map
--import Control.Lens

import Network.Scrapetition.Comment
import Network.Scrapetition.Env
import Network.Scrapetition.App
import qualified Network.Scrapetition.Scrapers.ZeitDe as ZeitDe (commentsThreadsAndNext, identifierZeitDe)
import qualified Network.Scrapetition.Scrapers.ZeitDe as ZeitDe (comments, comments')


--url = "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache/komplettansicht"


data Opts = Opts
  { url :: String
  , env :: Env
  }

opts_ :: Parser Opts
opts_ = Opts
  <$> argument str (metavar "URL"
                    <> help "An URL to start with.")
  <*> (pure Env)

main = execParser opts >>= run
  where opts = info (helper <*> opts_)
          (fullDesc
           <> progDesc
           "scrapetition scrapes discussions from social media web sites. It starts with a URL given by the user and tries to scrape all comments on this URL and subsequent URL."
           <> header "scrapetition  - scrape comments (discussions) from social media web sites.")

run :: Opts -> IO ()
run (Opts url env) = do
  cs <- runScraper env ZeitDe.commentsThreadsAndNext [url] []
  -- cs <- scrapeURL url ZeitDe.comments
  print cs
  print $ "Scraped " ++ (show $ length cs) ++ " comments"
  let cs' = Map.fromList(zip (map (ZeitDe.identifierZeitDe Nothing) cs) cs)
  print $ (show $ length $ Map.keys cs') ++ " are different."
