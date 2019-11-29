module Main where

import Text.HTML.Scalpel (scrapeURL, scrapeStringLike)

import Network.Scrapetition.Comment
import Network.Scrapetition.Env
import Network.Scrapetition.App
import qualified Network.Scrapetition.Scrapers.ZeitDe as ZeitDe (commentsThreadsAndNext)
import qualified Network.Scrapetition.Scrapers.ZeitDe as ZeitDe (comments, comments')


url = "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache/komplettansicht"


main :: IO ()
main = do
  cs <- runScraper Env ZeitDe.commentsThreadsAndNext [url] []
  -- cs <- scrapeURL url ZeitDe.comments
  print cs
  print $ length cs
  
