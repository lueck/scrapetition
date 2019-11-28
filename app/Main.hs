module Main where

import Text.HTML.Scalpel (scrapeURL, scrapeStringLike)

import Network.Scrapetition.Comment
import qualified Network.Scrapetition.Parsers.ZeitDe as ZeitDe (comments, comments')

url = "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache/komplettansicht"

testfile = "/home/clueck/src/scrapetition/test/examples/zeit.de.html"

main :: IO ()
main = do
  s <- readFile testfile
  let cs = scrapeStringLike s ZeitDe.comments
  print $ fmap (map _comment_name) cs
  print $ fmap length cs
  -- cs <- scrapeURL url ZeitDe.comments'
  --print cs
  
