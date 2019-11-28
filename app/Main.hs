module Main where

import Text.HTML.Scalpel (scrapeURL, scrapeStringLike)

import Network.Scrapetition.Comment
import qualified Network.Scrapetition.Parsers.ZeitDe as ZeitDe (comments, comments')

url = "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache/komplettansicht"


main :: IO ()
main = do
  cs <- scrapeURL url ZeitDe.comments
  print cs
  
