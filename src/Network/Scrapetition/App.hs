module Network.Scrapetition.App
  where

import Control.Concurrent
import Text.HTML.Scalpel
import Data.Maybe
import Control.Lens

import Network.Scrapetition.Comment
import Network.Scrapetition.Env


runScraper :: Env -> Scraper String ([Comment], [URL]) -> [URL] -> [URL] -> IO ([Comment])
runScraper conf scraper urls seen = do
  let maybeNext = nextUrl urls seen
  case maybeNext of
    Nothing -> do
      print "All URLs seen."
      return []
    Just next -> do
      print $ "Scraping " ++ next
      result <- scrapeURL next scraper
      let comments = fromMaybe [] $ fmap ((map (& comment_url .~ Just next)) . fst) result
          newUrls = fromMaybe [] $ fmap snd result
      print $ "Found " ++ (show $ length comments) ++ " comments, and "
        ++ (show $ length newUrls) ++ " URLs."
      threadDelay 2000000
      moreComments <- runScraper conf scraper (urls++newUrls) (next:seen)
      return $ comments ++ moreComments


nextUrl :: [URL] -> [URL] -> Maybe URL
nextUrl [] _ = Nothing
nextUrl (x:xs) seen
  | x `elem` seen = nextUrl xs seen
  | otherwise = Just x

