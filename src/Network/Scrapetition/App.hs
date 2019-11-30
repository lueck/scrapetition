module Network.Scrapetition.App
  where

import Control.Concurrent
import Text.HTML.Scalpel
import Data.Maybe
import Control.Lens
import qualified Database.HDBC as DB

import Network.Scrapetition.Comment
import Network.Scrapetition.Env
import Network.Scrapetition.Item
import Network.Scrapetition.Utils


runScraper :: (DB.IConnection c, Item i, ThreadItem i) =>
              Env c i                     -- ^ Environment with config
           -> Scraper String ([i], [URL]) -- ^ the scraper
           -> [URL]                       -- ^ URLs to scrape
           -> [URL]                       -- ^ URLs done
           -> IO ([i])
runScraper conf scraper urls seen = do
  let maybeNext = nextUrl urls seen
      threadItemIdentifier = envCommentIdentifier conf
      threadItemToSql = envThreadItemToSql conf
      insertItemStmt = envInsertItemStmt conf
  case maybeNext of
    Nothing -> do
      print "All URLs seen."
      return []
    Just next -> do
      print $ "Scraping " ++ next
      result <- scrapeURL next scraper
      let comments' = fromMaybe [] $ fmap ((map (flip setItemUrl $ Just next)) . fst) result
          comments = propagateThreads (threadItemIdentifier Nothing) comments'
          newUrls = fromMaybe [] $ fmap snd result
      print $ "Found " ++ (show $ length comments) ++ " items, and "
        ++ (show $ length newUrls) ++ " URLs."
      case (envConn conf) of
        Just conn -> do
          stmt <- DB.prepare conn insertItemStmt
          DB.executeMany stmt $ map threadItemToSql comments
          DB.commit conn
        Nothing -> do
          return ()
      threadDelay 2000000
      moreComments <- runScraper conf scraper (urls++newUrls) (next:seen)
      return $ comments ++ moreComments


nextUrl :: [URL] -> [URL] -> Maybe URL
nextUrl [] _ = Nothing
nextUrl (x:xs) seen
  | x `elem` seen = nextUrl xs seen
  | otherwise = Just x

