{-# LANGUAGE OverloadedStrings #-}
module Network.Scrapetition.Scrapers.ZeitDe
  where

-- | Scrape http://www.zeit.de for comments in the discussion.

import Control.Applicative
import Text.HTML.Scalpel
import Data.List.Split
import Data.List
import Data.Char
import Control.Lens
import Data.Maybe
import qualified Data.Text as T

import Network.Scrapetition.Comment
import Network.Scrapetition.Utils
import Network.Scrapetition.User
import Network.Scrapetition.Vote
import Network.Scrapetition.Env
import Network.Scrapetition.Item
import Network.Scrapetition.Dispatcher
import Network.Scrapetition.Scrapers.Generic


-- | A dispatcher for scraping comments from www.zeit.de
zeitDeCommentDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de.*"
  , _dptchr_scraper = commentsPacked
  , _dptchr_urlScraper = collectCommentUrls
  , _dptchr_insertItemStmt = commentInsertStmt
  , _dptchr_tableName = "comments"
  }

-- | A dispatcher for scraping authors of comments from www.zeit.de
zeitDeUserDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de.*"
  , _dptchr_scraper = usersPacked
  , _dptchr_urlScraper = collectCommentUrls
  , _dptchr_insertItemStmt = userInsertStmt
  , _dptchr_tableName = "users"
  }

-- | A dispatcher for scraping user IDs of votings from www.zeit.de
zeitDeVoterDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de.*"
  , _dptchr_scraper = votersPacked
  , _dptchr_urlScraper = collectCommentUrls
  , _dptchr_insertItemStmt = userInsertStmt
  , _dptchr_tableName = "voters" -- not users, so in future we can relate!
  }

-- | A dispatcher for scraping user IDs of votings from www.zeit.de
zeitDeVotingDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de.*"
  , _dptchr_scraper = votingsPacked
  , _dptchr_urlScraper = collectCommentUrls
  , _dptchr_insertItemStmt = voteInsertStmt
  , _dptchr_tableName = "comment_voting"
  }


zeitDeDispatchers :: [Dispatcher]
zeitDeDispatchers =
  [ zeitDeCommentDispatcher
  , zeitDeUserDispatcher
  , zeitDeVoterDispatcher
  , zeitDeVotingDispatcher
  ]

-- | Generate a unique identifier for a comment. For zeit.de this is
-- the domain name concatenated with an comment ID.
identifierZeitDe :: Maybe T.Text -> Comment -> T.Text
identifierZeitDe = identifier' (Just "www.zeit.de")


-- | Scrape comments and a reasonable set of URLs.
commentsThreadsAndNext :: Scraper T.Text ([Comment], [URL])
commentsThreadsAndNext = (\cs urls -> (cs, urls))
  <$> comments
  <*> threadsAndNextUrl


commentsPacked :: Scraper T.Text [ScrapedItem]
commentsPacked = fmap (map MkScrapedItem) comments

-- | Scrape all comments from a page.
comments :: Scraper T.Text [Comment]
comments = chroots ("article" @: [hasClass "comment"]) comment

-- | Scrape a single comment given in scalpel's chroot.
comment :: Scraper T.Text Comment
comment = Comment
  <$> (fmap T.strip $ innerHTML $ "div" @: [hasClass "comment__body"])
  <*> ((fmap (Just . T.strip . (T.takeWhile ((/=8212) . ord))) $
        text $ "a" @: [hasClass "comment-meta__date"])
       <|> (pure Nothing))      -- title: take #x.y as title, U+8212 is a em-dash
  <*> ((fmap (T.stripPrefix userPrefix) $ attr "href" $
        "div" @: [hasClass "comment-meta__name"] // "a")
       <|> (pure Nothing))
  <*> ((fmap Just $ text $ "div" @: [hasClass "comment-meta__name"] // "a")
       <|> (fmap (Just . T.strip) $
            text $ "div" @: [hasClass "comment-meta__name"]))
  <*> ((fmap (Just . T.strip . (T.dropWhile ((==8212) . ord)) . (T.dropWhile ((/=8212) . ord))) $
        text $ "a" @: [hasClass "comment-meta__date"])
       <|> (pure Nothing))      -- informal datetime
  <*> (pure Nothing)            -- no formal datetime
  <*> (attr "id" $ "article")   -- ID
  <*> ((fmap (Just . fragmentOrUrl) $ attr "href" $ "a" @: [hasClass "comment__origin"])
        <|> (pure Nothing))     -- comment__origin is parent! Verified!
  <*> (pure Nothing)            -- No thread id available! Verified!
  <*> (fmap (Just . countOfFans) $
       attr "data-fans" $
       "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"]) -- up votes
  <*> (pure Nothing)            -- down votes
  <*> (pure Nothing)            -- url
  <*> (pure Nothing)            -- scrapeDate
  <*> (pure Nothing)            -- scraper


usersPacked :: Scraper T.Text [ScrapedItem]
usersPacked = fmap (map MkScrapedItem) users

-- | Scrape users from a thread page. This means simply taking the users from the comments.
users :: Scraper T.Text [User]
users =
  fmap (catMaybes . (map contributor)) $
  chroots ("article" @: [hasClass "comment"]) comment


votingsPacked :: Scraper T.Text [ScrapedItem]
votingsPacked = fmap (map MkScrapedItem) votings

-- | Scrape votings or so.
votings :: Scraper T.Text [Vote]
votings =
  fmap (concat . (map mkVotes)) $
  chroots ("article" @: [hasClass "comment"]) commentAndVotingNumbers
  where
    mkVotes :: (Comment, [T.Text]) -> [Vote]
    mkVotes (comment, vs) = map (mkVote comment) vs
    mkVote :: Comment -> T.Text -> Vote
    mkVote c u = Vote u (_comment_id c) 1 Nothing Nothing Nothing 
  
votingNumbers :: Scraper T.Text [T.Text]
votingNumbers =
  fmap (T.splitOn ",") $
  attr "data-fans" $
  "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"]

commentAndVotingNumbers :: Scraper T.Text (Comment, [T.Text])
commentAndVotingNumbers = (,)
  <$> comment
  <*> votingNumbers


votersPacked :: Scraper T.Text [ScrapedItem]
votersPacked = fmap (map MkScrapedItem) voters

voters :: Scraper T.Text [User]
voters =
  fmap (concat . (map mkVoters)) $
  chroots ("article" @: [hasClass "comment"]) votingNumbers
  where
    mkVoters :: [T.Text] -> [User]
    mkVoters nums = map mkUser nums
    mkUser :: T.Text -> User
    mkUser num = User num Nothing Nothing Nothing Nothing


-- * URLs

-- | Collect URLs to further comments.
collectCommentUrls :: Scraper T.Text [URL]
collectCommentUrls = (\a b c -> concat [a, b, c])
  <$> commentJsLoaderUrls
  <*> commentSectionUrls
  <*> commentNextButtonUrl -- FIXME: can we use <+> or <> ???

-- | Collect the URLs for completing the shown threads plus the next
-- page of comments.
threadsAndNextUrl :: Scraper T.Text [URL]
threadsAndNextUrl = (\a b -> concat [a, b])
  <$> commentJsLoaderUrls
  <*> commentNextButtonUrl

-- | Collect the URLs under the comments.
commentJsLoaderUrls :: Scraper T.Text [URL]
commentJsLoaderUrls =
  chroots ("div" @: [hasClass "js-comment-loader"]) link


-- | Collect the URL from the next button in the footer.
commentNextButtonUrl :: Scraper T.Text [URL]
commentNextButtonUrl =
  chroots ("div" @: [hasClass "comment-section__item"] //
           "a" @: [hasClass "pager__button--next"]) link


-- | Collect the URLs from the pager in the footer.
commentSectionUrls :: Scraper T.Text [URL]
commentSectionUrls =
  chroots ("div" @: [hasClass "comment-section__item"] // "li") link


-- * Pure helper functions

-- | The prefix of user names
userPrefix = "https://profile.zeit.de/"

-- | Count the fans of a comment.
countOfFans :: T.Text -> Int
countOfFans = T.foldl (\acc c -> acc + (isComma c)) 1
  where
    isComma :: Char -> Int
    isComma ',' = 1
    isComma _ = 0

-- -- | Sprip whitespace from a 'T.Text'.
-- stripSpace :: T.Text -> T.Text
-- stripSpace = (T.dropWhile isSpace) . (dropWhileEnd isSpace)

-- | Return the fragment identifier or (if not present) the given url.
fragmentOrUrl :: T.Text -> T.Text
fragmentOrUrl s
  | length broken > 1 = last broken
  | otherwise = s
  where
    broken = T.splitOn "#" s


-- * This should only be used for development:

comments' :: Scraper T.Text [T.Text]
comments' = chroots ("article" @: [hasClass "comment"]) comment'

comment' :: Scraper T.Text T.Text
comment' =
  -- fmap (show . countOfFans) $ attr "data-fans" $ "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"]
  -- text $ "div" @: [hasClass "comment__body"]
  -- attr "href" $ "div" @: [hasClass "comment-meta__name"] // "a" -- nicht immer da! 50377209 fehlt
  -- text $ "div" @: [hasClass "comment-meta__name"] // "a" -- nicht immer da!
  attr "id" $ "article"

