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

import Network.Scrapetition.Comment
import Network.Scrapetition.Utils


-- | Generate a unique identifier for a comment. For zeit.de this is
-- the domain name concatenated with an comment ID.
identifierZeitDe :: Maybe String -> Comment -> String
identifierZeitDe = identifier' (Just "www.zeit.de")


-- | Scrape comments and a reasonable set of URLs.
commentsThreadsAndNext :: Scraper String ([Comment], [URL])
commentsThreadsAndNext = (\cs urls -> (cs, urls))
  <$> comments
  <*> threadsAndNextUrl


-- | Scrape all comments from a page.
comments :: Scraper String [Comment]
comments = chroots ("article" @: [hasClass "comment"]) comment

-- | Scrape a single comment given in scalpel's chroot.
comment :: Scraper String Comment
comment = Comment
  <$> (innerHTML $ "div" @: [hasClass "comment__body"])
  <*> ((fmap (stripPrefix userPrefix) $ attr "href" $
        "div" @: [hasClass "comment-meta__name"] // "a")
       <|> (pure Nothing))
  <*> ((fmap Just $ text $ "div" @: [hasClass "comment-meta__name"] // "a")
       <|> (fmap (Just . stripSpace) $
            text $ "div" @: [hasClass "comment-meta__name"]))
  <*> ((fmap (Just . stripSpace) $ text $ "a" @: [hasClass "comment-meta__date"])
       <|> (pure Nothing))
  <*> (attr "id" $ "article")
  <*> ((fmap (Just . fragmentOrUrl) $ attr "href" $ "a" @: [hasClass "comment__origin"])
       <|> (pure Nothing)) -- comment__origin is parent! Verified!
  <*> (pure Nothing) -- FIXME: is there a thread id available?
  <*> (fmap (Just . countOfFans) $
       attr "data-fans" $
       "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"])
  <*> (pure Nothing)
  <*> (fmap (Just . (flip zip (repeat 1)) . (splitOn ",")) $
       attr "data-fans" $
       "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"])
  <*> (pure Nothing) -- down voters
  <*> (pure Nothing) -- url


-- | Collect URLs to further comments.
collectCommentUrls :: Scraper String [URL]
collectCommentUrls = (\a b c -> concat [a, b, c])
  <$> commentJsLoaderUrls
  <*> commentSectionUrls
  <*> commentNextButtonUrl -- FIXME: can we use <+> or <> ???

-- | Collect the URLs for completing the shown threads plus the next
-- page of comments.
threadsAndNextUrl :: Scraper String [URL]
threadsAndNextUrl = (\a b -> concat [a, b])
  <$> commentJsLoaderUrls
  <*> commentNextButtonUrl

-- | Collect the URLs under the comments.
commentJsLoaderUrls :: Scraper String [URL]
commentJsLoaderUrls =
  chroots ("div" @: [hasClass "js-comment-loader"]) link


-- | Collect the URL from the next button in the footer.
commentNextButtonUrl :: Scraper String [URL]
commentNextButtonUrl =
  chroots ("div" @: [hasClass "comment-section__item"] //
           "a" @: [hasClass "pager__button--next"]) link


-- | Collect the URLs from the pager in the footer.
commentSectionUrls :: Scraper String [URL]
commentSectionUrls =
  chroots ("div" @: [hasClass "comment-section__item"] // "li") link


-- | Return the href attribute of an html ancor.
link :: Scraper String URL
link = attr "href" $ "a"


-- * Pure helper functions

-- | The prefix of user names
userPrefix = "https://profile.zeit.de/"

-- | Count the fans of a comment.
countOfFans :: String -> Int
countOfFans = foldl (\acc c -> acc + (isComma c)) 1
  where
    isComma :: Char -> Int
    isComma ',' = 1
    isComma _ = 0

-- | Sprip whitespace from a 'String'.
stripSpace :: String -> String
stripSpace = (dropWhile isSpace) . (dropWhileEnd isSpace)

-- | Return the fragment identifier or (if not present) the given url.
fragmentOrUrl :: String -> String
fragmentOrUrl s
  | length broken > 1 = last broken
  | otherwise = s
  where
    broken = splitOn "#" s


-- * This should only be used for development:

comments' :: Scraper String [String]
comments' = chroots ("article" @: [hasClass "comment"]) comment'

comment' :: Scraper String String
comment' =
  -- fmap (show . countOfFans) $ attr "data-fans" $ "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"]
  -- text $ "div" @: [hasClass "comment__body"]
  -- attr "href" $ "div" @: [hasClass "comment-meta__name"] // "a" -- nicht immer da! 50377209 fehlt
  -- text $ "div" @: [hasClass "comment-meta__name"] // "a" -- nicht immer da!
  attr "id" $ "article"

