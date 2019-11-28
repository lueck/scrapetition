{-# LANGUAGE OverloadedStrings #-}
module Network.Scrapetition.Parsers.ZeitDe
  where

import Control.Applicative
import Text.HTML.Scalpel
import Data.List.Split
import Data.List
import Data.Char

import Network.Scrapetition.Comment

comments :: Scraper String [Comment]
comments = chroots ("article" @: [hasClass "comment"]) comment

comment :: Scraper String Comment
comment = Comment
  <$> (text $ "div" @: [hasClass "comment__body"])
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
       <|> (pure Nothing))
  <*> (pure Nothing) -- FIXME: is there a thread id available?
  <*> (fmap (Just . countOfFans) $
       attr "data-fans" $
       "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"])
  <*> (pure Nothing)
  <*> (fmap (Just . (splitOn ",")) $
       attr "data-fans" $
       "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"])
  <*> (pure Nothing)


moreUrls :: Scraper String [URL]
moreUrls =
  -- chroots (("div" @: [hasClass "js-comment-loader"]) <|>
  --          (("div" @: [hasClass "comment-section__item"]) // "li")) link
  chroots ("div" @: [hasClass "comment-section__item"] // "li") link

link :: Scraper String URL
link = attr "href" $ "a"


userPrefix = "https://profile.zeit.de/"

countOfFans :: String -> Int
countOfFans = foldl (\acc c -> acc + (isComma c)) 1

isComma :: Char -> Int
isComma ',' = 1
isComma _ = 0

stripSpace :: String -> String
stripSpace = (dropWhile isSpace) . (dropWhileEnd isSpace)

fragmentOrUrl :: String -> String
fragmentOrUrl s
  | length broken > 1 = last broken
  | otherwise = s
  where
    broken = splitOn "#" s

-- * This should only be used for develogment:

comments' :: Scraper String [String]
comments' = chroots ("article" @: [hasClass "comment"]) comment'

comment' :: Scraper String String
comment' =
  -- fmap (show . countOfFans) $ attr "data-fans" $ "a" @: [hasClass "comment__reaction", hasClass "js-recommend-comment"]
  -- text $ "div" @: [hasClass "comment__body"]
  -- attr "href" $ "div" @: [hasClass "comment-meta__name"] // "a" -- nicht immer da! 50377209 fehlt
  -- text $ "div" @: [hasClass "comment-meta__name"] // "a" -- nicht immer da!
  attr "id" $ "article"

