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
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Data.Time
import Data.Time.Format

import Network.Scrapetition.Comment
import Network.Scrapetition.Utils
import Network.Scrapetition.User
import Network.Scrapetition.Vote
import Network.Scrapetition.Article
import Network.Scrapetition.Env
import Network.Scrapetition.Item
import Network.Scrapetition.Dispatcher
import Network.Scrapetition.Scrapers.Generic


-- | A dispatcher for scraping comments from www.zeit.de
zeitDeCommentDispatcher :: Dispatcher
zeitDeCommentDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de.*"
  , _dptchr_scraper = flip scrapeStringLike commentsPacked
  , _dptchr_urlScraper = flip scrapeStringLike (andArticleUrls collectCommentUrls)
  , _dptchr_insertItemStmt = commentInsertStmt
  , _dptchr_itemName = "comment"
  }

-- | A dispatcher for scraping authors of comments from www.zeit.de
zeitDeUserDispatcher :: Dispatcher
zeitDeUserDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de.*"
  , _dptchr_scraper = flip scrapeStringLike usersPacked
  , _dptchr_urlScraper = const Nothing
  , _dptchr_insertItemStmt = userInsertStmt
  , _dptchr_itemName = "user"
  }

-- | A dispatcher for scraping user IDs of votings from www.zeit.de
zeitDeVoterDispatcher :: Dispatcher
zeitDeVoterDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de.*"
  , _dptchr_scraper = flip scrapeStringLike votersPacked
  , _dptchr_urlScraper = const Nothing
  , _dptchr_insertItemStmt = userInsertStmt
  , _dptchr_itemName = "voter" -- not users, so in future we can relate!
  }

-- | A dispatcher for scraping user IDs of votings from www.zeit.de
zeitDeVotingDispatcher :: Dispatcher
zeitDeVotingDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de.*"
  , _dptchr_scraper = flip scrapeStringLike votingsPacked
  , _dptchr_urlScraper = const Nothing
  , _dptchr_insertItemStmt = voteInsertStmt
  , _dptchr_itemName = "comment_voting"
  }

-- | A dispatcher for scraping articles from www.zeit.de
zeitDeArticleDispatcher :: Dispatcher
zeitDeArticleDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de/[^?]*"
  , _dptchr_scraper = flip scrapeStringLike articlesPacked
  , _dptchr_urlScraper = const Nothing
  , _dptchr_insertItemStmt = articleInsertStmt
  , _dptchr_itemName = "article"
  }

-- | A dispatcher for scraping articles from www.zeit.de. This works
-- even for thread pages. It is necessary, because sometimes crawling
-- jumps to a thread page immediately, not to the article page
-- first. This is the case when scraping a users profile.
zeitDeArticleFromCommentDispatcher :: Dispatcher
zeitDeArticleFromCommentDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de/.*"
  , _dptchr_scraper = flip scrapeStringLike articlesFromCommentsPacked
  , _dptchr_urlScraper = const Nothing
  , _dptchr_insertItemStmt = articleInsertStmt
  , _dptchr_itemName = "article"
  }

zeitDeProfileDispatcher :: Dispatcher
zeitDeProfileDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?profile.zeit.de.*"
  , _dptchr_scraper = const Nothing
  , _dptchr_urlScraper = flip scrapeStringLike collectProfileUrls
  , _dptchr_insertItemStmt = Map.empty
  , _dptchr_itemName = "url"
  }

zeitDeSearchDispatcher :: Dispatcher
zeitDeSearchDispatcher = Dispatcher
  { _dptchr_urlScheme = "^(https?://)?www.zeit.de/suche/.*"
  , _dptchr_scraper = flip scrapeStringLike articlesSearchedPacked
  , _dptchr_urlScraper = flip scrapeStringLike collectSearchResultUrls
  , _dptchr_insertItemStmt = articleInsertStmt
  , _dptchr_itemName = "article"
  }

zeitDeDispatchers :: [Dispatcher]
zeitDeDispatchers =
  [ zeitDeArticleDispatcher
  , zeitDeArticleFromCommentDispatcher
  , zeitDeUserDispatcher
  , zeitDeCommentDispatcher
  , zeitDeVoterDispatcher
  , zeitDeVotingDispatcher
  , zeitDeProfileDispatcher
  , zeitDeSearchDispatcher
  ]


commentsPacked :: Scraper T.Text [ScrapedItem]
commentsPacked = fmap (map MkScrapedItem) comments

-- | Scrape all comments from a page.
comments :: Scraper T.Text [Comment]
comments = chroots ("article" @: [hasClass "comment"]) comment

-- | Scrape a single comment given in scalpel's chroot.
comment :: Scraper T.Text Comment
comment = Comment
  <$> (fmap T.strip $ innerHTML $ "div" @: [hasClass "comment__body"]) -- comment
  <*> ((fmap (Just . T.strip . (T.takeWhile ((/=8212) . ord))) $
        text $ "a" @: [hasClass "comment-meta__date"])
       <|> (pure Nothing))      -- title: take #x.y as title, U+8212 is a em-dash
  <*> ((fmap (T.stripPrefix userPrefix) $ attr "href" $
        -- CHANGE: "div" to "h4" on 2020-01-08, so we use AnyTag
        AnyTag @: [hasClass "comment-meta__name"] // "a")
       <|> (pure Nothing)) -- FIXME: Should we use the user name
                           -- instead? I think no, because the absence
                           -- of an identifier is an information.
  <*> ((fmap Just $ text $ AnyTag @: [hasClass "comment-meta__name"] // "a")
       <|> (fmap (Just . T.strip) $
            text $ AnyTag @: [hasClass "comment-meta__name"]))
  <*> ((fmap (Just . T.strip . (T.dropWhile ((==8212) . ord)) . (T.dropWhile ((/=8212) . ord))) $
        text $ "a" @: [hasClass "comment-meta__date"])
       <|> (pure Nothing))      -- informal datetime
  <*> (pure Nothing)            -- no formal datetime
  <*> (attr "id" $ "article")   -- ID
  <*> ((fmap (Just . fragmentOrUrl) $ attr "href" $ "a" @: [hasClass "comment__origin"])
       <|> -- CHANGE: form instead of link on 2020-01-08
       (fmap (Just . (\t -> "cid-"<>t)) $ attr "value" $
        "input" @: [match (\k v -> k=="name" && v=="cid")])
       <|>
       (pure Nothing))     -- comment__origin is parent! Verified!
  <*> (pure Nothing)            -- No thread id available! Verified!
  <*> (fmap (Just . countOfFans) $
       attr "data-fans" $
       -- CHANGE: link to form on 2020-01-08
       AnyTag @: [hasClass "comment__reaction", hasClass "js-recommend-comment"]) -- up votes
  <*> (pure Nothing)            -- down votes
  <*> ((fmap (Just . (T.takeWhile (/='?'))) $
        attr "href" $ "a" @: [hasClass "comment-meta__date"])
       <|> (pure Nothing))      -- article
  <*> (pure Nothing)            -- articleVoting
  <*> (pure Nothing)            -- parentVoting
  <*> (pure Nothing)            -- url
  <*> (pure Nothing)            -- scrapeDate
  <*> (pure Nothing)            -- scraper


usersPacked :: Scraper T.Text [ScrapedItem]
usersPacked = fmap (map MkScrapedItem) users

-- | Scrape users from a thread page. This means simply taking the users from the comments.
users :: Scraper T.Text [User]
users = fmap (catMaybes . (map contributor)) comments

-- * Article

-- | Scrape articles (only URIs) from a thread page.
articlesFromComments :: Scraper T.Text [Article]
articlesFromComments = fmap (nub . catMaybes . (map articleWithCanonicalUrl)) comments

articlesFromCommentsPacked :: Scraper T.Text [ScrapedItem]
articlesFromCommentsPacked = fmap (map MkScrapedItem) articles

-- | Scrape the article meta data from a article start page
articles :: Scraper T.Text [Article]
articles = chroots "html" article

articlesPacked :: Scraper T.Text [ScrapedItem]
articlesPacked = fmap (map MkScrapedItem) articles

article :: Scraper T.Text Article
article = Article
  <$> (attr "href" $ "link" @: [match (\k v -> k=="rel" && v=="canonical")])
  <*> (fmap (Just . T.strip . T.takeWhile (/='|')) $ text $ "title")
  <*> ((fmap (Just . T.strip) $
        attr "content" $ "meta" @: [match (\k v -> k=="name" && v=="description")])
        <|>
        (pure Nothing))
  <*> (fmap Just $ text $
       "div" @: [hasClass "byline"] // "span" @: [match (\k v -> k=="itemprop" && v=="name")])
  <*> (fmap (parseZeitDeDatetime . T.unpack) $ attr "content" $
       "meta" @: [match (\k v -> k=="name" && v=="date")])
  <*> (pure Nothing)
  <*> (pure Nothing)
  <*> (pure Nothing)


-- | Scrape articles from search results
articlesSearched :: Scraper T.Text [Article]
articlesSearched = chroots ("article" @: [hasClass "zon-teaser-standard"]) articleSearched

articlesSearchedPacked :: Scraper T.Text [ScrapedItem]
articlesSearchedPacked = fmap (map MkScrapedItem) articlesSearched

articleSearched :: Scraper T.Text Article
articleSearched = Article
  <$> (attr "href" $ "a" @: [hasClass "zon-teaser-standard__combined-link"])
  <*> (fmap (Just . T.strip) $
       text $ "span" @: [hasClass "zon-teaser-standard__title"])
  -- description: In the search result, the description is presented as text.
  <*> ((fmap (Just . T.strip) $
        text $ "p" @: [hasClass "zon-teaser-standard__text"])
       <|>
       (pure Nothing))
  <*> (fmap (Just . stripAuthor) $
       text $ "span" @: [hasClass "zon-teaser-standard__byline"])
  <*> (fmap (parseZeitDeDatetime . T.unpack) $ attr "datetime" $
       "time" @: [hasClass "zon-teaser-standard__datetime"])
  <*> (pure Nothing)
  <*> (pure Nothing)
  <*> (pure Nothing)


-- * Votings

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
  -- CHANGE: link to form on 2020-01-08
  AnyTag @: [hasClass "comment__reaction", hasClass "js-recommend-comment"]

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

-- | Add the implied article URLs to the URLs returned by a scraper.
andArticleUrls :: Scraper T.Text [URL] -> Scraper T.Text [URL]
andArticleUrls collector =
  fmap (\urls -> (nub $ map stripQuery urls) ++ urls) collector
  where
    stripQuery = takeWhile (/='?')

-- | Collect URLs to further comments.
collectCommentUrls :: Scraper T.Text [URL]
collectCommentUrls = (++)
  <$> commentJsLoaderUrls
  <*> pagerUrls

-- | Collect the URLs for completing the shown threads plus the next
-- page of comments. This is not enough when we start in the middle of
-- a discussion.
threadsAndNextUrl :: Scraper T.Text [URL]
threadsAndNextUrl = (++)
  <$> commentJsLoaderUrls
  <*> commentNextButtonUrl

-- | Collect the URLs under the comments.
commentJsLoaderUrls :: Scraper T.Text [URL]
commentJsLoaderUrls =
  chroots ("div" @: [hasClass "js-comment-loader"]) (fmap dropFragment link)


-- | Collect the URL from the next button in the footer. Caveat: This
-- is not enough when we jump into the middle of a discussion
-- e.g. from a user profile. Use 'pagerUrls' instead!
commentNextButtonUrl :: Scraper T.Text [URL]
commentNextButtonUrl =
  chroots ("div" @: [hasClass "comment-section__item"] //
           "a" @: [hasClass "pager__button--next"]) link

-- | Collect the URLs from a pager.
pagerUrls :: Scraper T.Text [URL]
pagerUrls =
  chroots ("ul" @: [hasClass "pager__pages"] //
           "li" @: [hasClass "pager__page"]) (fmap dropFragment link)

-- | URLs from the user profile. We only scrape URLs. There is a lack
-- of information on the comments there: no parent ID e.g.
collectProfileUrls :: Scraper T.Text [URL]
collectProfileUrls = (++)
  <$> (andArticleUrls profileDiscussionUrls)
  <*> pagerUrls

-- | Scrape a link to a discussion item.
profileDiscussionUrls :: Scraper T.Text [URL]
profileDiscussionUrls =
  chroots ("article" @: [hasClass "user-comment"]) discussionUrl

discussionUrl :: Scraper T.Text URL
discussionUrl =
  fmap (dropFragment . T.unpack) $
  attr "href" $ "a" @: [hasClass "user-comment__link"]

-- | Scrape links from search results and the pager for more search results.
collectSearchResultUrls :: Scraper T.Text [URL]
collectSearchResultUrls = (++)
  <$> pagerUrls
  <*> (fmap (map (T.unpack . _artcl_canonical)) articlesSearched)


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

-- | Drop the fragment identifier from the given url.
dropFragment :: URL -> URL
dropFragment = takeWhile (/='#')

-- | Parse a time string like "2020-01-09T14:15:58+01:00".
parseZeitDeDatetime :: String -> Maybe UTCTime
parseZeitDeDatetime s = id
  <$> (parseTimeM True defaultTimeLocale "%FT%T%z" s :: Maybe UTCTime)

-- | Strip descriptions like "Von" or "Ein Kolumne von" around the
-- author of an article. We use the fact, that there are several
-- spaces before the name.
stripAuthor :: T.Text -> T.Text
stripAuthor s = T.strip $ last $ T.splitOn "  " s


-- * Playing around

-- | Play with: stack runghc ZeitDe.hs
main :: IO ()
main = do
  let folder = "../../../../test/examples/"
      start = "zeit.de.article.html"
      thread = "zeit.de.thread.html"
      search = "zeit.de.search.html"
  s <- T.readFile $ folder ++ start -- search
  let content = scrapeStringLike s (andArticleUrls collectCommentUrls) -- articlesSearched
  print content
  print $ show $ fmap length content
