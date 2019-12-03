{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.Scrapetition.Scrapers.ZeitDe where

import Test.Framework

import Text.HTML.Scalpel (scrapeStringLike)

import Network.Scrapetition.Comment
import Network.Scrapetition.Scrapers.ZeitDe

testfile = "test/examples/zeit.de.html"

testurl = "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache/komplettansicht"


test_commentCount = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual (Just 8) (fmap length cs)

test_commentName = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just [Just "regelus", Just "FnordPrefect", Just "betreb",
           Just "1 Gehirnlein", Just "Wichtiger Hinweis", Just "The Council",
           Just "centenarium_kid", Just "teek"])
    (fmap (map _comment_name) cs)

test_commentUser = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just [Just "3096219", Just "5786217", Just "2831850", Nothing,
           Just "5119426", Just "5775461", Just "6415870", Just "2777961"])
    (fmap (map _comment_user) cs)

test_commentId = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just ["cid-50377101", "cid-50381108", "cid-50377129", "cid-50377209",
           "cid-50377144", "cid-50377186", "cid-50377152", "cid-50377368"])
    (fmap (map _comment_id) cs)

test_commentParent = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just [Nothing, Just "cid-50377101", Nothing, Just "cid-50377129",
           Nothing, Just "cid-50377144", Nothing, Just "cid-50377152"])
    (fmap (map _comment_parent) cs)

test_commentUpVotes = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just [Just 127, Just 1, Just 166, Just 9, Just 23, Just 260, Just 52,
           Just 11])
    (fmap (map _comment_upVotes) cs)

-- test_commentVoters = do
--   s <- readFile testfile
--   let cs = scrapeStringLike s comments
--   assertEqual
--     (Just [Just ("175599", 1), Just ("3245074", 1), Just ("99135", 1),
--            Just ("190268", 1), Just ("99135", 1), Just ("95448", 1),
--            Just ("209639", 1), Just ("305622", 1)])
--     (fmap (map (fmap head . _comment_voters)) cs)

test_commentDownVotes = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
           Nothing])
    (fmap (map _comment_downVotes) cs)

test_commentCommentJsLoaderUrls = do
  s <- readFile testfile
  let urls = scrapeStringLike s commentJsLoaderUrls
  assertEqual
    (Just 3)
    (fmap length urls)

test_commentSectionUrls = do
  s <- readFile testfile
  let urls = scrapeStringLike s commentSectionUrls
  assertEqual
    (Just 5)
    (fmap length urls)

test_commentNextButtonUrl = do
  s <- readFile testfile
  let urls = scrapeStringLike s commentNextButtonUrl
  assertEqual
    (Just 1)
    (fmap length urls)

test_threadsAndNextUrl = do
  s <- readFile testfile
  let urls = scrapeStringLike s threadsAndNextUrl
  assertEqual
    (Just 4)
    (fmap length urls)

test_collectCommentUrls = do
  s <- readFile testfile
  let urls = scrapeStringLike s collectCommentUrls
  assertEqual
    (Just 9)
    (fmap length urls)

test_collectCommentUrlsUrls = do
  s <- readFile testfile
  let urls = scrapeStringLike s collectCommentUrls
  assertEqual
    (Just ["https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?cid=50377209#cid-50377129",
           "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?cid=50377186#cid-50377144",
           "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?cid=50377368#cid-50377152",
           "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?page=2#comments",
           "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?page=3#comments",
           "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?page=4#comments",
           "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?page=5#comments",
           "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?page=35#comments",
           "https://www.zeit.de/arbeit/2019-10/diskriminierung-beruf-transsexualitaet-bewerbung-ansprache?page=2#comments"])
    urls
