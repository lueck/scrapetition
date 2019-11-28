{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.Scrapetition.Parsers.ZeitDe where

import Test.Framework

import Text.HTML.Scalpel (scrapeStringLike)

import Network.Scrapetition.Comment
import Network.Scrapetition.Parsers.ZeitDe

testfile = "test/examples/zeit.de.html"

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

test_commentUpVoters = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just [Just "175599", Just "3245074", Just "99135", Just "190268",
           Just "99135", Just "95448", Just "209639", Just "305622"])
    (fmap (map (fmap head . _comment_upVoters)) cs)

test_commentDownVotes = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
           Nothing])
    (fmap (map _comment_downVotes) cs)

test_commentDownVoters = do
  s <- readFile testfile
  let cs = scrapeStringLike s comments
  assertEqual
    (Just [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
           Nothing])
    (fmap (map _comment_downVoters) cs)

test_commentMoreUrls = do
  s <- readFile testfile
  let urls = scrapeStringLike s moreUrls
  assertEqual
    12
    (length urls)
