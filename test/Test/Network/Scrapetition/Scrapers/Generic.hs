{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.Scrapetition.Scrapers.Generic where

import Test.Framework

import qualified Data.Text as T
import Text.HTML.Scalpel

import Network.Scrapetition.Scrapers.Generic


test_links_empty = do
  assertEqual (Just []) $ scrapeStringLike "" links

test_links_withoutFrag = do
  assertEqual (Just ["github.com"]) $ scrapeStringLike "<a href=\"github.com\">" links

test_links_withFrag = do
  assertEqual (Just ["github.com/lueck/scrapetition#readme"]) $ scrapeStringLike "<a href=\"github.com/lueck/scrapetition#readme\">" links

test_links_onlyFrag = do
  assertEqual (Just ["#readme"]) $ scrapeStringLike "<a href=\"#readme\">" links


test_linksDropFrag_empty = do
  assertEqual (Just []) $ scrapeStringLike "" linksDropFrag

test_linksDropFrag_withoutFrag = do
  assertEqual (Just ["github.com"]) $ scrapeStringLike "<a href=\"github.com\">" linksDropFrag

test_linksDropFrag_withFrag = do
  assertEqual (Just ["github.com/lueck/scrapetition"]) $ scrapeStringLike "<a href=\"github.com/lueck/scrapetition#readme\">" linksDropFrag

test_linksDropFrag_onlyFrag = do
  assertEqual (Just []) $ scrapeStringLike "<a href=\"#readme\">" linksDropFrag

test_linksDropFrag_withoutHref = do
  assertEqual (Just []) $ scrapeStringLike "<a name=\"github.com\">" linksDropFrag

test_linksDropFrag_mailto = do
  assertEqual (Just []) $ scrapeStringLike "<a href=\"mailto:me@github.com\">" linksDropFrag

test_linksDropFrag_javascript = do
  assertEqual (Just []) $ scrapeStringLike "<a href=\"javascript:history.go(-1)\">" linksDropFrag

test_linksDropFrag_relative = do
  assertEqual (Just ["/content"]) $ scrapeStringLike "<a href=\"/content\">" linksDropFrag
