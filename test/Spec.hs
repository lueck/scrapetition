{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest

import {-@ HTF_TESTS @-} Test.Network.Scrapetition.Utils
import {-@ HTF_TESTS @-} Test.Network.Scrapetition.SqlInjection
import {-@ HTF_TESTS @-} Test.Network.Scrapetition.Sql
import {-@ HTF_TESTS @-} Test.Network.Scrapetition.Scrapers.ZeitDe

main = htfMain htf_importedTests
