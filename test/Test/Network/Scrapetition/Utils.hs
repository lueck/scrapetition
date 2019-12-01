{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Network.Scrapetition.Utils where

import Test.Framework
import qualified Data.HashMap as Map
import Control.Lens

import Network.Scrapetition.Utils
import Network.Scrapetition.Comment


minC :: String -> Maybe String -> Maybe String -> Comment
minC id_ parent thread =
  Comment "Hello" Nothing Nothing Nothing Nothing id_ parent thread Nothing Nothing Nothing Nothing Nothing Nothing

cId :: Maybe String -> Comment -> String
cId = identifier "|" (Just "testdomain")

csToMap :: [Comment] -> Map.Map String Comment
csToMap cs = Map.fromList $ zip (map (cId Nothing) cs) cs


test_propagateThreadsAllThreadStarters = do
  let cs = [ minC "1" Nothing Nothing
           , minC "2" Nothing Nothing
           , minC "3" Nothing Nothing
           ]
      sc = [ minC "1" Nothing $ Just "1"
           , minC "2" Nothing $ Just "2"
           , minC "3" Nothing $ Just "3"
           ]
  assertEqual sc (propagateThreads cId cs)


test_propagateThreads = do
  let cs = [ minC "1" Nothing Nothing
           , minC "2" Nothing Nothing
           , minC "3" Nothing Nothing
           , minC "4" (Just "1") Nothing
           , minC "5" (Just "4") Nothing
           , minC "6" (Just "4") Nothing
           , minC "7" (Just "2") Nothing
           , minC "8" (Just "4") Nothing
           , minC "9" (Just "6") Nothing
           ]
      sc = [ minC "1" Nothing $ Just "1"
           , minC "2" Nothing $ Just "2"
           , minC "3" Nothing $ Just "3"
           , minC "4" (Just "1") (Just "1") 
           , minC "5" (Just "4") (Just "1")
           , minC "6" (Just "4") (Just "1")
           , minC "7" (Just "2") (Just "2")
           , minC "8" (Just "4") (Just "1")
           , minC "9" (Just "6") (Just "1")
           ]
  assertEqual (csToMap sc) (propagateThreads' cId $ csToMap cs)


test_propagateThreadsWithDuplicate = do
  let cs = [ minC "1" Nothing Nothing
           , minC "2" Nothing Nothing
           , minC "3" Nothing Nothing
           , minC "4" (Just "1") Nothing
           , minC "5" (Just "4") Nothing
           , minC "6" (Just "4") Nothing
           , minC "7" (Just "2") Nothing
           , minC "8" (Just "4") Nothing
           , minC "7" (Just "6") Nothing
           ]
      sc = [ minC "1" Nothing $ Just "1"
           , minC "2" Nothing $ Just "2"
           , minC "3" Nothing $ Just "3"
           , minC "4" (Just "1") (Just "1") 
           , minC "5" (Just "4") (Just "1")
           , minC "6" (Just "4") (Just "1")
           , minC "7" (Just "2") (Just "2")
           , minC "8" (Just "4") (Just "1")
           , minC "7" (Just "6") (Just "1")
           ]
  assertEqual (csToMap sc) (propagateThreads' cId $ csToMap cs)
  -- But the duplicate ID is gone and we have one fewer comment!
  assertNotEqual (length sc) (length $ propagateThreads cId cs)

-- | Test propagation, if it will be incomplete. The function must
-- stop in case of no progress.
test_propagateThreadsIncomplete = do
  let cs = [ minC "1" Nothing Nothing
           , minC "2" Nothing Nothing
           , minC "3" (Just "4") Nothing
           ]
      sc = [ minC "1" Nothing $ Just "1"
           , minC "2" Nothing $ Just "2"
           , minC "3" (Just "4") Nothing
           ]
  assertEqual sc (propagateThreads cId cs)


test_propagateThreadsIncomplete2 = do
  let cs = [ minC "1" (Just "4") Nothing
           , minC "2" (Just "4") Nothing
           , minC "3" (Just "4") Nothing
           ]
      sc = [ minC "1" (Just "4") Nothing
           , minC "2" (Just "4") Nothing
           , minC "3" (Just "4") Nothing
           ]
  assertEqual sc (propagateThreads cId cs)


test_propagateThreadsIncomplete3 = do
  let cs = [ minC "1" Nothing Nothing
           , minC "2" Nothing Nothing
           , minC "3" Nothing Nothing
           , minC "4" (Just "1") Nothing
           , minC "5" (Just "4") Nothing
           , minC "6" (Just "A") Nothing
           , minC "7" (Just "2") Nothing
           , minC "8" (Just "4") Nothing
           , minC "9" (Just "6") Nothing
           ]
      sc = [ minC "1" Nothing $ Just "1"
           , minC "2" Nothing $ Just "2"
           , minC "3" Nothing $ Just "3"
           , minC "4" (Just "1") (Just "1")
           , minC "5" (Just "4") (Just "1")
           , minC "6" (Just "A") Nothing
           , minC "7" (Just "2") (Just "2")
           , minC "8" (Just "4") (Just "1")
           , minC "9" (Just "6") Nothing
           ]
  assertEqual (csToMap sc) (propagateThreads' cId $ csToMap cs)


test_identifier = do
  assertEqual "unkown|1" (identifier "|" Nothing Nothing c)
  assertEqual "test|1" (identifier "|" (Just "test") Nothing c)
  assertEqual "test|3" (identifier "|" (Just "test") (Just "3") c)
  assertEqual "unkown|3" (identifier "|" Nothing (Just "3") c)
  assertEqual "feu.de|3" (identifier "|" Nothing (Just "3") (c & comment_url .~ (Just "http://feu.de/a")))
  assertEqual "testdomain|1" (cId Nothing c)
  assertEqual "testdomain|ah" (cId (Just "ah") c)
  where
    c = minC "1" Nothing Nothing


test_domain = do
  assertEqual (Just "www.feu.de") (domain $ Just "http://www.feu.de/ksw/literatur/lueck.html#Kontakt")
  assertEqual (Just "www.feu.de") (domain $ Just "ftp://www.feu.de/ksw/literatur/lueck.html#Kontakt")
  assertEqual Nothing (domain $ Just "www.feu.de/ksw/literatur/lueck.html#Kontakt")
  assertEqual Nothing (domain $ Just "urn:www.feu.de/ksw/literatur/lueck.html#Kontakt")
  
