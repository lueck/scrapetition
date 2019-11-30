module Network.Scrapetition.Utils
  ( domain
  , identifier
  , identifier'
  , propagateThreads
  , propagateThreads'
  )
  where


import Data.Maybe
import Network.URI
import Control.Monad
import Control.Lens
import qualified Data.HashMap as Map

import Network.Scrapetition.Item


-- | Get the domain name from an URL.
domain :: Maybe String -> Maybe String
domain url =
  join $ fmap (fmap uriRegName . join . fmap uriAuthority . parseURI) url

-- | Generate an ID for a scraped item. It is reasonable to choose
-- some combination of the domain name and the item's ID. The type of
-- the item may be encoded in the separator, if it's not present in
-- the item's ID.
identifier :: (Item i) =>
              String            -- ^ Separator between domain and comment ID
           -> Maybe String      -- ^ A domain maybe provided and if Just overrides.
           -> Maybe String      -- ^ An other comment ID may be provided.
           -> i                 -- ^ The comment.
           -> String
identifier sep Nothing Nothing item =
  (fromMaybe "unkown" $ domain $ (itemUrl item)) ++ sep ++ (itemId item)
identifier sep (Just d) Nothing item =
  d ++ sep ++ (itemId item)
identifier sep Nothing (Just other) item =
  (fromMaybe "unkown" $ domain $ (itemUrl item)) ++ sep ++ other
identifier sep (Just d) (Just other) item =
  d ++ sep ++ other

-- | Like 'identifier', but with a standard Separator between domain
-- and comment ID.
identifier' :: (Item i) => Maybe String -> Maybe String -> i -> String
identifier' = identifier "|"


-- | Add thread ID to each comment based on recursive links to
-- parents. Comments without a parent are asumed to be thread starters
-- and get their ID as thread ID. Work is done in 'propageThreads\'\''.
propagateThreads :: (Item i, ThreadItem i) =>
                    (Maybe String -> i -> String) -- ^ function for making a unique key
                 -> [i]                           -- ^ list of comments to work on
                 -> [i]
propagateThreads f cs =
  map snd $ Map.toList $ (propagateThreads' f) $ Map.fromList $ zip (map (f Nothing) cs) cs


-- | Same as 'propagateThreads'.
propagateThreads' :: (Item i, ThreadItem i) =>
                     (Maybe String -> i -> String)
                  -> Map.Map String i
                  -> Map.Map String i
propagateThreads' = propagateThreads'' (-1)

-- | Same as 'propagateThreads'. Passing the count of comments already
-- done is absolutely important to avoid an infinite recursion: If we
-- have a comment pointing to a parent, that is not in the data, it
-- can't be assigned to a thread. So incomplete scraping or a denial
-- by the server or even deleted comment would cause our programm to
-- crash without checking the progress this way. See tests.
propagateThreads'' :: (Item i, ThreadItem i) =>
                      Int       -- ^ Call with (-1)!
                   -> (Maybe String -> i -> String)
                   -> Map.Map String i
                   -> Map.Map String i
propagateThreads'' lastCnt f cs
  | cntDone == lastCnt
  -- No progress since last recursion step. Stop and return map.
  = cs
  | cntDone == Map.size cs
  -- All comments done. Return map.
  = cs
  | cntDone == 0
  -- No comments done yet. Assign threads to comments without parents
  -- and recurse.
  = propagateThreads'' 0 f (Map.map (\c -> if (itemParent c) == Nothing then (setItemThread c $ Just $ itemId c) else c) cs)
  | otherwise
  -- Somewhere in the middle: Propagate some IDs and do recursion.
  = propagateThreads'' cntDone f $ Map.map propagate cs
  where
    done = Map.filter (isJust . itemThread) cs
    cntDone = Map.size done
    -- propagate :: (Item i, ThreadItem i) => i -> i
    propagate c
      | isJust $ itemThread c = c
      | isJust parent && isJust thread = setItemThread c thread
      | otherwise = c
      where
        parent = itemParent c
        thread = join $ fmap itemThread $ Map.lookup (f parent c) done
