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

import Network.Scrapetition.Comment


-- | Get the domain name from an URL.
domain :: Maybe String -> Maybe String
domain url =
  join $ fmap (fmap uriRegName . join . fmap uriAuthority . parseURI) url

-- | Generate an ID for a comment.
identifier :: String            -- ^ Separator between domain and comment ID
           -> Maybe String      -- ^ A domain maybe provided and if Just overrides.
           -> Maybe String      -- ^ An other comment ID may be provided.
           -> Comment           -- ^ The comment.
           -> String
identifier sep Nothing Nothing c =
  (fromMaybe "unkown" $ domain $ c^.comment_url) ++ sep ++ (c^.comment_id)
identifier sep (Just d) Nothing c =
  d ++ sep ++ (c^.comment_id)
identifier sep Nothing (Just other) c =
  (fromMaybe "unkown" $ domain $ c^.comment_url) ++ sep ++ other
identifier sep (Just d) (Just other) c =
  d ++ sep ++ other

-- | Like 'identifier', but with a standard Separator between domain
-- and comment ID.
identifier' :: Maybe String -> Maybe String -> Comment -> String
identifier' = identifier "|"


-- | Add thread ID to each comment based on recursive links to
-- parents. Comments without a parent are asumed to be thread starters
-- and get their ID as thread ID. Work is done in 'propageThreads\'\''.
propagateThreads :: (Maybe String -> Comment -> String) -- ^ function for making a unique key
                 -> [Comment]                           -- ^ list of comments to work on
                 -> [Comment]
propagateThreads f cs =
  map snd $ Map.toList $ (propagateThreads' f) $ Map.fromList $ zip (map (f Nothing) cs) cs


-- | Same as 'propagateThreads'.
propagateThreads' :: (Maybe String -> Comment -> String)
                  -> Map.Map String Comment
                  -> Map.Map String Comment
propagateThreads' = propagateThreads'' (-1)

-- | Same as 'propagateThreads'. Passing the count of comments already
-- done is absolutely important to avoid an infinite recursion: If we
-- have a comment pointing to a parent, that is not in the data, it
-- can't be assigned to a thread. So incomplete scraping or a denial
-- by the server or even deleted comment would cause our programm to
-- crash without checking the progress this way. See tests.
propagateThreads'' :: Int
                   -> (Maybe String -> Comment -> String)
                   -> Map.Map String Comment
                   -> Map.Map String Comment
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
  = propagateThreads'' 0 f (Map.map (\c -> if c^.comment_parent == Nothing then c & comment_thread .~ (Just $ c^.comment_id) else c) cs)
  | otherwise
  -- Somewhere in the middle: Propagate some IDs and do recursion.
  = propagateThreads'' cntDone f $ Map.map propagate cs
  where
    done = Map.filter (isJust . (^.comment_thread)) cs
    cntDone = Map.size done
    propagate :: Comment -> Comment
    propagate c
      | isJust $ c^.comment_thread = c
      | isJust parent && isJust thread = c & comment_thread .~ thread
      | otherwise = c
      where
        parent = c^.comment_parent
        thread = join $ fmap (^.comment_thread) $ Map.lookup (f parent c) done
