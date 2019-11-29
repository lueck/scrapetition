module Network.Scrapetition.Utils
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



-- | Add thread ID each comment based on recursive links to
-- parents. Comments without a parent are asumed to be thread starters
-- and get their ID as thread ID. Work is done in 'propageThreads\''.
propagateThreads :: [Comment] -> [Comment]
propagateThreads cs =
  map snd $ Map.toList $ propagateThreads' $ Map.fromList $ zip (map (^.comment_id) cs) cs

-- | Same as 'propagateThreads'. The keys of the hashmaps must be made
-- from the comments' IDs.
propagateThreads' :: Map.Map String Comment -> Map.Map String Comment
propagateThreads' cs
  | Map.size done == Map.size cs
  = cs
  | Map.size done == 0
  = propagateThreads' (Map.map (\c -> if c^.comment_parent == Nothing then c & comment_thread .~ (Just $ c^.comment_id) else c) cs)
  | otherwise
  = propagateThreads' $ Map.map propagate cs
  where
    done = Map.filter (isJust . (^.comment_thread)) cs
    propagate :: Comment -> Comment
    propagate c
      | isJust $ c^.comment_thread = c
      | isJust parent && isJust thread = c & comment_thread .~ thread
      | otherwise = c
      where
        parent = c^.comment_parent
        thread = join $ fmap (^.comment_thread) $ Map.lookup (fromMaybe "unkown" parent) done
        
-- propagateThreads :: Map.Map String Comment -> Map.Map String Comment -> [Comment]
-- propagateThreads done Map.empty = Map.toList done
-- propagateThreads Map.empty todo =
--   uncurry propagateThreads $ Map.partition (isJust . (.^comment_thread)) propagated
--   where
--     propagated =
--       Map.map (\c -> if c.^comment_parent == Nothing
--                      then c & comment_thread .~ (Just c.^comment_id)) todo
-- propagateThreads done todo = 
--   uncurry propagateThreads $ Map.partition (isJust . (.^comment_thread)) propagated
--   where
--     propagated =
--       Map.map (\c -> if isJust c.^comment_parent
--                      then c & comment_thread .~ (Map.lookup done   c.^comment_id)) todo
--     propagate c
--       | isJust c.^comment_thread = c
--       | isJust parent && isJust thread = c & comment_thread .~ thread
--       | otherwise = c
--       where
--         parent = c.^comment_parent
--         thread = (.^comment_thread) $ Map.lookup (fromMaybe "unkown" parent) done 
