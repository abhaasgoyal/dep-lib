{-# LANGUAGE TupleSections #-}
module DepGraph
  ( cyclicCheck
  , addDeps
  , makeDepList
  , makeDepGraph
  , Dependencies
  , Graph
  ) where

-- | Usage of map and set data structures to form adjacency sets
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

-- | Dependency from single parent to multiple children
type Dependencies = (T.Text, S.Set T.Text)

-- | Graph structure through adjacency sets
type Graph = M.Map T.Text (S.Set T.Text)

--------------------------------
{--

Cyclic graph check through topological sort (DFS)

--}

-- | Delete minimum leaf node value in graph and remove from adjacency lists
-- Precondition - Atleast 1 leaf node is present
deleteMinLeaf :: Graph -> Graph
deleteMinLeaf g | null g    = M.empty
                | otherwise = M.map (S.delete k) (M.delete k g)
  where (k, _) = M.findMin (M.filter S.null g) -- Precondition must be satisfied

-- | Check whether any leaf node exists
checkLeavesExists :: Graph -> Bool
checkLeavesExists graph = M.size (M.filter S.null graph) > 0

-- | Check if cycles are present
cyclicCheck :: Graph -> Maybe Graph
cyclicCheck g | M.null g                  = Nothing
              | not $ checkLeavesExists g = Just g
              | otherwise                 = cyclicCheck $ deleteMinLeaf g

--------------------------------

-- | Add nodes to graph
addDeps :: Dependencies -> Graph -> Graph
addDeps deps@(_, v) g = uncurry (M.insertWith S.union) deps new_g
 where
  emptyNodes = M.fromList $ (, S.empty) <$> S.toList v
  new_g      = M.unionWith S.union g emptyNodes


-- | Generate input dependency graph
makeDepGraph :: [Dependencies] -> Graph
makeDepGraph = foldr addDeps M.empty

-- | Use adjacency sets to make the dependency list required
-- using DFS
-- Generate output dependency list (based on input key and input graph)
-- Precondition : input must be DAG
makeDepList :: Graph -> T.Text -> S.Set T.Text
makeDepList graph par = case M.lookup par graph of
  Nothing            -> S.empty
  Just neighbourList -> S.union
    neighbourList
    (S.unions $ map (makeDepList graph) (S.toList neighbourList))
