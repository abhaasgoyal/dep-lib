{-# LANGUAGE TupleSections #-}
module DepParser
  ( cyclicCheck
  , addDeps
  , makeDependenciesList
  , Dependencies
  , Graph
  ) where

-- | Usage of map and set data structures to form adjacency sets
import qualified Data.Map as M
                           ( (!)
                           , Map
                           , delete
                           , empty
                           , filter
                           , findMin
                           , insertWith
                           , map
                           , notMember
                           , null
                           , size
                           , fromList
                           , unionWith
                           )
import qualified Data.Set as S
                           ( Set
                           , delete
                           , empty
                           , null
                           , toList
                           , union
                           , unions
                           )

-- | Dependency from single parent to multiple children
type Dependencies = (String, S.Set String)

-- | Graph structure through adjacency sets
type Graph = M.Map String (S.Set String)

--------------------------------
{--

Cyclic graph check through topological sort (DFS)

--}

-- | Delete minimum leaf node value in graph and remove from adjacency lists
-- Precondition - Atleast 1 leaf node is present
deleteMinLeaf :: Graph -> Graph
deleteMinLeaf g | null g    = M.empty
                | otherwise = M.map (S.delete k) (M.delete k g)
  where (k, _) = M.findMin (M.filter S.null g) -- delete with condition of left node

-- | Check whether any leaf node exists
checkLeavesExists :: Graph -> Bool
checkLeavesExists graph = M.size (M.filter S.null graph) > 0

-- | Check if cycles are present
cyclicCheck :: Graph -> Maybe Graph
cyclicCheck g | M.null g                = Nothing
              | not $ checkLeavesExists g = Just g
              | otherwise               = cyclicCheck $ deleteMinLeaf g

--------------------------------

-- | Add nodes to graph
addDeps :: Dependencies -> Graph -> Graph
addDeps deps@(_, v) g = uncurry (M.insertWith S.union) deps new_g
  where
    emptyNodes = M.fromList $ (,S.empty) <$> S.toList v
    new_g = M.unionWith S.union g emptyNodes

-- | Use adjacency sets to make the dependency list required
-- using DFS
-- Precondition : input must be DAG
makeDependenciesList :: Graph -> String -> S.Set String
makeDependenciesList graph par
  | M.notMember par graph = S.empty
  | otherwise = S.union
    neighbourList
    (S.unions $ map (makeDependenciesList graph) (S.toList neighbourList))
  where neighbourList = graph M.! par

