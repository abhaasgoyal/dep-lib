module DepParser
  ( deleteLeaf
  , checkLeafExists
  , cyclicCheck
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
                           , fromList
                           , insertWith
                           , map
                           , notMember
                           , null
                           , size
                           )
import qualified Data.Set as S
                           ( Set
                           , delete
                           , empty
                           , fromList
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

-- | Delete minimum node value in graph and remove from adjacency lists
-- Predicate - Atleast 1 leaf node is present
deleteLeaf :: Graph -> Graph
deleteLeaf g | null g    = M.empty
             | otherwise = M.map (S.delete k) (M.delete k g)
  where (k, _) = M.findMin (M.filter S.null g) -- delete with condition of left node

checkLeafExists :: Graph -> Bool
checkLeafExists graph = M.size (M.filter S.null graph) > 0

cyclicCheck :: Graph -> Maybe Graph
cyclicCheck g | M.null g                = Nothing
              | not $ checkLeafExists g = Just g
              | otherwise               = cyclicCheck $ deleteLeaf g

--------------------------------

-- | Add nodes to graph
addDeps :: Dependencies -> Graph -> Graph
addDeps deps@(_, v) g = uncurry (M.insertWith $ flip S.union) deps new_g
  where new_g = foldr (\i -> M.insertWith S.union i S.empty) g (S.toList v)

-- | Use adjacency sets to make the dependency list required for assignment
-- Precondition : input must be DAG
makeDependenciesList :: Graph -> String -> S.Set String
makeDependenciesList graph par
  | M.notMember par graph = S.empty
  | otherwise = S.union
    neighbourList
    (S.unions $ map (makeDependenciesList graph) (S.toList neighbourList))
  where neighbourList = graph M.! par

