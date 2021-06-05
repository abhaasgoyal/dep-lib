module DepParser where

import qualified Data.Map                      as M
import qualified Data.Set                      as S

-- | Dependency from single parent to multiple children
type Dependencies = (String, S.Set String)

-- | Dependency from single parent to single child
type Dependency = (String, String)

-- | Graph structure through adjacency sets
type Graph = M.Map String (S.Set String)

--------------------------------
{--

Cyclic graph check through topological sort (DFS)

--}

-- | Delete minimum node value in graph and remove from adjacency lists
deleteNode :: Graph -> Graph
deleteNode g = M.map (S.delete k) deleted_g
  where ((k, _), deleted_g) = M.deleteFindMin g

checkLeaves :: Graph -> Bool
checkLeaves graph = M.size (M.filter S.null graph) > 0

cyclicCheck :: Graph -> Bool
cyclicCheck g | M.null g      = False
              | checkLeaves g = True
              | otherwise     = cyclicCheck $ deleteNode g

--------------------------------

-- | Add nodes to graph
addDeps :: Dependencies -> Graph -> Graph
addDeps = uncurry . M.insertWith $ flip S.union

-- | Use adjacency sets to make a
makeDependenciesList :: Graph -> String -> S.Set String
makeDependenciesList graph par
  | M.notMember par graph = S.empty
  | otherwise = S.union
    neighbourList
    (S.unions $ map (makeDependenciesList graph) (S.toList neighbourList))
  where neighbourList = graph M.! par

-- | Check whether the string "depends on" appears on correct location
checkDependsOn :: [[String]] -> Bool
checkDependsOn = all helpCheckDepend
 where
  helpCheckDepend :: [String] -> Bool
  helpCheckDepend s | (s !! 1) ++ (s !! 2) == "depends on" = True
                    | otherwise                            = False

-- | Model dependencies read from the file
extractDeps :: [[String]] -> [Dependencies]
extractDeps = map (\s -> (head s, S.fromList $ drop 3 s))
