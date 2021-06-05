module DepParser where

import qualified Data.Map                      as M
import qualified Data.Set                      as S

type Dependencies = (String, S.Set String)
type Dependency = (String, String)
type Graph = M.Map String (S.Set String)

addDeps :: Dependencies -> Graph -> Graph
addDeps = uncurry . M.insertWith $ flip S.union

delDep :: Graph -> Dependency -> Graph
delDep = undefined

checkDep :: Graph -> Dependency -> Graph
checkDep = undefined

cyclicCheck :: Graph -> Bool
cyclicCheck g | M.null g  = False
              | otherwise = True

makeDependenciesList :: Graph -> String -> S.Set String
makeDependenciesList graph par
  | M.notMember par graph = S.empty
  | otherwise = S.union
    neighbourList
    (S.unions $ map (makeDependenciesList graph) (S.toList neighbourList))
  where neighbourList = graph M.! par

addEdge :: Graph -> Graph
addEdge = undefined

checkDependsOn :: [[String]] -> Bool
checkDependsOn = all helpCheckDepend
 where
  helpCheckDepend :: [String] -> Bool
  helpCheckDepend s | (s !! 1) ++ (s !! 2) == "depends on" = True
                    | otherwise                            = False

extractDeps :: [[String]] -> [Dependencies]
extractDeps = map (\s -> (head s, S.fromList $ drop 3 s))
