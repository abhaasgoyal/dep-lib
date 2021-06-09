module Main where

import           Control.Monad
import           Data.Foldable                  ( traverse_ )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           DepParser                      ( Dependencies
                                                , Graph
                                                , addDeps
                                                , cyclicCheck
                                                , makeDependenciesList
                                                )
import           FileHandlers                   ( checkInput
                                                , extractDepsFromFile
                                                , printOutput
                                                )
import           System.Exit                    ( exitFailure )
import           System.Random                  ( StdGen
                                                , newStdGen
                                                , random
                                                , randomR
                                                , randomRs
                                                )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup
  "Unit Tests"
  [ testGroup
    "testMakeDependenciesList"
    [ testCase "input1" $ testMakeDependenciesList inputGraph1 @?= outputGraph1
    , testCase "input2" $ testMakeDependenciesList inputGraph2 @?= outputGraph2
    , testCase "input3" $ testMakeDependenciesList inputGraph3 @?= outputGraph3
    , testCase "input4" $ testMakeDependenciesList inputGraph4 @?= outputGraph4
    ]
  , testGroup
    "testCyclicCheck"
    [ testCase "input1" $ cyclicCheck inputGraph1 @?= Nothing
    , testCase "input2" $ cyclicCheck inputGraph2 @?= Nothing
    , testCase "input3" $ cyclicCheck inputGraph3 @?= Nothing
    , testCase "input4" $ cyclicCheck inputGraph4 @?= Nothing
    , testCase "inputCycle1" $ cyclicCheck inputCycle1 @?= Just
      (makeTestGraph [("X", ["Y"]), ("Y", ["X"])])
    , testCase "inputCycle2" $ cyclicCheck inputCycle2 @?= Just
      (makeTestGraph [("A", ["Z"]), ("X", ["Y"]), ("Y", ["Z"]), ("Z", ["A"])])
    , testCase "inputCycle3" $ cyclicCheck inputCycle3 @?= Just
      (makeTestGraph [("A", ["B"]), ("B", ["E"]), ("D", ["A"]), ("E", ["D"])])
    , testCase "Stress Tests" $ do
      stressTests
    ]
  ]

testMakeDependenciesList :: Graph -> Graph
testMakeDependenciesList graph =
  M.mapWithKey (const . makeDependenciesList graph) graph

-- These graphs are known to be acyclic
inputGraph1 :: Graph
inputGraph1 = makeTestGraph [("X", ["Y", "R"]), ("Y", ["Z"])]

inputGraph2 :: Graph
inputGraph2 = makeTestGraph
  [("Y", ["Z"]), ("A", ["Q", "R", "S"]), ("X", ["Y"]), ("Z", ["A", "B"])]


inputGraph3 :: Graph
inputGraph3 = makeTestGraph
  [ ("A", ["B", "C"])
  , ("B", ["C", "E"])
  , ("C", ["G"])
  , ("D", ["A", "F"])
  , ("E", ["F"])
  , ("F", ["H"])
  ]

inputGraph4 :: Graph
inputGraph4 = makeTestGraph []

outputGraph1 :: Graph
outputGraph1 = makeTestGraph [("X", ["Y", "R", "Z"]), ("Y", ["Z"])]

outputGraph2 :: Graph
outputGraph2 = makeTestGraph
  [ ("Y", ["A", "B", "Q", "R", "S", "Z"])
  , ("A", ["Q", "R", "S"])
  , ("X", ["A", "B", "Q", "R", "S", "Y", "Z"])
  , ("Z", ["A", "B", "Q", "R", "S"])
  ]

outputGraph3 :: Graph
outputGraph3 = makeTestGraph
  [ ("A", ["B", "C", "E", "F", "G", "H"])
  , ("B", ["C", "E", "F", "G", "H"])
  , ("C", ["G"])
  , ("D", ["A", "B", "C", "E", "F", "G", "H"])
  , ("E", ["F", "H"])
  , ("F", ["H"])
  ]

outputGraph4 :: Graph
outputGraph4 = makeTestGraph []

-- These graphs are known to be acyclic
inputCycle1 :: Graph
inputCycle1 = makeTestGraph [("X", ["Y"]), ("Y", ["X"])]

inputCycle2 :: Graph
inputCycle2 = makeTestGraph
  [("Y", ["Z"]), ("A", ["Q", "R", "S", "Z"]), ("X", ["Y"]), ("Z", ["A", "B"])]


inputCycle3 :: Graph
inputCycle3 = makeTestGraph
  [ ("A", ["B", "C"])
  , ("B", ["C", "E"])
  , ("C", ["G"])
  , ("D", ["A", "F"])
  , ("E", ["F", "D"]) -- D acts as cycle component
  , ("F", ["H"])
  ]

makeTestGraph :: [(String, [String])] -> Graph
makeTestGraph = foldr (\(x, y) g -> addDeps (x, S.fromList y) g) M.empty

stressTests :: IO ()
stressTests = do
  replicateM_ 200 (fmap cyclicCheck $ randomDAG 100)
  replicateM_ 200 (fmap testMakeDependenciesList $ randomDAG 100) -- max_nodes = 100, max_child_nodes = 100
  return ()

randomDAG :: Int -> IO Graph
randomDAG n_nodes = do
  a            <- randNodeName
  n_nodes_seed <- newStdGen

  let generated_nodes = node_gen n_nodes n_nodes_seed
  node_names              <- generated_nodes
  potential_node_children <- replicateM n_nodes generated_nodes

  -- Using property of DAG of ascending order dependencies
  let finalGraph = map (\(p, c_list) -> (p, filter (> p) c_list))
                       (zip node_names potential_node_children)
  return $ makeTestGraph finalGraph

node_gen :: Int -> StdGen -> IO [String]
node_gen max_nodes seed = do
  let n_nodes = (fst (random seed :: (Int, StdGen))) `mod` max_nodes
  replicateM n_nodes randNodeName

randNodeName :: IO String
randNodeName = do
  seed <- newStdGen
  -- Node names are 2 letters long
  return (take 2 $ randomRs ('a', 'z') $ seed :: [Char])
