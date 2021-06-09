{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Foldable                  ( traverse_ )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           DepGraph                       ( Dependencies
                                                , Graph
                                                , addDeps
                                                , cyclicCheck
                                                , makeDepList
                                                )
import           ErrorHandling                  ( Error(..) )
import           FileParser                     ( parseInput )
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

-- | List of tests
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
    ]
  , testGroup
    "testAddDep"
    [ testCase "Same Parent New Node"
    $   addDeps ("X", S.fromList ["J"]) inputGraph1
    @?= M.insertWith S.union
                     "X"
                     (S.fromList ["J"])
                     (M.insert "J" S.empty inputGraph1)
    , testCase "Different Parent same node"
    $   addDeps ("S", S.fromList ["B"]) inputGraph2
    @?= M.insertWith S.union "S" (S.fromList ["B"]) inputGraph2
    , testCase "New parent new node"
    $   addDeps ("A", S.fromList ["B", "C", "D"]) inputGraph4
    @?= M.fromList
          [ ("A", S.fromList ["B", "C", "D"])
          , ("B", S.empty)
          , ("C", S.empty)
          , ("D", S.empty)
          ]
    ]
  , testGroup
    "testInput"
    [ testCase "input1" $ parseInput [["H", "depends", "on", "A"]] @?= Right
      [("H", S.fromList ["A"])]
    , testCase "input2"
    $   parseInput [["incorrect", "input"]]
    @?= Left InvalidInputFile
    , -- Treat as (H,[]) in mapping instead of throwing error
      testCase "input3" $ parseInput [["H", "depends", "on"]] @?= Right
      [("H", S.fromList [])]
    , testCase "input4"
    $   parseInput [["H", "depending", "on", "A"]]
    @?= Left InvalidInputFile
    , testCase "input5"
    $   parseInput [["H", "depending", "A"]]
    @?= Left InvalidInputFile
    , testCase "input6"
    $   parseInput [["H", "depends", "on", "A", "B"]]
    @?= Right [("H", S.fromList ["A", "B"])]
    , testCase "input7"
    $   parseInput [["X", "depends", "on", "Y"], ["Y", "depends", "on", "X"]]
    @?= Right [("X", S.fromList ["Y"]), ("Y", S.fromList ["X"])]
    ]
  , testCase "Stress Tests" $ do
    stressTests 300 100
  ]

testMakeDependenciesList :: Graph -> Graph
testMakeDependenciesList graph = M.mapWithKey (const . makeDepList graph) graph

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

-- | Make graph with mandatory empty leaf nodes (even for child dependencies)
makeTestGraph :: [(T.Text, [T.Text])] -> Graph
makeTestGraph = foldr (\(x, y) g -> addDeps (x, S.fromList y) g) M.empty

-- | Stress testing cyclicCheck and makeDepList (the most heavy usage)
stressTests :: Int -> Int -> IO ()
stressTests iterations n_nodes = do
  replicateM_ iterations $ cyclicCheck <$> randomDAG n_nodes
  replicateM_ iterations $ testMakeDependenciesList <$> randomDAG n_nodes
  pure ()


----- Tests helper functions

-- | Generate random DAG based on maximum nodes/ children
randomDAG :: Int -> IO Graph
randomDAG n_nodes = do
  n_nodes_seed <- newStdGen

  let generated_nodes = node_gen n_nodes n_nodes_seed
  node_names              <- generated_nodes
  potential_node_children <- replicateM n_nodes generated_nodes

  -- Using property of DAG of ascending order dependencies
  let finalGraph = map (\(p, c_list) -> (p, filter (> p) c_list))
                       (zip node_names potential_node_children)
  pure $ makeTestGraph finalGraph

-- | Generate n nodes
node_gen :: Int -> StdGen -> IO [T.Text]
node_gen max_nodes seed = do
  let n_nodes = (fst (random seed :: (Int, StdGen))) `mod` max_nodes
  replicateM n_nodes randNodeName

-- | Generate 2 letter random node name
randNodeName :: IO T.Text
randNodeName = do
  seed <- newStdGen
  -- Node names are 2 letters long
  pure (T.pack (take 2 $ randomRs ('a','z') $ seed :: [Char]))
