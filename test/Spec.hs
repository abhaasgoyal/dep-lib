module Main where

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
import           Test.Tasty
import           Test.Tasty.HUnit

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
  ,
    testGroup
      "testCyclicCheck"
      [ testCase "input1" $ cyclicCheck inputGraph1 @?= False
      , testCase "input2" $ cyclicCheck inputGraph2 @?= False
      , testCase "input3" $ cyclicCheck inputGraph3 @?= False
      , testCase "input4" $ cyclicCheck inputGraph4 @?= False
      , testCase "inputCycle1" $ cyclicCheck inputCycle1 @?= True
      , testCase "inputCycle2" $ cyclicCheck inputCycle2 @?= True
      , testCase "inputCycle3" $ cyclicCheck inputCycle3 @?= True
      ]
  ]

testMakeDependenciesList :: Graph -> Graph
testMakeDependenciesList graph = M.mapWithKey (const . makeDependenciesList graph) graph

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
makeTestGraph = foldr (\(x,y) g -> addDeps (x, S.fromList y) g) M.empty
