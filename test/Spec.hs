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
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests"
        [testGroup "Conversion" [
           -- testCase "input1" $ (M.mapWithKey (makeDependenciesList inputGraph1) inputGraph1) @?= outputGraph1
                                ]]

inputGraph1 :: Graph
inputGraph1 = makeTestGraph
  [ ("X", ["Y", "R"]),
    ("Y", ["Z"])]

inputGraph2 :: Graph
inputGraph2  = makeTestGraph
  [ ("Y", ["Z"]),
    ("A", ["Q", "R", "S"]),
    ("X", ["Y"]),
    ("Z", ["A", "B"])]


inputGraph3 :: Graph
inputGraph3 = makeTestGraph
  [ ("A", ["B", "C"])
  , ("B", ["C", "E"])
  , ("C", ["G"])
  , ("D", ["A", "F"])
  , ("E", ["F"])
  , ("F", ["H"])
  ]

outputGraph1 :: Graph
outputGraph1 = makeTestGraph
  [ ("X", ["Y", "R", "Z"]),
    ("Y", ["Z"])]

outputGraph2 :: Graph
outputGraph2  = makeTestGraph
  [ ("Y", ["Z"]),
    ("A", ["Q", "R", "S"]),
    ("X", ["Y", "Z"]),
    ("Z", ["A", "B", "Q", "R", "S"])]

outputGraph3 :: Graph
outputGraph3 = makeTestGraph
  [ ("A", ["B", "C", "E", "F", "G", "H"])
  , ("B", ["C", "E", "F", "G", "H"])
  , ("C", ["G"])
  , ("D", ["A", "B", "C", "D", "E", "F", "G", "H"])
  , ("E", ["F"])
  , ("F", ["H"])
  ]

makeTestGraph :: [(String, [String])] -> Graph
makeTestGraph = M.fromList . map (fmap S.fromList)
