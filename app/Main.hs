module Main where
import           Data.Foldable                  ( for_ )
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           DepGraph                       ( Dependencies
                                                , Graph
                                                , cyclicCheck
                                                , makeDepGraph
                                                , makeDepList
                                                )
import           ErrorHandling
import           FileParser                     ( parseInput
                                                , printOutput
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitSuccess )

-- | Driver function
main :: IO ()
main = do
  file           <- gArgs
  dependencyList <- readInput file
  let inputDepOrder    = map fst dependencyList
  let constructedGraph = makeDepGraph dependencyList
  acyclicCheck constructedGraph
  calcAndPrintResult inputDepOrder constructedGraph
  exitSuccess

-- | Get filename
gArgs :: IO String -- String is ok instead of T.Text since a single filename
gArgs = do
  args <- getArgs
  case args of
    [fileName] -> pure fileName
    _          -> do
      handleError InvalidArgs
      pure []

-- | Read filename
readInput :: String -> IO [Dependencies]
readInput file = do
  content <- map T.words . T.lines <$> TIO.readFile file
  case parseInput content of
    Left err -> do
      handleError err
      pure []
    Right dependencyList -> pure dependencyList

-- | Checking whether constructed graph is acyclic
acyclicCheck :: Graph -> IO ()
acyclicCheck graph =
  for_ (cyclicCheck graph) (handleError . CircularDependency)

-- | Calculate using `makeDepList` and print out the final result
-- by traversing over input keys in order
calcAndPrintResult :: [T.Text] -> Graph -> IO ()
calcAndPrintResult inputDepOrder graph =
  for_ inputDepOrder $ \i -> printOutput i . S.toList $ makeDepList graph i
